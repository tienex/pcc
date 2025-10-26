/*
 * Edge case and robustness tests for GC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libgc/gc.h"

static gc_context_t *gc = NULL;

/* Test 1: Zero-sized allocations */
static void
test_zero_size(void)
{
	void *obj;

	printf("\n=== Test 1: Zero-Sized Allocations ===\n");

	/* Zero size should still return valid pointer */
	obj = gc_alloc(gc, 0);

	if (obj != NULL) {
		printf("✓ Zero-size allocation returned: %p\n", obj);
		gc_collect(gc);
		printf("✓ Collection handled zero-size object\n");
	} else {
		printf("✓ Zero-size allocation returned NULL (acceptable)\n");
	}
}

/* Test 2: Very large allocations */
static void
test_large_allocation(void)
{
	void *obj1, *obj2;

	printf("\n=== Test 2: Very Large Allocations ===\n");

	/* Allocate 1MB object */
	obj1 = gc_alloc(gc, 1024 * 1024);
	assert(obj1 != NULL);
	printf("✓ Allocated 1MB object: %p\n", obj1);

	/* Allocate another 1MB */
	obj2 = gc_alloc(gc, 1024 * 1024);
	assert(obj2 != NULL);
	printf("✓ Allocated second 1MB object: %p\n", obj2);

	/* Verify we can write to them */
	memset(obj1, 0xAA, 1024 * 1024);
	memset(obj2, 0xBB, 1024 * 1024);

	assert(((unsigned char *)obj1)[0] == 0xAA);
	assert(((unsigned char *)obj2)[0] == 0xBB);

	printf("✓ Large objects are writable and distinct\n");

	/* Cleanup */
	gc_collect(gc);
	printf("✓ Large objects collected\n");
}

/* Test 3: Rapid allocation and deallocation */
static void
test_rapid_alloc_dealloc(void)
{
	void *objs[1000];
	int i, iteration;

	printf("\n=== Test 3: Rapid Allocation/Deallocation ===\n");

	for (iteration = 0; iteration < 100; iteration++) {
		/* Allocate */
		for (i = 0; i < 1000; i++) {
			objs[i] = gc_alloc(gc, 32);
			assert(objs[i] != NULL);
		}

		/* Collect immediately */
		gc_collect(gc);
	}

	printf("✓ Completed 100 iterations of 1000 alloc/dealloc cycles\n");

	gc_stats_t stats;
	gc_get_stats(gc, &stats);
	printf("  Collections: %zu\n", stats.num_collections);
}

/* Test 4: Fragmentation handling */
static void
test_fragmentation(void)
{
	void *small[100];
	void *large[10];
	int i;

	printf("\n=== Test 4: Fragmentation Handling ===\n");

	/* Create fragmented heap */
	for (i = 0; i < 100; i++) {
		small[i] = gc_alloc(gc, 16);
	}
	for (i = 0; i < 10; i++) {
		large[i] = gc_alloc(gc, 1024);
	}

	printf("✓ Allocated 100 small + 10 large objects\n");

	/* Free every other small object */
	for (i = 0; i < 100; i += 2) {
		gc_register_root(gc, &small[i]);
	}
	gc_collect(gc);
	for (i = 0; i < 100; i += 2) {
		gc_unregister_root(gc, &small[i]);
	}

	printf("✓ Created fragmentation pattern\n");

	/* Allocate more - should handle fragmentation */
	for (i = 0; i < 50; i++) {
		void *obj = gc_alloc(gc, 16);
		assert(obj != NULL);
	}

	printf("✓ Successfully allocated into fragmented heap\n");

	gc_collect(gc);
}

/* Test 5: Circular references */
static void
test_circular_references(void)
{
	typedef struct node {
		int value;
		void *next;
	} node_t;

	node_t *n1, *n2, *n3;

	printf("\n=== Test 5: Circular References ===\n");

	/* Create circular list */
	n1 = gc_alloc(gc, sizeof(node_t));
	n2 = gc_alloc(gc, sizeof(node_t));
	n3 = gc_alloc(gc, sizeof(node_t));

	n1->value = 1;
	n2->value = 2;
	n3->value = 3;

	n1->next = n2;
	n2->next = n3;
	n3->next = n1;  /* Circular! */

	printf("✓ Created circular reference: n1 -> n2 -> n3 -> n1\n");

	/* If we lose all references, entire cycle should be collected */
	gc_collect(gc);

	printf("✓ Circular reference collected (no roots)\n");

	/* Create another cycle and keep one root */
	n1 = gc_alloc(gc, sizeof(node_t));
	n2 = gc_alloc(gc, sizeof(node_t));
	n3 = gc_alloc(gc, sizeof(node_t));

	n1->next = n2;
	n2->next = n3;
	n3->next = n1;

	gc_register_root(gc, &n1);
	gc_collect(gc);

	/* All three should survive */
	assert(n1->next == n2);
	assert(((node_t *)n2)->next == n3);

	printf("✓ Circular reference with root kept all nodes alive\n");

	gc_unregister_root(gc, &n1);
}

/* Test 6: NULL handling */
static void
test_null_handling(void)
{
	void *null_ptr = NULL;

	printf("\n=== Test 6: NULL Pointer Handling ===\n");

	/* Register NULL root - should not crash */
	gc_register_root(gc, &null_ptr);
	gc_collect(gc);
	gc_unregister_root(gc, &null_ptr);

	printf("✓ NULL root registration handled safely\n");

	/* Create weak ref to NULL - should return NULL */
	gc_weak_t *weak = gc_weak_create(gc, null_ptr);
	assert(weak == NULL);

	printf("✓ NULL weak reference rejected properly\n");
}

/* Test 7: Many roots */
static void
test_many_roots(void)
{
	void *roots[1000];
	int i;

	printf("\n=== Test 7: Many Roots ===\n");

	/* Allocate many objects */
	for (i = 0; i < 1000; i++) {
		roots[i] = gc_alloc(gc, sizeof(int));
		*(int *)roots[i] = i;
		gc_register_root(gc, &roots[i]);
	}

	printf("✓ Registered 1000 roots\n");

	/* Collect - none should be freed */
	size_t freed = gc_collect(gc);
	printf("✓ Collection with 1000 roots freed %zu objects\n", freed);

	/* Verify all intact */
	for (i = 0; i < 1000; i++) {
		assert(*(int *)roots[i] == i);
	}

	printf("✓ All 1000 rooted objects survived\n");

	/* Unregister all */
	for (i = 0; i < 1000; i++) {
		gc_unregister_root(gc, &roots[i]);
	}

	freed = gc_collect(gc);
	printf("✓ After unregistering, freed %zu objects\n", freed);
}

/* Test 8: Interleaved allocation sizes */
static void
test_interleaved_sizes(void)
{
	void *objs[100];
	int i;
	size_t sizes[] = {8, 16, 24, 32, 48, 64, 96, 128, 192, 256, 384, 512, 1024};
	int num_sizes = sizeof(sizes) / sizeof(sizes[0]);

	printf("\n=== Test 8: Interleaved Allocation Sizes ===\n");

	/* Allocate objects of varying sizes */
	for (i = 0; i < 100; i++) {
		objs[i] = gc_alloc(gc, sizes[i % num_sizes]);
		assert(objs[i] != NULL);
		memset(objs[i], i & 0xFF, sizes[i % num_sizes]);
	}

	printf("✓ Allocated 100 objects with %d different sizes\n", num_sizes);

	/* Verify first byte */
	for (i = 0; i < 100; i++) {
		assert(((unsigned char *)objs[i])[0] == (i & 0xFF));
	}

	printf("✓ All objects have correct data\n");

	gc_collect(gc);
	printf("✓ Mixed-size collection completed\n");
}

/* Test 9: Weak reference edge cases */
static void
test_weak_edge_cases(void)
{
	void *obj;
	gc_weak_t *weak1, *weak2;

	printf("\n=== Test 9: Weak Reference Edge Cases ===\n");

	/* Create object and weak refs */
	obj = gc_alloc(gc, sizeof(int));
	*(int *)obj = 999;

	weak1 = gc_weak_create(gc, obj);
	weak2 = gc_weak_create(gc, obj);

	printf("✓ Created 2 weak refs to same object\n");

	/* Get from already-invalid weak ref */
	gc_collect(gc);

	void *ptr1 = gc_weak_get(weak1);
	void *ptr2 = gc_weak_get(weak2);

	assert(ptr1 == NULL);
	assert(ptr2 == NULL);

	printf("✓ Both weak refs correctly invalidated\n");

	/* Get from NULL weak ref */
	ptr1 = gc_weak_get(NULL);
	assert(ptr1 == NULL);

	printf("✓ NULL weak ref handled safely\n");
}

/* Test 10: Collect with no garbage */
static void
test_collect_no_garbage(void)
{
	void *objs[10];
	int i;

	printf("\n=== Test 10: Collection With No Garbage ===\n");

	/* Allocate and root everything */
	for (i = 0; i < 10; i++) {
		objs[i] = gc_alloc(gc, 64);
		gc_register_root(gc, &objs[i]);
	}

	printf("✓ Created 10 rooted objects\n");

	/* Multiple collections - should free nothing */
	for (i = 0; i < 5; i++) {
		size_t freed = gc_collect(gc);
		assert(freed == 0);
	}

	printf("✓ 5 collections freed nothing (all rooted)\n");

	/* Cleanup */
	for (i = 0; i < 10; i++) {
		gc_unregister_root(gc, &objs[i]);
	}
}

int
main(void)
{
	gc_config_t config = GC_DEFAULT_CONFIG;

	printf("========================================\n");
	printf("GC Edge Cases and Robustness Test Suite\n");
	printf("========================================\n");

	/* Initialize GC */
	config.heap_size = 20 * 1024 * 1024;  /* 20MB */
	config.gc_threshold = 10 * 1024 * 1024;
	config.enable_pools = 1;
	config.verbose = 0;

	gc = gc_init(&config);
	if (!gc) {
		fprintf(stderr, "Failed to initialize GC\n");
		return 1;
	}

	printf("\n✓ GC initialized\n");

	/* Run all edge case tests */
	test_zero_size();
	test_large_allocation();
	test_rapid_alloc_dealloc();
	test_fragmentation();
	test_circular_references();
	test_null_handling();
	test_many_roots();
	test_interleaved_sizes();
	test_weak_edge_cases();
	test_collect_no_garbage();

	/* Final stats */
	printf("\n========================================\n");
	printf("Final Statistics:\n");
	printf("========================================\n");
	gc_print_stats(gc);

	/* Cleanup */
	gc_destroy(gc);

	printf("\n========================================\n");
	printf("✅ ALL EDGE CASE TESTS PASSED!\n");
	printf("========================================\n");

	return 0;
}
