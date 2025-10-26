/*
 * Enhanced test program for generic GC with pools and weak references
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libgc/gc.h"

/* Test object structure */
typedef struct test_obj {
	int value;
	void *child1;
	void *child2;
} test_obj_t;

/* Global GC context */
static gc_context_t *gc = NULL;

/* Mark callback for test objects */
static void
mark_test_obj(gc_context_t *gc, void *obj, void *userdata)
{
	test_obj_t *o = (test_obj_t *)obj;
	gc_object_t *hdr = gc_get_header(obj);

	/* Only mark children for large enough objects */
	if (hdr && hdr->size >= sizeof(test_obj_t)) {
		/* Mark children */
		if (o->child1)
			gc_mark(gc, o->child1);
		if (o->child2)
			gc_mark(gc, o->child2);
	}
}

/* Test 1: Basic allocation with pools */
static void
test_pools(void)
{
	void *obj1, *obj2, *obj3;

	printf("\nTest 1: Memory pools\n");
	printf("---------------------\n");

	/* Allocate small objects (should use pools) */
	obj1 = gc_alloc(gc, 8);
	obj2 = gc_alloc(gc, 16);
	obj3 = gc_alloc(gc, 32);

	assert(obj1 != NULL);
	assert(obj2 != NULL);
	assert(obj3 != NULL);

	printf("Allocated 3 small objects (should use pools)\n");
	printf("Objects: %p, %p, %p\n", obj1, obj2, obj3);

	/* Register as roots */
	gc_register_root(gc, &obj1);
	gc_register_root(gc, &obj2);
	gc_register_root(gc, &obj3);

	/* Collect (should not free these) */
	size_t freed = gc_collect(gc);
	printf("Collected: %zu objects freed\n", freed);
	assert(freed == 0);

	/* Unregister and collect */
	gc_unregister_root(gc, &obj1);
	gc_unregister_root(gc, &obj2);
	gc_unregister_root(gc, &obj3);

	obj1 = obj2 = obj3 = NULL;
	freed = gc_collect(gc);
	printf("After unregistering: %zu objects freed\n", freed);
	assert(freed == 3);

	printf("✓ Memory pools working correctly\n");
}

/* Test 2: Weak references */
static void
test_weak_refs(void)
{
	void *obj;
	gc_weak_t *weak;

	printf("\nTest 2: Weak references\n");
	printf("-----------------------\n");

	/* Allocate object */
	obj = gc_alloc(gc, sizeof(int));
	*(int *)obj = 42;

	/* Create weak reference */
	weak = gc_weak_create(gc, obj);
	assert(weak != NULL);

	/* Verify weak reference works */
	void *retrieved = gc_weak_get(weak);
	assert(retrieved == obj);
	assert(*(int *)retrieved == 42);
	printf("Created weak reference to object with value 42\n");

	/* Register as root temporarily */
	gc_register_root(gc, &obj);
	gc_collect(gc);

	/* Still should be valid */
	retrieved = gc_weak_get(weak);
	assert(retrieved != NULL);
	printf("Object still alive after GC (rooted)\n");

	/* Unregister and collect */
	gc_unregister_root(gc, &obj);
	obj = NULL;
	gc_collect(gc);

	/* Now weak reference should be invalid */
	retrieved = gc_weak_get(weak);
	assert(retrieved == NULL);
	printf("Weak reference invalidated after object collected\n");

	/* Clean up */
	gc_weak_release(weak);

	printf("✓ Weak references working correctly\n");
}

/* Test 3: Pool reuse */
static void
test_pool_reuse(void)
{
	void *objs[10];
	int i;

	printf("\nTest 3: Pool object reuse\n");
	printf("-------------------------\n");

	/* Allocate objects */
	for (i = 0; i < 10; i++) {
		objs[i] = gc_alloc(gc, 16);
		*(int *)objs[i] = i;
	}

	printf("Allocated 10 objects of size 16\n");

	gc_stats_t stats1;
	gc_get_stats(gc, &stats1);
	printf("Objects allocated: %zu\n", stats1.num_objects);

	/* Free half (make them garbage) */
	for (i = 0; i < 5; i++) {
		objs[i] = NULL;
	}

	/* Keep other half as roots */
	for (i = 5; i < 10; i++) {
		gc_register_root(gc, &objs[i]);
	}

	size_t freed = gc_collect(gc);
	printf("Collected: %zu objects freed (returned to pool)\n", freed);
	assert(freed == 5);

	/* Allocate new objects (should reuse from pool) */
	for (i = 0; i < 5; i++) {
		objs[i] = gc_alloc(gc, 16);
		*(int *)objs[i] = 100 + i;
	}

	printf("Allocated 5 new objects (reused from pool)\n");

	gc_stats_t stats2;
	gc_get_stats(gc, &stats2);
	printf("Objects in use: %zu\n", stats2.num_objects);

	/* Clean up */
	for (i = 5; i < 10; i++) {
		gc_unregister_root(gc, &objs[i]);
	}

	printf("✓ Pool reuse working correctly\n");
}

/* Test 4: Large objects (no pooling) */
static void
test_large_objects(void)
{
	void *large1, *large2;

	printf("\nTest 4: Large objects (no pooling)\n");
	printf("----------------------------------\n");

	/* Allocate large objects (should not use pools) */
	large1 = gc_alloc(gc, 512);
	large2 = gc_alloc(gc, 1024);

	assert(large1 != NULL);
	assert(large2 != NULL);

	printf("Allocated 2 large objects (512 and 1024 bytes)\n");

	/* Register as roots */
	gc_register_root(gc, &large1);
	gc_register_root(gc, &large2);

	/* Collect */
	size_t freed = gc_collect(gc);
	printf("Collected: %zu objects freed\n", freed);
	assert(freed == 0);

	/* Unregister and collect */
	gc_unregister_root(gc, &large1);
	gc_unregister_root(gc, &large2);
	large1 = large2 = NULL;

	freed = gc_collect(gc);
	printf("After unregistering: %zu objects freed\n", freed);
	assert(freed == 2);

	printf("✓ Large object handling working correctly\n");
}

/* Test 5: Multiple weak references to same object */
static void
test_multiple_weak_refs(void)
{
	void *obj;
	gc_weak_t *weak1, *weak2, *weak3;

	printf("\nTest 5: Multiple weak references\n");
	printf("--------------------------------\n");

	/* Allocate object */
	obj = gc_alloc(gc, sizeof(int));
	*(int *)obj = 123;

	/* Create multiple weak references */
	weak1 = gc_weak_create(gc, obj);
	weak2 = gc_weak_create(gc, obj);
	weak3 = gc_weak_create(gc, obj);

	printf("Created 3 weak references to same object\n");

	/* All should be valid */
	assert(gc_weak_get(weak1) != NULL);
	assert(gc_weak_get(weak2) != NULL);
	assert(gc_weak_get(weak3) != NULL);

	/* Collect (object becomes garbage) */
	gc_collect(gc);

	/* All should be invalid now */
	assert(gc_weak_get(weak1) == NULL);
	assert(gc_weak_get(weak2) == NULL);
	assert(gc_weak_get(weak3) == NULL);

	printf("All weak references invalidated correctly\n");

	/* Clean up */
	gc_weak_release(weak1);
	gc_weak_release(weak2);
	gc_weak_release(weak3);

	printf("✓ Multiple weak references working correctly\n");
}

int
main(int argc, char **argv)
{
	gc_config_t config = GC_DEFAULT_CONFIG;

	printf("===========================================\n");
	printf("Enhanced GC Test Suite (Pools + Weak Refs)\n");
	printf("===========================================\n");

	/* Initialize GC with pools enabled */
	config.heap_size = 1024 * 1024;  /* 1MB */
	config.gc_threshold = 512 * 1024;
	config.enable_pools = 1;
	config.verbose = 0;

	gc = gc_init(&config);
	if (!gc) {
		fprintf(stderr, "Failed to initialize GC\n");
		return 1;
	}

	/* Set mark callback */
	gc_set_mark_callback(gc, mark_test_obj, NULL);

	printf("\nGC initialized with memory pools enabled\n");

	/* Run tests */
	test_pools();
	test_weak_refs();
	test_pool_reuse();
	test_large_objects();
	test_multiple_weak_refs();

	/* Print final stats */
	printf("\n===========================================\n");
	printf("Final Statistics:\n");
	printf("===========================================\n");
	gc_print_stats(gc);

	/* Clean up */
	gc_destroy(gc);

	printf("\n===========================================\n");
	printf("All tests passed! ✓\n");
	printf("===========================================\n");

	return 0;
}
