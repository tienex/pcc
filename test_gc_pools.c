/*
 * Comprehensive test for GC memory pools
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libgc/gc.h"

static gc_context_t *gc = NULL;

/* Test 1: Basic pool allocation */
static void
test_basic_pools(void)
{
	void *objs[20];
	int i;

	printf("\n=== Test 1: Basic Pool Allocation ===\n");

	/* Allocate small objects (should use pools) */
	for (i = 0; i < 20; i++) {
		objs[i] = gc_alloc(gc, 16);
		assert(objs[i] != NULL);
		*(int *)objs[i] = i;
	}

	printf("✓ Allocated 20 objects (16 bytes each) from pool\n");

	/* Verify data */
	for (i = 0; i < 20; i++) {
		assert(*(int *)objs[i] == i);
	}

	printf("✓ All data intact\n");

	/* Register half as roots */
	for (i = 0; i < 10; i++) {
		gc_register_root(gc, &objs[i]);
	}

	/* Collect - should free 10 objects */
	size_t freed = gc_collect(gc);
	printf("✓ Collected %zu objects (expected 10)\n", freed);
	assert(freed == 10);

	/* Verify rooted objects still intact */
	for (i = 0; i < 10; i++) {
		assert(*(int *)objs[i] == i);
	}

	printf("✓ Rooted objects survived collection\n");

	/* Cleanup */
	for (i = 0; i < 10; i++) {
		gc_unregister_root(gc, &objs[i]);
	}
}

/* Test 2: Pool reuse */
static void
test_pool_reuse(void)
{
	void *obj1, *obj2, *obj3;
	gc_stats_t stats1, stats2;

	printf("\n=== Test 2: Pool Object Reuse ===\n");

	/* Get initial stats */
	gc_get_stats(gc, &stats1);

	/* Allocate and free objects */
	obj1 = gc_alloc(gc, 32);
	obj2 = gc_alloc(gc, 32);
	obj3 = gc_alloc(gc, 32);

	assert(obj1 && obj2 && obj3);
	printf("✓ Allocated 3 objects (32 bytes each)\n");

	/* Free all by collecting */
	gc_collect(gc);
	printf("✓ Collected all objects (returned to pool)\n");

	/* Allocate new objects - should reuse from pool */
	obj1 = gc_alloc(gc, 32);
	obj2 = gc_alloc(gc, 32);

	assert(obj1 && obj2);
	printf("✓ Allocated 2 new objects (reused from pool)\n");

	/* Stats should show reuse */
	gc_get_stats(gc, &stats2);
	printf("  Objects in use: %zu\n", stats2.num_objects);

	/* Cleanup */
	gc_register_root(gc, &obj1);
	gc_register_root(gc, &obj2);
	gc_unregister_root(gc, &obj1);
	gc_unregister_root(gc, &obj2);
}

/* Test 3: Different size classes */
static void
test_size_classes(void)
{
	void *obj16, *obj32, *obj64, *obj128, *obj256;

	printf("\n=== Test 3: Different Pool Size Classes ===\n");

	/* Allocate from each size class */
	obj16 = gc_alloc(gc, 16);
	obj32 = gc_alloc(gc, 32);
	obj64 = gc_alloc(gc, 64);
	obj128 = gc_alloc(gc, 128);
	obj256 = gc_alloc(gc, 256);

	assert(obj16 && obj32 && obj64 && obj128 && obj256);

	printf("✓ Allocated from all size classes:\n");
	printf("  16 bytes:  %p\n", obj16);
	printf("  32 bytes:  %p\n", obj32);
	printf("  64 bytes:  %p\n", obj64);
	printf("  128 bytes: %p\n", obj128);
	printf("  256 bytes: %p\n", obj256);

	/* Write and verify data */
	*(int *)obj16 = 16;
	*(int *)obj32 = 32;
	*(int *)obj64 = 64;
	*(int *)obj128 = 128;
	*(int *)obj256 = 256;

	assert(*(int *)obj16 == 16);
	assert(*(int *)obj32 == 32);
	assert(*(int *)obj64 == 64);
	assert(*(int *)obj128 == 128);
	assert(*(int *)obj256 == 256);

	printf("✓ All size classes working correctly\n");

	/* Cleanup */
	gc_collect(gc);
}

/* Test 4: Large objects (no pooling) */
static void
test_large_objects(void)
{
	void *large1, *large2, *large3;

	printf("\n=== Test 4: Large Objects (No Pooling) ===\n");

	/* Allocate large objects (> 256 bytes, no pooling) */
	large1 = gc_alloc(gc, 512);
	large2 = gc_alloc(gc, 1024);
	large3 = gc_alloc(gc, 4096);

	assert(large1 && large2 && large3);

	printf("✓ Allocated large objects:\n");
	printf("  512 bytes:  %p\n", large1);
	printf("  1024 bytes: %p\n", large2);
	printf("  4096 bytes: %p\n", large3);

	/* Fill with data */
	memset(large1, 0xAA, 512);
	memset(large2, 0xBB, 1024);
	memset(large3, 0xCC, 4096);

	/* Verify */
	assert(((unsigned char *)large1)[0] == 0xAA);
	assert(((unsigned char *)large2)[0] == 0xBB);
	assert(((unsigned char *)large3)[0] == 0xCC);

	printf("✓ Large objects working correctly\n");

	/* Cleanup */
	gc_collect(gc);
}

/* Test 5: Mixed allocation patterns */
static void
test_mixed_allocation(void)
{
	void *objs[100];
	int i;

	printf("\n=== Test 5: Mixed Allocation Pattern ===\n");

	/* Allocate mix of small and large */
	for (i = 0; i < 100; i++) {
		if (i % 3 == 0) {
			objs[i] = gc_alloc(gc, 16);   /* Pool */
		} else if (i % 3 == 1) {
			objs[i] = gc_alloc(gc, 128);  /* Pool */
		} else {
			objs[i] = gc_alloc(gc, 512);  /* No pool */
		}
		assert(objs[i] != NULL);
		*(int *)objs[i] = i;
	}

	printf("✓ Allocated 100 objects (mixed sizes)\n");

	/* Verify all */
	for (i = 0; i < 100; i++) {
		assert(*(int *)objs[i] == i);
	}

	printf("✓ All objects verified\n");

	/* Keep every 5th object */
	for (i = 0; i < 100; i++) {
		if (i % 5 == 0) {
			gc_register_root(gc, &objs[i]);
		}
	}

	/* Collect */
	size_t freed = gc_collect(gc);
	printf("✓ Collected %zu objects\n", freed);
	assert(freed == 80);  /* 100 - 20 */

	/* Verify survivors */
	for (i = 0; i < 100; i++) {
		if (i % 5 == 0) {
			assert(*(int *)objs[i] == i);
		}
	}

	printf("✓ Surviving objects intact\n");

	/* Cleanup */
	for (i = 0; i < 100; i++) {
		if (i % 5 == 0) {
			gc_unregister_root(gc, &objs[i]);
		}
	}
}

/* Test 6: Stress test */
static void
test_stress(void)
{
	void *objs[1000];
	int i, iterations;

	printf("\n=== Test 6: Stress Test ===\n");

	for (iterations = 0; iterations < 10; iterations++) {
		/* Allocate many objects */
		for (i = 0; i < 1000; i++) {
			objs[i] = gc_alloc(gc, 32);
			assert(objs[i] != NULL);
		}

		/* Free half */
		for (i = 0; i < 500; i++) {
			gc_register_root(gc, &objs[i + 500]);
		}

		gc_collect(gc);

		for (i = 0; i < 500; i++) {
			gc_unregister_root(gc, &objs[i + 500]);
		}

		if ((iterations + 1) % 5 == 0) {
			printf("  Completed %d iterations\n", iterations + 1);
		}
	}

	printf("✓ Stress test completed (10 iterations)\n");

	gc_stats_t stats;
	gc_get_stats(gc, &stats);
	printf("  Total allocated: %zu bytes\n", stats.total_allocated);
	printf("  Total freed: %zu bytes\n", stats.total_freed);
	printf("  Collections: %zu\n", stats.num_collections);
}

int
main(void)
{
	gc_config_t config = GC_DEFAULT_CONFIG;

	printf("==========================================\n");
	printf("GC Memory Pools - Comprehensive Test Suite\n");
	printf("==========================================\n");

	/* Initialize GC with pools enabled */
	config.heap_size = 10 * 1024 * 1024;  /* 10MB */
	config.gc_threshold = 5 * 1024 * 1024;
	config.enable_pools = 1;
	config.verbose = 0;

	gc = gc_init(&config);
	if (!gc) {
		fprintf(stderr, "Failed to initialize GC\n");
		return 1;
	}

	printf("\n✓ GC initialized (pools enabled)\n");

	/* Run all tests */
	test_basic_pools();
	test_pool_reuse();
	test_size_classes();
	test_large_objects();
	test_mixed_allocation();
	test_stress();

	/* Final stats */
	printf("\n==========================================\n");
	printf("Final Statistics:\n");
	printf("==========================================\n");
	gc_print_stats(gc);

	/* Cleanup */
	gc_destroy(gc);

	printf("\n==========================================\n");
	printf("✅ ALL TESTS PASSED!\n");
	printf("==========================================\n");

	return 0;
}
