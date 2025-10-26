/*
 * Test GC statistics tracking
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "libgc/gc.h"

int
main(void)
{
	gc_config_t config = GC_DEFAULT_CONFIG;
	gc_context_t *gc;
	gc_stats_t stats;
	void *objs[100];
	int i;

	printf("========================================\n");
	printf("GC Statistics Validation Tests\n");
	printf("========================================\n");

	/* Test 1: Initial stats */
	printf("\n1. Verifying initial statistics...\n");

	config.verbose = 0;
	gc = gc_init(&config);

	gc_get_stats(gc, &stats);

	printf("Initial stats:\n");
	printf("  Total allocated: %zu\n", stats.total_allocated);
	printf("  Total freed: %zu\n", stats.total_freed);
	printf("  Current usage: %zu\n", stats.current_usage);
	printf("  Num objects: %zu\n", stats.num_objects);
	printf("  Collections: %zu\n", stats.num_collections);

	assert(stats.total_allocated == 0);
	assert(stats.total_freed == 0);
	assert(stats.current_usage == 0);
	assert(stats.num_objects == 0);
	assert(stats.num_collections == 0);

	printf("✓ Initial statistics are zero\n");

	/* Test 2: Stats after allocation */
	printf("\n2. Verifying stats after allocation...\n");

	for (i = 0; i < 10; i++) {
		objs[i] = gc_alloc(gc, 64);
	}

	gc_get_stats(gc, &stats);

	printf("After 10 allocations (64 bytes each):\n");
	printf("  Total allocated: %zu\n", stats.total_allocated);
	printf("  Current usage: %zu\n", stats.current_usage);
	printf("  Num objects: %zu\n", stats.num_objects);

	assert(stats.total_allocated > 0);
	assert(stats.current_usage > 0);
	assert(stats.num_objects == 10);

	printf("✓ Allocation stats correct\n");

	/* Test 3: Stats after collection */
	printf("\n3. Verifying stats after collection...\n");

	size_t allocated_before = stats.total_allocated;
	size_t usage_before = stats.current_usage;

	gc_collect(gc);

	gc_get_stats(gc, &stats);

	printf("After collection (all garbage):\n");
	printf("  Total freed: %zu\n", stats.total_freed);
	printf("  Current usage: %zu\n", stats.current_usage);
	printf("  Num objects: %zu\n", stats.num_objects);
	printf("  Collections: %zu\n", stats.num_collections);

	assert(stats.total_freed > 0);
	assert(stats.current_usage < usage_before);
	assert(stats.num_objects == 0);
	assert(stats.num_collections == 1);

	printf("✓ Collection stats correct\n");

	/* Test 4: Stats with partial collection */
	printf("\n4. Verifying stats with partial collection...\n");

	/* Allocate and root half */
	for (i = 0; i < 20; i++) {
		objs[i] = gc_alloc(gc, 32);
	}

	for (i = 0; i < 10; i++) {
		gc_register_root(gc, &objs[i]);
	}

	gc_get_stats(gc, &stats);
	size_t objects_before = stats.num_objects;

	printf("Before collection: %zu objects\n", objects_before);
	assert(objects_before == 20);

	gc_collect(gc);

	gc_get_stats(gc, &stats);

	printf("After collection (half rooted):\n");
	printf("  Num objects: %zu (expected 10)\n", stats.num_objects);

	assert(stats.num_objects == 10);

	printf("✓ Partial collection stats correct\n");

	/* Test 5: Stats accumulation */
	printf("\n5. Verifying stats accumulation...\n");

	size_t initial_allocated = stats.total_allocated;
	size_t initial_freed = stats.total_freed;
	size_t initial_collections = stats.num_collections;

	/* Multiple alloc/collect cycles */
	for (i = 0; i < 5; i++) {
		void *temp[10];
		int j;
		for (j = 0; j < 10; j++) {
			temp[j] = gc_alloc(gc, 16);
		}
		gc_collect(gc);
	}

	gc_get_stats(gc, &stats);

	printf("After 5 alloc/collect cycles:\n");
	printf("  Total allocated increased: %zu bytes\n",
	       stats.total_allocated - initial_allocated);
	printf("  Total freed increased: %zu bytes\n",
	       stats.total_freed - initial_freed);
	printf("  Collections increased: %zu\n",
	       stats.num_collections - initial_collections);

	assert(stats.total_allocated > initial_allocated);
	assert(stats.total_freed > initial_freed);
	assert(stats.num_collections == initial_collections + 5);

	printf("✓ Stats accumulation correct\n");

	/* Test 6: Usage tracking */
	printf("\n6. Verifying current usage tracking...\n");

	/* Unregister all roots */
	for (i = 0; i < 10; i++) {
		gc_unregister_root(gc, &objs[i]);
	}

	gc_collect(gc);

	gc_get_stats(gc, &stats);
	size_t usage_after_cleanup = stats.current_usage;

	printf("Current usage after cleanup: %zu bytes\n", usage_after_cleanup);
	printf("Num objects after cleanup: %zu\n", stats.num_objects);

	/* Allocate known amount */
	for (i = 0; i < 5; i++) {
		objs[i] = gc_alloc(gc, 100);
		gc_register_root(gc, &objs[i]);
	}

	gc_get_stats(gc, &stats);

	printf("After allocating 5 x 100 byte objects:\n");
	printf("  Current usage: %zu bytes\n", stats.current_usage);
	printf("  Num objects: %zu\n", stats.num_objects);

	assert(stats.num_objects == 5);
	assert(stats.current_usage > usage_after_cleanup);

	printf("✓ Usage tracking correct\n");

	/* Test 7: Verify stats invariants */
	printf("\n7. Verifying stats invariants...\n");

	gc_get_stats(gc, &stats);

	printf("Checking invariants:\n");
	printf("  total_allocated >= total_freed: %s\n",
	       stats.total_allocated >= stats.total_freed ? "✓" : "✗");
	printf("  current_usage = allocated - freed: ");

	size_t expected_usage = stats.total_allocated - stats.total_freed;
	if (stats.current_usage == expected_usage) {
		printf("✓ (%zu)\n", stats.current_usage);
	} else {
		printf("✗ (expected %zu, got %zu)\n", expected_usage, stats.current_usage);
	}

	assert(stats.total_allocated >= stats.total_freed);
	assert(stats.current_usage == (stats.total_allocated - stats.total_freed));

	printf("✓ All invariants satisfied\n");

	/* Cleanup */
	for (i = 0; i < 5; i++) {
		gc_unregister_root(gc, &objs[i]);
	}

	/* Test 8: Final stats */
	printf("\n8. Final statistics summary...\n");

	gc_get_stats(gc, &stats);

	printf("Final stats:\n");
	printf("  Total allocated: %zu bytes\n", stats.total_allocated);
	printf("  Total freed: %zu bytes\n", stats.total_freed);
	printf("  Current usage: %zu bytes\n", stats.current_usage);
	printf("  Num objects: %zu\n", stats.num_objects);
	printf("  Collections: %zu\n", stats.num_collections);
	printf("  Heap size: %zu bytes\n", stats.heap_size);

	assert(stats.num_collections >= 7);  /* At least our test collections */

	printf("✓ Final stats recorded\n");

	gc_destroy(gc);

	printf("\n========================================\n");
	printf("✅ ALL STATISTICS TESTS PASSED!\n");
	printf("========================================\n");

	return 0;
}
