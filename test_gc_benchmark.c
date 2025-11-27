/*
 * Performance benchmark for GC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include "libgc/gc.h"

/* Timing helper */
static double
get_time(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return ts.tv_sec + ts.tv_nsec / 1e9;
}

/* Benchmark 1: Allocation speed */
static void
benchmark_allocation(void)
{
	gc_context_t *gc;
	gc_config_t config = GC_DEFAULT_CONFIG;
	double start, end, elapsed;
	int i, iterations = 100000;
	void *obj;

	printf("\n=== Benchmark 1: Allocation Speed ===\n");

	/* Test with pools */
	config.enable_pools = 1;
	gc = gc_init(&config);

	start = get_time();
	for (i = 0; i < iterations; i++) {
		obj = gc_alloc(gc, 32);
		(void)obj;
	}
	end = get_time();
	elapsed = end - start;

	printf("WITH POOLS:\n");
	printf("  Allocated %d objects (32 bytes each)\n", iterations);
	printf("  Time: %.6f seconds\n", elapsed);
	printf("  Rate: %.0f allocs/sec\n", iterations / elapsed);
	printf("  Avg: %.3f µs per allocation\n", (elapsed * 1e6) / iterations);

	gc_destroy(gc);

	/* Test without pools */
	config.enable_pools = 0;
	gc = gc_init(&config);

	start = get_time();
	for (i = 0; i < iterations; i++) {
		obj = gc_alloc(gc, 32);
		(void)obj;
	}
	end = get_time();
	elapsed = end - start;

	printf("\nWITHOUT POOLS:\n");
	printf("  Allocated %d objects (32 bytes each)\n", iterations);
	printf("  Time: %.6f seconds\n", elapsed);
	printf("  Rate: %.0f allocs/sec\n", iterations / elapsed);
	printf("  Avg: %.3f µs per allocation\n", (elapsed * 1e6) / iterations);

	gc_destroy(gc);
}

/* Benchmark 2: Collection speed */
static void
benchmark_collection(void)
{
	gc_context_t *gc;
	gc_config_t config = GC_DEFAULT_CONFIG;
	double start, end, elapsed;
	int i, num_objects = 10000;
	void **objs;

	printf("\n=== Benchmark 2: Collection Speed ===\n");

	config.enable_pools = 1;
	config.verbose = 0;
	gc = gc_init(&config);

	objs = malloc(num_objects * sizeof(void *));

	/* Allocate objects */
	for (i = 0; i < num_objects; i++) {
		objs[i] = gc_alloc(gc, 64);
	}

	printf("Allocated %d objects (64 bytes each)\n", num_objects);

	/* Benchmark full collection (all garbage) */
	start = get_time();
	gc_collect(gc);
	end = get_time();
	elapsed = end - start;

	printf("\nFull collection (all garbage):\n");
	printf("  Time: %.6f seconds\n", elapsed);
	printf("  Objects/sec: %.0f\n", num_objects / elapsed);

	/* Allocate again */
	for (i = 0; i < num_objects; i++) {
		objs[i] = gc_alloc(gc, 64);
	}

	/* Keep half alive */
	for (i = 0; i < num_objects / 2; i++) {
		gc_register_root(gc, &objs[i]);
	}

	/* Benchmark partial collection */
	start = get_time();
	gc_collect(gc);
	end = get_time();
	elapsed = end - start;

	printf("\nPartial collection (50%% garbage):\n");
	printf("  Time: %.6f seconds\n", elapsed);
	printf("  Objects checked/sec: %.0f\n", num_objects / elapsed);

	/* Cleanup */
	for (i = 0; i < num_objects / 2; i++) {
		gc_unregister_root(gc, &objs[i]);
	}

	free(objs);
	gc_destroy(gc);
}

/* Benchmark 3: Weak reference performance */
static void
benchmark_weak_refs(void)
{
	gc_context_t *gc;
	gc_config_t config = GC_DEFAULT_CONFIG;
	double start, end, elapsed;
	int i, num_refs = 10000;
	void **objs;
	gc_weak_t **weaks;

	printf("\n=== Benchmark 3: Weak Reference Performance ===\n");

	gc = gc_init(&config);

	objs = malloc(num_refs * sizeof(void *));
	weaks = malloc(num_refs * sizeof(gc_weak_t *));

	/* Allocate objects */
	for (i = 0; i < num_refs; i++) {
		objs[i] = gc_alloc(gc, 32);
	}

	/* Benchmark weak ref creation */
	start = get_time();
	for (i = 0; i < num_refs; i++) {
		weaks[i] = gc_weak_create(gc, objs[i]);
	}
	end = get_time();
	elapsed = end - start;

	printf("Weak reference creation:\n");
	printf("  Created %d weak refs\n", num_refs);
	printf("  Time: %.6f seconds\n", elapsed);
	printf("  Rate: %.0f creates/sec\n", num_refs / elapsed);

	/* Benchmark weak ref access */
	start = get_time();
	for (i = 0; i < num_refs; i++) {
		void *ptr = gc_weak_get(weaks[i]);
		(void)ptr;
	}
	end = get_time();
	elapsed = end - start;

	printf("\nWeak reference access:\n");
	printf("  Accessed %d weak refs\n", num_refs);
	printf("  Time: %.6f seconds\n", elapsed);
	printf("  Rate: %.0f accesses/sec\n", num_refs / elapsed);

	/* Benchmark invalidation (via collection) */
	start = get_time();
	gc_collect(gc);
	end = get_time();
	elapsed = end - start;

	printf("\nWeak reference invalidation (via GC):\n");
	printf("  Invalidated %d weak refs\n", num_refs);
	printf("  Time: %.6f seconds\n", elapsed);

	free(objs);
	free(weaks);
	gc_destroy(gc);
}

/* Benchmark 4: Memory throughput */
static void
benchmark_throughput(void)
{
	gc_context_t *gc;
	gc_config_t config = GC_DEFAULT_CONFIG;
	double start, end, elapsed;
	int i, iterations = 1000;
	size_t total_allocated = 0;

	printf("\n=== Benchmark 4: Memory Throughput ===\n");

	config.enable_pools = 1;
	config.verbose = 0;
	gc = gc_init(&config);

	start = get_time();

	for (i = 0; i < iterations; i++) {
		/* Allocate 1000 objects */
		int j;
		for (j = 0; j < 1000; j++) {
			void *obj = gc_alloc(gc, 64);
			total_allocated += 64;
			(void)obj;
		}

		/* Collect */
		gc_collect(gc);
	}

	end = get_time();
	elapsed = end - start;

	printf("Memory throughput test:\n");
	printf("  Iterations: %d\n", iterations);
	printf("  Objects per iteration: 1000\n");
	printf("  Total allocated: %.2f MB\n", total_allocated / (1024.0 * 1024.0));
	printf("  Time: %.6f seconds\n", elapsed);
	printf("  Throughput: %.2f MB/sec\n", (total_allocated / (1024.0 * 1024.0)) / elapsed);

	gc_destroy(gc);
}

/* Benchmark 5: Pool efficiency */
static void
benchmark_pool_efficiency(void)
{
	gc_context_t *gc;
	gc_config_t config = GC_DEFAULT_CONFIG;
	int i, iterations = 10;
	gc_stats_t stats_with, stats_without;

	printf("\n=== Benchmark 5: Pool Efficiency ===\n");

	/* With pools */
	config.enable_pools = 1;
	gc = gc_init(&config);

	for (i = 0; i < iterations; i++) {
		int j;
		for (j = 0; j < 1000; j++) {
			void *obj = gc_alloc(gc, 32);
			(void)obj;
		}
		gc_collect(gc);
	}

	gc_get_stats(gc, &stats_with);
	gc_destroy(gc);

	/* Without pools */
	config.enable_pools = 0;
	gc = gc_init(&config);

	for (i = 0; i < iterations; i++) {
		int j;
		for (j = 0; j < 1000; j++) {
			void *obj = gc_alloc(gc, 32);
			(void)obj;
		}
		gc_collect(gc);
	}

	gc_get_stats(gc, &stats_without);
	gc_destroy(gc);

	printf("WITH POOLS:\n");
	printf("  Collections: %zu\n", stats_with.num_collections);
	printf("  Total allocated: %zu bytes\n", stats_with.total_allocated);
	printf("  Total freed: %zu bytes\n", stats_with.total_freed);

	printf("\nWITHOUT POOLS:\n");
	printf("  Collections: %zu\n", stats_without.num_collections);
	printf("  Total allocated: %zu bytes\n", stats_without.total_allocated);
	printf("  Total freed: %zu bytes\n", stats_without.total_freed);

	printf("\nEfficiency comparison:\n");
	printf("  Pool overhead reduction: %.1f%%\n",
	       100.0 * (1.0 - (double)stats_with.total_allocated / stats_without.total_allocated));
}

int
main(void)
{
	printf("========================================\n");
	printf("GC Performance Benchmark Suite\n");
	printf("========================================\n");

	printf("\nRunning benchmarks...\n");

	benchmark_allocation();
	benchmark_collection();
	benchmark_weak_refs();
	benchmark_throughput();
	benchmark_pool_efficiency();

	printf("\n========================================\n");
	printf("✅ BENCHMARKS COMPLETE!\n");
	printf("========================================\n");

	return 0;
}
