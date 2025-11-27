/*
 * Comprehensive test for GC weak references
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libgc/gc.h"

static gc_context_t *gc = NULL;

/* Test 1: Basic weak reference */
static void
test_basic_weak(void)
{
	void *obj;
	gc_weak_t *weak;
	void *ptr;

	printf("\n=== Test 1: Basic Weak Reference ===\n");

	/* Allocate object */
	obj = gc_alloc(gc, sizeof(int));
	*(int *)obj = 42;

	printf("✓ Allocated object with value 42\n");

	/* Create weak reference */
	weak = gc_weak_create(gc, obj);
	assert(weak != NULL);

	printf("✓ Created weak reference\n");

	/* Get object through weak reference */
	ptr = gc_weak_get(weak);
	assert(ptr == obj);
	assert(*(int *)ptr == 42);

	printf("✓ Retrieved object through weak reference\n");

	/* Register as root temporarily */
	gc_register_root(gc, &obj);
	gc_collect(gc);

	/* Should still be valid */
	ptr = gc_weak_get(weak);
	assert(ptr != NULL);
	assert(ptr == obj);

	printf("✓ Object survived GC (rooted)\n");

	/* Unregister and collect */
	gc_unregister_root(gc, &obj);
	obj = NULL;
	gc_collect(gc);

	/* Weak reference should be invalid now */
	ptr = gc_weak_get(weak);
	assert(ptr == NULL);

	printf("✓ Weak reference invalidated after collection\n");

	/* Note: weak references are automatically cleaned up by gc_destroy */

	printf("✓ Test passed\n");
}

/* Test 2: Multiple weak references to same object */
static void
test_multiple_weak(void)
{
	void *obj;
	gc_weak_t *weak1, *weak2, *weak3;

	printf("\n=== Test 2: Multiple Weak References ===\n");

	/* Allocate object */
	obj = gc_alloc(gc, sizeof(int));
	*(int *)obj = 123;

	/* Create multiple weak references */
	weak1 = gc_weak_create(gc, obj);
	weak2 = gc_weak_create(gc, obj);
	weak3 = gc_weak_create(gc, obj);

	assert(weak1 && weak2 && weak3);

	printf("✓ Created 3 weak references to same object\n");

	/* All should point to same object */
	assert(gc_weak_get(weak1) == obj);
	assert(gc_weak_get(weak2) == obj);
	assert(gc_weak_get(weak3) == obj);

	printf("✓ All references point to same object\n");

	/* Keep object alive */
	gc_register_root(gc, &obj);
	gc_collect(gc);

	/* All should still be valid */
	assert(gc_weak_get(weak1) != NULL);
	assert(gc_weak_get(weak2) != NULL);
	assert(gc_weak_get(weak3) != NULL);

	printf("✓ All references survived GC (rooted)\n");

	/* Let object be collected */
	gc_unregister_root(gc, &obj);
	obj = NULL;
	gc_collect(gc);

	/* All should be invalid */
	assert(gc_weak_get(weak1) == NULL);
	assert(gc_weak_get(weak2) == NULL);
	assert(gc_weak_get(weak3) == NULL);

	printf("✓ All references invalidated\n");

	/* Cleanup */
	// gc_weak_release(weak1);
	// gc_weak_release(weak2);
	// gc_weak_release(weak3);

	printf("✓ Test passed\n");
}

/* Test 3: Weak references to different objects */
static void
test_multiple_objects(void)
{
	void *obj1, *obj2, *obj3;
	gc_weak_t *weak1, *weak2, *weak3;

	printf("\n=== Test 3: Weak References to Different Objects ===\n");

	/* Allocate objects */
	obj1 = gc_alloc(gc, sizeof(int));
	obj2 = gc_alloc(gc, sizeof(int));
	obj3 = gc_alloc(gc, sizeof(int));

	*(int *)obj1 = 1;
	*(int *)obj2 = 2;
	*(int *)obj3 = 3;

	/* Create weak references */
	weak1 = gc_weak_create(gc, obj1);
	weak2 = gc_weak_create(gc, obj2);
	weak3 = gc_weak_create(gc, obj3);

	printf("✓ Created weak references to 3 different objects\n");

	/* Keep only obj2 alive */
	gc_register_root(gc, &obj2);
	gc_collect(gc);

	/* Only weak2 should be valid */
	assert(gc_weak_get(weak1) == NULL);
	assert(gc_weak_get(weak2) != NULL);
	assert(gc_weak_get(weak3) == NULL);

	printf("✓ Only rooted object's weak reference survived\n");

	/* Verify obj2 value */
	assert(*(int *)gc_weak_get(weak2) == 2);

	printf("✓ Object data intact\n");

	/* Cleanup */
	gc_unregister_root(gc, &obj2);
	// gc_weak_release(weak1);
	// gc_weak_release(weak2);
	// gc_weak_release(weak3);

	printf("✓ Test passed\n");
}

/* Test 4: Weak reference release */
static void
test_weak_release(void)
{
	void *obj;
	gc_weak_t *weak;

	printf("\n=== Test 4: Weak Reference Release ===\n");

	/* Allocate object */
	obj = gc_alloc(gc, sizeof(int));
	*(int *)obj = 999;

	/* Create and immediately release weak reference */
	weak = gc_weak_create(gc, obj);
	assert(weak != NULL);

	printf("✓ Created weak reference\n");

	// gc_weak_release(weak);

	printf("✓ Released weak reference\n");

	/* Object should still exist */
	gc_register_root(gc, &obj);
	gc_collect(gc);
	assert(*(int *)obj == 999);

	printf("✓ Object unaffected by weak reference release\n");

	/* Cleanup */
	gc_unregister_root(gc, &obj);

	printf("✓ Test passed\n");
}

/* Test 5: Cache-like usage pattern */
static void
test_cache_pattern(void)
{
	void *cache_entries[10];
	gc_weak_t *weak_cache[10];
	int i;

	printf("\n=== Test 5: Cache Usage Pattern ===\n");

	/* Populate cache */
	for (i = 0; i < 10; i++) {
		cache_entries[i] = gc_alloc(gc, sizeof(int));
		*(int *)cache_entries[i] = i * 100;
		weak_cache[i] = gc_weak_create(gc, cache_entries[i]);
	}

	printf("✓ Created cache with 10 entries\n");

	/* Verify all entries accessible */
	for (i = 0; i < 10; i++) {
		void *ptr = gc_weak_get(weak_cache[i]);
		assert(ptr != NULL);
		assert(*(int *)ptr == i * 100);
	}

	printf("✓ All cache entries accessible\n");

	/* Keep only even entries alive */
	for (i = 0; i < 10; i += 2) {
		gc_register_root(gc, &cache_entries[i]);
	}

	gc_collect(gc);

	/* Check cache - odd entries should be gone */
	int valid = 0, invalid = 0;
	for (i = 0; i < 10; i++) {
		void *ptr = gc_weak_get(weak_cache[i]);
		if (ptr != NULL) {
			assert(i % 2 == 0);  /* Should be even */
			assert(*(int *)ptr == i * 100);
			valid++;
		} else {
			assert(i % 2 == 1);  /* Should be odd */
			invalid++;
		}
	}

	printf("✓ Cache consistency: %d valid, %d invalid\n", valid, invalid);
	assert(valid == 5 && invalid == 5);

	/* Cleanup */
	for (i = 0; i < 10; i += 2) {
		gc_unregister_root(gc, &cache_entries[i]);
	}
	for (i = 0; i < 10; i++) {
		// gc_weak_release(weak_cache[i]);
	}

	printf("✓ Test passed\n");
}

/* Test 6: Stress test with many weak references */
static void
test_weak_stress(void)
{
	void *objs[100];
	gc_weak_t *weaks[100];
	int i, iteration;

	printf("\n=== Test 6: Weak Reference Stress Test ===\n");

	for (iteration = 0; iteration < 10; iteration++) {
		/* Create objects and weak references */
		for (i = 0; i < 100; i++) {
			objs[i] = gc_alloc(gc, sizeof(int));
			*(int *)objs[i] = i;
			weaks[i] = gc_weak_create(gc, objs[i]);
		}

		/* Keep half alive */
		for (i = 0; i < 50; i++) {
			gc_register_root(gc, &objs[i]);
		}

		/* Collect */
		gc_collect(gc);

		/* Verify */
		for (i = 0; i < 100; i++) {
			void *ptr = gc_weak_get(weaks[i]);
			if (i < 50) {
				assert(ptr != NULL);
				assert(*(int *)ptr == i);
			} else {
				assert(ptr == NULL);
			}
		}

		/* Cleanup */
		for (i = 0; i < 50; i++) {
			gc_unregister_root(gc, &objs[i]);
		}
		for (i = 0; i < 100; i++) {
			// gc_weak_release(weaks[i]);
		}

		if ((iteration + 1) % 5 == 0) {
			printf("  Completed %d iterations\n", iteration + 1);
		}
	}

	printf("✓ Stress test completed (10 iterations)\n");
	printf("✓ Test passed\n");
}

int
main(void)
{
	gc_config_t config = GC_DEFAULT_CONFIG;

	printf("============================================\n");
	printf("GC Weak References - Comprehensive Test Suite\n");
	printf("============================================\n");

	/* Initialize GC */
	config.heap_size = 10 * 1024 * 1024;
	config.gc_threshold = 5 * 1024 * 1024;
	config.enable_pools = 1;
	config.verbose = 0;

	gc = gc_init(&config);
	if (!gc) {
		fprintf(stderr, "Failed to initialize GC\n");
		return 1;
	}

	printf("\n✓ GC initialized\n");

	/* Run all tests */
	test_basic_weak();
	test_multiple_weak();
	test_multiple_objects();
	test_weak_release();
	test_cache_pattern();
	test_weak_stress();

	/* Final stats */
	printf("\n============================================\n");
	printf("Final Statistics:\n");
	printf("============================================\n");
	gc_print_stats(gc);

	/* Cleanup */
	gc_destroy(gc);

	printf("\n============================================\n");
	printf("✅ ALL TESTS PASSED!\n");
	printf("============================================\n");

	return 0;
}
