/*
 * Simple test for weak references
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
	void *obj;
	gc_weak_t *weak;
	void *ptr;

	printf("Test: Basic weak reference\n\n");

	/* Initialize GC */
	config.verbose = 1;
	gc = gc_init(&config);
	if (!gc) {
		fprintf(stderr, "Failed to init GC\n");
		return 1;
	}

	/* Test 1: Create weak reference */
	printf("1. Creating object and weak reference\n");
	obj = gc_alloc(gc, sizeof(int));
	*(int *)obj = 42;

	weak = gc_weak_create(gc, obj);
	assert(weak != NULL);

	ptr = gc_weak_get(weak);
	assert(ptr == obj);
	assert(*(int *)ptr == 42);
	printf("✓ Weak reference created and working\n\n");

	/* Test 2: Object survives when rooted */
	printf("2. Collecting with object rooted\n");
	gc_register_root(gc, &obj);
	gc_collect(gc);

	ptr = gc_weak_get(weak);
	assert(ptr != NULL);
	assert(ptr == obj);
	printf("✓ Weak reference still valid (object rooted)\n\n");

	/* Test 3: Object collected when not rooted */
	printf("3. Collecting with object not rooted\n");
	gc_unregister_root(gc, &obj);
	obj = NULL;
	gc_collect(gc);

	ptr = gc_weak_get(weak);
	if (ptr == NULL) {
		printf("✓ Weak reference invalidated (object collected)\n\n");
	} else {
		printf("✗ Weak reference should be NULL but is %p\n", ptr);
		return 1;
	}

	/* Cleanup */
	printf("4. Cleanup\n");
	gc_destroy(gc);

	printf("\n✅ TEST PASSED!\n");
	return 0;
}
