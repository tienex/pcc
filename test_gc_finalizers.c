/*
 * Test GC finalizer callbacks
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libgc/gc.h"

static int finalize_count = 0;
static int finalize_sum = 0;

/* Finalizer callback */
static void
my_finalizer(void *obj, void *userdata)
{
	int *value = (int *)obj;
	finalize_count++;
	finalize_sum += *value;
	printf("  Finalized object with value: %d\n", *value);
}

int
main(void)
{
	gc_config_t config = GC_DEFAULT_CONFIG;
	gc_context_t *gc;
	void *obj1, *obj2, *obj3;
	int i;

	printf("========================================\n");
	printf("GC Finalizer Callback Tests\n");
	printf("========================================\n");

	/* Test 1: Basic finalizer */
	printf("\n1. Testing basic finalizer...\n");

	gc = gc_init(&config);
	gc_set_finalize_callback(gc, my_finalizer, NULL);

	obj1 = gc_alloc(gc, sizeof(int));
	obj2 = gc_alloc(gc, sizeof(int));
	obj3 = gc_alloc(gc, sizeof(int));

	*(int *)obj1 = 10;
	*(int *)obj2 = 20;
	*(int *)obj3 = 30;

	printf("Created 3 objects: 10, 20, 30\n");

	/* Collect - should finalize all 3 */
	finalize_count = 0;
	finalize_sum = 0;

	gc_collect(gc);

	printf("✓ Finalizer called %d times\n", finalize_count);
	printf("✓ Sum of finalized values: %d (expected 60)\n", finalize_sum);

	assert(finalize_count == 3);
	assert(finalize_sum == 60);

	gc_destroy(gc);

	/* Test 2: Selective finalization */
	printf("\n2. Testing selective finalization...\n");

	gc = gc_init(&config);
	gc_set_finalize_callback(gc, my_finalizer, NULL);

	obj1 = gc_alloc(gc, sizeof(int));
	obj2 = gc_alloc(gc, sizeof(int));
	obj3 = gc_alloc(gc, sizeof(int));

	*(int *)obj1 = 100;
	*(int *)obj2 = 200;
	*(int *)obj3 = 300;

	/* Keep obj2 alive */
	gc_register_root(gc, &obj2);

	printf("Created 3 objects, keeping one rooted\n");

	finalize_count = 0;
	finalize_sum = 0;

	gc_collect(gc);

	printf("✓ Finalizer called %d times (expected 2)\n", finalize_count);
	printf("✓ Sum of finalized values: %d (expected 400)\n", finalize_sum);

	assert(finalize_count == 2);
	assert(finalize_sum == 400);

	gc_unregister_root(gc, &obj2);
	gc_destroy(gc);

	/* Test 3: Many objects */
	printf("\n3. Testing many objects with finalizers...\n");

	gc = gc_init(&config);
	gc_set_finalize_callback(gc, my_finalizer, NULL);

	for (i = 0; i < 100; i++) {
		void *obj = gc_alloc(gc, sizeof(int));
		*(int *)obj = i;
	}

	printf("Created 100 objects\n");

	finalize_count = 0;
	finalize_sum = 0;

	gc_collect(gc);

	printf("✓ Finalizer called %d times\n", finalize_count);
	printf("✓ Sum of finalized values: %d (expected 4950)\n", finalize_sum);

	assert(finalize_count == 100);
	assert(finalize_sum == 4950);  /* sum(0..99) */

	gc_destroy(gc);

	printf("\n========================================\n");
	printf("✅ ALL FINALIZER TESTS PASSED!\n");
	printf("========================================\n");

	return 0;
}
