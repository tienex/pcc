/*
 * Test program for generic GC library
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libgc/gc.h"

/* Test object structure */
typedef struct test_obj {
	int value;
	struct test_obj *next;
	struct test_obj *child;
} test_obj_t;

/* Global GC context */
static gc_context_t *gc;

/* Mark callback for test objects */
void test_mark_callback(gc_context_t *gc_ctx, void *obj, void *userdata)
{
	test_obj_t *tobj = (test_obj_t *)obj;

	/* Mark next */
	if (tobj->next)
		gc_mark(gc_ctx, tobj->next);

	/* Mark child */
	if (tobj->child)
		gc_mark(gc_ctx, tobj->child);
}

/* Create test object */
test_obj_t *create_test_obj(int value)
{
	test_obj_t *obj = gc_alloc(gc, sizeof(test_obj_t));
	if (obj) {
		obj->value = value;
		obj->next = NULL;
		obj->child = NULL;
	}
	return obj;
}

int main(void)
{
	test_obj_t *root1, *root2, *obj;
	gc_stats_t stats;
	int i;

	printf("=== Generic GC Library Test ===\n\n");

	/* Initialize GC */
	gc_config_t config = GC_DEFAULT_CONFIG;
	config.heap_size = 1024 * 1024;  /* 1MB */
	config.gc_threshold = 512 * 1024; /* 512KB */
	config.verbose = 1;

	gc = gc_init(&config);
	if (!gc) {
		fprintf(stderr, "Failed to initialize GC\n");
		return 1;
	}

	/* Set mark callback */
	gc_set_mark_callback(gc, test_mark_callback, NULL);

	printf("GC initialized\n\n");

	/* Test 1: Basic allocation */
	printf("Test 1: Basic allocation\n");
	root1 = create_test_obj(42);
	printf("Allocated object with value: %d\n", root1->value);
	gc_register_root(gc, (void **)&root1);
	gc_print_stats(gc);
	printf("\n");

	/* Test 2: Create a linked list */
	printf("Test 2: Create linked list\n");
	root2 = create_test_obj(1);
	gc_register_root(gc, (void **)&root2);

	obj = root2;
	for (i = 2; i <= 10; i++) {
		obj->next = create_test_obj(i);
		obj = obj->next;
	}

	printf("Created list of 10 objects\n");
	gc_print_stats(gc);
	printf("\n");

	/* Test 3: Create some garbage */
	printf("Test 3: Create garbage (unreachable objects)\n");
	for (i = 0; i < 100; i++) {
		create_test_obj(i + 100);
	}
	printf("Created 100 unreachable objects\n");
	gc_print_stats(gc);
	printf("\n");

	/* Test 4: Garbage collection */
	printf("Test 4: Run garbage collection\n");
	size_t freed = gc_collect(gc);
	printf("Freed %zu objects\n", freed);
	gc_print_stats(gc);
	printf("\n");

	/* Test 5: Create tree structure */
	printf("Test 5: Create tree structure\n");
	root1->child = create_test_obj(100);
	root1->child->child = create_test_obj(200);
	root1->child->next = create_test_obj(300);
	printf("Created tree with 3 children\n");
	gc_print_stats(gc);
	printf("\n");

	/* Test 6: Unregister root and collect */
	printf("Test 6: Unregister root2 and collect\n");
	gc_unregister_root(gc, (void **)&root2);
	root2 = NULL;
	freed = gc_collect(gc);
	printf("Freed %zu objects (should be ~10 from list)\n", freed);
	gc_print_stats(gc);
	printf("\n");

	/* Test 7: Object flags */
	printf("Test 7: Object flags\n");
	gc_set_flags(root1, 0x42);
	uint8_t flags = gc_get_flags(root1);
	printf("Set flags to 0x42, got: 0x%02x\n", flags);
	printf("\n");

	/* Test 8: Heap compaction */
	printf("Test 8: Heap compaction\n");
	if (gc_resize_heap(gc, 2 * 1024 * 1024) == 0) {
		printf("Resized heap to 2MB\n");
	}
	size_t moved = gc_compact(gc);
	printf("Compaction moved %zu objects\n", moved);
	gc_print_stats(gc);
	printf("\n");

	/* Test 9: Stress test */
	printf("Test 9: Stress test (allocate many objects)\n");
	for (i = 0; i < 1000; i++) {
		create_test_obj(i);
	}
	printf("Allocated 1000 objects\n");
	gc_get_stats(gc, &stats);
	printf("Usage before GC: %zu bytes\n", stats.current_usage);

	freed = gc_collect(gc);
	printf("Freed %zu objects\n", freed);

	gc_get_stats(gc, &stats);
	printf("Usage after GC: %zu bytes\n", stats.current_usage);
	printf("Total collections: %zu\n", stats.num_collections);
	printf("\n");

	/* Final stats */
	printf("=== Final Statistics ===\n");
	gc_print_stats(gc);

	/* Cleanup */
	gc_destroy(gc);

	printf("\n=== All tests passed! ===\n");

	return 0;
}
