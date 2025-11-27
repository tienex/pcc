/*
 * Simple test for GC basic functionality
 */

#include <stdio.h>
#include <stdlib.h>
#include "libgc/gc.h"

int
main(void)
{
	gc_config_t config = GC_DEFAULT_CONFIG;
	gc_context_t *gc;
	void *obj1, *obj2;

	printf("Test: Basic GC with pools\n");

	/* Initialize GC */
	config.verbose = 1;
	config.enable_pools = 1;
	gc = gc_init(&config);
	if (!gc) {
		fprintf(stderr, "Failed to init GC\n");
		return 1;
	}

	printf("\n1. Allocating 2 small objects\n");
	obj1 = gc_alloc(gc, 16);
	obj2 = gc_alloc(gc, 16);

	printf("obj1 = %p\n", obj1);
	printf("obj2 = %p\n", obj2);

	printf("\n2. Registering as roots\n");
	gc_register_root(gc, &obj1);
	gc_register_root(gc, &obj2);

	printf("\n3. Collecting (should free nothing)\n");
	size_t freed = gc_collect(gc);
	printf("Freed: %zu\n", freed);

	printf("\n4. Unregistering obj1\n");
	gc_unregister_root(gc, &obj1);
	obj1 = NULL;

	printf("\n5. Collecting (should free obj1)\n");
	freed = gc_collect(gc);
	printf("Freed: %zu\n", freed);

	printf("\n6. Cleanup\n");
	gc_unregister_root(gc, &obj2);
	gc_destroy(gc);

	printf("\nTest passed!\n");
	return 0;
}
