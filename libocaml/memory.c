/*
 * Copyright (c) 2025 PCC OCaml Runtime Library
 *
 * Memory management using generic GC
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ocaml_runtime.h"
#include "../libgc/gc.h"

/* Global GC context */
static gc_context_t *ocaml_gc = NULL;

/*
 * Mark callback for OCaml values
 */
static void
ocaml_mark_value(gc_context_t *gc, void *obj, void *userdata)
{
	ocaml_value_t val = (ocaml_value_t)obj;
	ocaml_header_t *hdr;
	size_t i;

	/* Get header */
	hdr = HEADER(val);

	/* Mark children based on tag */
	for (i = 0; i < hdr->size; i++) {
		ocaml_value_t field = FIELD(val, i);

		/* Only mark if it's a pointer (not an integer) */
		if (IS_BLOCK(field)) {
			gc_mark(gc, (void *)field);
		}
	}
}

/*
 * Initialize OCaml GC
 */
void
ocaml_gc_init(size_t heap_size)
{
	gc_config_t config = GC_DEFAULT_CONFIG;

	if (heap_size > 0) {
		config.heap_size = heap_size;
		config.gc_threshold = heap_size / 2;
	}

	config.verbose = 0;
	config.enable_compaction = 0;

	ocaml_gc = gc_init(&config);
	if (!ocaml_gc) {
		fprintf(stderr, "ocaml_gc_init: failed to initialize GC\n");
		exit(1);
	}

	/* Set mark callback */
	gc_set_mark_callback(ocaml_gc, ocaml_mark_value, NULL);
}

/*
 * Register a root for GC
 */
void
ocaml_gc_register_root(ocaml_value_t *root)
{
	if (ocaml_gc) {
		gc_register_root(ocaml_gc, (void **)root);
	}
}

/*
 * Run garbage collection
 */
void
ocaml_gc_collect(void)
{
	if (ocaml_gc) {
		gc_collect(ocaml_gc);
	}
}

/*
 * Allocate a block using generic GC
 */
ocaml_value_t
ocaml_alloc(size_t size, uint8_t tag)
{
	ocaml_header_t *hdr;
	void *data;
	size_t total_size;

	if (!ocaml_gc) {
		fprintf(stderr, "ocaml_alloc: GC not initialized\n");
		exit(1);
	}

	/* Allocate header + data from GC */
	total_size = sizeof(ocaml_header_t) + size * sizeof(ocaml_value_t);
	hdr = gc_alloc(ocaml_gc, total_size);

	if (!hdr) {
		fprintf(stderr, "ocaml_alloc: out of memory\n");
		exit(1);
	}

	/* Initialize OCaml header */
	hdr->size = size;
	hdr->tag = tag;
	hdr->color = 0;

	/* Data follows header */
	data = (void *)(hdr + 1);

	return (ocaml_value_t)data;
}

/*
 * Get GC statistics
 */
void
ocaml_gc_stats(void)
{
	if (ocaml_gc) {
		gc_print_stats(ocaml_gc);
	}
}
