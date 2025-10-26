/*
 * Copyright (c) 2025 PCC OCaml Runtime Library
 *
 * Memory management and garbage collection
 * Simple mark-and-sweep collector
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ocaml_runtime.h"

/* GC colors */
#define COLOR_WHITE 0
#define COLOR_GRAY  1
#define COLOR_BLACK 2

/* Heap management */
typedef struct heap_block {
	void *start;
	size_t size;
	size_t used;
	struct heap_block *next;
} heap_block_t;

static heap_block_t *heap_blocks = NULL;
static size_t total_allocated = 0;
static size_t gc_threshold = 1024 * 1024; /* 1MB default */

/* Root set for GC */
#define MAX_ROOTS 1024
static ocaml_value_t *gc_roots[MAX_ROOTS];
static size_t num_roots = 0;

/* Allocation list for GC */
typedef struct alloc_node {
	ocaml_header_t *header;
	void *data;
	struct alloc_node *next;
} alloc_node_t;

static alloc_node_t *alloc_list = NULL;

/*
 * Initialize garbage collector
 */
void
ocaml_gc_init(size_t heap_size)
{
	heap_block_t *block;

	if (heap_size == 0)
		heap_size = 16 * 1024 * 1024; /* 16MB default */

	block = malloc(sizeof(heap_block_t));
	if (!block) {
		fprintf(stderr, "ocaml_gc_init: out of memory\n");
		exit(1);
	}

	block->size = heap_size;
	block->start = malloc(heap_size);
	if (!block->start) {
		fprintf(stderr, "ocaml_gc_init: out of memory\n");
		exit(1);
	}

	block->used = 0;
	block->next = NULL;
	heap_blocks = block;

	gc_threshold = heap_size / 2;
}

/*
 * Register a root for GC
 */
void
ocaml_gc_register_root(ocaml_value_t *root)
{
	if (num_roots >= MAX_ROOTS) {
		fprintf(stderr, "ocaml_gc_register_root: too many roots\n");
		return;
	}

	gc_roots[num_roots++] = root;
}

/*
 * Mark phase of GC
 */
static void
mark_value(ocaml_value_t val)
{
	ocaml_header_t *hdr;
	size_t i;

	if (IS_INT(val))
		return;

	hdr = HEADER(val);

	/* Already marked? */
	if (hdr->color == COLOR_BLACK)
		return;

	/* Mark this block */
	hdr->color = COLOR_BLACK;

	/* Mark children */
	for (i = 0; i < hdr->size; i++) {
		mark_value(FIELD(val, i));
	}
}

/*
 * Sweep phase of GC
 */
static void
sweep(void)
{
	alloc_node_t *node, *prev, *next;
	size_t freed = 0;

	prev = NULL;
	node = alloc_list;

	while (node) {
		next = node->next;

		if (node->header->color == COLOR_WHITE) {
			/* Free unmarked block */
			free(node->header);
			free(node);

			if (prev)
				prev->next = next;
			else
				alloc_list = next;

			freed++;
		} else {
			/* Reset color for next GC */
			node->header->color = COLOR_WHITE;
			prev = node;
		}

		node = next;
	}
}

/*
 * Run garbage collection
 */
void
ocaml_gc_collect(void)
{
	size_t i;

	/* Mark phase: mark all reachable values */
	for (i = 0; i < num_roots; i++) {
		if (gc_roots[i])
			mark_value(*gc_roots[i]);
	}

	/* Sweep phase: free unmarked blocks */
	sweep();
}

/*
 * Allocate a block
 */
ocaml_value_t
ocaml_alloc(size_t size, uint8_t tag)
{
	ocaml_header_t *hdr;
	void *data;
	alloc_node_t *node;
	size_t total_size;

	/* Check if GC is needed */
	if (total_allocated > gc_threshold) {
		ocaml_gc_collect();
	}

	/* Allocate header + data */
	total_size = sizeof(ocaml_header_t) + size * sizeof(ocaml_value_t);
	hdr = calloc(1, total_size);
	if (!hdr) {
		/* Try GC and retry */
		ocaml_gc_collect();
		hdr = calloc(1, total_size);
		if (!hdr) {
			fprintf(stderr, "ocaml_alloc: out of memory\n");
			exit(1);
		}
	}

	/* Initialize header */
	hdr->size = size;
	hdr->tag = tag;
	hdr->color = COLOR_WHITE;

	/* Data follows header */
	data = (void *)(hdr + 1);

	/* Add to allocation list */
	node = malloc(sizeof(alloc_node_t));
	if (node) {
		node->header = hdr;
		node->data = data;
		node->next = alloc_list;
		alloc_list = node;
	}

	total_allocated += total_size;

	return (ocaml_value_t)data;
}
