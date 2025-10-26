/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Mode (type) management for CHILL
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Predefined modes */
MODE_DEF *mode_int;
MODE_DEF *mode_bool;
MODE_DEF *mode_char;
MODE_DEF *mode_real;
MODE_DEF *mode_duration;
MODE_DEF *mode_time;

/*
 * Create basic mode structure
 */
static MODE_DEF *
alloc_mode(int type, int size, int align)
{
	MODE_DEF *m = (MODE_DEF *)malloc(sizeof(MODE_DEF));
	if (m == NULL) {
		error("out of memory");
		exit(1);
	}
	m->mtype = type;
	m->msize = size;
	m->malign = align;
	return m;
}

/*
 * Initialize predefined modes
 */
void
init_modes(void)
{
	mode_int = alloc_mode(MINT, 4, 4);
	mode_bool = alloc_mode(MBOOL, 1, 1);
	mode_char = alloc_mode(MCHAR, 1, 1);
	mode_real = alloc_mode(MREAL, 8, 8);
	mode_duration = alloc_mode(MDURATION, 8, 8);
	mode_time = alloc_mode(MTIME, 8, 8);
}

/*
 * Create integer mode
 */
MODE_DEF *
mkmode_int(int size, int is_signed)
{
	return alloc_mode(MINT, size, size);
}

/*
 * Create real mode
 */
MODE_DEF *
mkmode_real(int size)
{
	return alloc_mode(MREAL, size, size);
}

/*
 * Create boolean mode
 */
MODE_DEF *
mkmode_bool(void)
{
	return mode_bool;
}

/*
 * Create character mode
 */
MODE_DEF *
mkmode_char(void)
{
	return mode_char;
}

/*
 * Create array mode
 */
MODE_DEF *
mkmode_array(MODE_DEF *elem, int *dims, int ndims)
{
	MODE_DEF *m = alloc_mode(MARRAY, 0, elem->malign);
	m->mattr.array.elem_mode = elem;
	m->mattr.array.ndims = ndims;
	m->mattr.array.index_modes = dims;
	/* Calculate size */
	m->msize = elem->msize;
	for (int i = 0; i < ndims; i++)
		m->msize *= dims[i];
	return m;
}

/*
 * Create structure mode
 */
MODE_DEF *
mkmode_struct(char *tag, FIELD_LIST *fields)
{
	MODE_DEF *m = alloc_mode(MSTRUCT, 0, 1);
	m->mattr.structure.tag = tag;
	m->mattr.structure.fields = fields;
	/* Calculate size and alignment */
	int offset = 0;
	int max_align = 1;
	for (FIELD_LIST *f = fields; f != NULL; f = f->fnext) {
		int align = f->fmode->malign;
		if (align > max_align)
			max_align = align;
		offset = (offset + align - 1) & ~(align - 1);
		f->foffset = offset;
		offset += f->fmode->msize;
	}
	m->msize = offset;
	m->malign = max_align;
	return m;
}

/*
 * Create reference (pointer) mode
 */
MODE_DEF *
mkmode_ref(MODE_DEF *base)
{
	MODE_DEF *m = alloc_mode(MREF, sizeof(void*), sizeof(void*));
	m->mattr.ref.base = base;
	return m;
}

/*
 * Create range mode
 */
MODE_DEF *
mkmode_range(int64_t low, int64_t high, MODE_DEF *parent)
{
	MODE_DEF *m = alloc_mode(MRANGE, parent->msize, parent->malign);
	m->mattr.range.low = low;
	m->mattr.range.high = high;
	m->mattr.range.parent = parent;
	return m;
}

/*
 * Create powerset mode
 */
MODE_DEF *
mkmode_powerset(NAME_LIST *names)
{
	MODE_DEF *m = alloc_mode(MPOWERSET, sizeof(uint64_t), sizeof(uint64_t));
	m->mattr.powerset.names = names;
	return m;
}

/*
 * Create procedure mode
 */
MODE_DEF *
mkmode_proc(MODE_DEF *retmode, PARAM_LIST *params)
{
	MODE_DEF *m = alloc_mode(MPROC, sizeof(void*), sizeof(void*));
	m->mattr.proc.ret_mode = retmode;
	m->mattr.proc.params = params;
	return m;
}

/*
 * Create bits mode (bit string)
 */
MODE_DEF *
mkmode_bits(int length)
{
	int size = (length + 7) / 8;  /* Round up to bytes */
	return alloc_mode(MBITS, size, 1);
}

/*
 * Create chars mode (character string)
 */
MODE_DEF *
mkmode_chars(int length)
{
	return alloc_mode(MCHARS, length, 1);
}

/*
 * Check if two modes are compatible
 */
int
mode_compatible(MODE_DEF *m1, MODE_DEF *m2)
{
	if (m1 == m2)
		return 1;
	if (m1->mtype != m2->mtype)
		return 0;
	/* Add more sophisticated compatibility checks as needed */
	return 1;
}

/*
 * Get size of a mode
 */
int
mode_size(MODE_DEF *m)
{
	return m->msize;
}
