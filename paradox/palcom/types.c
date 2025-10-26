/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Type system implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Type size table (in bytes) */
static int type_sizes[] = {
	[TNULL] = 0,
	[TSMALLINT] = 2,
	[TSHORTINT] = 1,
	[TLONGINT] = 4,
	[TNUMBER] = 8,          /* BCD number */
	[TCURRENCY] = 8,
	[TLOGICAL] = 1,
	[TALPHANUMERIC] = 256,  /* Default string size */
	[TDATE] = 4,
	[TTIME] = 4,
	[TDATETIME] = 8,
	[TTIMESTAMP] = 8,
	[TMEMO] = 4,            /* Pointer to memo */
	[TBLOB] = 4,            /* Pointer to BLOB */
	[TGRAPHIC] = 4,         /* Pointer to graphic */
	[TFORMATTED_MEMO] = 4,
	[TAUTOINCREMENT] = 4,
	[TBYTES] = 4,           /* Pointer to bytes */
	[TARRAY] = 0,           /* Computed */
	[TRECORD] = 0,          /* Computed */
	[TOBJECT] = 4,          /* Pointer to object */
	[TPOINTER] = 4,
	[TSUBRANGE] = 4,
	[TENUM] = 4,
	[TPROCEDURE] = 4,       /* Pointer to procedure */
	[TVARIANT] = 16,        /* Variant structure */
};

TNODE *mktype(int basictype)
{
	TNODE *t = (TNODE *)malloc(sizeof(TNODE));
	if (!t) {
		fatal("Out of memory");
	}

	t->ttype = basictype;
	t->tsize = (basictype < sizeof(type_sizes)/sizeof(type_sizes[0]))
		? type_sizes[basictype] : 0;
	t->talign = (t->tsize > 0) ? t->tsize : 4;

	return t;
}

TNODE *mkarray(TNODE *elemtype, int *dims, int ndims)
{
	TNODE *t = (TNODE *)malloc(sizeof(TNODE));
	if (!t) {
		fatal("Out of memory");
	}

	t->ttype = TARRAY;
	t->tattr.array.elem_type = elemtype;
	t->tattr.array.dim_count = ndims;
	t->tattr.array.dimensions = (int *)malloc(ndims * sizeof(int));
	if (!t->tattr.array.dimensions) {
		fatal("Out of memory");
	}
	memcpy(t->tattr.array.dimensions, dims, ndims * sizeof(int));

	/* Calculate size */
	int total = 1;
	int i;
	for (i = 0; i < ndims; i++) {
		total *= dims[i];
	}
	t->tsize = total * type_size(elemtype);
	t->talign = elemtype->talign;

	return t;
}

TNODE *mkrecord(char *tag, FIELD_LIST *fields)
{
	TNODE *t = (TNODE *)malloc(sizeof(TNODE));
	if (!t) {
		fatal("Out of memory");
	}

	t->ttype = TRECORD;
	t->tattr.object.tag = tag ? strdup(tag) : NULL;
	t->tattr.object.fields = fields;
	t->tattr.object.methods = NULL;
	t->tattr.object.parent = NULL;

	/* Calculate size and layout */
	int offset = 0;
	int max_align = 1;
	FIELD_LIST *f;
	for (f = fields; f != NULL; f = f->fnext) {
		int align = f->ftype->talign;
		if (align > max_align) {
			max_align = align;
		}
		/* Align offset */
		offset = (offset + align - 1) & ~(align - 1);
		f->foffset = offset;
		offset += type_size(f->ftype);
	}

	/* Align total size */
	t->tsize = (offset + max_align - 1) & ~(max_align - 1);
	t->talign = max_align;

	return t;
}

TNODE *mkobject(char *tag, FIELD_LIST *fields, METHOD_LIST *methods, TNODE *parent)
{
	TNODE *t = mkrecord(tag, fields);
	t->ttype = TOBJECT;
	t->tattr.object.methods = methods;
	t->tattr.object.parent = parent;

	/* If there's a parent, inherit its size */
	if (parent && parent->ttype == TOBJECT) {
		/* Add parent size to this object's size */
		t->tsize += parent->tsize;
	}

	return t;
}

TNODE *mkpointer(TNODE *basetype)
{
	TNODE *t = (TNODE *)malloc(sizeof(TNODE));
	if (!t) {
		fatal("Out of memory");
	}

	t->ttype = TPOINTER;
	t->tattr.ptr.base = basetype;
	t->tsize = 4;  /* Pointer size */
	t->talign = 4;

	return t;
}

TNODE *mksubrange(int low, int high)
{
	TNODE *t = (TNODE *)malloc(sizeof(TNODE));
	if (!t) {
		fatal("Out of memory");
	}

	t->ttype = TSUBRANGE;
	t->tattr.subrange.low = low;
	t->tattr.subrange.high = high;
	t->tsize = 4;  /* Use integer size */
	t->talign = 4;

	return t;
}

TNODE *mkenum(ENUM_LIST *values)
{
	TNODE *t = (TNODE *)malloc(sizeof(TNODE));
	if (!t) {
		fatal("Out of memory");
	}

	t->ttype = TENUM;
	t->tattr.enumtype.values = values;
	t->tsize = 4;  /* Use integer size */
	t->talign = 4;

	return t;
}

TNODE *mkprocedure(TNODE *rettype, PARAM_LIST *params)
{
	TNODE *t = (TNODE *)malloc(sizeof(TNODE));
	if (!t) {
		fatal("Out of memory");
	}

	t->ttype = TPROCEDURE;
	t->tattr.proc.ret_type = rettype;
	t->tattr.proc.params = params;
	t->tsize = 4;  /* Pointer to procedure */
	t->talign = 4;

	return t;
}

int type_compatible(TNODE *t1, TNODE *t2)
{
	if (t1 == NULL || t2 == NULL) {
		return 0;
	}

	/* Same type */
	if (t1->ttype == t2->ttype) {
		return 1;
	}

	/* Variant is compatible with everything */
	if (t1->ttype == TVARIANT || t2->ttype == TVARIANT) {
		return 1;
	}

	/* Numeric types are compatible with each other */
	if ((t1->ttype == TSMALLINT || t1->ttype == TSHORTINT ||
	     t1->ttype == TLONGINT || t1->ttype == TNUMBER ||
	     t1->ttype == TCURRENCY) &&
	    (t2->ttype == TSMALLINT || t2->ttype == TSHORTINT ||
	     t2->ttype == TLONGINT || t2->ttype == TNUMBER ||
	     t2->ttype == TCURRENCY)) {
		return 1;
	}

	/* Date/time types are compatible */
	if ((t1->ttype == TDATE || t1->ttype == TTIME ||
	     t1->ttype == TDATETIME || t1->ttype == TTIMESTAMP) &&
	    (t2->ttype == TDATE || t2->ttype == TTIME ||
	     t2->ttype == TDATETIME || t2->ttype == TTIMESTAMP)) {
		return 1;
	}

	return 0;
}

int type_size(TNODE *t)
{
	if (t == NULL) {
		return 0;
	}
	return t->tsize;
}
