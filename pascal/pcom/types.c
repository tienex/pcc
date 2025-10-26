/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Type system implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Predefined types */
TNODE *integer_type;
TNODE *real_type;
TNODE *boolean_type;
TNODE *char_type;
TNODE *text_type;

/*
 * Create a basic type node
 */
TNODE *
mktype(int basictype)
{
	TNODE *t;

	t = calloc(1, sizeof(TNODE));
	if (t == NULL)
		fatal("out of memory");

	t->ttype = basictype;

	/* Set size and alignment based on type */
	switch (basictype) {
	case TINTEGER:
	case TLONGINT:
		t->tsize = dialect_features->default_int_size / 8;
		t->talign = t->tsize;
		break;
	case TSHORTINT:
	case TBYTE:
		t->tsize = 1;
		t->talign = 1;
		break;
	case TSMALLINT:
	case TWORD:
		t->tsize = 2;
		t->talign = 2;
		break;
	case TDWORD:
		t->tsize = 4;
		t->talign = 4;
		break;
	case TINT64:
		t->tsize = 8;
		t->talign = 8;
		break;
	case TREAL:
	case TSINGLE:
		t->tsize = 4;
		t->talign = 4;
		break;
	case TDOUBLE:
		t->tsize = 8;
		t->talign = 8;
		break;
	case TEXTENDED:
		t->tsize = 10;  /* 80-bit extended */
		t->talign = 4;  /* Usually aligned to 4 bytes */
		break;
	case TBOOLEAN:
	case TCHAR:
		t->tsize = 1;
		t->talign = 1;
		break;
	case TPOINTER:
		t->tsize = sizeof(void *);
		t->talign = sizeof(void *);
		break;
	default:
		t->tsize = 0;
		t->talign = 1;
		break;
	}

	return t;
}

/*
 * Create array type
 */
TNODE *
mkarray(TNODE *elemtype, int *dims, int ndims)
{
	TNODE *t;
	int i, total_size;

	t = mktype(TARRAY);
	t->tattr.array.elem_type = elemtype;
	t->tattr.array.dim_count = ndims;
	t->tattr.array.dimensions = malloc(ndims * sizeof(int));

	total_size = type_size(elemtype);
	for (i = 0; i < ndims; i++) {
		t->tattr.array.dimensions[i] = dims[i];
		total_size *= dims[i];
	}

	t->tsize = total_size;
	t->talign = elemtype->talign;

	return t;
}

/*
 * Create record type
 */
TNODE *
mkrecord(char *tag, FIELD_LIST *fields)
{
	TNODE *t;
	FIELD_LIST *f;
	int offset = 0;

	t = mktype(TRECORD);
	t->tattr.record.tag = tag ? strdup(tag) : NULL;
	t->tattr.record.fields = fields;

	/* Calculate field offsets and total size */
	for (f = fields; f != NULL; f = f->fnext) {
		int fsize = type_size(f->ftype);
		int falign = f->ftype->talign;

		/* Align field */
		offset = (offset + falign - 1) & ~(falign - 1);
		f->foffset = offset;
		offset += fsize;
	}

	/* Align total size */
	t->tsize = offset;
	t->talign = 4;  /* Default alignment for records */

	return t;
}

/*
 * Create pointer type
 */
TNODE *
mkpointer(TNODE *basetype)
{
	TNODE *t;

	t = mktype(TPOINTER);
	t->tattr.ptr.base = basetype;

	return t;
}

/*
 * Create subrange type
 */
TNODE *
mksubrange(int low, int high)
{
	TNODE *t;

	if (low > high) {
		error("subrange lower bound (%d) exceeds upper bound (%d)",
		    low, high);
	}

	t = mktype(TSUBRANGE);
	t->tattr.subrange.low = low;
	t->tattr.subrange.high = high;

	/* Size depends on range */
	if (high - low < 256)
		t->tsize = 1;
	else if (high - low < 65536)
		t->tsize = 2;
	else
		t->tsize = 4;

	t->talign = t->tsize;

	return t;
}

/*
 * Create enumeration type
 */
TNODE *
mkenum(ENUM_LIST *values)
{
	TNODE *t;
	ENUM_LIST *e;
	int count = 0;

	t = mktype(TENUM);
	t->tattr.enumtype.values = values;

	/* Count values */
	for (e = values; e != NULL; e = e->enext)
		count++;

	/* Size depends on number of values */
	if (count < 256)
		t->tsize = 1;
	else if (count < 65536)
		t->tsize = 2;
	else
		t->tsize = 4;

	t->talign = t->tsize;

	return t;
}

/*
 * Create set type
 */
TNODE *
mkset(TNODE *basetype)
{
	TNODE *t;

	if (!ALLOW_SET_OPERATORS()) {
		error("set types not allowed in current dialect");
	}

	t = mktype(TSET);
	t->tattr.set.base = basetype;

	/* Set is implemented as bit array */
	/* For now, assume max 256 elements = 32 bytes */
	t->tsize = 32;
	t->talign = 4;

	return t;
}

/*
 * Create function type
 */
TNODE *
mkfunction(TNODE *rettype, PARAM_LIST *params)
{
	TNODE *t;

	t = mktype(TFUNCTION);
	t->tattr.func.ret_type = rettype;
	t->tattr.func.params = params;

	return t;
}

/*
 * Create procedure type
 */
TNODE *
mkprocedure(PARAM_LIST *params)
{
	TNODE *t;

	t = mktype(TPROCEDURE);
	t->tattr.func.ret_type = NULL;
	t->tattr.func.params = params;

	return t;
}

/*
 * Check if two types are compatible
 */
int
type_compatible(TNODE *t1, TNODE *t2)
{
	if (t1 == NULL || t2 == NULL)
		return 0;

	/* Same type pointer */
	if (t1 == t2)
		return 1;

	/* Same basic type */
	if (t1->ttype == t2->ttype) {
		switch (t1->ttype) {
		case TINTEGER:
		case TREAL:
		case TBOOLEAN:
		case TCHAR:
			return 1;

		case TARRAY:
			return type_compatible(t1->tattr.array.elem_type,
			                      t2->tattr.array.elem_type);

		case TPOINTER:
			return type_compatible(t1->tattr.ptr.base,
			                      t2->tattr.ptr.base);

		case TRECORD:
			/* Records are compatible if they have same tag */
			if (t1->tattr.record.tag && t2->tattr.record.tag)
				return strcmp(t1->tattr.record.tag,
				             t2->tattr.record.tag) == 0;
			return 0;

		default:
			return 0;
		}
	}

	/* Real and integer are compatible in some contexts */
	if ((t1->ttype == TREAL && t2->ttype == TINTEGER) ||
	    (t1->ttype == TINTEGER && t2->ttype == TREAL))
		return 1;

	/* Subranges are compatible with their base type */
	if (t1->ttype == TSUBRANGE && t2->ttype == TINTEGER)
		return 1;
	if (t2->ttype == TSUBRANGE && t1->ttype == TINTEGER)
		return 1;

	return 0;
}

/*
 * Get size of type in bytes
 */
int
type_size(TNODE *t)
{
	if (t == NULL)
		return 0;

	return t->tsize;
}

/*
 * Initialize predefined types
 */
void
init_types(void)
{
	integer_type = mktype(TINTEGER);
	real_type = mktype(TREAL);
	boolean_type = mktype(TBOOLEAN);
	char_type = mktype(TCHAR);
	text_type = mktype(TTEXT);
}
