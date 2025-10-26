/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Type system implementation for Xbase++
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/*
 * Create a basic type node
 */
TNODE *
make_type(int ttype)
{
	TNODE *tp;

	tp = (TNODE *)malloc(sizeof(TNODE));
	if (tp == NULL) {
		error("out of memory");
		exit(1);
	}

	memset(tp, 0, sizeof(TNODE));
	tp->ttype = ttype;
	tp->tsize = type_size(tp);
	tp->talign = tp->tsize;

	return tp;
}

/*
 * Create array type
 */
TNODE *
make_array_type(TNODE *elem, int *dims, int ndims)
{
	TNODE *tp;
	int i;

	tp = make_type(TARRAY);
	tp->tattr.array.elem_type = elem;
	tp->tattr.array.dim_count = ndims;

	if (ndims > 0) {
		tp->tattr.array.dimensions = (int *)malloc(ndims * sizeof(int));
		for (i = 0; i < ndims; i++) {
			tp->tattr.array.dimensions[i] = dims[i];
		}
	}

	return tp;
}

/*
 * Create function type
 */
TNODE *
make_func_type(TNODE *ret, PARAM_LIST *params)
{
	TNODE *tp;
	PARAM_LIST *p;
	int count = 0;

	tp = make_type(TVARIANT);  /* Functions can return any type in Xbase++ */
	tp->tattr.func.ret_type = ret;
	tp->tattr.func.params = params;

	/* Count parameters */
	for (p = params; p != NULL; p = p->pnext)
		count++;
	tp->tattr.func.param_count = count;

	return tp;
}

/*
 * Create object/class type
 */
TNODE *
make_object_type(char *tag)
{
	TNODE *tp;

	tp = make_type(TOBJECT);
	tp->tattr.object.tag = strdup(tag);
	tp->tattr.object.fields = NULL;
	tp->tattr.object.methods = NULL;
	tp->tattr.object.parent = NULL;

	return tp;
}

/*
 * Check if two types are equal
 */
int
type_equal(TNODE *t1, TNODE *t2)
{
	if (t1 == NULL || t2 == NULL)
		return 0;

	if (t1->ttype != t2->ttype)
		return 0;

	/* For now, just check basic type */
	/* TODO: Add more sophisticated type checking for arrays, objects, etc. */
	return 1;
}

/*
 * Check if two types are compatible (for assignment, comparison, etc.)
 * Xbase++ has very loose type compatibility - almost everything can be converted
 */
int
type_compatible(TNODE *t1, TNODE *t2)
{
	if (t1 == NULL || t2 == NULL)
		return 1;  /* Untyped is compatible with everything */

	/* TVARIANT is compatible with everything */
	if (t1->ttype == TVARIANT || t2->ttype == TVARIANT)
		return 1;

	/* NIL is compatible with everything */
	if (t1->ttype == TNIL || t2->ttype == TNIL)
		return 1;

	/* Numeric types are compatible with each other */
	if ((t1->ttype == TNUMERIC || t1->ttype == TINTEGER || t1->ttype == TFLOAT) &&
	    (t2->ttype == TNUMERIC || t2->ttype == TINTEGER || t2->ttype == TFLOAT))
		return 1;

	/* Same type is always compatible */
	if (t1->ttype == t2->ttype)
		return 1;

	/* Otherwise not compatible */
	return 0;
}

/*
 * Get size of a type in bytes
 */
int
type_size(TNODE *tp)
{
	int size;
	int i;

	if (tp == NULL)
		return 0;

	switch (tp->ttype) {
	case TNULL:
	case TNIL:
		return 0;

	case TLOGICAL:
		return 1;

	case TINTEGER:
		return 8;  /* 64-bit integer */

	case TFLOAT:
	case TNUMERIC:
		return 8;  /* 64-bit double */

	case TCHAR:
		return 8;  /* Pointer to string */

	case TDATE:
	case TTIMESTAMP:
		return 8;  /* Internal date representation */

	case TARRAY:
		/* Arrays are reference types (pointers) */
		return 8;

	case TOBJECT:
		/* Objects are reference types (pointers) */
		return 8;

	case TCODEBLOCK:
		/* Code blocks are pointers to code */
		return 8;

	case TMEMO:
		/* Memo fields are pointers */
		return 8;

	case TVARIANT:
		/* Variant holds type tag + value */
		return 16;

	default:
		return 8;  /* Default pointer size */
	}
}

/*
 * Get alignment of a type
 */
int
type_align(TNODE *tp)
{
	if (tp == NULL)
		return 1;

	/* For simplicity, alignment is same as size (up to 8 bytes) */
	int size = type_size(tp);
	return (size > 8) ? 8 : size;
}

/*
 * Get type name (for error messages)
 */
const char *
type_name(TNODE *tp)
{
	if (tp == NULL)
		return "unknown";

	switch (tp->ttype) {
	case TNULL:     return "null";
	case TNUMERIC:  return "numeric";
	case TCHAR:     return "character";
	case TDATE:     return "date";
	case TLOGICAL:  return "logical";
	case TARRAY:    return "array";
	case TOBJECT:   return "object";
	case TCODEBLOCK: return "codeblock";
	case TMEMO:     return "memo";
	case TNIL:      return "nil";
	case TVARIANT:  return "variant";
	case TINTEGER:  return "integer";
	case TFLOAT:    return "float";
	case TTIMESTAMP: return "timestamp";
	case TSYMBOL:   return "symbol";
	default:        return "unknown";
	}
}
