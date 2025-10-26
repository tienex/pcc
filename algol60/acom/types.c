/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Type system implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Predefined types */
TNODE *type_integer;
TNODE *type_real;
TNODE *type_boolean;
TNODE *type_string;

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
		t->tsize = 4;  /* 32-bit integer */
		t->talign = 4;
		break;
	case TREAL:
		t->tsize = 8;  /* 64-bit float (double) */
		t->talign = 8;
		break;
	case TBOOLEAN:
		t->tsize = 1;  /* boolean */
		t->talign = 1;
		break;
	case TSTRING:
		t->tsize = sizeof(void *);  /* pointer to string data */
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
mkarray(TNODE *elemtype, int ndims, int *lowers, int *uppers)
{
	TNODE *t;
	int i, total_elements;

	t = mktype(TARRAY);
	t->tattr.array.elem_type = elemtype;
	t->tattr.array.dim_count = ndims;

	/* Allocate and set bounds */
	t->tattr.array.bounds = malloc(ndims * sizeof(*t->tattr.array.bounds));
	if (t->tattr.array.bounds == NULL)
		fatal("out of memory");

	total_elements = 1;
	for (i = 0; i < ndims; i++) {
		t->tattr.array.bounds[i].lower = lowers[i];
		t->tattr.array.bounds[i].upper = uppers[i];
		total_elements *= (uppers[i] - lowers[i] + 1);
	}

	t->tsize = total_elements * elemtype->tsize;
	t->talign = elemtype->talign;

	return t;
}

/*
 * Create function type (procedure returning value)
 */
TNODE *
mkfunction(TNODE *rettype, PARAM_LIST *params)
{
	TNODE *t;

	t = mktype(TFUNCTION);
	t->tattr.func.ret_type = rettype;
	t->tattr.func.params = params;
	t->tsize = 0;  /* Functions don't have a size */
	t->talign = 1;

	return t;
}

/*
 * Create procedure type (no return value)
 */
TNODE *
mkprocedure(PARAM_LIST *params)
{
	TNODE *t;

	t = mktype(TPROCEDURE);
	t->tattr.func.ret_type = NULL;
	t->tattr.func.params = params;
	t->tsize = 0;
	t->talign = 1;

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

	/* Same type */
	if (t1->ttype == t2->ttype)
		return 1;

	/* Allow implicit integer to real conversion */
	if ((t1->ttype == TREAL && t2->ttype == TINTEGER) ||
	    (t1->ttype == TINTEGER && t2->ttype == TREAL))
		return 1;

	return 0;
}

/*
 * Get size of type
 */
int
type_size(TNODE *t)
{
	if (t == NULL)
		return 0;
	return t->tsize;
}

/*
 * Initialize built-in types
 */
void
init_types(void)
{
	type_integer = mktype(TINTEGER);
	type_real = mktype(TREAL);
	type_boolean = mktype(TBOOLEAN);
	type_string = mktype(TSTRING);
}
