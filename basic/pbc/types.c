/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * Type system implementation for BASIC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Built-in type nodes */
static TNODE *type_integer = NULL;
static TNODE *type_long = NULL;
static TNODE *type_single = NULL;
static TNODE *type_double = NULL;
static TNODE *type_string = NULL;

/*
 * Initialize type system
 */
void
init_types(void)
{
	/* Create basic types */
	type_integer = mktype(TINTEGER);
	type_long = mktype(TLONG);
	type_single = mktype(TSINGLE);
	type_double = mktype(TDOUBLE);
	type_string = mktype(TSTRING);
}

/*
 * Create a basic type node
 */
TNODE *
mktype(int basictype)
{
	TNODE *t = malloc(sizeof(TNODE));
	memset(t, 0, sizeof(TNODE));
	t->ttype = basictype;

	/* Set sizes based on type */
	switch (basictype) {
	case TINTEGER:
		t->tsize = 2;
		t->talign = 2;
		break;
	case TLONG:
		t->tsize = 4;
		t->talign = 4;
		break;
	case TSINGLE:
		t->tsize = 4;
		t->talign = 4;
		break;
	case TDOUBLE:
		t->tsize = 8;
		t->talign = 8;
		break;
	case TSTRING:
		t->tsize = sizeof(void *);  /* Pointer to string */
		t->talign = sizeof(void *);
		break;
	default:
		t->tsize = 4;
		t->talign = 4;
		break;
	}

	return t;
}

/*
 * Create array type
 */
TNODE *
mkarray(TNODE *elemtype, int *dims, int *lower, int ndims)
{
	TNODE *t = malloc(sizeof(TNODE));
	memset(t, 0, sizeof(TNODE));
	t->ttype = TARRAY;
	t->tattr.array.elem_type = elemtype;
	t->tattr.array.dim_count = ndims;
	t->tattr.array.dimensions = malloc(ndims * sizeof(int));
	memcpy(t->tattr.array.dimensions, dims, ndims * sizeof(int));

	if (lower) {
		t->tattr.array.lower_bounds = malloc(ndims * sizeof(int));
		memcpy(t->tattr.array.lower_bounds, lower, ndims * sizeof(int));
	}

	return t;
}

/*
 * Create fixed-length string type
 */
TNODE *
mkstring(int length)
{
	TNODE *t = mktype(TSTRING);
	t->tattr.string.length = length;
	return t;
}

/*
 * Create record/UDT type
 */
TNODE *
mkrecord(FIELD_LIST *fields)
{
	TNODE *t = malloc(sizeof(TNODE));
	memset(t, 0, sizeof(TNODE));
	t->ttype = TRECORD;
	t->tattr.record.fields = fields;
	return t;
}

/*
 * Create function type
 */
TNODE *
mkfunction(TNODE *rettype, PARAM_LIST *params)
{
	TNODE *t = malloc(sizeof(TNODE));
	memset(t, 0, sizeof(TNODE));
	t->tattr.func.ret_type = rettype;
	t->tattr.func.params = params;
	return t;
}

/*
 * Check type compatibility
 */
int
type_compatible(TNODE *t1, TNODE *t2)
{
	if (t1 == NULL || t2 == NULL)
		return 0;

	if (t1->ttype == t2->ttype)
		return 1;

	/* Numeric types are compatible */
	if (is_numeric_type(t1->ttype) && is_numeric_type(t2->ttype))
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
 * Get variable type from suffix ($ % & ! #)
 */
TNODE *
get_var_type(char *name)
{
	char *suffix = type_suffix(name);

	if (suffix == NULL || *suffix == '\0')
		return type_single;  /* Default type */

	switch (*suffix) {
	case '%':
		return type_integer;
	case '&':
		return type_long;
	case '!':
		return type_single;
	case '#':
		return type_double;
	case '$':
		return type_string;
	default:
		return type_single;
	}
}

/*
 * Get type suffix from variable name
 */
char *
type_suffix(char *name)
{
	int len = strlen(name);
	if (len == 0)
		return NULL;

	char last = name[len-1];
	if (last == '$' || last == '%' || last == '&' ||
	    last == '!' || last == '#')
		return &name[len-1];

	return NULL;
}

/*
 * Check if type is numeric
 */
int
is_numeric_type(int ttype)
{
	return (ttype == TINTEGER || ttype == TLONG ||
	        ttype == TSINGLE || ttype == TDOUBLE ||
	        ttype == TBYTE);
}

/*
 * Check if type is string
 */
int
is_string_type(int ttype)
{
	return (ttype == TSTRING);
}
