/*
 * Copyright (c) 2025 PCC OCaml Compiler
 *
 * Type system implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Predefined types */
TNODE *int_type;
TNODE *float_type;
TNODE *bool_type;
TNODE *char_type;
TNODE *string_type;
TNODE *unit_type;

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
	case TINT:
		t->tsize = sizeof(int);
		t->talign = sizeof(int);
		break;
	case TFLOAT:
		t->tsize = sizeof(double);
		t->talign = sizeof(double);
		break;
	case TCHAR:
		t->tsize = sizeof(char);
		t->talign = sizeof(char);
		break;
	case TSTRING:
		t->tsize = sizeof(void *);
		t->talign = sizeof(void *);
		break;
	case TBOOL:
		t->tsize = sizeof(char);
		t->talign = sizeof(char);
		break;
	case TUNIT:
		t->tsize = 0;
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
 * Create an array type
 */
TNODE *
mkarray(TNODE *elemtype)
{
	TNODE *t;

	t = mktype(TARRAY);
	t->tsubtype = elemtype;
	t->tsize = sizeof(void *);  /* Array is a pointer */
	t->talign = sizeof(void *);

	return t;
}

/*
 * Create a function type
 */
TNODE *
mkfunction(TNODE *argtype, TNODE *rettype)
{
	TNODE *t;

	t = mktype(TFUNCTION);
	t->tsubtype = argtype;
	t->tnext = rettype;

	return t;
}

/*
 * Check if two types are compatible
 */
int
compatible_types(TNODE *t1, TNODE *t2)
{
	if (t1 == NULL || t2 == NULL)
		return 0;

	if (t1->ttype != t2->ttype)
		return 0;

	/* Check subtypes for complex types */
	switch (t1->ttype) {
	case TARRAY:
	case TLIST:
		return compatible_types(t1->tsubtype, t2->tsubtype);
	case TFUNCTION:
		return compatible_types(t1->tsubtype, t2->tsubtype) &&
		       compatible_types(t1->tnext, t2->tnext);
	default:
		return 1;
	}
}

/*
 * Initialize predefined types
 */
void
init_types(void)
{
	int_type = mktype(TINT);
	float_type = mktype(TFLOAT);
	bool_type = mktype(TBOOL);
	char_type = mktype(TCHAR);
	string_type = mktype(TSTRING);
	unit_type = mktype(TUNIT);
}
