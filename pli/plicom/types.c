/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Type system implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Create basic type */
TNODE *mktype(int basictype) {
	TNODE *t = malloc(sizeof(TNODE));
	if (t == NULL) {
		fatal("out of memory");
	}

	memset(t, 0, sizeof(TNODE));
	t->ttype = basictype;

	/* Set default sizes */
	switch (basictype) {
	case TFIXED:
		t->tsize = 4;  /* FIXED BINARY(31) */
		t->talign = 4;
		t->tprec = 31;
		t->tscale = 0;
		break;
	case TFLOAT:
		t->tsize = 8;  /* FLOAT BINARY(53) - double precision */
		t->talign = 8;
		t->tprec = 53;
		break;
	case TPOINTER:
		t->tsize = sizeof(void*);
		t->talign = sizeof(void*);
		break;
	case TOFFSET:
		t->tsize = sizeof(void*);
		t->talign = sizeof(void*);
		break;
	case TBYTE:
		t->tsize = 1;
		t->talign = 1;
		break;
	case TWORD:
		t->tsize = 2;
		t->talign = 2;
		break;
	case TDWORD:
		t->tsize = 4;
		t->talign = 4;
		break;
	case TADDRESS:
		t->tsize = sizeof(void*);
		t->talign = sizeof(void*);
		break;
	case TINTEGER:
		t->tsize = 2;
		t->talign = 2;
		break;
	case TREAL:
		t->tsize = 4;
		t->talign = 4;
		break;
	default:
		t->tsize = 0;
		t->talign = 1;
		break;
	}

	return t;
}

/* Create FIXED type */
TNODE *mkfixed(int prec, int scale, int attrs) {
	TNODE *t = mktype(TFIXED);
	t->tprec = prec;
	t->tscale = scale;

	/* Adjust size based on precision */
	if (prec <= 7) {
		t->tsize = 1;
		t->talign = 1;
	} else if (prec <= 15) {
		t->tsize = 2;
		t->talign = 2;
	} else if (prec <= 31) {
		t->tsize = 4;
		t->talign = 4;
	} else {
		t->tsize = 8;
		t->talign = 8;
	}

	return t;
}

/* Create FLOAT type */
TNODE *mkfloat(int prec, int attrs) {
	TNODE *t = mktype(TFLOAT);
	t->tprec = prec;

	/* Single or double precision */
	if (prec <= 24) {
		t->tsize = 4;  /* Single precision */
		t->talign = 4;
	} else {
		t->tsize = 8;  /* Double precision */
		t->talign = 8;
	}

	return t;
}

/* Create BIT type */
TNODE *mkbit(int len) {
	TNODE *t = mktype(TBIT);
	t->tattr.string.len = len;
	t->tattr.string.varying = 0;

	/* Size in bytes (rounded up) */
	t->tsize = (len + 7) / 8;
	t->talign = 1;

	return t;
}

/* Create CHARACTER type */
TNODE *mkchar(int len, int varying) {
	TNODE *t = mktype(TCHAR);
	t->tattr.string.len = len;
	t->tattr.string.varying = varying;

	if (varying) {
		t->tsize = len + 2;  /* +2 for length prefix */
	} else {
		t->tsize = len;
	}
	t->talign = 1;

	return t;
}

/* Create array type */
TNODE *mkarray(TNODE *elemtype, int ndims, int *lower, int *upper) {
	TNODE *t = mktype(TARRAY);
	t->tattr.array.elem_type = elemtype;
	t->tattr.array.ndims = ndims;

	/* Allocate and copy bounds */
	t->tattr.array.lower = malloc(ndims * sizeof(int));
	t->tattr.array.upper = malloc(ndims * sizeof(int));
	memcpy(t->tattr.array.lower, lower, ndims * sizeof(int));
	memcpy(t->tattr.array.upper, upper, ndims * sizeof(int));

	/* Calculate total size */
	int total_elems = 1;
	for (int i = 0; i < ndims; i++) {
		total_elems *= (upper[i] - lower[i] + 1);
	}
	t->tsize = total_elems * elemtype->tsize;
	t->talign = elemtype->talign;

	return t;
}

/* Create structure type */
TNODE *mkstruct(char *tag, MEMBER_LIST *members, int level) {
	TNODE *t = mktype(TSTRUCT);
	t->tattr.structure.tag = tag ? strdup(tag) : NULL;
	t->tattr.structure.members = members;
	t->tattr.structure.level = level;

	/* Calculate size and alignment */
	int size = 0;
	int align = 1;

	for (MEMBER_LIST *m = members; m != NULL; m = m->mnext) {
		if (m->mtype->talign > align) {
			align = m->mtype->talign;
		}
		/* Align member */
		size = (size + m->mtype->talign - 1) & ~(m->mtype->talign - 1);
		m->moffset = size;
		size += m->mtype->tsize;
	}

	/* Pad to alignment */
	size = (size + align - 1) & ~(align - 1);

	t->tsize = size;
	t->talign = align;

	return t;
}

/* Create pointer type */
TNODE *mkpointer(TNODE *basetype) {
	TNODE *t = mktype(TPOINTER);
	t->tattr.ptr.base = basetype;
	t->tattr.ptr.area = 0;
	return t;
}

/* Create picture type */
TNODE *mkpicture(char *pic) {
	TNODE *t = mktype(TPICTURE);
	t->tattr.picture.picture = strdup(pic);
	t->tattr.picture.base_type = NULL;  /* TBD based on picture */
	return t;
}

/* Check type compatibility */
int type_compatible(TNODE *t1, TNODE *t2) {
	if (t1 == NULL || t2 == NULL) {
		return 0;
	}

	/* Same type */
	if (t1->ttype == t2->ttype) {
		switch (t1->ttype) {
		case TFIXED:
			/* FIXED types compatible if same precision/scale */
			return t1->tprec == t2->tprec && t1->tscale == t2->tscale;
		case TFLOAT:
			/* FLOAT types compatible if same precision */
			return t1->tprec == t2->tprec;
		case TBIT:
		case TCHAR:
			/* String types compatible if same length */
			return t1->tattr.string.len == t2->tattr.string.len &&
			       t1->tattr.string.varying == t2->tattr.string.varying;
		case TPOINTER:
			/* Pointers compatible if same base type */
			return type_compatible(t1->tattr.ptr.base, t2->tattr.ptr.base);
		default:
			return 1;
		}
	}

	/* Numeric types are convertible */
	if ((t1->ttype == TFIXED || t1->ttype == TFLOAT ||
	     t1->ttype == TBYTE || t1->ttype == TWORD || t1->ttype == TDWORD ||
	     t1->ttype == TINTEGER || t1->ttype == TREAL) &&
	    (t2->ttype == TFIXED || t2->ttype == TFLOAT ||
	     t2->ttype == TBYTE || t2->ttype == TWORD || t2->ttype == TDWORD ||
	     t2->ttype == TINTEGER || t2->ttype == TREAL)) {
		return 1;
	}

	return 0;
}

/* Get type size */
int type_size(TNODE *t) {
	if (t == NULL) {
		return 0;
	}
	return t->tsize;
}
