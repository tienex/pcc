/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Type system implementation
 * Maps Go types to MIP IR types
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Predefined type nodes */
static TNODE *type_bool;
static TNODE *type_int8;
static TNODE *type_uint8;
static TNODE *type_int16;
static TNODE *type_uint16;
static TNODE *type_int32;
static TNODE *type_uint32;
static TNODE *type_int64;
static TNODE *type_uint64;
static TNODE *type_int;
static TNODE *type_uint;
static TNODE *type_uintptr;
static TNODE *type_float32;
static TNODE *type_float64;
static TNODE *type_string;

/*
 * Map Go type to MIP IR type
 */
static TWORD
go_to_mip_type(int go_type)
{
	switch (go_type) {
	case TBOOL:
		return BOOL;
	case TINT8:
		return CHAR;
	case TUINT8:
		return UCHAR;
	case TINT16:
		return SHORT;
	case TUINT16:
		return USHORT;
	case TINT32:
		return INT;
	case TUINT32:
		return UNSIGNED;
	case TINT64:
		return LONGLONG;
	case TUINT64:
		return ULONGLONG;
	case TINT:
		/* Platform-dependent: use LONG or INT */
#ifdef TARGET_64BIT
		return LONGLONG;
#else
		return INT;
#endif
	case TUINT:
#ifdef TARGET_64BIT
		return ULONGLONG;
#else
		return UNSIGNED;
#endif
	case TUINTPTR:
		return ULONG;
	case TFLOAT32:
		return FLOAT;
	case TFLOAT64:
		return DOUBLE;
	default:
		return INT;  /* Default fallback */
	}
}

/*
 * Initialize type system
 */
void
init_types(void)
{
	/* Create predefined types */
	type_bool = mktype(TBOOL);
	type_int8 = mktype(TINT8);
	type_uint8 = mktype(TUINT8);
	type_int16 = mktype(TINT16);
	type_uint16 = mktype(TUINT16);
	type_int32 = mktype(TINT32);
	type_uint32 = mktype(TUINT32);
	type_int64 = mktype(TINT64);
	type_uint64 = mktype(TUINT64);
	type_int = mktype(TINT);
	type_uint = mktype(TUINT);
	type_uintptr = mktype(TUINTPTR);
	type_float32 = mktype(TFLOAT32);
	type_float64 = mktype(TFLOAT64);
	type_string = mktype(TSTRING);
}

/*
 * Create basic type
 */
TNODE *
mktype(int basictype)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = basictype;

	/* Set size based on type */
	switch (basictype) {
	case TBOOL:
	case TINT8:
	case TUINT8:
		t->tsize = 1;
		t->talign = 1;
		break;
	case TINT16:
	case TUINT16:
		t->tsize = 2;
		t->talign = 2;
		break;
	case TINT32:
	case TUINT32:
	case TFLOAT32:
		t->tsize = 4;
		t->talign = 4;
		break;
	case TINT64:
	case TUINT64:
	case TFLOAT64:
	case TCOMPLEX64:
		t->tsize = 8;
		t->talign = 8;
		break;
	case TCOMPLEX128:
		t->tsize = 16;
		t->talign = 8;
		break;
	case TINT:
	case TUINT:
	case TUINTPTR:
		/* Platform-dependent */
#ifdef TARGET_64BIT
		t->tsize = 8;
		t->talign = 8;
#else
		t->tsize = 4;
		t->talign = 4;
#endif
		break;
	case TSTRING:
		/* String is a struct { ptr, len } */
		t->tsize = sizeof(void *) * 2;
		t->talign = sizeof(void *);
		break;
	default:
		t->tsize = 0;
		t->talign = 1;
	}

	return t;
}

/*
 * Create array type
 */
TNODE *
mkarray(TNODE *elemtype, int length)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TARRAY;
	t->tattr.array.elem_type = elemtype;
	t->tattr.array.length = length;

	if (length > 0 && elemtype != NULL) {
		t->tsize = elemtype->tsize * length;
		t->talign = elemtype->talign;
	}

	return t;
}

/*
 * Create slice type
 */
TNODE *
mkslice(TNODE *elemtype)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TSLICE;
	t->tattr.array.elem_type = elemtype;
	t->tattr.array.length = -1;  /* -1 indicates slice */

	/* Slice is { ptr, len, cap } */
	t->tsize = sizeof(void *) * 3;
	t->talign = sizeof(void *);

	return t;
}

/*
 * Create struct type
 */
TNODE *
mkstruct(char *tag, FIELD_LIST *fields)
{
	TNODE *t;
	FIELD_LIST *f;
	int offset = 0;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TSTRUCT;
	t->tattr.structure.tag = tag ? str_copy(tag) : NULL;
	t->tattr.structure.fields = fields;

	/* Calculate offsets and size */
	for (f = fields; f != NULL; f = f->fnext) {
		/* Align field */
		if (f->ftype->talign > 0) {
			int align = f->ftype->talign;
			offset = (offset + align - 1) & ~(align - 1);
		}
		f->foffset = offset;
		offset += f->ftype->tsize;

		/* Update alignment */
		if (f->ftype->talign > t->talign)
			t->talign = f->ftype->talign;
	}

	/* Final size (aligned) */
	if (t->talign > 0)
		t->tsize = (offset + t->talign - 1) & ~(t->talign - 1);
	else
		t->tsize = offset;

	return t;
}

/*
 * Create pointer type
 */
TNODE *
mkpointer(TNODE *basetype)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TPOINTER;
	t->tattr.ptr.base = basetype;
	t->tsize = sizeof(void *);
	t->talign = sizeof(void *);

	return t;
}

/*
 * Create function type
 */
TNODE *
mkfunction(TNODE *rettype, PARAM_LIST *params, PARAM_LIST *results)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TFUNCTION;
	t->tattr.func.ret_type = rettype;
	t->tattr.func.params = params;
	t->tattr.func.results = results;
	t->tattr.func.is_variadic = 0;

	/* Function pointers */
	t->tsize = sizeof(void *);
	t->talign = sizeof(void *);

	return t;
}

/*
 * Create interface type
 */
TNODE *
mkinterface(char *name, METHOD_LIST *methods)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TINTERFACE;
	t->tattr.iface.iface_name = name ? str_copy(name) : NULL;
	t->tattr.iface.methods = methods;

	/* Interface is { type, data } */
	t->tsize = sizeof(void *) * 2;
	t->talign = sizeof(void *);

	return t;
}

/*
 * Create map type
 */
TNODE *
mkmap(TNODE *keytype, TNODE *valtype)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TMAP;
	t->tattr.map.key_type = keytype;
	t->tattr.map.val_type = valtype;

	/* Map is a pointer to runtime hashmap */
	t->tsize = sizeof(void *);
	t->talign = sizeof(void *);

	return t;
}

/*
 * Create channel type
 */
TNODE *
mkchan(TNODE *elemtype, int direction)
{
	TNODE *t;

	t = (TNODE *)xcalloc(1, sizeof(TNODE));
	t->ttype = TCHAN;
	t->tattr.chan.elem_type = elemtype;
	t->tattr.chan.direction = direction;

	/* Channel is a pointer to runtime channel */
	t->tsize = sizeof(void *);
	t->talign = sizeof(void *);

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

	/* Same type */
	if (t1 == t2)
		return 1;

	/* Same basic type */
	if (t1->ttype == t2->ttype) {
		switch (t1->ttype) {
		case TARRAY:
			return t1->tattr.array.length == t2->tattr.array.length &&
			       type_compatible(t1->tattr.array.elem_type,
			                       t2->tattr.array.elem_type);
		case TSLICE:
			return type_compatible(t1->tattr.array.elem_type,
			                       t2->tattr.array.elem_type);
		case TPOINTER:
			return type_compatible(t1->tattr.ptr.base,
			                       t2->tattr.ptr.base);
		default:
			return 1;
		}
	}

	return 0;
}

/*
 * Get type size
 */
int
type_size(TNODE *t)
{
	if (t == NULL)
		return 0;

	return t->tsize;
}
