/*	$Id$	*/

/*
 * Enhanced Type System Support for Debug Symbols
 * Provides comprehensive type handling including complex types
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Type cache for deduplication */
#define TYPE_CACHE_SIZE 1024

static struct {
	debug_type_t *cache[TYPE_CACHE_SIZE];
	int cache_count;
	int primitive_count;
	int composite_count;
} type_mgr;

/*
 * Hash a type for caching
 */
static unsigned int
hash_type(debug_type_encoding_t enc, unsigned int size)
{
	return ((unsigned int)enc * 31 + size) % TYPE_CACHE_SIZE;
}

/*
 * Initialize type manager
 */
void
debugsym_type_init(void)
{
	memset(&type_mgr, 0, sizeof(type_mgr));
}

/*
 * Find type in cache
 */
static debug_type_t *
find_cached_type(debug_type_encoding_t enc, unsigned int size)
{
	unsigned int hash = hash_type(enc, size);
	debug_type_t *t;

	for (t = type_mgr.cache[hash]; t != NULL; t = t->next_cached) {
		if (t->encoding == enc && t->size == size &&
		    t->base_type == NULL && t->array_dimensions == 0)
			return t;
	}
	return NULL;
}

/*
 * Add type to cache
 */
static void
cache_type(debug_type_t *type)
{
	unsigned int hash;

	if (type == NULL || type->base_type != NULL || type->array_dimensions > 0)
		return;

	hash = hash_type(type->encoding, type->size);
	type->next_cached = type_mgr.cache[hash];
	type_mgr.cache[hash] = type;
	type_mgr.cache_count++;
}

/*
 * Create or get cached primitive type
 */
debug_type_t *
debugsym_get_primitive_type(debug_type_encoding_t enc, unsigned int size)
{
	debug_type_t *type;

	/* Check cache first */
	type = find_cached_type(enc, size);
	if (type != NULL)
		return type;

	/* Create new type */
	type = debugsym_primitive_type(enc, size);
	if (type != NULL) {
		cache_type(type);
		type_mgr.primitive_count++;
	}

	return type;
}

/*
 * Create const-qualified type
 */
debug_type_t *
debugsym_const_type(debug_type_t *base)
{
	debug_type_t *type;

	if (base == NULL)
		return NULL;

	/* Check if already const */
	if (base->is_const)
		return base;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	memcpy(type, base, sizeof(debug_type_t));
	type->is_const = 1;
	type_mgr.composite_count++;

	return type;
}

/*
 * Create volatile-qualified type
 */
debug_type_t *
debugsym_volatile_type(debug_type_t *base)
{
	debug_type_t *type;

	if (base == NULL)
		return NULL;

	if (base->is_volatile)
		return base;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	memcpy(type, base, sizeof(debug_type_t));
	type->is_volatile = 1;
	type_mgr.composite_count++;

	return type;
}

/*
 * Create restrict-qualified type
 */
debug_type_t *
debugsym_restrict_type(debug_type_t *base)
{
	debug_type_t *type;

	if (base == NULL)
		return NULL;

	if (base->is_restrict)
		return base;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	memcpy(type, base, sizeof(debug_type_t));
	type->is_restrict = 1;
	type_mgr.composite_count++;

	return type;
}

/*
 * Create function type
 */
debug_type_t *
debugsym_function_type(debug_type_t *return_type, debug_type_t **param_types, int param_count)
{
	debug_type_t *type;
	int i;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	type->encoding = DBGTYPE_FUNCTION;
	type->base_type = return_type;
	type->size = sizeof(void *);

	/* Store parameter types */
	if (param_count > 0) {
		type->param_types = (debug_type_t **)calloc(param_count, sizeof(debug_type_t *));
		if (type->param_types != NULL) {
			for (i = 0; i < param_count; i++)
				type->param_types[i] = param_types[i];
			type->param_count = param_count;
		}
	}

	type_mgr.composite_count++;
	return type;
}

/*
 * Create struct/union type
 */
debug_type_t *
debugsym_composite_type(debug_type_encoding_t enc, const char *name,
    debug_member_t *members, int member_count, unsigned int size)
{
	debug_type_t *type;
	int i;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	type->encoding = enc;
	type->name = debugsym_strdup(name);
	type->size = size;

	if (member_count > 0 && members != NULL) {
		type->members = (debug_member_t *)calloc(member_count, sizeof(debug_member_t));
		if (type->members != NULL) {
			for (i = 0; i < member_count; i++) {
				type->members[i].name = debugsym_strdup(members[i].name);
				type->members[i].type = members[i].type;
				type->members[i].offset = members[i].offset;
				type->members[i].bit_size = members[i].bit_size;
				type->members[i].bit_offset = members[i].bit_offset;
			}
			type->member_count = member_count;
		}
	}

	type_mgr.composite_count++;
	return type;
}

/*
 * Create enumeration type
 */
debug_type_t *
debugsym_enum_type(const char *name, debug_enum_value_t *values, int value_count)
{
	debug_type_t *type;
	int i;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	type->encoding = DBGTYPE_ENUM;
	type->name = debugsym_strdup(name);
	type->size = sizeof(int);  /* Default enum size */

	if (value_count > 0 && values != NULL) {
		type->enum_values = (debug_enum_value_t *)calloc(value_count, sizeof(debug_enum_value_t));
		if (type->enum_values != NULL) {
			for (i = 0; i < value_count; i++) {
				type->enum_values[i].name = debugsym_strdup(values[i].name);
				type->enum_values[i].value = values[i].value;
			}
			type->enum_value_count = value_count;
		}
	}

	type_mgr.composite_count++;
	return type;
}

/*
 * Enhanced type extraction from PCC symbol table
 */
debug_type_t *
debugsym_get_type_enhanced(struct symtab *s)
{
	debug_type_t *type;
	debug_type_t *base_type;
	TWORD t;
	int dims[10];
	int ndims = 0;

	if (s == NULL)
		return NULL;

	t = s->stype;

	/* Handle pointer types */
	if (ISPTR(t)) {
		base_type = debugsym_get_type_enhanced_base(s, DECREF(t));
		type = debugsym_pointer_type(base_type);

		/* Apply qualifiers */
		if (s->squal & 1)
			type = debugsym_const_type(type);
		if (s->squal & 2)
			type = debugsym_volatile_type(type);

		return type;
	}

	/* Handle array types */
	if (ISARY(t)) {
		/* Extract dimensions */
		union dimfun *df = s->sdf;
		while (ISARY(t) && ndims < 10) {
			dims[ndims++] = df->ddim;
			t = DECREF(t);
			df++;
		}

		base_type = debugsym_get_type_enhanced_base(s, t);
		type = debugsym_array_type(base_type, dims, ndims);
		return type;
	}

	/* Handle function types */
	if (ISFTN(t)) {
		base_type = debugsym_get_type_enhanced_base(s, DECREF(t));
		/* Could extract parameter types here */
		type = debugsym_function_type(base_type, NULL, 0);
		return type;
	}

	/* Base types */
	return debugsym_get_type_enhanced_base(s, t);
}

/*
 * Get base type with proper sizing
 */
debug_type_t *
debugsym_get_type_enhanced_base(struct symtab *s, TWORD t)
{
	debug_type_encoding_t enc;
	unsigned int size;

	switch (BTYPE(t)) {
	case VOID:
		enc = DBGTYPE_VOID;
		size = 0;
		break;

	case BOOL:
		enc = DBGTYPE_BOOL;
		size = SZBOOL / SZCHAR;
		break;

	case CHAR:
		enc = (t & TUNSIGNED) ? DBGTYPE_UINT8 : DBGTYPE_CHAR;
		size = SZCHAR / SZCHAR;
		break;

	case SHORT:
		enc = (t & TUNSIGNED) ? DBGTYPE_UINT16 : DBGTYPE_INT16;
		size = SZSHORT / SZCHAR;
		break;

	case INT:
		enc = (t & TUNSIGNED) ? DBGTYPE_UINT32 : DBGTYPE_INT32;
		size = SZINT / SZCHAR;
		break;

	case LONG:
		if (SZLONG == SZLONGLONG)
			enc = (t & TUNSIGNED) ? DBGTYPE_UINT64 : DBGTYPE_INT64;
		else
			enc = (t & TUNSIGNED) ? DBGTYPE_UINT32 : DBGTYPE_INT32;
		size = SZLONG / SZCHAR;
		break;

	case LONGLONG:
		enc = (t & TUNSIGNED) ? DBGTYPE_UINT64 : DBGTYPE_INT64;
		size = SZLONGLONG / SZCHAR;
		break;

	case FLOAT:
		enc = DBGTYPE_FLOAT32;
		size = SZFLOAT / SZCHAR;
		break;

	case DOUBLE:
		enc = DBGTYPE_FLOAT64;
		size = SZDOUBLE / SZCHAR;
		break;

	case LDOUBLE:
		enc = DBGTYPE_FLOAT80;
		size = SZLDOUBLE / SZCHAR;
		break;

	case STRTY:
		/* Struct type - would need to extract members */
		return debugsym_composite_type(DBGTYPE_STRUCT,
		    s->sname ? s->sname : "(anonymous)", NULL, 0,
		    s->sap ? s->sap->asize / SZCHAR : 0);

	case UNIONTY:
		return debugsym_composite_type(DBGTYPE_UNION,
		    s->sname ? s->sname : "(anonymous)", NULL, 0,
		    s->sap ? s->sap->asize / SZCHAR : 0);

	case ENUMTY:
		return debugsym_enum_type(s->sname ? s->sname : "(anonymous)", NULL, 0);

	default:
		enc = DBGTYPE_INT32;
		size = SZINT / SZCHAR;
		break;
	}

	return debugsym_get_primitive_type(enc, size);
}

/*
 * Get type size in bytes
 */
unsigned int
debugsym_type_size(debug_type_t *type)
{
	if (type == NULL)
		return 0;

	if (type->encoding == DBGTYPE_ARRAY && type->base_type != NULL) {
		unsigned int elem_size = debugsym_type_size(type->base_type);
		unsigned int total = elem_size;
		int i;

		for (i = 0; i < type->array_dimensions && type->array_bounds; i++)
			total *= type->array_bounds[i];

		return total;
	}

	return type->size;
}

/*
 * Get type alignment
 */
unsigned int
debugsym_type_align(debug_type_t *type)
{
	if (type == NULL)
		return 1;

	switch (type->encoding) {
	case DBGTYPE_CHAR:
	case DBGTYPE_INT8:
	case DBGTYPE_UINT8:
	case DBGTYPE_BOOL:
		return 1;

	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return 2;

	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
	case DBGTYPE_FLOAT32:
		return 4;

	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:
	case DBGTYPE_FLOAT64:
	case DBGTYPE_POINTER:
		return (sizeof(void *) >= 8) ? 8 : 4;

	case DBGTYPE_FLOAT80:
		return 16;

	case DBGTYPE_FLOAT128:
		return 16;

	default:
		return type->size;
	}
}

/*
 * Print type statistics
 */
void
debugsym_type_stats(void)
{
	fprintf(stderr, "Type system statistics:\n");
	fprintf(stderr, "  Primitive types: %d\n", type_mgr.primitive_count);
	fprintf(stderr, "  Composite types: %d\n", type_mgr.composite_count);
	fprintf(stderr, "  Cached types: %d\n", type_mgr.cache_count);
}
