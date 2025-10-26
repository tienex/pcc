/*
 * Copyright (c) 2025 PCC Project
 *
 * Itanium C++ ABI implementation
 *
 * Reference: https://itanium-cxx-abi.github.io/cxx-abi/abi.html
 *
 * Used by:
 * - GCC on all platforms
 * - Clang on Unix/Linux/macOS
 * - Most Unix compilers
 * - ARM C++ ABI (variant)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "abi.h"

/* Mangling buffer helpers */
#define MANGLE_APPEND(ctx, str) do { \
	size_t len = strlen(str); \
	if ((ctx)->pos + len >= (ctx)->bufsize) { \
		(ctx)->bufsize *= 2; \
		(ctx)->buffer = realloc((ctx)->buffer, (ctx)->bufsize); \
	} \
	memcpy((ctx)->buffer + (ctx)->pos, str, len); \
	(ctx)->pos += len; \
	(ctx)->buffer[(ctx)->pos] = '\0'; \
} while (0)

#define MANGLE_APPENDC(ctx, c) do { \
	if ((ctx)->pos + 1 >= (ctx)->bufsize) { \
		(ctx)->bufsize *= 2; \
		(ctx)->buffer = realloc((ctx)->buffer, (ctx)->bufsize); \
	} \
	(ctx)->buffer[(ctx)->pos++] = (c); \
	(ctx)->buffer[(ctx)->pos] = '\0'; \
} while (0)

/*
 * Itanium name mangling
 *
 * Mangled names start with "_Z"
 * Format: _Z <encoding>
 *
 * <encoding> ::= <function name> <bare-function-type>
 *            ::= <data name>
 *            ::= <special-name>
 *
 * Type encodings:
 *   v = void, b = bool, c = char, a = signed char, h = unsigned char
 *   s = short, t = unsigned short, i = int, j = unsigned int
 *   l = long, m = unsigned long, x = long long, y = unsigned long long
 *   n = __int128, o = unsigned __int128
 *   f = float, d = double, e = long double, g = __float128
 *   Dh = IEEE 754r half-precision float (16 bits)
 *   P = pointer, R = reference, O = rvalue reference
 */

/* Forward declarations */
static void itanium_mangle_name(abi_mangle_ctx_t *ctx, const char *name);
static void itanium_mangle_nested_name(abi_mangle_ctx_t *ctx, const char *name);
static void itanium_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void itanium_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind);
static void itanium_mangle_function_params(abi_mangle_ctx_t *ctx, const abi_param_t *params);
static void itanium_mangle_qualifiers(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void itanium_mangle_number(abi_mangle_ctx_t *ctx, int n);

/*
 * Mangle a function name
 */
static char *
itanium_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (func == NULL)
		return NULL;

	/* C functions are not mangled */
	if (func->calling_conv == ABI_CC_C && !func->parent_class)
		return strdup(func->name);

	/* Initialize mangling context */
	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_ITANIUM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* All C++ symbols start with _Z */
	MANGLE_APPEND(&mctx, "_Z");

	/* Mangle the name */
	if (func->parent_class) {
		/* Method: _Z <nested-name> <bare-function-type> */
		MANGLE_APPENDC(&mctx, 'N');

		/* Class name */
		itanium_mangle_name(&mctx, func->parent_class->name);

		/* Function name */
		if (func->is_constructor) {
			MANGLE_APPENDC(&mctx, 'C');
			MANGLE_APPENDC(&mctx, '1');  /* Complete object constructor */
		} else if (func->is_destructor) {
			MANGLE_APPENDC(&mctx, 'D');
			MANGLE_APPENDC(&mctx, '1');  /* Complete object destructor */
		} else {
			itanium_mangle_name(&mctx, func->name);
		}

		MANGLE_APPENDC(&mctx, 'E');

		/* Const/volatile qualifiers */
		if (func->is_const)
			MANGLE_APPENDC(&mctx, 'K');
		if (func->is_volatile)
			MANGLE_APPENDC(&mctx, 'V');
	} else {
		/* Free function: _Z <name> <bare-function-type> */
		itanium_mangle_name(&mctx, func->name);
	}

	/* Mangle parameter types */
	if (!func->is_constructor && !func->is_destructor) {
		itanium_mangle_function_params(&mctx, func->params);
	} else {
		/* Constructors/destructors: mangle params */
		itanium_mangle_function_params(&mctx, func->params);
	}

	result = mctx.buffer;
	return result;
}

/*
 * Mangle a simple name with length prefix
 */
static void
itanium_mangle_name(abi_mangle_ctx_t *ctx, const char *name)
{
	char buf[32];
	size_t len;

	if (name == NULL)
		return;

	len = strlen(name);
	snprintf(buf, sizeof(buf), "%zu", len);
	MANGLE_APPEND(ctx, buf);
	MANGLE_APPEND(ctx, name);
}

/*
 * Mangle function parameters
 */
static void
itanium_mangle_function_params(abi_mangle_ctx_t *ctx, const abi_param_t *params)
{
	const abi_param_t *p;

	if (params == NULL) {
		/* No parameters = void */
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	for (p = params; p != NULL; p = p->next) {
		itanium_mangle_type_internal(ctx, p->type);
	}
}

/*
 * Mangle a type
 */
static void
itanium_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'v');  /* void */
		return;
	}

	/* Qualifiers */
	itanium_mangle_qualifiers(ctx, type);

	/* Base type */
	switch (type->kind) {
	case ABI_TYPE_VOID:
	case ABI_TYPE_BOOL:
	case ABI_TYPE_CHAR:
	case ABI_TYPE_SCHAR:
	case ABI_TYPE_UCHAR:
	case ABI_TYPE_SHORT:
	case ABI_TYPE_USHORT:
	case ABI_TYPE_INT:
	case ABI_TYPE_UINT:
	case ABI_TYPE_LONG:
	case ABI_TYPE_ULONG:
	case ABI_TYPE_LONGLONG:
	case ABI_TYPE_ULONGLONG:
	case ABI_TYPE_INT128:
	case ABI_TYPE_UINT128:
	case ABI_TYPE_FLOAT:
	case ABI_TYPE_DOUBLE:
	case ABI_TYPE_LONGDOUBLE:
		itanium_mangle_builtin_type(ctx, type->kind);
		break;

	case ABI_TYPE_POINTER:
		MANGLE_APPENDC(ctx, 'P');
		itanium_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_REFERENCE:
		MANGLE_APPENDC(ctx, 'R');
		itanium_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_RVALUE_REFERENCE:
		MANGLE_APPENDC(ctx, 'O');
		itanium_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_ARRAY:
		MANGLE_APPENDC(ctx, 'A');
		itanium_mangle_number(ctx, (int)type->array_size);
		MANGLE_APPENDC(ctx, '_');
		itanium_mangle_type_internal(ctx, type->u.element);
		break;

	case ABI_TYPE_CLASS:
	case ABI_TYPE_STRUCT:
		/* Class/struct: length-prefixed name */
		if (type->u.class_type && type->u.class_type->name)
			itanium_mangle_name(ctx, type->u.class_type->name);
		break;

	default:
		/* Unknown type */
		MANGLE_APPENDC(ctx, 'v');
		break;
	}
}

/*
 * Mangle built-in type
 */
static void
itanium_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind)
{
	switch (kind) {
	case ABI_TYPE_VOID:           MANGLE_APPENDC(ctx, 'v'); break;
	case ABI_TYPE_BOOL:           MANGLE_APPENDC(ctx, 'b'); break;
	case ABI_TYPE_CHAR:           MANGLE_APPENDC(ctx, 'c'); break;
	case ABI_TYPE_SCHAR:          MANGLE_APPENDC(ctx, 'a'); break;
	case ABI_TYPE_UCHAR:          MANGLE_APPENDC(ctx, 'h'); break;
	case ABI_TYPE_SHORT:          MANGLE_APPENDC(ctx, 's'); break;
	case ABI_TYPE_USHORT:         MANGLE_APPENDC(ctx, 't'); break;
	case ABI_TYPE_INT:            MANGLE_APPENDC(ctx, 'i'); break;
	case ABI_TYPE_UINT:           MANGLE_APPENDC(ctx, 'j'); break;
	case ABI_TYPE_LONG:           MANGLE_APPENDC(ctx, 'l'); break;
	case ABI_TYPE_ULONG:          MANGLE_APPENDC(ctx, 'm'); break;
	case ABI_TYPE_LONGLONG:       MANGLE_APPENDC(ctx, 'x'); break;
	case ABI_TYPE_ULONGLONG:      MANGLE_APPENDC(ctx, 'y'); break;
	case ABI_TYPE_INT128:         MANGLE_APPENDC(ctx, 'n'); break;
	case ABI_TYPE_UINT128:        MANGLE_APPENDC(ctx, 'o'); break;
	case ABI_TYPE_FLOAT:          MANGLE_APPENDC(ctx, 'f'); break;
	case ABI_TYPE_DOUBLE:         MANGLE_APPENDC(ctx, 'd'); break;
	case ABI_TYPE_LONGDOUBLE:     MANGLE_APPENDC(ctx, 'e'); break;
	case ABI_TYPE_FLOAT128:       MANGLE_APPENDC(ctx, 'g'); break;
	default:                      MANGLE_APPENDC(ctx, 'v'); break;
	}
}

/*
 * Mangle type qualifiers (const, volatile, restrict)
 */
static void
itanium_mangle_qualifiers(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (type->is_restrict)
		MANGLE_APPENDC(ctx, 'r');
	if (type->is_volatile)
		MANGLE_APPENDC(ctx, 'V');
	if (type->is_const)
		MANGLE_APPENDC(ctx, 'K');
}

/*
 * Mangle a number
 */
static void
itanium_mangle_number(abi_mangle_ctx_t *ctx, int n)
{
	char buf[32];

	if (n < 0) {
		MANGLE_APPENDC(ctx, 'n');
		n = -n;
	}

	snprintf(buf, sizeof(buf), "%d", n);
	MANGLE_APPEND(ctx, buf);
}

/*
 * Mangle a variable name
 */
static char *
itanium_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	/* Global variables in C++ are just the name */
	/* For namespaced/class static members, need full encoding */
	return strdup(name);
}

/*
 * Mangle a type (for RTTI)
 */
static char *
itanium_mangle_type(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_ITANIUM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	itanium_mangle_type_internal(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable name
 * Format: _ZTV <class-name>
 */
static char *
itanium_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (cls == NULL)
		return NULL;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_ITANIUM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "_ZTV");
	itanium_mangle_name(&mctx, cls->name);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle RTTI type_info name
 * Format: _ZTI <class-name>
 */
static char *
itanium_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (cls == NULL)
		return NULL;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_ITANIUM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "_ZTI");
	itanium_mangle_name(&mctx, cls->name);

	result = mctx.buffer;
	return result;
}

/*
 * Itanium class layout
 *
 * Rules:
 * 1. Primary base class is laid out at offset 0
 * 2. Non-virtual bases follow, each at appropriately aligned offset
 * 3. Virtual base classes are laid out after non-virtual data
 * 4. Vtable pointer is at offset 0 if the class has virtual functions
 * 5. Empty base optimization applies
 */

static void
itanium_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	size_t offset = 0;
	abi_base_t *base;
	abi_field_t *field;

	if (cls == NULL)
		return;

	/* Check if already laid out */
	if (cls->size > 0)
		return;

	/* Start with alignment of 1 */
	cls->alignment = 1;

	/* Layout bases first */
	if (ctx->ops && ctx->ops->layout_bases)
		ctx->ops->layout_bases(ctx, cls);

	/* If class has virtual functions, reserve space for vtable pointer */
	if (cls->has_vtable) {
		cls->vtable_offset = 0;
		offset = ctx->pointer_size;
		cls->alignment = ctx->pointer_align;
	}

	/* Layout non-static data members */
	for (field = cls->fields; field != NULL; field = field->next) {
		size_t field_size, field_align;

		if (field->is_static)
			continue;

		field_size = abi_sizeof_type(ctx, field->type);
		field_align = abi_alignof_type(ctx, field->type);

		/* Align field */
		offset = (offset + field_align - 1) & ~(field_align - 1);
		field->offset = offset;
		offset += field_size;

		/* Update class alignment */
		if (field_align > cls->alignment)
			cls->alignment = field_align;
	}

	/* Layout virtual bases at end */
	for (base = cls->bases; base != NULL; base = base->next) {
		if (base->is_virtual) {
			size_t base_align = base->base_class->alignment;
			offset = (offset + base_align - 1) & ~(base_align - 1);
			base->offset = offset;
			offset += base->base_class->size;
		}
	}

	/* Final size aligned to class alignment */
	cls->size = (offset + cls->alignment - 1) & ~(cls->alignment - 1);

	/* Empty class has size 1 */
	if (cls->size == 0 && cls->fields == NULL) {
		cls->size = 1;
		cls->is_empty = 1;
	}

	/* Check if POD */
	cls->is_pod = (!cls->has_vtable && !cls->has_vbases);
}

static void
itanium_layout_bases(abi_context_t *ctx, abi_class_t *cls)
{
	abi_base_t *base;
	size_t offset = 0;

	/* Layout non-virtual bases */
	for (base = cls->bases; base != NULL; base = base->next) {
		if (!base->is_virtual) {
			/* Recursively layout base class */
			if (base->base_class->size == 0)
				abi_layout_class(ctx, base->base_class);

			/* Align to base class alignment */
			offset = (offset + base->base_class->alignment - 1) &
			         ~(base->base_class->alignment - 1);
			base->offset = offset;

			/* Primary base at offset 0 */
			if (base->is_primary)
				base->offset = 0;
			else
				offset += base->base_class->size;
		}
	}
}

static void
itanium_layout_fields(abi_context_t *ctx, abi_class_t *cls)
{
	/* Already done in layout_class */
}

static void
itanium_layout_vtable(abi_context_t *ctx, abi_class_t *cls)
{
	abi_virtual_func_t *v;
	size_t index = 0;

	if (!cls->has_vtable)
		return;

	/* Assign vtable indices to virtual functions */
	for (v = cls->virtuals; v != NULL; v = v->next) {
		v->vtable_index = index++;
	}
}

/*
 * Compute vtable size
 */
static size_t
itanium_compute_vtable_size(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_virtual_func_t *v;
	size_t count = 0;

	/* Count virtual functions */
	for (v = cls->virtuals; v != NULL; v = v->next)
		count++;

	/* Add 2 slots for offset-to-top and RTTI */
	return count + 2;
}

/*
 * Build vtable
 */
static void
itanium_build_vtable(abi_context_t *ctx, abi_class_t *cls, void **vtable)
{
	abi_virtual_func_t *v;
	size_t i = 0;

	if (!cls->has_vtable || vtable == NULL)
		return;

	/* Slot 0: offset to top (0 for primary vtable) */
	vtable[i++] = NULL;

	/* Slot 1: RTTI pointer */
	vtable[i++] = NULL;  /* Would point to type_info */

	/* Virtual function pointers */
	for (v = cls->virtuals; v != NULL; v = v->next) {
		vtable[i++] = NULL;  /* Would point to function */
	}
}

/*
 * Type size/alignment
 */
static size_t
itanium_sizeof_type(abi_context_t *ctx, const abi_type_t *type)
{
	/* Use default implementation */
	return type->size;
}

static size_t
itanium_alignof_type(abi_context_t *ctx, const abi_type_t *type)
{
	/* Use default implementation */
	return type->alignment;
}

/*
 * Itanium ABI operations table
 */
const abi_ops_t itanium_abi_ops = {
	.mangle_function = itanium_mangle_function,
	.mangle_variable = itanium_mangle_variable,
	.mangle_type = itanium_mangle_type,
	.mangle_vtable = itanium_mangle_vtable,
	.mangle_rtti = itanium_mangle_rtti,
	.layout_class = itanium_layout_class,
	.layout_bases = itanium_layout_bases,
	.layout_fields = itanium_layout_fields,
	.layout_vtable = itanium_layout_vtable,
	.compute_vtable_size = itanium_compute_vtable_size,
	.build_vtable = itanium_build_vtable,
	.sizeof_type = itanium_sizeof_type,
	.alignof_type = itanium_alignof_type,
};
