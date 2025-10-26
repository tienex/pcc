/*
 * Copyright (c) 2025 PCC Project
 *
 * Watcom C++ ABI implementation
 *
 * Used by:
 * - Watcom C/C++ compiler
 * - Open Watcom
 *
 * Key features:
 * - Register-based calling convention
 * - Different name mangling from MSVC and Itanium
 * - Efficient code generation for 16-bit and 32-bit DOS/OS2
 * - Support for far/near pointers
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
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
 * Watcom name mangling
 *
 * Format: W?<name>$<class>$<modifiers><type-signature>
 *
 * Watcom uses a simpler mangling scheme than MSVC
 * Class members are prefixed with W?
 * Type encodings are similar to MSVC but simpler
 */

/* Forward declarations */
static void watcom_mangle_name(abi_mangle_ctx_t *ctx, const char *name);
static void watcom_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void watcom_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind);
static void watcom_mangle_params(abi_mangle_ctx_t *ctx, const abi_param_t *params);

/*
 * Mangle a function name
 */
static char *
watcom_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (func == NULL)
		return NULL;

	/* C functions use simple underscore prefix */
	if (func->calling_conv == ABI_CC_C && !func->parent_class) {
		result = malloc(strlen(func->name) + 2);
		sprintf(result, "_%s", func->name);
		return result;
	}

	/* Initialize mangling context */
	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_WATCOM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* Watcom C++ symbols start with W? */
	MANGLE_APPEND(&mctx, "W?");

	/* Function name */
	if (func->is_constructor) {
		MANGLE_APPEND(&mctx, "$ct");
	} else if (func->is_destructor) {
		MANGLE_APPEND(&mctx, "$dt");
	} else {
		watcom_mangle_name(&mctx, func->name);
	}

	/* Class name (if method) */
	if (func->parent_class) {
		MANGLE_APPENDC(&mctx, '$');
		watcom_mangle_name(&mctx, func->parent_class->name);
	}

	/* Modifiers */
	MANGLE_APPENDC(&mctx, '$');
	if (func->is_const)
		MANGLE_APPENDC(&mctx, 'c');
	if (func->is_volatile)
		MANGLE_APPENDC(&mctx, 'v');
	if (func->is_static)
		MANGLE_APPENDC(&mctx, 's');

	/* Return type and parameters */
	MANGLE_APPENDC(&mctx, '$');
	watcom_mangle_params(&mctx, func->params);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle a name
 */
static void
watcom_mangle_name(abi_mangle_ctx_t *ctx, const char *name)
{
	if (name == NULL)
		return;

	/* Watcom uses direct names with length */
	char buf[32];
	snprintf(buf, sizeof(buf), "%zu", strlen(name));
	MANGLE_APPEND(ctx, buf);
	MANGLE_APPENDC(ctx, '_');
	MANGLE_APPEND(ctx, name);
}

/*
 * Mangle parameters
 */
static void
watcom_mangle_params(abi_mangle_ctx_t *ctx, const abi_param_t *params)
{
	const abi_param_t *p;

	if (params == NULL) {
		MANGLE_APPENDC(ctx, 'v');  /* void */
		return;
	}

	for (p = params; p != NULL; p = p->next) {
		watcom_mangle_type_internal(ctx, p->type);
		if (p->next)
			MANGLE_APPENDC(ctx, '_');
	}
}

/*
 * Mangle a type
 */
static void
watcom_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	/* Qualifiers */
	if (type->is_const)
		MANGLE_APPENDC(ctx, 'c');
	if (type->is_volatile)
		MANGLE_APPENDC(ctx, 'V');

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
	case ABI_TYPE_FLOAT:
	case ABI_TYPE_DOUBLE:
	case ABI_TYPE_LONGDOUBLE:
		watcom_mangle_builtin_type(ctx, type->kind);
		break;

	case ABI_TYPE_POINTER:
		MANGLE_APPENDC(ctx, 'p');
		watcom_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_REFERENCE:
		MANGLE_APPENDC(ctx, 'r');
		watcom_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_ARRAY:
		MANGLE_APPENDC(ctx, 'a');
		watcom_mangle_type_internal(ctx, type->u.element);
		break;

	case ABI_TYPE_CLASS:
	case ABI_TYPE_STRUCT:
		MANGLE_APPENDC(ctx, 's');
		if (type->u.class_type && type->u.class_type->name)
			watcom_mangle_name(ctx, type->u.class_type->name);
		break;

	default:
		MANGLE_APPENDC(ctx, 'v');
		break;
	}
}

/*
 * Mangle built-in type
 */
static void
watcom_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind)
{
	switch (kind) {
	case ABI_TYPE_VOID:           MANGLE_APPENDC(ctx, 'v'); break;
	case ABI_TYPE_BOOL:           MANGLE_APPENDC(ctx, 'b'); break;
	case ABI_TYPE_CHAR:           MANGLE_APPENDC(ctx, 'c'); break;
	case ABI_TYPE_SCHAR:          MANGLE_APPENDC(ctx, 'C'); break;
	case ABI_TYPE_UCHAR:          MANGLE_APPENDC(ctx, 'u'); break;
	case ABI_TYPE_SHORT:          MANGLE_APPENDC(ctx, 's'); break;
	case ABI_TYPE_USHORT:         MANGLE_APPENDC(ctx, 'S'); break;
	case ABI_TYPE_INT:            MANGLE_APPENDC(ctx, 'i'); break;
	case ABI_TYPE_UINT:           MANGLE_APPENDC(ctx, 'I'); break;
	case ABI_TYPE_LONG:           MANGLE_APPENDC(ctx, 'l'); break;
	case ABI_TYPE_ULONG:          MANGLE_APPENDC(ctx, 'L'); break;
	case ABI_TYPE_LONGLONG:       MANGLE_APPENDC(ctx, 'x'); break;
	case ABI_TYPE_ULONGLONG:      MANGLE_APPENDC(ctx, 'X'); break;
	case ABI_TYPE_FLOAT:          MANGLE_APPENDC(ctx, 'f'); break;
	case ABI_TYPE_DOUBLE:         MANGLE_APPENDC(ctx, 'd'); break;
	case ABI_TYPE_LONGDOUBLE:     MANGLE_APPENDC(ctx, 'D'); break;
	default:                      MANGLE_APPENDC(ctx, 'v'); break;
	}
}

/*
 * Mangle a variable name
 */
static char *
watcom_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	/* Watcom uses simple underscore prefix for globals */
	char *result = malloc(strlen(name) + 2);
	sprintf(result, "_%s", name);
	return result;
}

/*
 * Mangle a type (standalone)
 */
static char *
watcom_mangle_type(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_WATCOM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	watcom_mangle_type_internal(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable name
 */
static char *
watcom_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (cls == NULL)
		return NULL;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_WATCOM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "W?$vft$");
	watcom_mangle_name(&mctx, cls->name);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle RTTI name
 */
static char *
watcom_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (cls == NULL)
		return NULL;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_WATCOM;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "W?$rtti$");
	watcom_mangle_name(&mctx, cls->name);

	result = mctx.buffer;
	return result;
}

/*
 * Watcom class layout
 * Similar to MSVC but with some differences for efficiency
 */

static void
watcom_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	size_t offset = 0;
	abi_field_t *field;

	if (cls == NULL)
		return;

	if (cls->size > 0)
		return;

	cls->alignment = 1;

	/* Vtable pointer at offset 0 */
	if (cls->has_vtable) {
		cls->vtable_offset = 0;
		offset = ctx->pointer_size;
		cls->alignment = ctx->pointer_align;
	}

	/* Layout bases */
	if (ctx->ops && ctx->ops->layout_bases)
		ctx->ops->layout_bases(ctx, cls);

	/* Layout fields */
	for (field = cls->fields; field != NULL; field = field->next) {
		size_t field_size, field_align;

		if (field->is_static)
			continue;

		field_size = abi_sizeof_type(ctx, field->type);
		field_align = abi_alignof_type(ctx, field->type);

		offset = (offset + field_align - 1) & ~(field_align - 1);
		field->offset = offset;
		offset += field_size;

		if (field_align > cls->alignment)
			cls->alignment = field_align;
	}

	cls->size = (offset + cls->alignment - 1) & ~(cls->alignment - 1);

	if (cls->size == 0) {
		cls->size = 1;
		cls->is_empty = 1;
	}

	cls->is_pod = (!cls->has_vtable && !cls->has_vbases);
}

static void
watcom_layout_bases(abi_context_t *ctx, abi_class_t *cls)
{
	abi_base_t *base;

	for (base = cls->bases; base != NULL; base = base->next) {
		if (!base->is_virtual) {
			if (base->base_class->size == 0)
				abi_layout_class(ctx, base->base_class);

			base->offset = 0;
		}
	}
}

static void
watcom_layout_fields(abi_context_t *ctx, abi_class_t *cls)
{
	/* Already done in layout_class */
}

static void
watcom_layout_vtable(abi_context_t *ctx, abi_class_t *cls)
{
	abi_virtual_func_t *v;
	size_t index = 0;

	if (!cls->has_vtable)
		return;

	for (v = cls->virtuals; v != NULL; v = v->next) {
		v->vtable_index = index++;
	}
}

static size_t
watcom_compute_vtable_size(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_virtual_func_t *v;
	size_t count = 0;

	for (v = cls->virtuals; v != NULL; v = v->next)
		count++;

	return count;
}

static void
watcom_build_vtable(abi_context_t *ctx, abi_class_t *cls, void **vtable)
{
	abi_virtual_func_t *v;
	size_t i = 0;

	if (!cls->has_vtable || vtable == NULL)
		return;

	for (v = cls->virtuals; v != NULL; v = v->next) {
		vtable[i++] = NULL;
	}
}

static size_t
watcom_sizeof_type(abi_context_t *ctx, const abi_type_t *type)
{
	return type->size;
}

static size_t
watcom_alignof_type(abi_context_t *ctx, const abi_type_t *type)
{
	return type->alignment;
}

/*
 * Watcom ABI operations table
 */
const abi_ops_t watcom_abi_ops = {
	.mangle_function = watcom_mangle_function,
	.mangle_variable = watcom_mangle_variable,
	.mangle_type = watcom_mangle_type,
	.mangle_vtable = watcom_mangle_vtable,
	.mangle_rtti = watcom_mangle_rtti,
	.layout_class = watcom_layout_class,
	.layout_bases = watcom_layout_bases,
	.layout_fields = watcom_layout_fields,
	.layout_vtable = watcom_layout_vtable,
	.compute_vtable_size = watcom_compute_vtable_size,
	.build_vtable = watcom_build_vtable,
	.sizeof_type = watcom_sizeof_type,
	.alignof_type = watcom_alignof_type,
};
