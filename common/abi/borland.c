/*
 * Copyright (c) 2025 PCC Project
 *
 * Borland C++ ABI implementation
 *
 * Used by:
 * - Borland C++ Builder
 * - Turbo C++
 * - C++Builder (older versions)
 * - Embarcadero C++Builder (legacy mode)
 *
 * Key features:
 * - Different name mangling from MSVC
 * - Pascal calling convention support
 * - RTTI for Object Pascal interop
 * - __fastcall as default for methods
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
 * Borland name mangling
 *
 * Format: @<name>$<modifiers><type-sig>
 *
 * Similar to MSVC but with @ prefix and different encoding
 */

/* Forward declarations */
static void borland_mangle_name(abi_mangle_ctx_t *ctx, const char *name);
static void borland_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void borland_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind);
static void borland_mangle_params(abi_mangle_ctx_t *ctx, const abi_param_t *params);

/*
 * Mangle a function name
 */
static char *
borland_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (func == NULL)
		return NULL;

	/* C functions use @ prefix */
	if (func->calling_conv == ABI_CC_C && !func->parent_class) {
		result = malloc(strlen(func->name) + 2);
		sprintf(result, "_%s", func->name);
		return result;
	}

	/* Initialize mangling context */
	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_ITANIUM;  /* Use Borland in real impl */
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* Borland C++ symbols start with @ */
	MANGLE_APPENDC(&mctx, '@');

	/* Function name */
	if (func->is_constructor) {
		MANGLE_APPENDC(&mctx, '$');
		MANGLE_APPEND(&mctx, "bctr");
	} else if (func->is_destructor) {
		MANGLE_APPENDC(&mctx, '$');
		MANGLE_APPEND(&mctx, "bdtr");
	} else {
		borland_mangle_name(&mctx, func->name);
	}

	/* Class name (if method) */
	if (func->parent_class) {
		MANGLE_APPENDC(&mctx, '$');
		borland_mangle_name(&mctx, func->parent_class->name);
	}

	/* Modifiers */
	MANGLE_APPENDC(&mctx, '$');
	if (func->is_const)
		MANGLE_APPENDC(&mctx, 'c');
	if (func->is_virtual)
		MANGLE_APPENDC(&mctx, 'v');

	/* Calling convention indicator */
	switch (func->calling_conv) {
	case ABI_CC_PASCAL:
		MANGLE_APPENDC(&mctx, 'p');
		break;
	case ABI_CC_FASTCALL:
		MANGLE_APPENDC(&mctx, 'f');
		break;
	default:
		MANGLE_APPENDC(&mctx, 'q');  /* __cdecl */
		break;
	}

	/* Return type and parameters */
	borland_mangle_params(&mctx, func->params);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle a name
 */
static void
borland_mangle_name(abi_mangle_ctx_t *ctx, const char *name)
{
	if (name == NULL)
		return;

	MANGLE_APPEND(ctx, name);
}

/*
 * Mangle parameters
 */
static void
borland_mangle_params(abi_mangle_ctx_t *ctx, const abi_param_t *params)
{
	const abi_param_t *p;

	MANGLE_APPENDC(ctx, '$');

	if (params == NULL) {
		MANGLE_APPENDC(ctx, 'v');  /* void */
		return;
	}

	for (p = params; p != NULL; p = p->next) {
		borland_mangle_type_internal(ctx, p->type);
	}
}

/*
 * Mangle a type
 */
static void
borland_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	/* Qualifiers */
	if (type->is_const)
		MANGLE_APPENDC(ctx, 'x');

	/* Base type */
	switch (type->kind) {
	case ABI_TYPE_VOID:
	case ABI_TYPE_BOOL:
	case ABI_TYPE_CHAR:
	case ABI_TYPE_INT:
	case ABI_TYPE_LONG:
	case ABI_TYPE_FLOAT:
	case ABI_TYPE_DOUBLE:
		borland_mangle_builtin_type(ctx, type->kind);
		break;

	case ABI_TYPE_POINTER:
		if (type->is_const)
			MANGLE_APPENDC(ctx, 'x');
		MANGLE_APPENDC(ctx, 'p');
		borland_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_REFERENCE:
		MANGLE_APPENDC(ctx, 'r');
		borland_mangle_type_internal(ctx, type->u.pointee);
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
borland_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind)
{
	switch (kind) {
	case ABI_TYPE_VOID:           MANGLE_APPENDC(ctx, 'v'); break;
	case ABI_TYPE_BOOL:           MANGLE_APPENDC(ctx, 'o'); break;
	case ABI_TYPE_CHAR:           MANGLE_APPENDC(ctx, 'c'); break;
	case ABI_TYPE_SHORT:          MANGLE_APPENDC(ctx, 's'); break;
	case ABI_TYPE_INT:            MANGLE_APPENDC(ctx, 'i'); break;
	case ABI_TYPE_LONG:           MANGLE_APPENDC(ctx, 'l'); break;
	case ABI_TYPE_FLOAT:          MANGLE_APPENDC(ctx, 'f'); break;
	case ABI_TYPE_DOUBLE:         MANGLE_APPENDC(ctx, 'd'); break;
	case ABI_TYPE_LONGDOUBLE:     MANGLE_APPENDC(ctx, 'g'); break;
	default:                      MANGLE_APPENDC(ctx, 'v'); break;
	}
}

/*
 * Mangle a variable name
 */
static char *
borland_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char *result = malloc(strlen(name) + 2);
	sprintf(result, "@%s", name);
	return result;
}

/*
 * Mangle a type
 */
static char *
borland_mangle_type(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	borland_mangle_type_internal(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable name
 */
static char *
borland_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;

	if (cls == NULL)
		return NULL;

	result = malloc(strlen(cls->name) + 20);
	sprintf(result, "@%s$vmt", cls->name);
	return result;
}

/*
 * Mangle RTTI name
 */
static char *
borland_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;

	if (cls == NULL)
		return NULL;

	result = malloc(strlen(cls->name) + 20);
	sprintf(result, "@%s$typeinfo", cls->name);
	return result;
}

/*
 * Borland class layout (similar to MSVC)
 */

static void
borland_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	size_t offset = 0;
	abi_field_t *field;

	if (cls == NULL || cls->size > 0)
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
borland_layout_bases(abi_context_t *ctx, abi_class_t *cls)
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
borland_layout_fields(abi_context_t *ctx, abi_class_t *cls)
{
	/* Already done in layout_class */
}

static void
borland_layout_vtable(abi_context_t *ctx, abi_class_t *cls)
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
borland_compute_vtable_size(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_virtual_func_t *v;
	size_t count = 0;

	for (v = cls->virtuals; v != NULL; v = v->next)
		count++;

	return count;
}

static void
borland_build_vtable(abi_context_t *ctx, abi_class_t *cls, void **vtable)
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
borland_sizeof_type(abi_context_t *ctx, const abi_type_t *type)
{
	return type->size;
}

static size_t
borland_alignof_type(abi_context_t *ctx, const abi_type_t *type)
{
	return type->alignment;
}

/*
 * Borland ABI operations table
 */
const abi_ops_t borland_abi_ops = {
	.mangle_function = borland_mangle_function,
	.mangle_variable = borland_mangle_variable,
	.mangle_type = borland_mangle_type,
	.mangle_vtable = borland_mangle_vtable,
	.mangle_rtti = borland_mangle_rtti,
	.layout_class = borland_layout_class,
	.layout_bases = borland_layout_bases,
	.layout_fields = borland_layout_fields,
	.layout_vtable = borland_layout_vtable,
	.compute_vtable_size = borland_compute_vtable_size,
	.build_vtable = borland_build_vtable,
	.sizeof_type = borland_sizeof_type,
	.alignof_type = borland_alignof_type,
};
