/*
 * Copyright (c) 2025 PCC Project
 *
 * Apple/NeXT Objective-C 1.0 ABI implementation (Fragile Runtime)
 *
 * The legacy Objective-C runtime used by:
 * - Mac OS X 10.0-10.4 (32-bit)
 * - NeXTSTEP/OPENSTEP
 * - Early iOS (32-bit)
 *
 * Key features:
 * - Fragile base class (adding ivars breaks ABI compatibility)
 * - Legacy metadata format
 * - No ARC support
 * - Fixed ivar offsets
 *
 * Name mangling is the same as ObjC 2.0:
 *   Instance methods: -[ClassName methodName:param:]
 *   Class methods:    +[ClassName methodName:param:]
 *
 * Internal symbols:
 *   Classes:     .objc_class_name_ClassName
 *   Metaclasses: .objc_metaclass_ClassName
 *   Methods:     -[ClassName methodName:param:]
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

/* Forward declarations */
static void objc1_mangle_method_name(abi_mangle_ctx_t *ctx, const abi_function_t *func);
static void objc1_encode_type(abi_mangle_ctx_t *ctx, const abi_type_t *type);

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
 * Mangle Objective-C method name (same as ObjC 2.0)
 */
static char *
objc1_mangle_function(abi_context_t *ctx, const abi_function_t *func)
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
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* Instance or class method */
	if (func->is_static)
		MANGLE_APPENDC(&mctx, '+');
	else
		MANGLE_APPENDC(&mctx, '-');

	MANGLE_APPENDC(&mctx, '[');

	/* Class name */
	if (func->parent_class && func->parent_class->name) {
		MANGLE_APPEND(&mctx, func->parent_class->name);
	} else {
		MANGLE_APPEND(&mctx, "UnknownClass");
	}

	MANGLE_APPENDC(&mctx, ' ');

	/* Method name */
	objc1_mangle_method_name(&mctx, func);

	MANGLE_APPENDC(&mctx, ']');

	result = mctx.buffer;
	return result;
}

/*
 * Mangle method name with parameters
 */
static void
objc1_mangle_method_name(abi_mangle_ctx_t *ctx, const abi_function_t *func)
{
	const abi_param_t *p;

	MANGLE_APPEND(ctx, func->name);

	/* Add colon for each parameter */
	for (p = func->params; p != NULL; p = p->next) {
		MANGLE_APPENDC(ctx, ':');
	}
}

/*
 * Encode type using Objective-C type encoding (same as ObjC 2.0)
 */
static void
objc1_encode_type(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	switch (type->kind) {
	case ABI_TYPE_VOID:
		MANGLE_APPENDC(ctx, 'v');
		break;
	case ABI_TYPE_BOOL:
		MANGLE_APPENDC(ctx, 'B');
		break;
	case ABI_TYPE_CHAR:
	case ABI_TYPE_SCHAR:
		MANGLE_APPENDC(ctx, 'c');
		break;
	case ABI_TYPE_UCHAR:
		MANGLE_APPENDC(ctx, 'C');
		break;
	case ABI_TYPE_SHORT:
		MANGLE_APPENDC(ctx, 's');
		break;
	case ABI_TYPE_USHORT:
		MANGLE_APPENDC(ctx, 'S');
		break;
	case ABI_TYPE_INT:
		MANGLE_APPENDC(ctx, 'i');
		break;
	case ABI_TYPE_UINT:
		MANGLE_APPENDC(ctx, 'I');
		break;
	case ABI_TYPE_LONG:
		MANGLE_APPENDC(ctx, 'l');
		break;
	case ABI_TYPE_ULONG:
		MANGLE_APPENDC(ctx, 'L');
		break;
	case ABI_TYPE_LONGLONG:
		MANGLE_APPENDC(ctx, 'q');
		break;
	case ABI_TYPE_ULONGLONG:
		MANGLE_APPENDC(ctx, 'Q');
		break;
	case ABI_TYPE_FLOAT:
		MANGLE_APPENDC(ctx, 'f');
		break;
	case ABI_TYPE_DOUBLE:
		MANGLE_APPENDC(ctx, 'd');
		break;
	case ABI_TYPE_POINTER:
		MANGLE_APPENDC(ctx, '^');
		objc1_encode_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_CLASS:
		MANGLE_APPENDC(ctx, '@');
		break;
	case ABI_TYPE_STRUCT:
		MANGLE_APPENDC(ctx, '{');
		if (type->u.class_type && type->u.class_type->name)
			MANGLE_APPEND(ctx, type->u.class_type->name);
		MANGLE_APPENDC(ctx, '}');
		break;
	default:
		MANGLE_APPENDC(ctx, 'v');
		break;
	}
}

/*
 * Mangle variable
 */
static char *
objc1_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "_OBJC_INSTANCE_VARIABLE_%s", name);
	return strdup(buf);
}

/*
 * Mangle type
 */
static char *
objc1_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	objc1_encode_type(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable (class structure)
 * Legacy format: .objc_class_name_ClassName
 */
static char *
objc1_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (cls == NULL)
		return NULL;

	snprintf(buf, sizeof(buf), ".objc_class_name_%s", cls->name);
	return strdup(buf);
}

/*
 * Mangle RTTI (metaclass)
 */
static char *
objc1_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (cls == NULL)
		return NULL;

	snprintf(buf, sizeof(buf), ".objc_metaclass_%s", cls->name);
	return strdup(buf);
}

/*
 * Layout Objective-C 1.0 class
 * Fragile ABI: fixed ivar offsets, changing base class breaks subclasses
 */
static void
objc1_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	abi_field_t *field;
	size_t offset = 0;
	size_t max_align = sizeof(void *);

	if (cls == NULL)
		return;

	/* ObjC objects start with 'isa' pointer */
	offset = sizeof(void *);

	/* If there's a base class, ivars come after base */
	if (cls->bases && cls->bases->base_class) {
		offset = cls->bases->base_class->size;
	}

	/* Layout instance variables with FIXED offsets (fragile!) */
	for (field = cls->fields; field != NULL; field = field->next) {
		if (field->is_static)
			continue;

		size_t field_size = field->type->size;
		size_t field_align = field->type->alignment;

		if (field_align == 0)
			field_align = 1;
		if (field_align > max_align)
			max_align = field_align;

		/* Align field */
		offset = (offset + field_align - 1) & ~(field_align - 1);
		field->offset = offset;
		offset += field_size;
	}

	/* Align total size */
	offset = (offset + max_align - 1) & ~(max_align - 1);
	cls->size = offset;
	cls->alignment = max_align;
}

/*
 * Apple Objective-C 1.0 ABI operations table
 */
const abi_ops_t apple_objc1_abi_ops = {
	.mangle_function = objc1_mangle_function,
	.mangle_variable = objc1_mangle_variable,
	.mangle_type = objc1_mangle_type_str,
	.mangle_vtable = objc1_mangle_vtable,
	.mangle_rtti = objc1_mangle_rtti,
	.layout_class = objc1_layout_class,
	.layout_bases = NULL,
	.layout_fields = NULL,
	.layout_vtable = NULL,
	.compute_vtable_size = NULL,
	.build_vtable = NULL,
	.sizeof_type = NULL,
	.alignof_type = NULL,
};
