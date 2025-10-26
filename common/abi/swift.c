/*
 * Copyright (c) 2025 PCC Project
 *
 * Swift ABI implementation
 *
 * Swift has its own ABI with a sophisticated name mangling scheme.
 *
 * Name mangling format (Swift 4+):
 *   $s<module><context><entity><type>
 *
 * Prefixes:
 *   $s = Swift symbol (Swift 4+)
 *   _T = Swift symbol (Swift 3.x)
 *
 * Context/Entity types:
 *   C = class, V = struct, O = enum, P = protocol
 *   F = function, v = variable
 *
 * Type encoding:
 *   y = void, Sb = Bool
 *   Si = Int, Sd = Double, Sf = Float
 *   SS = String, Sa = Array
 *   So = ObjC type
 *
 * Examples:
 *   MyClass.method() -> $s7MyClassC6methodyyF
 *   func foo(Int)    -> $s3fooySiF
 *   String.count     -> $sSS5countSivp
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

/* Forward declarations */
static void swift_mangle_name(abi_mangle_ctx_t *ctx, const char *name);
static void swift_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void swift_mangle_function_type(abi_mangle_ctx_t *ctx, const abi_param_t *params, const abi_type_t *ret);

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
 * Mangle function using Swift ABI
 *
 * Format: $s<module><class><function><signature>
 */
static char *
swift_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	char *result;
	char buf[32];

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

	/* Swift 4+ prefix */
	MANGLE_APPEND(&mctx, "$s");

	/* Module name (simplified as "Module") */
	const char *module = "Module";
	snprintf(buf, sizeof(buf), "%zu", strlen(module));
	MANGLE_APPEND(&mctx, buf);
	MANGLE_APPEND(&mctx, module);

	/* Class context if method */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%zu", strlen(func->parent_class->name));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, func->parent_class->name);
		MANGLE_APPENDC(&mctx, 'C');  /* Class context */
	}

	/* Function name */
	snprintf(buf, sizeof(buf), "%zu", strlen(func->name));
	MANGLE_APPEND(&mctx, buf);
	MANGLE_APPEND(&mctx, func->name);

	/* Function type signature */
	swift_mangle_function_type(&mctx, func->params, func->return_type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle function type
 * Format: <param_types>F or y if void
 */
static void
swift_mangle_function_type(abi_mangle_ctx_t *ctx, const abi_param_t *params, const abi_type_t *ret)
{
	const abi_param_t *p;

	/* Parameter types */
	if (params == NULL) {
		MANGLE_APPENDC(ctx, 'y');  /* void/empty params */
	} else {
		for (p = params; p != NULL; p = p->next) {
			swift_mangle_type(ctx, p->type);
		}
	}

	/* Function marker */
	MANGLE_APPENDC(ctx, 'F');
}

/*
 * Mangle type
 *
 * Swift type codes:
 *   y = void
 *   Sb = Bool
 *   Si = Int, Sd = Double, Sf = Float
 *   SS = String
 *   Sa = Array
 *   Sp = UnsafePointer
 *   <length><name> = custom type
 */
static void
swift_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	char buf[32];

	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'y');
		return;
	}

	switch (type->kind) {
	case ABI_TYPE_VOID:
		MANGLE_APPENDC(ctx, 'y');
		break;
	case ABI_TYPE_BOOL:
		MANGLE_APPEND(ctx, "Sb");
		break;
	case ABI_TYPE_CHAR:
	case ABI_TYPE_SCHAR:
	case ABI_TYPE_UCHAR:
		MANGLE_APPEND(ctx, "Si");  /* Treat as Int */
		break;
	case ABI_TYPE_SHORT:
	case ABI_TYPE_USHORT:
		MANGLE_APPEND(ctx, "Si");
		break;
	case ABI_TYPE_INT:
	case ABI_TYPE_UINT:
		MANGLE_APPEND(ctx, "Si");
		break;
	case ABI_TYPE_LONG:
	case ABI_TYPE_ULONG:
	case ABI_TYPE_LONGLONG:
	case ABI_TYPE_ULONGLONG:
		MANGLE_APPEND(ctx, "Si");  /* Swift Int */
		break;
	case ABI_TYPE_FLOAT:
		MANGLE_APPEND(ctx, "Sf");
		break;
	case ABI_TYPE_DOUBLE:
		MANGLE_APPEND(ctx, "Sd");
		break;
	case ABI_TYPE_LONGDOUBLE:
		MANGLE_APPEND(ctx, "Sd");  /* No separate long double in Swift */
		break;
	case ABI_TYPE_POINTER:
		MANGLE_APPEND(ctx, "Sp");  /* UnsafePointer */
		swift_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_REFERENCE:
		/* Swift doesn't have references, treat as value type */
		swift_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_CLASS:
		/* Custom class type */
		if (type->u.class_type && type->u.class_type->name) {
			snprintf(buf, sizeof(buf), "%zu", strlen(type->u.class_type->name));
			MANGLE_APPEND(ctx, buf);
			MANGLE_APPEND(ctx, type->u.class_type->name);
			MANGLE_APPENDC(ctx, 'C');
		} else {
			MANGLE_APPENDC(ctx, 'y');
		}
		break;
	case ABI_TYPE_STRUCT:
		/* Custom struct type */
		if (type->u.class_type && type->u.class_type->name) {
			snprintf(buf, sizeof(buf), "%zu", strlen(type->u.class_type->name));
			MANGLE_APPEND(ctx, buf);
			MANGLE_APPEND(ctx, type->u.class_type->name);
			MANGLE_APPENDC(ctx, 'V');  /* Struct/Value type */
		} else {
			MANGLE_APPENDC(ctx, 'y');
		}
		break;
	case ABI_TYPE_ARRAY:
		MANGLE_APPEND(ctx, "Sa");  /* Array */
		swift_mangle_type(ctx, type->u.element);
		break;
	default:
		MANGLE_APPENDC(ctx, 'y');
		break;
	}
}

/*
 * Mangle variable
 * Format: $s<module><name>v
 */
static char *
swift_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;
	char buf[32];

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "$s");

	/* Module name */
	const char *module = "Module";
	snprintf(buf, sizeof(buf), "%zu", strlen(module));
	MANGLE_APPEND(&mctx, buf);
	MANGLE_APPEND(&mctx, module);

	/* Variable name */
	snprintf(buf, sizeof(buf), "%zu", strlen(name));
	MANGLE_APPEND(&mctx, buf);
	MANGLE_APPEND(&mctx, name);

	/* Variable marker */
	MANGLE_APPENDC(&mctx, 'v');

	result = mctx.buffer;
	return result;
}

/*
 * Mangle type (standalone)
 */
static char *
swift_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	swift_mangle_type(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable
 * Swift metadata: $s<module><class>CMn (nominal type descriptor)
 */
static char *
swift_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;
	char buf[256];

	if (cls == NULL)
		return NULL;

	/* Swift uses metadata tables, not traditional vtables */
	snprintf(buf, sizeof(buf), "$s6Module%zu%sCMn",
	         strlen(cls->name), cls->name);
	result = strdup(buf);
	return result;
}

/*
 * Mangle RTTI (Type Metadata)
 * Swift type metadata: $s<module><class>CMa
 */
static char *
swift_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;
	char buf[256];

	if (cls == NULL)
		return NULL;

	snprintf(buf, sizeof(buf), "$s6Module%zu%sCMa",
	         strlen(cls->name), cls->name);
	result = strdup(buf);
	return result;
}

/*
 * Swift class layout
 * Swift uses reference counting and different memory layout
 */
static void
swift_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	abi_field_t *field;
	size_t offset = 0;
	size_t max_align = 1;

	if (cls == NULL)
		return;

	/* Swift objects have metadata pointer at offset 0 */
	offset = sizeof(void *);
	max_align = sizeof(void *);

	/* Layout fields */
	for (field = cls->fields; field != NULL; field = field->next) {
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

	/* Final class size and alignment */
	offset = (offset + max_align - 1) & ~(max_align - 1);
	cls->size = offset;
	cls->alignment = max_align;
}

/*
 * Swift ABI operations table
 */
const abi_ops_t swift_abi_ops = {
	.mangle_function = swift_mangle_function,
	.mangle_variable = swift_mangle_variable,
	.mangle_type = swift_mangle_type_str,
	.mangle_vtable = swift_mangle_vtable,
	.mangle_rtti = swift_mangle_rtti,
	/* Layout operations */
	.layout_class = swift_layout_class,
	.layout_bases = NULL,
	.layout_fields = NULL,
	.layout_vtable = NULL,
	.compute_vtable_size = NULL,
	.build_vtable = NULL,
	.sizeof_type = NULL,
	.alignof_type = NULL,
};
