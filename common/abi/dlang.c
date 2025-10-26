/*
 * Copyright (c) 2025 PCC Project
 *
 * D Language ABI implementation
 *
 * The D programming language has its own ABI distinct from C++.
 *
 * Name mangling format:
 *   _D<qualified_name><type_signature>
 *
 * Qualified names are encoded as:
 *   <length><name>[<length><name>]*
 *
 * Type encoding:
 *   v = void, b = bool, g = byte, h = ubyte
 *   s = short, t = ushort, i = int, k = uint
 *   l = long, m = ulong
 *   f = float, d = double, e = real (80-bit)
 *   a = char, u = wchar, w = dchar
 *   A = dynamic array, G = static array
 *   P = pointer, C = class, S = struct
 *   F = function
 *
 * Examples:
 *   void foo(int)        -> _D3fooFiZv
 *   module.Class.method  -> _D6module5Class6methodFZv
 *   void bar(int, float) -> _D3barFifZv
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

/* Forward declarations */
static void dlang_mangle_name(abi_mangle_ctx_t *ctx, const char *name);
static void dlang_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void dlang_mangle_qualified(abi_mangle_ctx_t *ctx, const char *module, const char *name);
static void dlang_mangle_signature(abi_mangle_ctx_t *ctx, const abi_param_t *params, const abi_type_t *ret);

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
 * Mangle function using D ABI
 *
 * Format: _D<module><class><function>F<params>Z<return>
 */
static char *
dlang_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	char *result;
	char buf[32];

	if (func == NULL)
		return NULL;

	/* extern(C) functions are not mangled */
	if (func->calling_conv == ABI_CC_C && !func->parent_class)
		return strdup(func->name);

	/* Initialize mangling context */
	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* D mangled names start with _D */
	MANGLE_APPEND(&mctx, "_D");

	/* Qualified name (module.class.function) */
	if (func->parent_class && func->parent_class->name) {
		/* Class method */
		const char *module = "module";  /* Would come from context */

		/* Module name */
		snprintf(buf, sizeof(buf), "%zu", strlen(module));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, module);

		/* Class name */
		snprintf(buf, sizeof(buf), "%zu", strlen(func->parent_class->name));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, func->parent_class->name);
	}

	/* Function name */
	snprintf(buf, sizeof(buf), "%zu", strlen(func->name));
	MANGLE_APPEND(&mctx, buf);
	MANGLE_APPEND(&mctx, func->name);

	/* Function signature */
	dlang_mangle_signature(&mctx, func->params, func->return_type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle function signature
 * Format: F<param_types>Z<return_type>
 */
static void
dlang_mangle_signature(abi_mangle_ctx_t *ctx, const abi_param_t *params, const abi_type_t *ret)
{
	const abi_param_t *p;

	MANGLE_APPENDC(ctx, 'F');

	/* Parameter types */
	for (p = params; p != NULL; p = p->next) {
		dlang_mangle_type(ctx, p->type);
	}

	/* End of parameters */
	MANGLE_APPENDC(ctx, 'Z');

	/* Return type */
	dlang_mangle_type(ctx, ret);
}

/*
 * Mangle type
 *
 * D type codes:
 *   v = void, b = bool
 *   g = byte, h = ubyte
 *   s = short, t = ushort
 *   i = int, k = uint
 *   l = long, m = ulong
 *   f = float, d = double, e = real
 *   a = char, u = wchar, w = dchar
 *   P = pointer, C = class, S = struct
 */
static void
dlang_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	char buf[32];

	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	switch (type->kind) {
	case ABI_TYPE_VOID:
		MANGLE_APPENDC(ctx, 'v');
		break;
	case ABI_TYPE_BOOL:
		MANGLE_APPENDC(ctx, 'b');
		break;
	case ABI_TYPE_CHAR:
		MANGLE_APPENDC(ctx, 'a');
		break;
	case ABI_TYPE_SCHAR:
		MANGLE_APPENDC(ctx, 'g');
		break;
	case ABI_TYPE_UCHAR:
		MANGLE_APPENDC(ctx, 'h');
		break;
	case ABI_TYPE_SHORT:
		MANGLE_APPENDC(ctx, 's');
		break;
	case ABI_TYPE_USHORT:
		MANGLE_APPENDC(ctx, 't');
		break;
	case ABI_TYPE_INT:
		MANGLE_APPENDC(ctx, 'i');
		break;
	case ABI_TYPE_UINT:
		MANGLE_APPENDC(ctx, 'k');
		break;
	case ABI_TYPE_LONG:
		MANGLE_APPENDC(ctx, 'l');
		break;
	case ABI_TYPE_ULONG:
		MANGLE_APPENDC(ctx, 'm');
		break;
	case ABI_TYPE_LONGLONG:
		MANGLE_APPENDC(ctx, 'l');  /* D long is 64-bit */
		break;
	case ABI_TYPE_ULONGLONG:
		MANGLE_APPENDC(ctx, 'm');  /* D ulong is 64-bit */
		break;
	case ABI_TYPE_FLOAT:
		MANGLE_APPENDC(ctx, 'f');
		break;
	case ABI_TYPE_DOUBLE:
		MANGLE_APPENDC(ctx, 'd');
		break;
	case ABI_TYPE_LONGDOUBLE:
		MANGLE_APPENDC(ctx, 'e');  /* real (80-bit) */
		break;
	case ABI_TYPE_POINTER:
		MANGLE_APPENDC(ctx, 'P');
		dlang_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_REFERENCE:
		/* D doesn't have references, treat as pointer */
		MANGLE_APPENDC(ctx, 'P');
		dlang_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_CLASS:
		MANGLE_APPENDC(ctx, 'C');
		if (type->u.class_type && type->u.class_type->name) {
			snprintf(buf, sizeof(buf), "%zu", strlen(type->u.class_type->name));
			MANGLE_APPEND(ctx, buf);
			MANGLE_APPEND(ctx, type->u.class_type->name);
		}
		break;
	case ABI_TYPE_STRUCT:
		MANGLE_APPENDC(ctx, 'S');
		if (type->u.class_type && type->u.class_type->name) {
			snprintf(buf, sizeof(buf), "%zu", strlen(type->u.class_type->name));
			MANGLE_APPEND(ctx, buf);
			MANGLE_APPEND(ctx, type->u.class_type->name);
		}
		break;
	case ABI_TYPE_ARRAY:
		MANGLE_APPENDC(ctx, 'A');  /* Dynamic array */
		dlang_mangle_type(ctx, type->u.element);
		break;
	default:
		MANGLE_APPENDC(ctx, 'v');
		break;
	}
}

/*
 * Mangle variable
 * Format: _D<module><name>
 */
static char *
dlang_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;
	char buf[32];

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "_D");

	/* Module name (simplified) */
	const char *module = "module";
	snprintf(buf, sizeof(buf), "%zu", strlen(module));
	MANGLE_APPEND(&mctx, buf);
	MANGLE_APPEND(&mctx, module);

	/* Variable name */
	snprintf(buf, sizeof(buf), "%zu", strlen(name));
	MANGLE_APPEND(&mctx, buf);
	MANGLE_APPEND(&mctx, name);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle type (standalone)
 */
static char *
dlang_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	dlang_mangle_type(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable
 * D vtables: __vtbl_<module>_<class>
 */
static char *
dlang_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;
	char buf[256];

	if (cls == NULL)
		return NULL;

	snprintf(buf, sizeof(buf), "__vtbl_module_%s", cls->name);
	result = strdup(buf);
	return result;
}

/*
 * Mangle RTTI (TypeInfo)
 * D TypeInfo: _D<module>8TypeInfo6<name>
 */
static char *
dlang_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;
	char buf[256];

	if (cls == NULL)
		return NULL;

	snprintf(buf, sizeof(buf), "_D6module8TypeInfo%zu%s",
	         strlen(cls->name), cls->name);
	result = strdup(buf);
	return result;
}

/*
 * D Language ABI operations table
 */
const abi_ops_t dlang_abi_ops = {
	.mangle_function = dlang_mangle_function,
	.mangle_variable = dlang_mangle_variable,
	.mangle_type = dlang_mangle_type_str,
	.mangle_vtable = dlang_mangle_vtable,
	.mangle_rtti = dlang_mangle_rtti,
	/* Layout operations - D uses similar layout to C++ */
	.layout_class = NULL,  /* Will use Itanium's */
	.layout_bases = NULL,
	.layout_fields = NULL,
	.layout_vtable = NULL,
	.compute_vtable_size = NULL,
	.build_vtable = NULL,
	.sizeof_type = NULL,
	.alignof_type = NULL,
};
