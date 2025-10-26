/*
 * Copyright (c) 2025 PCC Project
 *
 * Old GNU C++ ABI implementation (ARM/CFront style, GCC 2.x)
 *
 * Used by:
 * - GCC 2.x (before GCC 3.0)
 * - Old versions of g++
 * - ARM C++ (original ARM ABI before Itanium)
 * - CFront-based compilers
 *
 * This is the "version 2" mangling scheme, different from modern Itanium ABI.
 *
 * Mangling format:
 *   __<length><name>
 *   __<length><name>__<signature>
 *
 * Examples:
 *   foo(int)         -> foo__Fi
 *   Class::method()  -> method__5ClassFv
 *   operator+        -> __pl__FR3FooFi
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

/* Forward declarations */
static void gnu_old_mangle_name(abi_mangle_ctx_t *ctx, const char *name);
static void gnu_old_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void gnu_old_mangle_signature(abi_mangle_ctx_t *ctx, const abi_param_t *params);

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
 * Mangle function using old GNU ABI
 */
static char *
gnu_old_mangle_function(abi_context_t *ctx, const abi_function_t *func)
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

	/* Function name */
	gnu_old_mangle_name(&mctx, func->name);

	/* Class prefix if method */
	if (func->parent_class) {
		MANGLE_APPEND(&mctx, "__");
		snprintf(buf, sizeof(buf), "%zu", strlen(func->parent_class->name));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, func->parent_class->name);
	} else {
		MANGLE_APPEND(&mctx, "__");
	}

	/* Function signature */
	MANGLE_APPENDC(&mctx, 'F');
	gnu_old_mangle_signature(&mctx, func->params);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle a name (no encoding, just copy)
 */
static void
gnu_old_mangle_name(abi_mangle_ctx_t *ctx, const char *name)
{
	if (name == NULL)
		return;

	MANGLE_APPEND(ctx, name);
}

/*
 * Mangle function signature
 */
static void
gnu_old_mangle_signature(abi_mangle_ctx_t *ctx, const abi_param_t *params)
{
	const abi_param_t *p;

	if (params == NULL) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	for (p = params; p != NULL; p = p->next) {
		gnu_old_mangle_type(ctx, p->type);
	}
}

/*
 * Mangle type
 *
 * Old GNU type codes:
 *   v = void, c = char, s = short, i = int, l = long
 *   f = float, d = double, r = long double
 *   P = pointer, R = reference
 */
static void
gnu_old_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	/* Const qualifier */
	if (type->is_const)
		MANGLE_APPENDC(ctx, 'C');

	switch (type->kind) {
	case ABI_TYPE_VOID:
		MANGLE_APPENDC(ctx, 'v');
		break;
	case ABI_TYPE_CHAR:
		MANGLE_APPENDC(ctx, 'c');
		break;
	case ABI_TYPE_SHORT:
		MANGLE_APPENDC(ctx, 's');
		break;
	case ABI_TYPE_INT:
		MANGLE_APPENDC(ctx, 'i');
		break;
	case ABI_TYPE_LONG:
		MANGLE_APPENDC(ctx, 'l');
		break;
	case ABI_TYPE_LONGLONG:
		MANGLE_APPENDC(ctx, 'x');
		break;
	case ABI_TYPE_FLOAT:
		MANGLE_APPENDC(ctx, 'f');
		break;
	case ABI_TYPE_DOUBLE:
		MANGLE_APPENDC(ctx, 'd');
		break;
	case ABI_TYPE_LONGDOUBLE:
		MANGLE_APPENDC(ctx, 'r');
		break;
	case ABI_TYPE_POINTER:
		MANGLE_APPENDC(ctx, 'P');
		gnu_old_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_REFERENCE:
		MANGLE_APPENDC(ctx, 'R');
		gnu_old_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_CLASS:
	case ABI_TYPE_STRUCT:
		/* Class: length + name */
		if (type->u.class_type && type->u.class_type->name) {
			char buf[32];
			snprintf(buf, sizeof(buf), "%zu", strlen(type->u.class_type->name));
			MANGLE_APPEND(ctx, buf);
			MANGLE_APPEND(ctx, type->u.class_type->name);
		}
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
gnu_old_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	/* Global variables not mangled in old GNU ABI */
	return strdup(name);
}

/*
 * Mangle type (standalone)
 */
static char *
gnu_old_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	gnu_old_mangle_type(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable
 */
static char *
gnu_old_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;
	char buf[256];

	if (cls == NULL)
		return NULL;

	/* Old GNU: __vt_<length><name> */
	snprintf(buf, sizeof(buf), "__vt_%zu%s", strlen(cls->name), cls->name);
	result = strdup(buf);
	return result;
}

/*
 * Mangle RTTI
 */
static char *
gnu_old_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char *result;
	char buf[256];

	if (cls == NULL)
		return NULL;

	/* Old GNU: __ti_<length><name> */
	snprintf(buf, sizeof(buf), "__ti_%zu%s", strlen(cls->name), cls->name);
	result = strdup(buf);
	return result;
}

/*
 * Old GNU ABI operations table
 */
const abi_ops_t gnu_old_abi_ops = {
	.mangle_function = gnu_old_mangle_function,
	.mangle_variable = gnu_old_mangle_variable,
	.mangle_type = gnu_old_mangle_type_str,
	.mangle_vtable = gnu_old_mangle_vtable,
	.mangle_rtti = gnu_old_mangle_rtti,
	/* Layout operations same as Itanium */
	.layout_class = NULL,  /* Will use Itanium's */
	.layout_bases = NULL,
	.layout_fields = NULL,
	.layout_vtable = NULL,
	.compute_vtable_size = NULL,
	.build_vtable = NULL,
	.sizeof_type = NULL,
	.alignof_type = NULL,
};
