/*
 * Copyright (c) 2025 PCC Project
 *
 * AT&T Cfront C++ ABI implementation
 *
 * Cfront was the original AT&T C++ compiler (1980s-1990s).
 * Name mangling (Cfront 2.0/3.0):
 *   - <name>__F<class><args>
 *   - __<length><name> for namespace/class
 *   - Simple type codes: i=int, f=float, d=double, etc.
 *
 * Examples:
 *   foo(int) -> foo__Fi
 *   Class::method(int, double) -> method__5ClassFid
 *   operator+(int, int) -> __pl__Fii
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

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

static void
cfront_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (!type) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	switch (type->kind) {
	case ABI_TYPE_VOID: MANGLE_APPENDC(ctx, 'v'); break;
	case ABI_TYPE_CHAR: MANGLE_APPENDC(ctx, 'c'); break;
	case ABI_TYPE_SHORT: MANGLE_APPENDC(ctx, 's'); break;
	case ABI_TYPE_INT: MANGLE_APPENDC(ctx, 'i'); break;
	case ABI_TYPE_LONG: MANGLE_APPENDC(ctx, 'l'); break;
	case ABI_TYPE_FLOAT: MANGLE_APPENDC(ctx, 'f'); break;
	case ABI_TYPE_DOUBLE: MANGLE_APPENDC(ctx, 'd'); break;
	case ABI_TYPE_POINTER:
		MANGLE_APPENDC(ctx, 'P');
		cfront_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_REFERENCE:
		MANGLE_APPENDC(ctx, 'R');
		cfront_mangle_type(ctx, type->u.pointee);
		break;
	default:
		MANGLE_APPENDC(ctx, 'v');
		break;
	}
}

static char *
cfront_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	const abi_param_t *p;
	char buf[32];

	if (!func || !func->name)
		return NULL;

	/* C functions not mangled */
	if (func->calling_conv == ABI_CC_C && !func->parent_class)
		return strdup(func->name);

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* Function name */
	MANGLE_APPEND(&mctx, func->name);
	MANGLE_APPEND(&mctx, "__");

	/* Class name with length prefix */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%zu", strlen(func->parent_class->name));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, func->parent_class->name);
	}

	/* Parameters */
	MANGLE_APPENDC(&mctx, 'F');
	for (p = func->params; p != NULL; p = p->next) {
		cfront_mangle_type(&mctx, p->type);
	}

	return mctx.buffer;
}

static char *
cfront_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];

	if (!name)
		return NULL;

	/* Static class members */
	snprintf(buf, sizeof(buf), "%s__", name);
	return strdup(buf);
}

static char *
cfront_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 64;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	cfront_mangle_type(&mctx, type);

	return mctx.buffer;
}

static char *
cfront_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "__vtbl__%s", cls->name);
	return strdup(buf);
}

static char *
cfront_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "__ti__%s", cls->name);
	return strdup(buf);
}

const abi_ops_t cfront_abi_ops = {
	.mangle_function = cfront_mangle_function,
	.mangle_variable = cfront_mangle_variable,
	.mangle_type = cfront_mangle_type_str,
	.mangle_vtable = cfront_mangle_vtable,
	.mangle_rtti = cfront_mangle_rtti,
};
