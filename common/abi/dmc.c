/*
 * Copyright (c) 2025 PCC Project
 *
 * Digital Mars C++ ABI (Zortech/Symantec C++)
 *
 * DMC uses compressed name mangling for C++:
 *   ?name@Class@@QAE...
 *
 * This is similar to MSVC but with some differences.
 * Zortech was the first C++ compiler for DOS/Windows.
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
dmc_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (!type) {
		MANGLE_APPENDC(ctx, 'V');
		return;
	}

	switch (type->kind) {
	case ABI_TYPE_VOID: MANGLE_APPENDC(ctx, 'V'); break;
	case ABI_TYPE_CHAR: MANGLE_APPENDC(ctx, 'D'); break;
	case ABI_TYPE_INT: MANGLE_APPENDC(ctx, 'H'); break;
	case ABI_TYPE_LONG: MANGLE_APPENDC(ctx, 'J'); break;
	case ABI_TYPE_FLOAT: MANGLE_APPENDC(ctx, 'M'); break;
	case ABI_TYPE_DOUBLE: MANGLE_APPENDC(ctx, 'N'); break;
	case ABI_TYPE_POINTER:
		MANGLE_APPEND(ctx, "PA");
		dmc_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_REFERENCE:
		MANGLE_APPEND(ctx, "AA");
		dmc_mangle_type(ctx, type->u.pointee);
		break;
	case ABI_TYPE_CLASS:
		if (type->u.class_type && type->u.class_type->name) {
			MANGLE_APPENDC(ctx, 'V');
			MANGLE_APPEND(ctx, type->u.class_type->name);
			MANGLE_APPEND(ctx, "@@");
		}
		break;
	default:
		MANGLE_APPENDC(ctx, 'V');
		break;
	}
}

static char *
dmc_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	const abi_param_t *p;

	if (!func) return NULL;

	if (func->calling_conv == ABI_CC_C && !func->parent_class)
		return strdup(func->name);

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* DMC mangling starts with ? */
	MANGLE_APPENDC(&mctx, '?');
	MANGLE_APPEND(&mctx, func->name);

	if (func->parent_class && func->parent_class->name) {
		MANGLE_APPENDC(&mctx, '@');
		MANGLE_APPEND(&mctx, func->parent_class->name);
	}

	MANGLE_APPEND(&mctx, "@@");

	/* Calling convention and modifiers */
	if (func->is_static)
		MANGLE_APPEND(&mctx, "SA");
	else
		MANGLE_APPEND(&mctx, "QA");

	MANGLE_APPENDC(&mctx, 'E');  /* __cdecl */

	/* Parameter types */
	for (p = func->params; p != NULL; p = p->next)
		dmc_mangle_type(&mctx, p->type);

	/* Return type */
	dmc_mangle_type(&mctx, func->return_type);

	MANGLE_APPENDC(&mctx, 'Z');

	return mctx.buffer;
}

static char *
dmc_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "?%s@@3", name);
	return strdup(buf);
}

static char *
dmc_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';
	dmc_mangle_type(&mctx, type);
	return mctx.buffer;
}

static char *
dmc_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "??_7%s@@6B@", cls->name);
	return strdup(buf);
}

static char *
dmc_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "??_R0?AV%s@@@8", cls->name);
	return strdup(buf);
}

const abi_ops_t dmc_abi_ops = {
	.mangle_function = dmc_mangle_function,
	.mangle_variable = dmc_mangle_variable,
	.mangle_type = dmc_mangle_type_str,
	.mangle_vtable = dmc_mangle_vtable,
	.mangle_rtti = dmc_mangle_rtti,
};
