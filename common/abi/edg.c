/*
 * Copyright (c) 2025 PCC Project
 *
 * Edison Design Group (EDG) C++ ABI implementation
 *
 * EDG is a C++ frontend used by many compilers (Intel, Comeau, etc.)
 * Name mangling is similar to Itanium but with EDG-specific extensions.
 *
 * EDG mangling:
 *   - _Z prefix (like Itanium)
 *   - EDG-specific template encoding
 *   - Different compression scheme
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

/*
 * EDG uses Itanium-style mangling with minor variations
 * For simplicity, we use similar mangling
 */

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
edg_mangle_type(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (!type) {
		MANGLE_APPENDC(ctx, 'v');
		return;
	}

	switch (type->kind) {
	case ABI_TYPE_VOID: MANGLE_APPENDC(ctx, 'v'); break;
	case ABI_TYPE_BOOL: MANGLE_APPENDC(ctx, 'b'); break;
	case ABI_TYPE_CHAR: MANGLE_APPENDC(ctx, 'c'); break;
	case ABI_TYPE_INT: MANGLE_APPENDC(ctx, 'i'); break;
	case ABI_TYPE_LONG: MANGLE_APPENDC(ctx, 'l'); break;
	case ABI_TYPE_FLOAT: MANGLE_APPENDC(ctx, 'f'); break;
	case ABI_TYPE_DOUBLE: MANGLE_APPENDC(ctx, 'd'); break;
	case ABI_TYPE_POINTER:
		MANGLE_APPENDC(ctx, 'P');
		edg_mangle_type(ctx, type->u.pointee);
		break;
	default:
		MANGLE_APPENDC(ctx, 'v');
		break;
	}
}

static char *
edg_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	const abi_param_t *p;
	char buf[32];

	if (!func || !func->name)
		return NULL;

	if (func->calling_conv == ABI_CC_C && !func->parent_class)
		return strdup(func->name);

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* EDG uses _Z prefix */
	MANGLE_APPEND(&mctx, "_Z");

	/* Namespace/class */
	if (func->parent_class && func->parent_class->name) {
		MANGLE_APPENDC(&mctx, 'N');
		snprintf(buf, sizeof(buf), "%zu", strlen(func->parent_class->name));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, func->parent_class->name);
		snprintf(buf, sizeof(buf), "%zu", strlen(func->name));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, func->name);
		MANGLE_APPENDC(&mctx, 'E');
	} else {
		snprintf(buf, sizeof(buf), "%zu", strlen(func->name));
		MANGLE_APPEND(&mctx, buf);
		MANGLE_APPEND(&mctx, func->name);
	}

	/* Parameters */
	for (p = func->params; p != NULL; p = p->next) {
		edg_mangle_type(&mctx, p->type);
	}

	return mctx.buffer;
}

static char *
edg_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];

	if (!name)
		return NULL;

	snprintf(buf, sizeof(buf), "_Z%zu%s", strlen(name), name);
	return strdup(buf);
}

static char *
edg_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 64;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	edg_mangle_type(&mctx, type);

	return mctx.buffer;
}

static char *
edg_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "_ZTV%zu%s", strlen(cls->name), cls->name);
	return strdup(buf);
}

static char *
edg_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "_ZTI%zu%s", strlen(cls->name), cls->name);
	return strdup(buf);
}

const abi_ops_t edg_abi_ops = {
	.mangle_function = edg_mangle_function,
	.mangle_variable = edg_mangle_variable,
	.mangle_type = edg_mangle_type_str,
	.mangle_vtable = edg_mangle_vtable,
	.mangle_rtti = edg_mangle_rtti,
};
