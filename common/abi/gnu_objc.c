/*
 * Copyright (c) 2025 PCC Project
 *
 * GNU Objective-C runtime ABI implementation
 *
 * The GNU Objective-C runtime (libobjc) is used by GCC.
 * Supports both GNU runtime 1.0 (legacy) and 2.0 (modern).
 *
 * Name mangling is similar to Apple but with GNU-specific symbols:
 *   Instance methods: -[ClassName methodName:param:]
 *   Class methods:    +[ClassName methodName:param:]
 *
 * Internal symbols:
 *   Classes:     _OBJC_Class_ClassName
 *   Metaclasses: _OBJC_MetaClass_ClassName
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

static char *
gnu_objc_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	const abi_param_t *p;

	if (func == NULL)
		return NULL;

	if (func->calling_conv == ABI_CC_C && !func->parent_class)
		return strdup(func->name);

	memset(&mctx, 0, sizeof(mctx));
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPENDC(&mctx, func->is_static ? '+' : '-');
	MANGLE_APPENDC(&mctx, '[');

	if (func->parent_class && func->parent_class->name)
		MANGLE_APPEND(&mctx, func->parent_class->name);
	else
		MANGLE_APPEND(&mctx, "UnknownClass");

	MANGLE_APPENDC(&mctx, ' ');
	MANGLE_APPEND(&mctx, func->name);

	for (p = func->params; p != NULL; p = p->next)
		MANGLE_APPENDC(&mctx, ':');

	MANGLE_APPENDC(&mctx, ']');

	return mctx.buffer;
}

static char *
gnu_objc_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "_OBJC_INSTANCE_VARIABLE_%s", name);
	return strdup(buf);
}

static char *
gnu_objc_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (type == NULL)
		return strdup("v");

	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("v");
	case ABI_TYPE_CHAR: return strdup("c");
	case ABI_TYPE_INT: return strdup("i");
	case ABI_TYPE_FLOAT: return strdup("f");
	case ABI_TYPE_DOUBLE: return strdup("d");
	case ABI_TYPE_CLASS: return strdup("@");
	default: return strdup("v");
	}
}

static char *
gnu_objc_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	if (cls == NULL) return NULL;
	snprintf(buf, sizeof(buf), "_OBJC_Class_%s", cls->name);
	return strdup(buf);
}

static char *
gnu_objc_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	if (cls == NULL) return NULL;
	snprintf(buf, sizeof(buf), "_OBJC_MetaClass_%s", cls->name);
	return strdup(buf);
}

const abi_ops_t gnu_objc_abi_ops = {
	.mangle_function = gnu_objc_mangle_function,
	.mangle_variable = gnu_objc_mangle_variable,
	.mangle_type = gnu_objc_mangle_type_str,
	.mangle_vtable = gnu_objc_mangle_vtable,
	.mangle_rtti = gnu_objc_mangle_rtti,
	.layout_class = NULL,  /* Use standard layout */
	.layout_bases = NULL,
	.layout_fields = NULL,
	.layout_vtable = NULL,
	.compute_vtable_size = NULL,
	.build_vtable = NULL,
	.sizeof_type = NULL,
	.alignof_type = NULL,
};
