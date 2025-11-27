/*
 * Copyright (c) 2025 PCC Project
 *
 * Zig ABI implementation
 *
 * Zig mangling format:
 *   namespace.Type.function
 *
 * Zig uses C ABI for extern functions, and custom mangling for native.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
zig_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->calling_conv == ABI_CC_C && !func->parent_class)
		return strdup(func->name);

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%s.%s",
		         func->parent_class->name, func->name);
	} else {
		return strdup(func->name);
	}

	return strdup(buf);
}

static char *
zig_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	return strdup(name);
}

static char *
zig_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("void");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_INT: return strdup("i32");
	case ABI_TYPE_LONG: return strdup("i64");
	case ABI_TYPE_FLOAT: return strdup("f32");
	case ABI_TYPE_DOUBLE: return strdup("f64");
	default: return strdup("void");
	}
}

static char *
zig_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "%s.vtable", cls->name);
	return strdup(buf);
}

static char *
zig_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "%s.typeinfo", cls->name);
	return strdup(buf);
}

const abi_ops_t zig_abi_ops = {
	.mangle_function = zig_mangle_function,
	.mangle_variable = zig_mangle_variable,
	.mangle_type = zig_mangle_type_str,
	.mangle_vtable = zig_mangle_vtable,
	.mangle_rtti = zig_mangle_rtti,
};
