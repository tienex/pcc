/*
 * Copyright (c) 2025 PCC Project
 *
 * Rust ABI implementation
 *
 * Rust uses Itanium-style mangling with "R" prefix in modern versions.
 * Legacy mangling: _ZN...E format
 * Modern v0 mangling: _R<symbol-name>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
rust_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	/* Simplified Rust mangling */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "_ZN%zu%s%zu%sE",
		         strlen(func->parent_class->name), func->parent_class->name,
		         strlen(func->name), func->name);
	} else {
		snprintf(buf, sizeof(buf), "_ZN%zu%sE",
		         strlen(func->name), func->name);
	}

	return strdup(buf);
}

static char *
rust_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "_ZN%zu%sE", strlen(name), name);
	return strdup(buf);
}

static char *
rust_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("()");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("()");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_INT: return strdup("i32");
	case ABI_TYPE_LONG: return strdup("i64");
	case ABI_TYPE_FLOAT: return strdup("f32");
	case ABI_TYPE_DOUBLE: return strdup("f64");
	default: return strdup("()");
	}
}

static char *
rust_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_ZN%zu%s5vtableE", strlen(cls->name), cls->name);
	return strdup(buf);
}

static char *
rust_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_ZN%zu%s4typeE", strlen(cls->name), cls->name);
	return strdup(buf);
}

const abi_ops_t rust_abi_ops = {
	.mangle_function = rust_mangle_function,
	.mangle_variable = rust_mangle_variable,
	.mangle_type = rust_mangle_type_str,
	.mangle_vtable = rust_mangle_vtable,
	.mangle_rtti = rust_mangle_rtti,
};
