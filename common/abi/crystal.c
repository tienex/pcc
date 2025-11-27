/*
 * Copyright (c) 2025 PCC Project
 *
 * Crystal ABI implementation
 *
 * Crystal uses LLVM and Itanium-style mangling.
 * Format: *<module>::<class>#<method>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
crystal_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "*%s#%s",
		         func->parent_class->name, func->name);
	} else {
		snprintf(buf, sizeof(buf), "*%s", func->name);
	}

	return strdup(buf);
}

static char *
crystal_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "*%s", name);
	return strdup(buf);
}

static char *
crystal_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("Nil");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("Nil");
	case ABI_TYPE_BOOL: return strdup("Bool");
	case ABI_TYPE_INT: return strdup("Int32");
	case ABI_TYPE_LONG: return strdup("Int64");
	case ABI_TYPE_FLOAT: return strdup("Float32");
	case ABI_TYPE_DOUBLE: return strdup("Float64");
	default: return strdup("Nil");
	}
}

static char *
crystal_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "*%s::vtable", cls->name);
	return strdup(buf);
}

static char *
crystal_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "*%s::class", cls->name);
	return strdup(buf);
}

const abi_ops_t crystal_abi_ops = {
	.mangle_function = crystal_mangle_function,
	.mangle_variable = crystal_mangle_variable,
	.mangle_type = crystal_mangle_type_str,
	.mangle_vtable = crystal_mangle_vtable,
	.mangle_rtti = crystal_mangle_rtti,
};
