/*
 * Copyright (c) 2025 PCC Project
 *
 * Kotlin/Native ABI implementation
 *
 * Kotlin/Native uses custom mangling similar to Swift.
 * Format: kfun:package.Class#method
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
kotlin_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "kfun:%s#%s",
		         func->parent_class->name, func->name);
	} else {
		snprintf(buf, sizeof(buf), "kfun:%s", func->name);
	}

	return strdup(buf);
}

static char *
kotlin_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "kvar:%s", name);
	return strdup(buf);
}

static char *
kotlin_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("Unit");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("Unit");
	case ABI_TYPE_BOOL: return strdup("Boolean");
	case ABI_TYPE_INT: return strdup("Int");
	case ABI_TYPE_LONG: return strdup("Long");
	case ABI_TYPE_FLOAT: return strdup("Float");
	case ABI_TYPE_DOUBLE: return strdup("Double");
	default: return strdup("Any");
	}
}

static char *
kotlin_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "kvtable:%s", cls->name);
	return strdup(buf);
}

static char *
kotlin_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "ktype:%s", cls->name);
	return strdup(buf);
}

const abi_ops_t kotlin_abi_ops = {
	.mangle_function = kotlin_mangle_function,
	.mangle_variable = kotlin_mangle_variable,
	.mangle_type = kotlin_mangle_type_str,
	.mangle_vtable = kotlin_mangle_vtable,
	.mangle_rtti = kotlin_mangle_rtti,
};
