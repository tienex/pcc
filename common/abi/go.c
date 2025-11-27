/*
 * Copyright (c) 2025 PCC Project
 *
 * Go (golang) ABI implementation
 *
 * Go name mangling for exported symbols:
 *   package.Type.Method
 *   main.main
 *
 * Internal ABI uses plan9-style calling convention on most platforms.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
go_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "go.%s.%s",
		         func->parent_class->name, func->name);
	} else {
		snprintf(buf, sizeof(buf), "go.main.%s", func->name);
	}

	return strdup(buf);
}

static char *
go_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "go.main.%s", name);
	return strdup(buf);
}

static char *
go_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("void");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_INT: return strdup("int");
	case ABI_TYPE_LONG: return strdup("int64");
	case ABI_TYPE_FLOAT: return strdup("float32");
	case ABI_TYPE_DOUBLE: return strdup("float64");
	case ABI_TYPE_CLASS: return strdup("interface{}");
	default: return strdup("void");
	}
}

static char *
go_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "go.itab.%s", cls->name);
	return strdup(buf);
}

static char *
go_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "go.type.%s", cls->name);
	return strdup(buf);
}

const abi_ops_t go_abi_ops = {
	.mangle_function = go_mangle_function,
	.mangle_variable = go_mangle_variable,
	.mangle_type = go_mangle_type_str,
	.mangle_vtable = go_mangle_vtable,
	.mangle_rtti = go_mangle_rtti,
};
