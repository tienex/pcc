/*
 * Copyright (c) 2025 PCC Project
 *
 * Dart Native ABI implementation
 *
 * Dart FFI uses C calling convention.
 * Dart AOT compilation uses internal mangling.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
dart_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "Dart_%s_%s",
		         func->parent_class->name, func->name);
	} else {
		snprintf(buf, sizeof(buf), "Dart_%s", func->name);
	}

	return strdup(buf);
}

static char *
dart_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "Dart_%s", name);
	return strdup(buf);
}

static char *
dart_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("void");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_INT: return strdup("int");
	case ABI_TYPE_LONG: return strdup("int");
	case ABI_TYPE_FLOAT: return strdup("double");
	case ABI_TYPE_DOUBLE: return strdup("double");
	default: return strdup("dynamic");
	}
}

static char *
dart_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "Dart_vtable_%s", cls->name);
	return strdup(buf);
}

static char *
dart_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "Dart_Type_%s", cls->name);
	return strdup(buf);
}

const abi_ops_t dart_abi_ops = {
	.mangle_function = dart_mangle_function,
	.mangle_variable = dart_mangle_variable,
	.mangle_type = dart_mangle_type_str,
	.mangle_vtable = dart_mangle_vtable,
	.mangle_rtti = dart_mangle_rtti,
};
