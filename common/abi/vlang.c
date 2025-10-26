/*
 * Copyright (c) 2025 PCC Project
 *
 * V Language ABI implementation
 *
 * V name mangling:
 *   - <module>__<function>
 *   - Generic parameters included
 *   - Simple underscore separation
 *   - C interop uses plain names
 *
 * Examples:
 *   fn foo() -> main__foo
 *   module math, fn add -> math__add
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
vlang_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func || !func->name)
		return NULL;

	/* C interop - no mangling */
	if (func->calling_conv == ABI_CC_C)
		return strdup(func->name);

	/* module__function */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%s__%s",
		         func->parent_class->name,
		         func->name);
	} else {
		snprintf(buf, sizeof(buf), "main__%s", func->name);
	}

	return strdup(buf);
}

static char *
vlang_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];

	if (!name)
		return NULL;

	snprintf(buf, sizeof(buf), "main__%s", name);
	return strdup(buf);
}

static char *
vlang_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("void");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("int");
	case ABI_TYPE_FLOAT: return strdup("f32");
	case ABI_TYPE_DOUBLE: return strdup("f64");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_CHAR: return strdup("rune");
	default: return strdup("voidptr");
	}
}

static char *
vlang_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "vtable__%s", cls->name);
	return strdup(buf);
}

static char *
vlang_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "typeinfo__%s", cls->name);
	return strdup(buf);
}

const abi_ops_t vlang_abi_ops = {
	.mangle_function = vlang_mangle_function,
	.mangle_variable = vlang_mangle_variable,
	.mangle_type = vlang_mangle_type_str,
	.mangle_vtable = vlang_mangle_vtable,
	.mangle_rtti = vlang_mangle_rtti,
};
