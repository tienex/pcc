/*
 * Copyright (c) 2025 PCC Project
 *
 * Nim ABI implementation
 *
 * Nim name mangling:
 *   - <name>__<module>_<unique_id>
 *   - Underscores are significant
 *   - Module name encoded
 *   - Generic parameters included in mangling
 *
 * Examples:
 *   proc foo() -> foo__module_123
 *   module.bar -> bar__module_456
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static int nim_unique_id = 100;

static char *
nim_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func || !func->name)
		return NULL;

	/* Nim: name__module_id */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%s__%s_%d",
		         func->name,
		         func->parent_class->name,
		         nim_unique_id++);
	} else {
		snprintf(buf, sizeof(buf), "%s__module_%d",
		         func->name,
		         nim_unique_id++);
	}

	return strdup(buf);
}

static char *
nim_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];

	if (!name)
		return NULL;

	snprintf(buf, sizeof(buf), "%s__global_%d", name, nim_unique_id++);
	return strdup(buf);
}

static char *
nim_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("void");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("int");
	case ABI_TYPE_FLOAT: return strdup("float32");
	case ABI_TYPE_DOUBLE: return strdup("float64");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_CHAR: return strdup("char");
	default: return strdup("pointer");
	}
}

static char *
nim_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "vtable__%s_%d",
	         cls->name, nim_unique_id++);
	return strdup(buf);
}

static char *
nim_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "TNimType__%s_%d",
	         cls->name, nim_unique_id++);
	return strdup(buf);
}

const abi_ops_t nim_abi_ops = {
	.mangle_function = nim_mangle_function,
	.mangle_variable = nim_mangle_variable,
	.mangle_type = nim_mangle_type_str,
	.mangle_vtable = nim_mangle_vtable,
	.mangle_rtti = nim_mangle_rtti,
};
