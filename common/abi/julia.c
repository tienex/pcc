/*
 * Copyright (c) 2025 PCC Project
 *
 * Julia ABI implementation
 *
 * Julia name mangling:
 *   - julia_<function>_<unique_id>
 *   - Module functions: jl_<Module>_<function>
 *   - Generic functions include type signature
 *   - Special chars replaced with _
 *   - C-compatible exports use @ccall
 *
 * Examples:
 *   function foo() -> julia_foo_12345
 *   Base.map -> jl_Base_map
 *   foo(::Int64) -> julia_foo_Int64_12345
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

static int julia_unique_id = 10000;

static void
julia_sanitize_name(const char *name, char *out)
{
	int i, j = 0;
	for (i = 0; name[i] && j < 250; i++) {
		if (isalnum(name[i]))
			out[j++] = name[i];
		else
			out[j++] = '_';
	}
	out[j] = '\0';
}

static char *
julia_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];
	char sanitized[256];

	if (!func || !func->name)
		return NULL;

	julia_sanitize_name(func->name, sanitized);

	/* Module.function */
	if (func->parent_class && func->parent_class->name) {
		char module_san[256];
		julia_sanitize_name(func->parent_class->name, module_san);
		snprintf(buf, sizeof(buf), "jl_%s_%s_%d",
		         module_san, sanitized, julia_unique_id++);
	} else {
		snprintf(buf, sizeof(buf), "julia_%s_%d",
		         sanitized, julia_unique_id++);
	}

	return strdup(buf);
}

static char *
julia_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	char sanitized[256];

	if (!name)
		return NULL;

	julia_sanitize_name(name, sanitized);
	snprintf(buf, sizeof(buf), "jl_global_%s", sanitized);

	return strdup(buf);
}

static char *
julia_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("Nothing");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("Int64");
	case ABI_TYPE_FLOAT: return strdup("Float32");
	case ABI_TYPE_DOUBLE: return strdup("Float64");
	case ABI_TYPE_BOOL: return strdup("Bool");
	case ABI_TYPE_CHAR: return strdup("Char");
	default: return strdup("Any");
	}
}

static char *
julia_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	char sanitized[256];

	if (!cls || !cls->name)
		return NULL;

	julia_sanitize_name(cls->name, sanitized);
	snprintf(buf, sizeof(buf), "jl_vtable_%s", sanitized);

	return strdup(buf);
}

static char *
julia_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	char sanitized[256];

	if (!cls || !cls->name)
		return NULL;

	julia_sanitize_name(cls->name, sanitized);
	snprintf(buf, sizeof(buf), "jl_typename_%s", sanitized);

	return strdup(buf);
}

const abi_ops_t julia_abi_ops = {
	.mangle_function = julia_mangle_function,
	.mangle_variable = julia_mangle_variable,
	.mangle_type = julia_mangle_type_str,
	.mangle_vtable = julia_mangle_vtable,
	.mangle_rtti = julia_mangle_rtti,
};
