/*
 * Copyright (c) 2025 PCC Project
 *
 * F# ABI implementation
 *
 * F# name mangling (on .NET):
 *   - Uses .NET naming conventions
 *   - Namespace.Module.function
 *   - Special characters encoded
 *   - Generic parameters: `N suffix (e.g., Map`2)
 *   - Operators: op_Addition, op_Multiply, etc.
 *
 * Examples:
 *   List.map -> FSharp.Collections.ListModule.Map
 *   (+) -> op_Addition
 *   List.map<'a,'b> -> ListModule.Map`2
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static const char *
fsharp_operator_name(const char *op)
{
	if (strcmp(op, "+") == 0) return "op_Addition";
	if (strcmp(op, "-") == 0) return "op_Subtraction";
	if (strcmp(op, "*") == 0) return "op_Multiply";
	if (strcmp(op, "/") == 0) return "op_Division";
	if (strcmp(op, "%") == 0) return "op_Modulus";
	if (strcmp(op, "=") == 0) return "op_Equality";
	if (strcmp(op, "<>") == 0) return "op_Inequality";
	if (strcmp(op, "<") == 0) return "op_LessThan";
	if (strcmp(op, ">") == 0) return "op_GreaterThan";
	if (strcmp(op, "<=") == 0) return "op_LessThanOrEqual";
	if (strcmp(op, ">=") == 0) return "op_GreaterThanOrEqual";
	if (strcmp(op, "::") == 0) return "op_ColonColon";
	if (strcmp(op, "@") == 0) return "op_Append";
	if (strcmp(op, "^") == 0) return "op_Concatenate";
	if (strcmp(op, "|>") == 0) return "op_PipeRight";
	if (strcmp(op, "||") == 0) return "op_BooleanOr";
	if (strcmp(op, "&&") == 0) return "op_BooleanAnd";
	return op;
}

static char *
fsharp_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];
	const char *func_name;

	if (!func || !func->name)
		return NULL;

	/* Check if it's an operator */
	func_name = fsharp_operator_name(func->name);

	/* Module.function */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%s.%s",
		         func->parent_class->name, func_name);
	} else {
		snprintf(buf, sizeof(buf), "%s", func_name);
	}

	return strdup(buf);
}

static char *
fsharp_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	if (!name)
		return NULL;

	return strdup(name);
}

static char *
fsharp_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("unit");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("int");
	case ABI_TYPE_FLOAT: return strdup("float32");
	case ABI_TYPE_DOUBLE: return strdup("float");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_CHAR: return strdup("char");
	default: return strdup("obj");
	}
}

static char *
fsharp_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "%s.VTable", cls->name);
	return strdup(buf);
}

static char *
fsharp_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "%s.TypeInfo", cls->name);
	return strdup(buf);
}

const abi_ops_t fsharp_abi_ops = {
	.mangle_function = fsharp_mangle_function,
	.mangle_variable = fsharp_mangle_variable,
	.mangle_type = fsharp_mangle_type_str,
	.mangle_vtable = fsharp_mangle_vtable,
	.mangle_rtti = fsharp_mangle_rtti,
};
