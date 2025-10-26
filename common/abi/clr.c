/*
 * Copyright (c) 2025 PCC Project
 *
 * CLR (Common Language Runtime) .NET ABI
 *
 * .NET name mangling for C++/CLI and P/Invoke:
 *   Namespace.ClassName.MethodName
 *
 * IL (Intermediate Language) uses different name mangling than native code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
clr_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%s.%s",
		         func->parent_class->name, func->name);
	} else {
		snprintf(buf, sizeof(buf), "%s", func->name);
	}

	return strdup(buf);
}

static char *
clr_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	return strdup(name);
}

static char *
clr_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("System.Void");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("System.Void");
	case ABI_TYPE_BOOL: return strdup("System.Boolean");
	case ABI_TYPE_CHAR: return strdup("System.Char");
	case ABI_TYPE_INT: return strdup("System.Int32");
	case ABI_TYPE_LONG: return strdup("System.Int64");
	case ABI_TYPE_FLOAT: return strdup("System.Single");
	case ABI_TYPE_DOUBLE: return strdup("System.Double");
	case ABI_TYPE_CLASS: return strdup("System.Object");
	default: return strdup("System.Void");
	}
}

static char *
clr_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "%s_vtable", cls->name);
	return strdup(buf);
}

static char *
clr_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "%s_TypeInfo", cls->name);
	return strdup(buf);
}

const abi_ops_t clr_abi_ops = {
	.mangle_function = clr_mangle_function,
	.mangle_variable = clr_mangle_variable,
	.mangle_type = clr_mangle_type_str,
	.mangle_vtable = clr_mangle_vtable,
	.mangle_rtti = clr_mangle_rtti,
};
