/*
 * Copyright (c) 2025 PCC Project
 *
 * eCere SDK ABI implementation
 *
 * eCere is an object-oriented C framework.
 * Uses C-style naming with class prefixes.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
ecere_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%s_%s",
		         func->parent_class->name, func->name);
	} else {
		return strdup(func->name);
	}

	return strdup(buf);
}

static char *
ecere_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	return strdup(name);
}

static char *
ecere_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("void");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_INT: return strdup("int");
	case ABI_TYPE_FLOAT: return strdup("float");
	case ABI_TYPE_DOUBLE: return strdup("double");
	default: return strdup("void");
	}
}

static char *
ecere_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_vtbl_%s", cls->name);
	return strdup(buf);
}

static char *
ecere_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_class_%s", cls->name);
	return strdup(buf);
}

const abi_ops_t ecere_abi_ops = {
	.mangle_function = ecere_mangle_function,
	.mangle_variable = ecere_mangle_variable,
	.mangle_type = ecere_mangle_type_str,
	.mangle_vtable = ecere_mangle_vtable,
	.mangle_rtti = ecere_mangle_rtti,
};
