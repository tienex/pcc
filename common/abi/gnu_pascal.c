/*
 * Copyright (c) 2025 PCC Project
 *
 * GNU Pascal ABI implementation
 *
 * GNU Pascal (GPC) uses C-style naming with underscores.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

static char *
to_lower(const char *s)
{
	static char buf[256];
	int i;
	for (i = 0; s[i] && i < 255; i++)
		buf[i] = tolower(s[i]);
	buf[i] = '\0';
	return buf;
}

static char *
gpc_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "_p_%s_%s",
		         to_lower(func->parent_class->name),
		         to_lower(func->name));
	} else {
		snprintf(buf, sizeof(buf), "_p_%s", to_lower(func->name));
	}

	return strdup(buf);
}

static char *
gpc_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "_v_%s", to_lower(name));
	return strdup(buf);
}

static char *
gpc_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("void");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_BOOL: return strdup("boolean");
	case ABI_TYPE_INT: return strdup("integer");
	case ABI_TYPE_FLOAT: return strdup("real");
	case ABI_TYPE_DOUBLE: return strdup("real");
	default: return strdup("void");
	}
}

static char *
gpc_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_vtbl_%s", to_lower(cls->name));
	return strdup(buf);
}

static char *
gpc_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_rtti_%s", to_lower(cls->name));
	return strdup(buf);
}

const abi_ops_t gnu_pascal_abi_ops = {
	.mangle_function = gpc_mangle_function,
	.mangle_variable = gpc_mangle_variable,
	.mangle_type = gpc_mangle_type_str,
	.mangle_vtable = gpc_mangle_vtable,
	.mangle_rtti = gpc_mangle_rtti,
};
