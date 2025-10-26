/*
 * Copyright (c) 2025 PCC Project
 *
 * FreePascal ABI implementation
 *
 * FreePascal uses mangling for units and methods:
 *   UNIT_CLASSNAME_$_METHODNAME
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

static char *
to_upper(const char *s)
{
	static char buf[256];
	int i;
	for (i = 0; s[i] && i < 255; i++)
		buf[i] = toupper(s[i]);
	buf[i] = '\0';
	return buf;
}

static char *
fpc_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "UNIT_%s_$_%s",
		         to_upper(func->parent_class->name),
		         to_upper(func->name));
	} else {
		snprintf(buf, sizeof(buf), "P$%s", to_upper(func->name));
	}

	return strdup(buf);
}

static char *
fpc_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "U_$%s", to_upper(name));
	return strdup(buf);
}

static char *
fpc_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("V");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("V");
	case ABI_TYPE_BOOL: return strdup("B");
	case ABI_TYPE_INT: return strdup("I");
	case ABI_TYPE_LONG: return strdup("L");
	case ABI_TYPE_FLOAT: return strdup("S");
	case ABI_TYPE_DOUBLE: return strdup("D");
	default: return strdup("V");
	}
}

static char *
fpc_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "VMT_%s", to_upper(cls->name));
	return strdup(buf);
}

static char *
fpc_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "RTTI_%s", to_upper(cls->name));
	return strdup(buf);
}

const abi_ops_t freepascal_abi_ops = {
	.mangle_function = fpc_mangle_function,
	.mangle_variable = fpc_mangle_variable,
	.mangle_type = fpc_mangle_type_str,
	.mangle_vtable = fpc_mangle_vtable,
	.mangle_rtti = fpc_mangle_rtti,
};
