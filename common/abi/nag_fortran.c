/*
 * Copyright (c) 2025 PCC Project
 *
 * NAG Fortran Compiler ABI implementation
 *
 * NAG Fortran name mangling:
 *   - Uppercase by default: foo -> FOO
 *   - Can be configured with -mismatch_all for C interop
 *   - Module procedures: module_name_MP_procedure_name
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
nag_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func || !func->name)
		return NULL;

	/* Module procedure */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "%s_MP_%s",
		         to_upper(func->parent_class->name),
		         to_upper(func->name));
	} else {
		/* Regular procedure: uppercase */
		snprintf(buf, sizeof(buf), "%s", to_upper(func->name));
	}

	return strdup(buf);
}

static char *
nag_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	if (!name)
		return NULL;

	return strdup(to_upper(name));
}

static char *
nag_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("VOID");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("INTEGER");
	case ABI_TYPE_FLOAT: return strdup("REAL");
	case ABI_TYPE_DOUBLE: return strdup("DOUBLE");
	case ABI_TYPE_BOOL: return strdup("LOGICAL");
	default: return strdup("TYPE");
	}
}

static char *
nag_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls || !cls->name)
		return NULL;

	char buf[256];
	snprintf(buf, sizeof(buf), "VTABLE_%s", to_upper(cls->name));
	return strdup(buf);
}

static char *
nag_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls || !cls->name)
		return NULL;

	char buf[256];
	snprintf(buf, sizeof(buf), "TYPEINFO_%s", to_upper(cls->name));
	return strdup(buf);
}

const abi_ops_t nag_fortran_abi_ops = {
	.mangle_function = nag_mangle_function,
	.mangle_variable = nag_mangle_variable,
	.mangle_type = nag_mangle_type_str,
	.mangle_vtable = nag_mangle_vtable,
	.mangle_rtti = nag_mangle_rtti,
};
