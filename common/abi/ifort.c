/*
 * Copyright (c) 2025 PCC Project
 *
 * Intel Fortran Compiler (ifort/ifx) ABI implementation
 *
 * Intel Fortran name mangling (default):
 *   - Lowercase with trailing underscore: FOO -> foo_
 *   - Can be configured with compiler options:
 *     -assume nounderscore: foo
 *     -assume underscore: foo_
 *     -names lowercase: foo_
 *     -names uppercase: FOO_
 *   - Mixed-case preservation with /names:as_is on Windows
 *
 * This implementation uses default Linux behavior.
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
ifort_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func || !func->name)
		return NULL;

	/* Intel Fortran default: lowercase with underscore */
	snprintf(buf, sizeof(buf), "%s_", to_lower(func->name));

	return strdup(buf);
}

static char *
ifort_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];

	if (!name)
		return NULL;

	snprintf(buf, sizeof(buf), "%s_", to_lower(name));
	return strdup(buf);
}

static char *
ifort_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("void");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("integer");
	case ABI_TYPE_FLOAT: return strdup("real");
	case ABI_TYPE_DOUBLE: return strdup("double");
	case ABI_TYPE_BOOL: return strdup("logical");
	default: return strdup("type");
	}
}

static char *
ifort_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls || !cls->name)
		return NULL;

	char buf[256];
	snprintf(buf, sizeof(buf), "_vtable_%s_", to_lower(cls->name));
	return strdup(buf);
}

static char *
ifort_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls || !cls->name)
		return NULL;

	char buf[256];
	snprintf(buf, sizeof(buf), "_typeinfo_%s_", to_lower(cls->name));
	return strdup(buf);
}

const abi_ops_t ifort_abi_ops = {
	.mangle_function = ifort_mangle_function,
	.mangle_variable = ifort_mangle_variable,
	.mangle_type = ifort_mangle_type_str,
	.mangle_vtable = ifort_mangle_vtable,
	.mangle_rtti = ifort_mangle_rtti,
};
