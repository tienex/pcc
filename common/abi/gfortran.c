/*
 * Copyright (c) 2025 PCC Project
 *
 * GNU Fortran (gfortran) ABI implementation
 *
 * Fortran name mangling (gfortran):
 *   - Lowercase with trailing underscore: foo -> foo_
 *   - Module procedures: __modulename_MOD_procedurename
 *   - Double underscore if name contains underscore: foo_bar -> foo_bar__
 *   - Common blocks: __BLNK__ (blank common) or __name__
 *
 * Examples:
 *   SUBROUTINE FOO        -> foo_
 *   SUBROUTINE FOO_BAR    -> foo_bar__
 *   MODULE mymod
 *     SUBROUTINE mysub    -> __mymod_MOD_mysub
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

static char *to_lower(const char *s);
static int contains_underscore(const char *s);

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

static int
contains_underscore(const char *s)
{
	return strchr(s, '_') != NULL;
}

/*
 * Mangle Fortran function/subroutine
 */
static char *
gfortran_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];
	const char *name;

	if (!func || !func->name)
		return NULL;

	name = to_lower(func->name);

	/* Module procedure: __module_MOD_procedure */
	if (func->parent_class && func->parent_class->name) {
		const char *module = to_lower(func->parent_class->name);
		snprintf(buf, sizeof(buf), "__%s_MOD_%s", module, name);
	}
	/* Regular procedure with underscore in name: double underscore */
	else if (contains_underscore(func->name)) {
		snprintf(buf, sizeof(buf), "%s__", name);
	}
	/* Regular procedure: single underscore */
	else {
		snprintf(buf, sizeof(buf), "%s_", name);
	}

	return strdup(buf);
}

/*
 * Mangle Fortran variable (common block or module variable)
 */
static char *
gfortran_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	const char *lower_name;

	if (!name)
		return NULL;

	lower_name = to_lower(name);

	/* Blank common block */
	if (strcmp(lower_name, "blank") == 0) {
		return strdup("__BLNK__");
	}

	/* Named common block or variable */
	if (contains_underscore(name)) {
		snprintf(buf, sizeof(buf), "%s__", lower_name);
	} else {
		snprintf(buf, sizeof(buf), "%s_", lower_name);
	}

	return strdup(buf);
}

/*
 * Mangle type (for type-bound procedures)
 */
static char *
gfortran_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("void");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("integer");
	case ABI_TYPE_FLOAT: return strdup("real");
	case ABI_TYPE_DOUBLE: return strdup("double_precision");
	case ABI_TYPE_BOOL: return strdup("logical");
	case ABI_TYPE_CHAR: return strdup("character");
	default: return strdup("type");
	}
}

/*
 * Mangle vtable (for OOP Fortran 2003+)
 */
static char *
gfortran_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "__vtab_%s", to_lower(cls->name));
	return strdup(buf);
}

/*
 * Mangle RTTI (type descriptor for Fortran 2003+ derived types)
 */
static char *
gfortran_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "__def_init_%s", to_lower(cls->name));
	return strdup(buf);
}

const abi_ops_t gfortran_abi_ops = {
	.mangle_function = gfortran_mangle_function,
	.mangle_variable = gfortran_mangle_variable,
	.mangle_type = gfortran_mangle_type_str,
	.mangle_vtable = gfortran_mangle_vtable,
	.mangle_rtti = gfortran_mangle_rtti,
};
