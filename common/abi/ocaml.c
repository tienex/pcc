/*
 * Copyright (c) 2025 PCC Project
 *
 * OCaml ABI implementation
 *
 * OCaml name mangling:
 *   - Module.function -> camlModule__function_<unique_id>
 *   - caml prefix for all OCaml symbols
 *   - __ to separate module from function
 *   - Unique numeric suffix for disambiguation
 *   - Special: camlModule__entry for module initialization
 *
 * Examples:
 *   List.map -> camlList__map_1234
 *   Array.get -> camlArray__get_5678
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

static int unique_counter = 1000;

static char *
ocaml_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func || !func->name)
		return NULL;

	/* Module.function */
	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "caml%s__%s_%d",
		         func->parent_class->name,
		         func->name,
		         unique_counter++);
	} else {
		snprintf(buf, sizeof(buf), "caml%s_%d",
		         func->name,
		         unique_counter++);
	}

	return strdup(buf);
}

static char *
ocaml_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];

	if (!name)
		return NULL;

	snprintf(buf, sizeof(buf), "caml%s", name);
	return strdup(buf);
}

static char *
ocaml_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("unit");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("int");
	case ABI_TYPE_FLOAT: return strdup("float");
	case ABI_TYPE_DOUBLE: return strdup("float");
	case ABI_TYPE_BOOL: return strdup("bool");
	case ABI_TYPE_CHAR: return strdup("char");
	default: return strdup("'a");
	}
}

static char *
ocaml_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "caml%s__vtable", cls->name);
	return strdup(buf);
}

static char *
ocaml_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "caml%s__type", cls->name);
	return strdup(buf);
}

const abi_ops_t ocaml_abi_ops = {
	.mangle_function = ocaml_mangle_function,
	.mangle_variable = ocaml_mangle_variable,
	.mangle_type = ocaml_mangle_type_str,
	.mangle_vtable = ocaml_mangle_vtable,
	.mangle_rtti = ocaml_mangle_rtti,
};
