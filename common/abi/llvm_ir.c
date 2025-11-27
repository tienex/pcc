/*
 * Copyright (c) 2025 PCC Project
 *
 * LLVM IR ABI implementation
 *
 * LLVM IR naming:
 *   - @<name> for global symbols
 *   - %<name> for local symbols
 *   - Special characters escaped
 *   - Type information in function signatures
 *
 * Examples:
 *   define i32 @foo(i32 %x)
 *   @global_var = global i32 0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
llvm_escape_name(const char *name)
{
	/* LLVM allows most characters, but some need quoting */
	char buf[512];
	int i, j = 0;

	for (i = 0; name[i] && j < 500; i++) {
		char c = name[i];
		if ((c >= 'a' && c <= 'z') ||
		    (c >= 'A' && c <= 'Z') ||
		    (c >= '0' && c <= '9') ||
		    c == '_' || c == '.' || c == '-') {
			buf[j++] = c;
		} else {
			/* Escape special chars */
			buf[j++] = '_';
		}
	}
	buf[j] = '\0';

	return strdup(buf);
}

static char *
llvm_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];
	char *escaped;

	if (!func || !func->name)
		return NULL;

	escaped = llvm_escape_name(func->name);

	/* LLVM global function: @name */
	snprintf(buf, sizeof(buf), "@%s", escaped);

	free(escaped);
	return strdup(buf);
}

static char *
llvm_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	char *escaped;

	if (!name)
		return NULL;

	escaped = llvm_escape_name(name);
	snprintf(buf, sizeof(buf), "@%s", escaped);
	free(escaped);

	return strdup(buf);
}

static char *
llvm_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("void");

	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_BOOL: return strdup("i1");
	case ABI_TYPE_CHAR: return strdup("i8");
	case ABI_TYPE_SHORT: return strdup("i16");
	case ABI_TYPE_INT: return strdup("i32");
	case ABI_TYPE_LONG: return strdup("i64");
	case ABI_TYPE_FLOAT: return strdup("float");
	case ABI_TYPE_DOUBLE: return strdup("double");
	case ABI_TYPE_POINTER: return strdup("ptr");
	default: return strdup("i32");
	}
}

static char *
llvm_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	char *escaped;

	if (!cls || !cls->name)
		return NULL;

	escaped = llvm_escape_name(cls->name);
	snprintf(buf, sizeof(buf), "@_ZTV%s", escaped);
	free(escaped);

	return strdup(buf);
}

static char *
llvm_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	char *escaped;

	if (!cls || !cls->name)
		return NULL;

	escaped = llvm_escape_name(cls->name);
	snprintf(buf, sizeof(buf), "@_ZTI%s", escaped);
	free(escaped);

	return strdup(buf);
}

const abi_ops_t llvm_ir_abi_ops = {
	.mangle_function = llvm_mangle_function,
	.mangle_variable = llvm_mangle_variable,
	.mangle_type = llvm_mangle_type_str,
	.mangle_vtable = llvm_mangle_vtable,
	.mangle_rtti = llvm_mangle_rtti,
};
