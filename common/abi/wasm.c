/*
 * Copyright (c) 2025 PCC Project
 *
 * WebAssembly ABI implementation
 *
 * WebAssembly naming:
 *   - Names are UTF-8 strings
 *   - Export names use original function names
 *   - Import format: $env.<name>
 *   - Internal names use $ prefix
 *
 * Examples:
 *   (func $foo (param i32) (result i32))
 *   (export "foo" (func $foo))
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
wasm_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func || !func->name)
		return NULL;

	/* WebAssembly uses simple names with $ prefix for internal */
	snprintf(buf, sizeof(buf), "$%s", func->name);

	return strdup(buf);
}

static char *
wasm_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];

	if (!name)
		return NULL;

	snprintf(buf, sizeof(buf), "$%s", name);
	return strdup(buf);
}

static char *
wasm_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("void");

	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_INT: return strdup("i32");
	case ABI_TYPE_LONG: return strdup("i64");
	case ABI_TYPE_FLOAT: return strdup("f32");
	case ABI_TYPE_DOUBLE: return strdup("f64");
	default: return strdup("i32");
	}
}

static char *
wasm_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "$vtable_%s", cls->name);
	return strdup(buf);
}

static char *
wasm_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];

	if (!cls || !cls->name)
		return NULL;

	snprintf(buf, sizeof(buf), "$typeinfo_%s", cls->name);
	return strdup(buf);
}

const abi_ops_t wasm_abi_ops = {
	.mangle_function = wasm_mangle_function,
	.mangle_variable = wasm_mangle_variable,
	.mangle_type = wasm_mangle_type_str,
	.mangle_vtable = wasm_mangle_vtable,
	.mangle_rtti = wasm_mangle_rtti,
};
