/*
 * Copyright (c) 2025 PCC Project
 *
 * Glasgow Haskell Compiler (GHC) ABI implementation
 *
 * GHC name mangling (Z-encoding):
 *   - Package.Module.function -> Package_Module_function
 *   - Special characters are Z-encoded:
 *     z -> zz, Z -> ZZ
 *     ( -> ZL, ) -> ZR
 *     [ -> ZM, ] -> ZN
 *     : -> ZC, & -> za, | -> zb, etc.
 *   - Closure: _closure suffix
 *   - Info table: _info suffix
 *   - Entry code: _entry suffix
 *
 * Examples:
 *   main -> Main_main_closure
 *   foo :: Int -> Int -> foo_closure
 *   (++) -> ZCzpzp_closure
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

/*
 * Z-encode special characters
 */
static void
ghc_zencode_char(char c, char *out, int *pos)
{
	switch (c) {
	case 'z': out[(*pos)++] = 'z'; out[(*pos)++] = 'z'; break;
	case 'Z': out[(*pos)++] = 'Z'; out[(*pos)++] = 'Z'; break;
	case '(': out[(*pos)++] = 'Z'; out[(*pos)++] = 'L'; break;
	case ')': out[(*pos)++] = 'Z'; out[(*pos)++] = 'R'; break;
	case '[': out[(*pos)++] = 'Z'; out[(*pos)++] = 'M'; break;
	case ']': out[(*pos)++] = 'Z'; out[(*pos)++] = 'N'; break;
	case ':': out[(*pos)++] = 'Z'; out[(*pos)++] = 'C'; break;
	case '&': out[(*pos)++] = 'z'; out[(*pos)++] = 'a'; break;
	case '|': out[(*pos)++] = 'z'; out[(*pos)++] = 'b'; break;
	case '^': out[(*pos)++] = 'z'; out[(*pos)++] = 'c'; break;
	case '$': out[(*pos)++] = 'z'; out[(*pos)++] = 'd'; break;
	case '=': out[(*pos)++] = 'z'; out[(*pos)++] = 'e'; break;
	case '>': out[(*pos)++] = 'z'; out[(*pos)++] = 'g'; break;
	case '#': out[(*pos)++] = 'z'; out[(*pos)++] = 'h'; break;
	case '<': out[(*pos)++] = 'z'; out[(*pos)++] = 'l'; break;
	case '-': out[(*pos)++] = 'z'; out[(*pos)++] = 'm'; break;
	case '!': out[(*pos)++] = 'z'; out[(*pos)++] = 'n'; break;
	case '+': out[(*pos)++] = 'z'; out[(*pos)++] = 'p'; break;
	case '\'': out[(*pos)++] = 'z'; out[(*pos)++] = 'q'; break;
	case '\\': out[(*pos)++] = 'z'; out[(*pos)++] = 'r'; break;
	case '/': out[(*pos)++] = 'z'; out[(*pos)++] = 's'; break;
	case '*': out[(*pos)++] = 'z'; out[(*pos)++] = 't'; break;
	case '_': out[(*pos)++] = 'z'; out[(*pos)++] = 'u'; break;
	case '%': out[(*pos)++] = 'z'; out[(*pos)++] = 'v'; break;
	case '.': out[(*pos)++] = 'z'; out[(*pos)++] = 'i'; break;
	default:
		if (isalnum(c))
			out[(*pos)++] = c;
		else {
			/* Unicode or other: use hex encoding */
			int n = snprintf(out + *pos, 10, "z%02x", (unsigned char)c);
			*pos += n;
		}
		break;
	}
}

static char *
ghc_zencode(const char *name)
{
	char buf[512];
	int pos = 0;
	int i;

	for (i = 0; name[i] && pos < 500; i++) {
		ghc_zencode_char(name[i], buf, &pos);
	}
	buf[pos] = '\0';

	return strdup(buf);
}

static char *
ghc_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];
	char *encoded_name;

	if (!func || !func->name)
		return NULL;

	encoded_name = ghc_zencode(func->name);

	/* Module.function -> Module_function_closure */
	if (func->parent_class && func->parent_class->name) {
		char *encoded_module = ghc_zencode(func->parent_class->name);
		snprintf(buf, sizeof(buf), "%s_%s_closure", encoded_module, encoded_name);
		free(encoded_module);
	} else {
		snprintf(buf, sizeof(buf), "%s_closure", encoded_name);
	}

	free(encoded_name);
	return strdup(buf);
}

static char *
ghc_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	char *encoded;

	if (!name)
		return NULL;

	encoded = ghc_zencode(name);
	snprintf(buf, sizeof(buf), "%s_closure", encoded);
	free(encoded);

	return strdup(buf);
}

static char *
ghc_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type)
		return strdup("void");

	switch (type->kind) {
	case ABI_TYPE_INT: return strdup("Int");
	case ABI_TYPE_FLOAT: return strdup("Float");
	case ABI_TYPE_DOUBLE: return strdup("Double");
	case ABI_TYPE_BOOL: return strdup("Bool");
	case ABI_TYPE_CHAR: return strdup("Char");
	default: return strdup("a");
	}
}

static char *
ghc_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	char *encoded;

	if (!cls || !cls->name)
		return NULL;

	encoded = ghc_zencode(cls->name);
	snprintf(buf, sizeof(buf), "%s_con_info", encoded);
	free(encoded);

	return strdup(buf);
}

static char *
ghc_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	char buf[256];
	char *encoded;

	if (!cls || !cls->name)
		return NULL;

	encoded = ghc_zencode(cls->name);
	snprintf(buf, sizeof(buf), "%s_info", encoded);
	free(encoded);

	return strdup(buf);
}

const abi_ops_t ghc_abi_ops = {
	.mangle_function = ghc_mangle_function,
	.mangle_variable = ghc_mangle_variable,
	.mangle_type = ghc_mangle_type_str,
	.mangle_vtable = ghc_mangle_vtable,
	.mangle_rtti = ghc_mangle_rtti,
};
