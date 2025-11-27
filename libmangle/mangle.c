/*
 * Copyright (c) 2025 PCC Project
 * Main symbol mangling implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "mangle.h"

/* Forward declarations for scheme-specific manglers */
char *mangle_c(const symbol_info_t *info);
char *mangle_cxx_itanium(const symbol_info_t *info);
char *mangle_cxx_msvc(const symbol_info_t *info);
char *mangle_pascal(const symbol_info_t *info);
char *mangle_delphi(const symbol_info_t *info);
char *mangle_modula2(const symbol_info_t *info);
char *mangle_modula3(const symbol_info_t *info);
char *mangle_oberon(const symbol_info_t *info);
char *mangle_ada(const symbol_info_t *info);
char *mangle_pcc(const symbol_info_t *info);

char *mangle_symbol(const symbol_info_t *info, mangle_scheme_t scheme) {
	if (!info || !info->name) {
		return NULL;
	}

	switch (scheme) {
	case MANGLE_NONE:
	case MANGLE_C:
		return mangle_c(info);
	case MANGLE_CXX_ITANIUM:
		return mangle_cxx_itanium(info);
	case MANGLE_CXX_MSVC:
		return mangle_cxx_msvc(info);
	case MANGLE_PASCAL:
		return mangle_pascal(info);
	case MANGLE_DELPHI:
		return mangle_delphi(info);
	case MANGLE_MODULA2:
		return mangle_modula2(info);
	case MANGLE_MODULA3:
		return mangle_modula3(info);
	case MANGLE_OBERON:
		return mangle_oberon(info);
	case MANGLE_ADA:
		return mangle_ada(info);
	case MANGLE_PCC:
		return mangle_pcc(info);
	default:
		return strdup(info->name);
	}
}

/* C mangling - no mangling, just return the name */
char *mangle_c(const symbol_info_t *info) {
	return strdup(info->name);
}

/* Pascal mangling - uppercase with optional length prefix */
char *mangle_pascal(const symbol_info_t *info) {
	const char *name = info->name;
	size_t len = strlen(name);
	char *result = (char *)malloc(len + 1);

	if (!result) {
		return NULL;
	}

	/* Convert to uppercase */
	for (size_t i = 0; i < len; i++) {
		result[i] = toupper((unsigned char)name[i]);
	}
	result[len] = '\0';

	return result;
}

/* Delphi mangling - Unit.Name format */
char *mangle_delphi(const symbol_info_t *info) {
	if (!info->scope) {
		/* No unit, just return the name */
		return strdup(info->name);
	}

	size_t scope_len = strlen(info->scope);
	size_t name_len = strlen(info->name);
	size_t total_len = scope_len + 1 + name_len + 1;

	char *result = (char *)malloc(total_len);
	if (!result) {
		return NULL;
	}

	snprintf(result, total_len, "%s.%s", info->scope, info->name);
	return result;
}

/* Modula-2 mangling - module_name format */
char *mangle_modula2(const symbol_info_t *info) {
	if (!info->scope) {
		return strdup(info->name);
	}

	size_t scope_len = strlen(info->scope);
	size_t name_len = strlen(info->name);
	size_t total_len = scope_len + 1 + name_len + 1;

	char *result = (char *)malloc(total_len);
	if (!result) {
		return NULL;
	}

	snprintf(result, total_len, "%s_%s", info->scope, info->name);
	return result;
}

/* Modula-3 mangling - module__name format (double underscore) */
char *mangle_modula3(const symbol_info_t *info) {
	if (!info->scope) {
		return strdup(info->name);
	}

	size_t scope_len = strlen(info->scope);
	size_t name_len = strlen(info->name);
	size_t total_len = scope_len + 2 + name_len + 1;

	char *result = (char *)malloc(total_len);
	if (!result) {
		return NULL;
	}

	snprintf(result, total_len, "%s__%s", info->scope, info->name);
	return result;
}

/* Oberon mangling - Module.Name format (like Delphi but case-sensitive) */
char *mangle_oberon(const symbol_info_t *info) {
	if (!info->scope) {
		return strdup(info->name);
	}

	size_t scope_len = strlen(info->scope);
	size_t name_len = strlen(info->name);
	size_t total_len = scope_len + 1 + name_len + 1;

	char *result = (char *)malloc(total_len);
	if (!result) {
		return NULL;
	}

	snprintf(result, total_len, "%s.%s", info->scope, info->name);
	return result;
}

/* Ada mangling - package__subpackage__name format */
char *mangle_ada(const symbol_info_t *info) {
	if (!info->scope) {
		return strdup(info->name);
	}

	/* Ada uses lowercase */
	size_t scope_len = strlen(info->scope);
	size_t name_len = strlen(info->name);
	size_t total_len = scope_len + 2 + name_len + 1;

	char *result = (char *)malloc(total_len);
	if (!result) {
		return NULL;
	}

	snprintf(result, total_len, "%s__%s", info->scope, info->name);

	/* Convert to lowercase */
	for (size_t i = 0; result[i]; i++) {
		result[i] = tolower((unsigned char)result[i]);
	}

	return result;
}

mangle_scheme_t detect_mangle_scheme(const char *symbol) {
	if (!symbol || !*symbol) {
		return MANGLE_NONE;
	}

	/* Check for C++ Itanium mangling */
	if (symbol[0] == '_' && symbol[1] == 'Z') {
		return MANGLE_CXX_ITANIUM;
	}

	/* Check for MSVC C++ mangling */
	if (symbol[0] == '?') {
		return MANGLE_CXX_MSVC;
	}

	/* Check for PCC mangling */
	if (strncmp(symbol, "_P_", 3) == 0) {
		return MANGLE_PCC;
	}

	/* Check for Modula-3 (double underscore) */
	if (strstr(symbol, "__")) {
		return MANGLE_MODULA3;
	}

	/* Check for module separator patterns */
	if (strchr(symbol, '.')) {
		/* Could be Delphi or Oberon */
		/* If all uppercase, likely Delphi */
		int all_upper = 1;
		for (const char *p = symbol; *p; p++) {
			if (isalpha(*p) && !isupper(*p)) {
				all_upper = 0;
				break;
			}
		}
		return all_upper ? MANGLE_DELPHI : MANGLE_OBERON;
	}

	/* Check for single underscore (Modula-2) */
	if (strchr(symbol, '_')) {
		return MANGLE_MODULA2;
	}

	/* Check if all uppercase (Pascal) */
	int all_upper = 1;
	for (const char *p = symbol; *p; p++) {
		if (isalpha(*p) && !isupper(*p)) {
			all_upper = 0;
			break;
		}
	}
	if (all_upper && isalpha(symbol[0])) {
		return MANGLE_PASCAL;
	}

	/* Default to C */
	return MANGLE_C;
}

int is_mangled(const char *symbol) {
	mangle_scheme_t scheme = detect_mangle_scheme(symbol);
	return scheme != MANGLE_C && scheme != MANGLE_NONE;
}

char *mangle_function(const char *name, const char *scope,
                      mangle_type_t *return_type,
                      mangle_type_t **param_types, int param_count,
                      mangle_scheme_t scheme) {
	symbol_info_t *info = symbol_info_create(name, scope);
	if (!info) {
		return NULL;
	}

	info->sym_type = SYM_FUNCTION;
	info->type = mangle_type_function(return_type, param_types, param_count);

	char *result = mangle_symbol(info, scheme);

	/* Don't free type components as they're owned by caller */
	info->type->base = NULL;
	info->type->params = NULL;
	symbol_info_free(info);

	return result;
}

char *mangle_variable(const char *name, const char *scope,
                      mangle_type_t *type, mangle_scheme_t scheme) {
	symbol_info_t *info = symbol_info_create(name, scope);
	if (!info) {
		return NULL;
	}

	info->sym_type = SYM_VARIABLE;
	info->type = type;

	char *result = mangle_symbol(info, scheme);

	/* Don't free type as it's owned by caller */
	info->type = NULL;
	symbol_info_free(info);

	return result;
}

char *mangle_type_name(const char *name, const char *scope,
                       mangle_scheme_t scheme) {
	symbol_info_t *info = symbol_info_create(name, scope);
	if (!info) {
		return NULL;
	}

	info->sym_type = SYM_TYPE;

	char *result = mangle_symbol(info, scheme);
	symbol_info_free(info);

	return result;
}

size_t mangle_estimate_length(const symbol_info_t *info, mangle_scheme_t scheme) {
	if (!info || !info->name) {
		return 0;
	}

	size_t base_len = strlen(info->name);
	size_t scope_len = info->scope ? strlen(info->scope) : 0;

	switch (scheme) {
	case MANGLE_NONE:
	case MANGLE_C:
	case MANGLE_PASCAL:
		return base_len + 1;

	case MANGLE_DELPHI:
	case MANGLE_OBERON:
		return scope_len + 1 + base_len + 1;

	case MANGLE_MODULA2:
		return scope_len + 1 + base_len + 1;

	case MANGLE_MODULA3:
	case MANGLE_ADA:
		return scope_len + 2 + base_len + 1;

	case MANGLE_CXX_ITANIUM:
		/* Rough estimate for C++ mangling */
		return 2 + scope_len + base_len + (info->type ? 20 : 0) + 1;

	case MANGLE_PCC:
		/* PCC universal mangling estimate */
		return 3 + scope_len + base_len + 10;

	default:
		return base_len * 2;
	}
}

int mangle_validate(const char *mangled_name, mangle_scheme_t scheme) {
	if (!mangled_name || !*mangled_name) {
		return 0;
	}

	switch (scheme) {
	case MANGLE_CXX_ITANIUM:
		return (mangled_name[0] == '_' && mangled_name[1] == 'Z');

	case MANGLE_CXX_MSVC:
		return (mangled_name[0] == '?');

	case MANGLE_PCC:
		return (strncmp(mangled_name, "_P_", 3) == 0);

	default:
		/* For simpler schemes, just check it's a valid identifier */
		return isalpha(mangled_name[0]) || mangled_name[0] == '_';
	}
}
