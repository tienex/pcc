/*
 * Copyright (c) 2025 PCC Project
 * Type descriptor construction and manipulation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mangle.h"

mangle_type_t *mangle_type_create(type_kind_t kind, const char *name) {
	mangle_type_t *type = (mangle_type_t *)calloc(1, sizeof(mangle_type_t));
	if (!type) {
		return NULL;
	}

	type->kind = kind;
	type->qualifiers = QUAL_NONE;
	type->name = name ? strdup(name) : NULL;
	type->base = NULL;
	type->params = NULL;
	type->param_count = 0;
	type->array_size = 0;

	return type;
}

mangle_type_t *mangle_type_pointer(mangle_type_t *base, type_qualifier_t quals) {
	mangle_type_t *type = mangle_type_create(TYPE_POINTER, NULL);
	if (type) {
		type->base = base;
		type->qualifiers = quals;
	}
	return type;
}

mangle_type_t *mangle_type_reference(mangle_type_t *base) {
	mangle_type_t *type = mangle_type_create(TYPE_REFERENCE, NULL);
	if (type) {
		type->base = base;
	}
	return type;
}

mangle_type_t *mangle_type_array(mangle_type_t *elem, size_t size) {
	mangle_type_t *type = mangle_type_create(TYPE_ARRAY, NULL);
	if (type) {
		type->base = elem;
		type->array_size = size;
	}
	return type;
}

mangle_type_t *mangle_type_function(mangle_type_t *ret,
                                     mangle_type_t **params, int count) {
	mangle_type_t *type = mangle_type_create(TYPE_FUNCTION, NULL);
	if (type) {
		type->base = ret;
		type->params = params;
		type->param_count = count;
	}
	return type;
}

void mangle_type_free(mangle_type_t *type) {
	if (!type) {
		return;
	}

	if (type->name) {
		free((void *)type->name);
	}

	if (type->base) {
		mangle_type_free(type->base);
	}

	if (type->params) {
		for (int i = 0; i < type->param_count; i++) {
			mangle_type_free(type->params[i]);
		}
		free(type->params);
	}

	free(type);
}

symbol_info_t *symbol_info_create(const char *name, const char *scope) {
	symbol_info_t *info = (symbol_info_t *)calloc(1, sizeof(symbol_info_t));
	if (!info) {
		return NULL;
	}

	info->name = name ? strdup(name) : NULL;
	info->scope = scope ? strdup(scope) : NULL;
	info->sym_type = SYM_FUNCTION;
	info->type = NULL;
	info->call_conv = CALL_CDECL;
	info->is_const = 0;
	info->is_static = 0;
	info->is_extern = 0;
	info->is_inline = 0;
	info->is_virtual = 0;
	info->is_template = 0;
	info->template_params = NULL;
	info->template_param_count = 0;

	return info;
}

void symbol_info_free(symbol_info_t *info) {
	if (!info) {
		return;
	}

	if (info->name) {
		free((void *)info->name);
	}
	if (info->scope) {
		free((void *)info->scope);
	}
	if (info->type) {
		mangle_type_free(info->type);
	}
	if (info->template_params) {
		for (int i = 0; i < info->template_param_count; i++) {
			if (info->template_params[i]) {
				free((void *)info->template_params[i]);
			}
		}
		free(info->template_params);
	}

	free(info);
}

void demangle_info_free(demangle_info_t *info) {
	if (!info) {
		return;
	}

	if (info->full_name) {
		free(info->full_name);
	}
	if (info->name) {
		free(info->name);
	}
	if (info->scope) {
		free(info->scope);
	}
	if (info->return_type) {
		free(info->return_type);
	}
	if (info->param_types) {
		for (int i = 0; i < info->param_count; i++) {
			if (info->param_types[i]) {
				free(info->param_types[i]);
			}
		}
		free(info->param_types);
	}

	free(info);
}

const char *mangle_scheme_name(mangle_scheme_t scheme) {
	switch (scheme) {
	case MANGLE_NONE:        return "none";
	case MANGLE_C:           return "C";
	case MANGLE_CXX_ITANIUM: return "C++ Itanium ABI";
	case MANGLE_CXX_MSVC:    return "MSVC C++";
	case MANGLE_PASCAL:      return "Pascal";
	case MANGLE_DELPHI:      return "Delphi";
	case MANGLE_MODULA2:     return "Modula-2";
	case MANGLE_MODULA3:     return "Modula-3";
	case MANGLE_OBERON:      return "Oberon";
	case MANGLE_ADA:         return "Ada";
	case MANGLE_PCC:         return "PCC Universal";
	default:                 return "unknown";
	}
}
