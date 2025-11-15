/*
 * Copyright (c) 2025 PCC Project
 * PCC Universal Name Mangling
 *
 * PCC universal mangling format:
 * _P_<lang><scope_len><scope><name_len><name>[<type_encoding>]
 *
 * Language codes:
 *   C - C language
 *   X - C++
 *   P - Pascal
 *   D - Delphi
 *   M - Modula-2
 *   N - Modula-3
 *   O - Oberon
 *   A - Ada
 *   F - Fortran
 *   B - BLISS
 *
 * This allows cross-language linking and symbol resolution
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "mangle.h"

static char get_pcc_lang_code(const char *scope) {
	if (!scope) {
		return 'C';  /* Default to C */
	}

	/* Try to infer language from scope patterns */
	if (strstr(scope, "::")) return 'X';  /* C++ */
	if (strstr(scope, "__")) {
		if (islower(scope[0])) return 'A';  /* Ada */
		return 'N';  /* Modula-3 */
	}
	if (strchr(scope, '.')) {
		if (isupper(scope[0])) return 'D';  /* Delphi */
		return 'O';  /* Oberon */
	}
	if (strchr(scope, '_')) return 'M';  /* Modula-2 */

	return 'C';  /* Default */
}

static void append_type_encoding(char *buf, size_t *pos, size_t max,
                                   const mangle_type_t *type) {
	if (!type || *pos >= max - 1) {
		return;
	}

	switch (type->kind) {
	case TYPE_VOID:      buf[(*pos)++] = 'v'; break;
	case TYPE_BOOL:      buf[(*pos)++] = 'b'; break;
	case TYPE_CHAR:      buf[(*pos)++] = 'c'; break;
	case TYPE_INT:       buf[(*pos)++] = 'i'; break;
	case TYPE_UINT:      buf[(*pos)++] = 'u'; break;
	case TYPE_LONG:      buf[(*pos)++] = 'l'; break;
	case TYPE_FLOAT:     buf[(*pos)++] = 'f'; break;
	case TYPE_DOUBLE:    buf[(*pos)++] = 'd'; break;

	case TYPE_POINTER:
		buf[(*pos)++] = 'p';
		append_type_encoding(buf, pos, max, type->base);
		break;

	case TYPE_ARRAY:
		buf[(*pos)++] = 'a';
		*pos += snprintf(buf + *pos, max - *pos, "%zu_", type->array_size);
		append_type_encoding(buf, pos, max, type->base);
		break;

	case TYPE_FUNCTION:
		buf[(*pos)++] = 'F';
		append_type_encoding(buf, pos, max, type->base);
		for (int i = 0; i < type->param_count; i++) {
			append_type_encoding(buf, pos, max, type->params[i]);
		}
		buf[(*pos)++] = 'E';
		break;

	default:
		buf[(*pos)++] = 'x';  /* Unknown */
		break;
	}

	if (*pos < max) {
		buf[*pos] = '\0';
	}
}

char *mangle_pcc(const symbol_info_t *info) {
	if (!info || !info->name) {
		return NULL;
	}

	char lang_code = get_pcc_lang_code(info->scope);

	size_t name_len = strlen(info->name);
	size_t scope_len = info->scope ? strlen(info->scope) : 0;

	/* Estimate buffer size */
	size_t buf_size = 3 + 1 + 10 + scope_len + 10 + name_len + 100;
	char *buf = (char *)malloc(buf_size);
	if (!buf) {
		return NULL;
	}

	size_t pos = 0;

	/* Prefix */
	buf[pos++] = '_';
	buf[pos++] = 'P';
	buf[pos++] = '_';
	buf[pos++] = lang_code;

	/* Scope */
	if (scope_len > 0) {
		pos += snprintf(buf + pos, buf_size - pos, "%zu%s", scope_len, info->scope);
	} else {
		buf[pos++] = '0';
	}

	/* Name */
	pos += snprintf(buf + pos, buf_size - pos, "%zu%s", name_len, info->name);

	/* Type encoding for functions */
	if (info->sym_type == SYM_FUNCTION && info->type) {
		append_type_encoding(buf, &pos, buf_size, info->type);
	}

	buf[pos] = '\0';

	return buf;
}

char *mangle_pcc_universal(const symbol_info_t *info) {
	return mangle_pcc(info);
}

/* Convert between mangling schemes */
char *mangle_convert(const char *mangled_name,
                     mangle_scheme_t from_scheme,
                     mangle_scheme_t to_scheme) {
	if (!mangled_name) {
		return NULL;
	}

	if (from_scheme == to_scheme) {
		return strdup(mangled_name);
	}

	/* For now, implement basic conversion through demangling */
	/* Full implementation would demangle and re-mangle */

	/* Simplified: just return copy for schemes we can't convert */
	return strdup(mangled_name);
}
