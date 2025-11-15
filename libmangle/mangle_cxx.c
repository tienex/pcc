/*
 * Copyright (c) 2025 PCC Project
 * C++ Itanium ABI name mangling
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mangle.h"

/* Buffer for building mangled names */
typedef struct {
	char *data;
	size_t size;
	size_t capacity;
} mangle_buffer_t;

static mangle_buffer_t *buffer_create(void) {
	mangle_buffer_t *buf = (mangle_buffer_t *)malloc(sizeof(mangle_buffer_t));
	if (!buf) {
		return NULL;
	}

	buf->capacity = 256;
	buf->data = (char *)malloc(buf->capacity);
	if (!buf->data) {
		free(buf);
		return NULL;
	}

	buf->data[0] = '\0';
	buf->size = 0;

	return buf;
}

static void buffer_append(mangle_buffer_t *buf, const char *str) {
	if (!buf || !str) {
		return;
	}

	size_t len = strlen(str);
	while (buf->size + len + 1 > buf->capacity) {
		buf->capacity *= 2;
		char *new_data = (char *)realloc(buf->data, buf->capacity);
		if (!new_data) {
			return;
		}
		buf->data = new_data;
	}

	memcpy(buf->data + buf->size, str, len);
	buf->size += len;
	buf->data[buf->size] = '\0';
}

static void buffer_append_char(mangle_buffer_t *buf, char c) {
	char str[2] = {c, '\0'};
	buffer_append(buf, str);
}

static void buffer_append_int(mangle_buffer_t *buf, int num) {
	char str[32];
	snprintf(str, sizeof(str), "%d", num);
	buffer_append(buf, str);
}

static char *buffer_finish(mangle_buffer_t *buf) {
	if (!buf) {
		return NULL;
	}
	char *result = buf->data;
	free(buf);
	return result;
}

static void mangle_type_itanium(mangle_buffer_t *buf, const mangle_type_t *type) {
	if (!type) {
		buffer_append_char(buf, 'v');  /* void */
		return;
	}

	switch (type->kind) {
	case TYPE_VOID:      buffer_append_char(buf, 'v'); break;
	case TYPE_BOOL:      buffer_append_char(buf, 'b'); break;
	case TYPE_CHAR:      buffer_append_char(buf, 'c'); break;
	case TYPE_SCHAR:     buffer_append_char(buf, 'a'); break;
	case TYPE_UCHAR:     buffer_append_char(buf, 'h'); break;
	case TYPE_SHORT:     buffer_append_char(buf, 's'); break;
	case TYPE_USHORT:    buffer_append_char(buf, 't'); break;
	case TYPE_INT:       buffer_append_char(buf, 'i'); break;
	case TYPE_UINT:      buffer_append_char(buf, 'j'); break;
	case TYPE_LONG:      buffer_append_char(buf, 'l'); break;
	case TYPE_ULONG:     buffer_append_char(buf, 'm'); break;
	case TYPE_LONGLONG:  buffer_append_char(buf, 'x'); break;
	case TYPE_ULONGLONG: buffer_append_char(buf, 'y'); break;
	case TYPE_FLOAT:     buffer_append_char(buf, 'f'); break;
	case TYPE_DOUBLE:    buffer_append_char(buf, 'd'); break;
	case TYPE_LONGDOUBLE: buffer_append_char(buf, 'e'); break;

	case TYPE_POINTER:
		buffer_append_char(buf, 'P');
		if (type->qualifiers & QUAL_CONST) buffer_append_char(buf, 'K');
		if (type->qualifiers & QUAL_VOLATILE) buffer_append_char(buf, 'V');
		if (type->qualifiers & QUAL_RESTRICT) buffer_append_char(buf, 'r');
		mangle_type_itanium(buf, type->base);
		break;

	case TYPE_REFERENCE:
		buffer_append_char(buf, 'R');
		mangle_type_itanium(buf, type->base);
		break;

	case TYPE_RVALUE_REF:
		buffer_append_char(buf, 'O');
		mangle_type_itanium(buf, type->base);
		break;

	case TYPE_ARRAY:
		buffer_append_char(buf, 'A');
		buffer_append_int(buf, type->array_size);
		buffer_append_char(buf, '_');
		mangle_type_itanium(buf, type->base);
		break;

	case TYPE_FUNCTION:
		buffer_append_char(buf, 'F');
		mangle_type_itanium(buf, type->base);  /* Return type */
		for (int i = 0; i < type->param_count; i++) {
			mangle_type_itanium(buf, type->params[i]);
		}
		buffer_append_char(buf, 'E');
		break;

	case TYPE_CLASS:
	case TYPE_STRUCT:
		if (type->name) {
			buffer_append_int(buf, strlen(type->name));
			buffer_append(buf, type->name);
		}
		break;

	default:
		buffer_append_char(buf, 'v');  /* Unknown - treat as void */
		break;
	}
}

char *mangle_cxx_itanium(const symbol_info_t *info) {
	mangle_buffer_t *buf = buffer_create();
	if (!buf) {
		return NULL;
	}

	/* Start with _Z prefix */
	buffer_append(buf, "_Z");

	/* Handle namespaces/scope */
	if (info->scope) {
		buffer_append_char(buf, 'N');  /* Nested name */

		/* Split scope by :: */
		char *scope_copy = strdup(info->scope);
		char *token = strtok(scope_copy, ":");
		while (token) {
			if (*token) {  /* Skip empty tokens */
				buffer_append_int(buf, strlen(token));
				buffer_append(buf, token);
			}
			token = strtok(NULL, ":");
		}
		free(scope_copy);

		/* Add the function name */
		buffer_append_int(buf, strlen(info->name));
		buffer_append(buf, info->name);

		buffer_append_char(buf, 'E');  /* End nested name */
	} else {
		/* Simple name */
		buffer_append_int(buf, strlen(info->name));
		buffer_append(buf, info->name);
	}

	/* Add parameter types for functions */
	if (info->sym_type == SYM_FUNCTION && info->type &&
	    info->type->kind == TYPE_FUNCTION) {
		if (info->type->param_count == 0) {
			buffer_append_char(buf, 'v');  /* void parameters */
		} else {
			for (int i = 0; i < info->type->param_count; i++) {
				mangle_type_itanium(buf, info->type->params[i]);
			}
		}
	}

	return buffer_finish(buf);
}

char *mangle_cxx_msvc(const symbol_info_t *info) {
	/* MSVC C++ mangling is more complex - simplified implementation */
	mangle_buffer_t *buf = buffer_create();
	if (!buf) {
		return NULL;
	}

	/* Start with ? prefix */
	buffer_append_char(buf, '?');

	/* Add the name */
	buffer_append(buf, info->name);

	/* Add scope marker */
	buffer_append_char(buf, '@');

	/* Add scope if present */
	if (info->scope) {
		buffer_append(buf, info->scope);
		buffer_append_char(buf, '@');
	}

	/* Add @@ terminator for scope */
	buffer_append_char(buf, '@');

	/* Add calling convention and other modifiers */
	/* This is a simplified version */
	if (info->sym_type == SYM_FUNCTION) {
		buffer_append(buf, "YA");  /* Default calling convention */
	} else {
		buffer_append_char(buf, '3');  /* Variable marker */
	}

	return buffer_finish(buf);
}
