/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Unified String Operations
 * - Pascal short/long strings
 * - Modula-2/Oberon ARRAY OF CHAR
 * - Modula-3 TEXT (reference counted)
 * - Ada String (constrained arrays)
 * - Wide/Unicode strings
 * - Cross-language string conversions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include "wirthrt.h"

/*
 * Pascal Short String Operations (255 max)
 */

void wirth_str_assign(WirthShortString *dest, const char *src) {
	if (!dest || !src) {
		return;
	}

	size_t len = strlen(src);
	if (len > 255) {
		len = 255;
	}

	dest->length = (uint8_t)len;
	memcpy(dest->data, src, len);
	dest->data[len] = '\0';  /* Null terminate for C compatibility */
}

void wirth_str_concat(WirthShortString *dest, const WirthShortString *s1, const WirthShortString *s2) {
	if (!dest || !s1 || !s2) {
		return;
	}

	size_t total_len = s1->length + s2->length;
	if (total_len > 255) {
		total_len = 255;
	}

	size_t len1 = s1->length;
	size_t len2 = total_len - len1;

	memcpy(dest->data, s1->data, len1);
	memcpy(dest->data + len1, s2->data, len2);
	dest->length = (uint8_t)total_len;
	dest->data[total_len] = '\0';
}

int wirth_str_compare(const WirthShortString *s1, const WirthShortString *s2) {
	if (!s1 || !s2) {
		return 0;
	}

	size_t min_len = (s1->length < s2->length) ? s1->length : s2->length;
	int cmp = memcmp(s1->data, s2->data, min_len);

	if (cmp != 0) {
		return cmp;
	}

	/* If prefixes are equal, longer string is greater */
	return s1->length - s2->length;
}

int wirth_str_length(const WirthShortString *s) {
	return s ? s->length : 0;
}

/*
 * Pascal Long String Operations (dynamic)
 */

WirthLongString *wirth_lstr_create(const char *src) {
	WirthLongString *s = (WirthLongString *)malloc(sizeof(WirthLongString));
	if (!s) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate long string");
		return NULL;
	}

	size_t len = src ? strlen(src) : 0;
	s->length = len;
	s->capacity = len + 16;  /* Add some extra space */
	s->data = (char *)malloc(s->capacity);

	if (!s->data) {
		free(s);
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate string data");
		return NULL;
	}

	if (src && len > 0) {
		memcpy(s->data, src, len);
	}
	s->data[len] = '\0';

	return s;
}

void wirth_lstr_free(WirthLongString *s) {
	if (s) {
		if (s->data) {
			free(s->data);
		}
		free(s);
	}
}

void wirth_lstr_assign(WirthLongString *dest, const char *src) {
	if (!dest) {
		return;
	}

	size_t len = src ? strlen(src) : 0;

	/* Reallocate if needed */
	if (len >= dest->capacity) {
		char *new_data = (char *)realloc(dest->data, len + 16);
		if (!new_data) {
			wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot reallocate string");
			return;
		}
		dest->data = new_data;
		dest->capacity = len + 16;
	}

	dest->length = len;
	if (src && len > 0) {
		memcpy(dest->data, src, len);
	}
	dest->data[len] = '\0';
}

/*
 * Modula-2/Oberon ARRAY OF CHAR Operations
 */

void wirth_array_assign(char *dest, const char *src, size_t maxlen) {
	if (!dest || !src || maxlen == 0) {
		return;
	}

	size_t len = strlen(src);
	if (len >= maxlen) {
		len = maxlen - 1;
	}

	memcpy(dest, src, len);
	dest[len] = '\0';
}

int wirth_array_compare(const char *s1, const char *s2) {
	if (!s1 || !s2) {
		return 0;
	}
	return strcmp(s1, s2);
}

void wirth_array_concat(char *dest, const char *s1, const char *s2, size_t maxlen) {
	if (!dest || !s1 || !s2 || maxlen == 0) {
		return;
	}

	size_t len1 = strlen(s1);
	size_t len2 = strlen(s2);
	size_t total_len = len1 + len2;

	if (total_len >= maxlen) {
		total_len = maxlen - 1;
		if (len1 >= maxlen - 1) {
			len1 = maxlen - 1;
			len2 = 0;
		} else {
			len2 = total_len - len1;
		}
	}

	memcpy(dest, s1, len1);
	memcpy(dest + len1, s2, len2);
	dest[total_len] = '\0';
}

size_t wirth_array_length(const char *s) {
	return s ? strlen(s) : 0;
}

/*
 * Modula-3 TEXT Operations (reference counted)
 */

WirthText *wirth_text_create(const char *src) {
	WirthText *t = (WirthText *)malloc(sizeof(WirthText));
	if (!t) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate TEXT");
		return NULL;
	}

	size_t len = src ? strlen(src) : 0;
	t->length = len;
	t->refcount = 1;
	t->data = (char *)malloc(len + 1);

	if (!t->data) {
		free(t);
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate TEXT data");
		return NULL;
	}

	if (src && len > 0) {
		memcpy(t->data, src, len);
	}
	t->data[len] = '\0';

	return t;
}

void wirth_text_free(WirthText *t) {
	if (t && --t->refcount == 0) {
		if (t->data) {
			free(t->data);
		}
		free(t);
	}
}

WirthText *wirth_text_concat(const WirthText *t1, const WirthText *t2) {
	if (!t1 || !t2) {
		return NULL;
	}

	size_t total_len = t1->length + t2->length;
	WirthText *result = (WirthText *)malloc(sizeof(WirthText));
	if (!result) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate TEXT");
		return NULL;
	}

	result->length = total_len;
	result->refcount = 1;
	result->data = (char *)malloc(total_len + 1);

	if (!result->data) {
		free(result);
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate TEXT data");
		return NULL;
	}

	memcpy(result->data, t1->data, t1->length);
	memcpy(result->data + t1->length, t2->data, t2->length);
	result->data[total_len] = '\0';

	return result;
}

int wirth_text_compare(const WirthText *t1, const WirthText *t2) {
	if (!t1 || !t2) {
		return 0;
	}

	size_t min_len = (t1->length < t2->length) ? t1->length : t2->length;
	int cmp = memcmp(t1->data, t2->data, min_len);

	if (cmp != 0) {
		return cmp;
	}

	return (int)(t1->length - t2->length);
}

WirthText *wirth_text_addref(WirthText *t) {
	if (t) {
		t->refcount++;
	}
	return t;
}

void wirth_text_release(WirthText *t) {
	wirth_text_free(t);
}

/*
 * Ada String Operations (constrained arrays)
 */

void wirth_ada_str_assign(WirthAdaString *dest, const char *src, size_t first, size_t last) {
	if (!dest || !src) {
		return;
	}

	dest->first = first;
	dest->last = last;
	size_t len = last - first + 1;

	dest->data = (char *)malloc(len + 1);
	if (!dest->data) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate Ada string");
		return;
	}

	size_t src_len = strlen(src);
	if (src_len > len) {
		src_len = len;
	}

	memcpy(dest->data, src, src_len);
	/* Ada strings are space-padded if source is shorter */
	for (size_t i = src_len; i < len; i++) {
		dest->data[i] = ' ';
	}
	dest->data[len] = '\0';
}

int wirth_ada_str_compare(const WirthAdaString *s1, const WirthAdaString *s2) {
	if (!s1 || !s2) {
		return 0;
	}

	size_t len1 = s1->last - s1->first + 1;
	size_t len2 = s2->last - s2->first + 1;
	size_t min_len = (len1 < len2) ? len1 : len2;

	int cmp = memcmp(s1->data, s2->data, min_len);
	if (cmp != 0) {
		return cmp;
	}

	return (int)(len1 - len2);
}

WirthAdaString *wirth_ada_str_slice(const WirthAdaString *s, size_t first, size_t last) {
	if (!s || first < s->first || last > s->last || first > last) {
		wirth_runtime_error(WIRTH_ERR_CONSTRAINT_ERROR, "Ada string slice out of bounds");
		return NULL;
	}

	WirthAdaString *result = (WirthAdaString *)malloc(sizeof(WirthAdaString));
	if (!result) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate Ada string");
		return NULL;
	}

	result->first = first;
	result->last = last;
	size_t len = last - first + 1;
	size_t offset = first - s->first;

	result->data = (char *)malloc(len + 1);
	if (!result->data) {
		free(result);
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate Ada string data");
		return NULL;
	}

	memcpy(result->data, s->data + offset, len);
	result->data[len] = '\0';

	return result;
}

/*
 * Wide/Unicode String Operations
 */

WirthWideString *wirth_wstr_create(const uint16_t *src, size_t len) {
	WirthWideString *s = (WirthWideString *)malloc(sizeof(WirthWideString));
	if (!s) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate wide string");
		return NULL;
	}

	s->length = len;
	s->capacity = len + 16;
	s->data = (uint16_t *)malloc(s->capacity * sizeof(uint16_t));

	if (!s->data) {
		free(s);
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate wide string data");
		return NULL;
	}

	if (src && len > 0) {
		memcpy(s->data, src, len * sizeof(uint16_t));
	}
	s->data[len] = 0;  /* Null terminate */

	return s;
}

void wirth_wstr_free(WirthWideString *s) {
	if (s) {
		if (s->data) {
			free(s->data);
		}
		free(s);
	}
}

/*
 * String Conversion Functions (for interoperability)
 */

void wirth_convert_pascal_to_modula(WirthShortString *pas, char *mod, size_t maxlen) {
	if (!pas || !mod || maxlen == 0) {
		return;
	}

	size_t len = pas->length;
	if (len >= maxlen) {
		len = maxlen - 1;
	}

	memcpy(mod, pas->data, len);
	mod[len] = '\0';
}

void wirth_convert_modula_to_pascal(const char *mod, WirthShortString *pas) {
	if (!mod || !pas) {
		return;
	}

	size_t len = strlen(mod);
	if (len > 255) {
		len = 255;
	}

	pas->length = (uint8_t)len;
	memcpy(pas->data, mod, len);
	pas->data[len] = '\0';
}

WirthText *wirth_convert_pascal_to_text(const WirthShortString *pas) {
	if (!pas) {
		return NULL;
	}

	WirthText *t = (WirthText *)malloc(sizeof(WirthText));
	if (!t) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate TEXT");
		return NULL;
	}

	t->length = pas->length;
	t->refcount = 1;
	t->data = (char *)malloc(t->length + 1);

	if (!t->data) {
		free(t);
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate TEXT data");
		return NULL;
	}

	memcpy(t->data, pas->data, pas->length);
	t->data[pas->length] = '\0';

	return t;
}

void wirth_convert_text_to_pascal(const WirthText *text, WirthShortString *pas) {
	if (!text || !pas) {
		return;
	}

	size_t len = text->length;
	if (len > 255) {
		len = 255;
	}

	pas->length = (uint8_t)len;
	memcpy(pas->data, text->data, len);
	pas->data[len] = '\0';
}

void wirth_convert_ada_to_modula(const WirthAdaString *ada, char *mod, size_t maxlen) {
	if (!ada || !mod || maxlen == 0) {
		return;
	}

	size_t len = ada->last - ada->first + 1;
	if (len >= maxlen) {
		len = maxlen - 1;
	}

	memcpy(mod, ada->data, len);
	mod[len] = '\0';

	/* Trim trailing spaces (Ada convention) */
	while (len > 0 && mod[len - 1] == ' ') {
		mod[--len] = '\0';
	}
}

void wirth_convert_modula_to_ada(const char *mod, WirthAdaString *ada, size_t first, size_t last) {
	wirth_ada_str_assign(ada, mod, first, last);
}
