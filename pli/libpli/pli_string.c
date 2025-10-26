/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * String manipulation functions
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* INDEX - Find substring position (1-based, 0 if not found) */
pli_fixed_t pli_index(const char *haystack, const char *needle) {
	if (!haystack || !needle) return 0;

	const char *p = strstr(haystack, needle);
	if (p) {
		return (pli_fixed_t)(p - haystack + 1);  /* 1-based indexing */
	}
	return 0;
}

/* LENGTH - Get string length */
pli_fixed_t pli_length(const char *s) {
	if (!s) return 0;
	return (pli_fixed_t)strlen(s);
}

/* SUBSTR - Extract substring */
void pli_substr(char *dest, const char *src, pli_fixed_t start, pli_fixed_t len) {
	if (!dest || !src) return;

	size_t src_len = strlen(src);

	/* PL/I uses 1-based indexing */
	if (start < 1 || start > src_len) {
		dest[0] = '\0';
		return;
	}

	/* Convert to 0-based */
	size_t start_idx = start - 1;
	size_t copy_len = (len < 0) ? (src_len - start_idx) : (size_t)len;

	if (start_idx + copy_len > src_len) {
		copy_len = src_len - start_idx;
	}

	strncpy(dest, src + start_idx, copy_len);
	dest[copy_len] = '\0';
}

/* REPEAT - Repeat string n times */
void pli_repeat(char *dest, const char *s, pli_fixed_t count) {
	if (!dest || !s || count <= 0) {
		if (dest) dest[0] = '\0';
		return;
	}

	size_t len = strlen(s);
	char *p = dest;

	for (pli_fixed_t i = 0; i < count; i++) {
		strcpy(p, s);
		p += len;
	}
}

/* TRIM - Remove trailing spaces */
void pli_trim(char *dest, const char *src) {
	if (!dest || !src) return;

	size_t len = strlen(src);

	/* Find last non-space character */
	while (len > 0 && isspace((unsigned char)src[len-1])) {
		len--;
	}

	strncpy(dest, src, len);
	dest[len] = '\0';
}

/* VERIFY - Find first character not in set (1-based, 0 if all match) */
pli_fixed_t pli_verify(const char *s, const char *set) {
	if (!s || !set) return 0;

	size_t pos = strspn(s, set);
	if (s[pos] == '\0') {
		return 0;  /* All characters match */
	}
	return (pli_fixed_t)(pos + 1);  /* 1-based */
}

/* Concatenation */
void pli_concat(char *dest, const char *s1, const char *s2) {
	if (!dest) return;

	if (s1) {
		strcpy(dest, s1);
		if (s2) {
			strcat(dest, s2);
		}
	} else if (s2) {
		strcpy(dest, s2);
	} else {
		dest[0] = '\0';
	}
}

/* VARYING string management */
pli_string_t *pli_string_create(size_t max_len) {
	pli_string_t *s = malloc(sizeof(pli_string_t));
	if (!s) return NULL;

	s->data = malloc(max_len + 1);
	if (!s->data) {
		free(s);
		return NULL;
	}

	s->length = 0;
	s->max_length = max_len;
	s->data[0] = '\0';

	return s;
}

void pli_string_destroy(pli_string_t *s) {
	if (!s) return;
	if (s->data) {
		free(s->data);
	}
	free(s);
}

void pli_string_assign(pli_string_t *dest, const char *src) {
	if (!dest || !src) return;

	size_t len = strlen(src);
	if (len > dest->max_length) {
		len = dest->max_length;
	}

	strncpy(dest->data, src, len);
	dest->data[len] = '\0';
	dest->length = len;
}

void pli_string_concat(pli_string_t *dest, const char *s1, const char *s2) {
	if (!dest) return;

	size_t len1 = s1 ? strlen(s1) : 0;
	size_t len2 = s2 ? strlen(s2) : 0;
	size_t total = len1 + len2;

	if (total > dest->max_length) {
		total = dest->max_length;
	}

	if (s1 && len1 > 0) {
		size_t copy1 = (len1 > dest->max_length) ? dest->max_length : len1;
		strncpy(dest->data, s1, copy1);
		dest->data[copy1] = '\0';

		if (s2 && total > len1) {
			size_t copy2 = total - len1;
			strncat(dest->data, s2, copy2);
		}
	} else if (s2) {
		strncpy(dest->data, s2, total);
		dest->data[total] = '\0';
	} else {
		dest->data[0] = '\0';
	}

	dest->length = strlen(dest->data);
}

/* Bit string functions */
pli_bit_t pli_bool(pli_bit_t a, pli_bit_t b, pli_bit_t op) {
	/* op: 0=AND, 1=OR, 2=XOR */
	switch (op) {
	case 0: return a & b;
	case 1: return a | b;
	case 2: return a ^ b;
	default: return 0;
	}
}

pli_bit_t pli_high(size_t n) {
	/* Return bit string of n '1' bits */
	if (n >= 8 * sizeof(pli_bit_t)) {
		return (pli_bit_t)-1;
	}
	return (pli_bit_t)((1u << n) - 1);
}

pli_bit_t pli_low(size_t n) {
	/* Return bit string of n '0' bits */
	return 0;
}
