/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * Type conversion functions
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* BINARY - Convert string to binary (integer) */
pli_fixed_t pli_binary(const char *s) {
	if (!s) return 0;
	return (pli_fixed_t)strtol(s, NULL, 2);  /* Base 2 */
}

/* BIT - Convert integer to bit string */
void pli_bit(char *dest, pli_fixed_t value, size_t len) {
	if (!dest) return;

	for (size_t i = 0; i < len; i++) {
		size_t bit_pos = len - i - 1;
		dest[i] = (value & (1 << bit_pos)) ? '1' : '0';
	}
	dest[len] = '\0';
}

/* CHAR - Convert integer to character string */
void pli_char(char *dest, pli_fixed_t value) {
	if (!dest) return;
	sprintf(dest, "%d", value);
}

/* DECIMAL - Convert string to decimal (integer) */
pli_fixed_t pli_decimal(const char *s) {
	if (!s) return 0;
	return (pli_fixed_t)atoi(s);
}

/* FIXED - Convert float to fixed (integer) */
pli_fixed_t pli_fixed(pli_float_long_t x) {
	return (pli_fixed_t)x;
}

/* FLOAT - Convert fixed to float */
pli_float_long_t pli_float(pli_fixed_t x) {
	return (pli_float_long_t)x;
}

/* UNSPEC - Get uninterpreted bit representation */
void pli_unspec(pli_bit_t *dest, const void *src, size_t len) {
	if (!dest || !src) return;
	memcpy(dest, src, len);
}
