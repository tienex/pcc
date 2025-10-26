/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Utility functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Safe strdup with error checking */
char *strdup_check(const char *s) {
	char *new;

	if (!s)
		return NULL;

	new = strdup(s);
	if (!new)
		fatal("Out of memory");

	return new;
}

/* Safe malloc with error checking */
void *malloc_check(size_t size) {
	void *ptr = malloc(size);

	if (!ptr)
		fatal("Out of memory");

	memset(ptr, 0, size);
	return ptr;
}

/* Hash function for strings */
unsigned int hash_string(const char *str) {
	unsigned int hash = 5381;
	int c;

	if (!str)
		return 0;

	while ((c = *str++))
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return hash;
}
