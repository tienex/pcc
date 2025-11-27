/*
 * COBOL compiler utility functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

char *
xstrdup(const char *s)
{
	char *p;

	if (s == NULL)
		return NULL;

	p = malloc(strlen(s) + 1);
	if (p == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}

	strcpy(p, s);
	return p;
}

void *
xmalloc(size_t size)
{
	void *p = malloc(size);

	if (p == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}

	return p;
}
