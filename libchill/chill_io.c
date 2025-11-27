/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL I/O runtime functions
 */

#include "chill.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Read functions
 */

void
chill_read_int(chill_int_t *value)
{
	if (scanf("%d", value) != 1) {
		chill_raise(CHILL_EXC_IOERROR, "Failed to read integer");
	}
}

void
chill_read_real(chill_real_t *value)
{
	if (scanf("%f", value) != 1) {
		chill_raise(CHILL_EXC_IOERROR, "Failed to read real");
	}
}

void
chill_read_char(chill_char_t *value)
{
	int c = getchar();
	if (c == EOF) {
		chill_raise(CHILL_EXC_IOERROR, "Failed to read character");
	}
	*value = (chill_char_t)c;
}

void
chill_read_chars(chill_chars_t *str)
{
	char buffer[1024];
	if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
		chill_raise(CHILL_EXC_IOERROR, "Failed to read string");
	}

	/* Remove trailing newline if present */
	size_t len = strlen(buffer);
	if (len > 0 && buffer[len - 1] == '\n') {
		buffer[len - 1] = '\0';
		len--;
	}

	/* Allocate or reallocate string storage */
	if (str->capacity < len + 1) {
		str->data = (char *)realloc(str->data, len + 1);
		if (str->data == NULL) {
			chill_raise(CHILL_EXC_OUTOFMEM, "Out of memory reading string");
		}
		str->capacity = len + 1;
	}

	strcpy(str->data, buffer);
	str->length = len;
}

/*
 * Write functions
 */

void
chill_write_int(chill_int_t value)
{
	printf("%d", value);
}

void
chill_write_real(chill_real_t value)
{
	printf("%g", value);
}

void
chill_write_char(chill_char_t value)
{
	putchar(value);
}

void
chill_write_chars(const chill_chars_t *str)
{
	if (str != NULL && str->data != NULL) {
		printf("%.*s", (int)str->length, str->data);
	}
}

void
chill_write_string(const char *str)
{
	if (str != NULL) {
		fputs(str, stdout);
	}
}

void
chill_writeln(void)
{
	putchar('\n');
}
