/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * BASIC Runtime Library - I/O Functions
 */

#include "basicrt.h"

/*
 * PRINT functions
 */

void
basic_print_int(int value)
{
	printf("%d", value);
}

void
basic_print_long(long value)
{
	printf("%ld", value);
}

void
basic_print_float(float value)
{
	printf("%g", value);
}

void
basic_print_double(double value)
{
	printf("%g", value);
}

void
basic_print_string(const char *str)
{
	if (str != NULL)
		printf("%s", str);
}

void
basic_print_newline(void)
{
	printf("\n");
	fflush(stdout);
}

/*
 * INPUT functions
 */

void
basic_input_int(const char *prompt, int *value)
{
	if (prompt != NULL && *prompt != '\0')
		printf("%s? ", prompt);
	scanf("%d", value);
}

void
basic_input_string(const char *prompt, basic_string_t *str)
{
	char buffer[1024];

	if (prompt != NULL && *prompt != '\0')
		printf("%s? ", prompt);

	if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
		/* Remove trailing newline */
		size_t len = strlen(buffer);
		if (len > 0 && buffer[len-1] == '\n')
			buffer[len-1] = '\0';

		/* Allocate or reallocate string */
		if (str->allocated < (int)strlen(buffer) + 1) {
			free(str->data);
			str->data = strdup(buffer);
			str->allocated = strlen(buffer) + 1;
		} else {
			strcpy(str->data, buffer);
		}
		str->length = strlen(buffer);
	}
}
