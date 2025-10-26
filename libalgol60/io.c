/*
 * Copyright (c) 2025 PCC ALGOL 60+ Runtime Library
 *
 * I/O functions for ALGOL 60+ programs
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "algol60.h"

/*
 * Output functions
 */

void
algol_write_integer(algol_integer value)
{
	printf("%d", value);
	fflush(stdout);
}

void
algol_write_real(algol_real value)
{
	/* Use ALGOL-style exponential notation for very large/small numbers */
	if (value == 0.0) {
		printf("0.0");
	} else if (value >= -1e6 && value <= 1e6 && value >= -1e-6 && value <= 1e-6) {
		/* Normal decimal notation for reasonable range */
		printf("%.10g", value);
	} else {
		/* Exponential notation */
		printf("%.10e", value);
	}
	fflush(stdout);
}

void
algol_write_boolean(algol_boolean value)
{
	printf("%s", value ? "true" : "false");
	fflush(stdout);
}

void
algol_write_string(const char *str)
{
	if (str != NULL) {
		printf("%s", str);
		fflush(stdout);
	}
}

void
algol_write_newline(void)
{
	printf("\n");
	fflush(stdout);
}

/*
 * Input functions
 */

algol_integer
algol_read_integer(void)
{
	algol_integer value;
	int result;

	result = scanf("%d", &value);
	if (result != 1) {
		algol_error(ALGOL_ERR_IO, "failed to read integer");
		return 0;
	}

	return value;
}

algol_real
algol_read_real(void)
{
	algol_real value;
	int result;

	result = scanf("%lf", &value);
	if (result != 1) {
		algol_error(ALGOL_ERR_IO, "failed to read real number");
		return 0.0;
	}

	return value;
}

algol_boolean
algol_read_boolean(void)
{
	char buffer[16];
	int result;

	result = scanf("%15s", buffer);
	if (result != 1) {
		algol_error(ALGOL_ERR_IO, "failed to read boolean");
		return ALGOL_FALSE;
	}

	/* Accept various forms */
	if (strcmp(buffer, "true") == 0 || strcmp(buffer, "TRUE") == 0 ||
	    strcmp(buffer, "1") == 0) {
		return ALGOL_TRUE;
	} else if (strcmp(buffer, "false") == 0 || strcmp(buffer, "FALSE") == 0 ||
	           strcmp(buffer, "0") == 0) {
		return ALGOL_FALSE;
	} else {
		algol_error(ALGOL_ERR_IO, "invalid boolean value: %s", buffer);
		return ALGOL_FALSE;
	}
}

algol_string
algol_read_string(void)
{
	char buffer[1024];
	int result;

	/* Read up to whitespace or newline */
	result = scanf("%1023s", buffer);
	if (result != 1) {
		algol_error(ALGOL_ERR_IO, "failed to read string");
		return algol_string_create("");
	}

	return algol_string_create(buffer);
}

/*
 * Formatted output (extension)
 */

void
algol_writef(const char *format, ...)
{
	va_list ap;

	va_start(ap, format);
	vprintf(format, ap);
	va_end(ap);
	fflush(stdout);
}

/*
 * Formatted input (extension)
 */

int
algol_readf(const char *format, ...)
{
	va_list ap;
	int result;

	va_start(ap, format);
	result = vscanf(format, ap);
	va_end(ap);

	return result;
}
