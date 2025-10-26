/*
 * Copyright (c) 2025 PCC BLISS Runtime Library
 *
 * I/O Operations Implementation
 */

#include "blissrt.h"
#include <stdio.h>
#include <string.h>

/*
 * Output a character
 */
void bliss_putchar(int ch)
{
	putchar(ch);
}

/*
 * Output a C string
 */
void bliss_puts(const char *str)
{
	fputs(str, stdout);
}

/*
 * Output a BLISS string
 */
void bliss_put_string(bliss_string_t str)
{
	if (str.data && str.length > 0) {
		fwrite(str.data, 1, str.length, stdout);
	}
}

/*
 * Output a newline
 */
void bliss_putcrlf(void)
{
	putchar('\n');
}

/*
 * Output an integer in decimal
 */
void bliss_put_decimal(bliss_word_t value)
{
	printf("%ld", (long)value);
}

/*
 * Output an integer in hexadecimal
 */
void bliss_put_hex(bliss_word_t value)
{
	printf("%lX", (unsigned long)value);
}

/*
 * Output an integer in octal
 */
void bliss_put_octal(bliss_word_t value)
{
	printf("%lo", (unsigned long)value);
}

/*
 * Input a character
 */
int bliss_getchar(void)
{
	return getchar();
}

/*
 * Input a line
 */
int bliss_getline(char *buffer, size_t size)
{
	if (fgets(buffer, size, stdin) == NULL) {
		return -1;
	}

	/* Remove trailing newline if present */
	size_t len = strlen(buffer);
	if (len > 0 && buffer[len - 1] == '\n') {
		buffer[len - 1] = '\0';
		len--;
	}

	return (int)len;
}
