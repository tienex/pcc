/*
 * COBOL Runtime Library - I/O Operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cobolrt.h"

void
__cobol_accept(cobol_field_t *field)
{
	char buffer[1024];
	size_t len;

	if (!field || !field->data) {
		__cobol_fatal_error("ACCEPT: invalid field");
		return;
	}

	/* Read from stdin */
	if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
		memset(field->data, ' ', field->size);
		return;
	}

	/* Remove trailing newline */
	len = strlen(buffer);
	if (len > 0 && buffer[len-1] == '\n')
		buffer[--len] = '\0';

	/* Copy to field, truncate or pad as needed */
	if (len > field->size)
		len = field->size;

	memcpy(field->data, buffer, len);

	/* Pad with spaces if needed */
	if (len < field->size)
		memset((char *)field->data + len, ' ', field->size - len);
}

void
__cobol_display(cobol_field_t *field)
{
	size_t i;
	char *data;

	if (!field || !field->data) {
		return;
	}

	data = (char *)field->data;

	/* Display the field */
	switch (field->type) {
	case COB_TYPE_NUMERIC:
		/* Format numeric field */
		if (field->scale > 0) {
			/* Has decimal places */
			double val = __cobol_get_double(field);
			printf("%.*f", field->scale, val);
		} else {
			/* Integer */
			long long val = __cobol_get_long(field);
			printf("%lld", val);
		}
		break;

	case COB_TYPE_ALPHABETIC:
	case COB_TYPE_ALPHANUMERIC:
		/* Display as string, trim trailing spaces */
		for (i = field->size; i > 0 && data[i-1] == ' '; i--)
			;
		fwrite(data, 1, i, stdout);
		break;

	default:
		/* Raw display */
		fwrite(data, 1, field->size, stdout);
		break;
	}
}

void
__cobol_display_line(cobol_field_t *fields[], int count)
{
	int i;

	for (i = 0; i < count; i++) {
		if (i > 0)
			putchar(' ');
		__cobol_display(fields[i]);
	}
	putchar('\n');
}
