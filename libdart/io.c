/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - I/O Functions
 */

#include "dart.h"
#include <stdio.h>
#include <stdlib.h>

void
dart_print(DartObject *obj)
{
	if (!obj || dart_is_null(obj)) {
		printf("null\n");
		return;
	}

	DartString *str = dart_object_to_string(obj);
	printf("%s\n", dart_string_cstr(str));

	/* Release if newly created string */
	if (str != (DartString *)obj) {
		dart_object_release((DartObject *)str);
	}
}

void
dart_print_string(const char *str)
{
	printf("%s\n", str ? str : "null");
}

DartString *
dart_read_line(void)
{
	char buffer[4096];

	if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
		return dart_string_empty();
	}

	/* Remove newline */
	size_t len = strlen(buffer);
	if (len > 0 && buffer[len - 1] == '\n') {
		buffer[len - 1] = '\0';
		len--;
	}

	return dart_string_new_with_length(buffer, len);
}
