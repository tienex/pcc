/*
 * Hello World example using Dart runtime library
 */

#include "../dart.h"
#include <stdio.h>

int
main(void)
{
	dart_runtime_init();

	/* Create and print a string */
	DartString *message = dart_string_new("Hello, Dart Runtime!");
	dart_print((DartObject *)message);

	/* String operations */
	DartString *upper = dart_string_to_upper(message);
	dart_print((DartObject *)upper);

	DartString *lower = dart_string_to_lower(message);
	dart_print((DartObject *)lower);

	/* Cleanup */
	dart_object_release((DartObject *)message);
	dart_object_release((DartObject *)upper);
	dart_object_release((DartObject *)lower);

	dart_runtime_cleanup();
	return 0;
}
