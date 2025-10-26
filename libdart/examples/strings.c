/*
 * String operations example using Dart runtime library
 */

#include "../dart.h"
#include <stdio.h>

int
main(void)
{
	dart_runtime_init();

	printf("=== String Operations ===\n\n");

	/* Create strings */
	DartString *str1 = dart_string_new("Hello");
	DartString *str2 = dart_string_new(" World!");

	/* Concatenation */
	DartString *concat = dart_string_concat(str1, str2);
	printf("Concatenation: %s\n", dart_string_cstr(concat));

	/* Length */
	printf("Length: %zu\n", dart_string_length(concat));

	/* Substring */
	DartString *sub = dart_string_substring(concat, 0, 5);
	printf("Substring (0, 5): %s\n", dart_string_cstr(sub));

	/* Case conversion */
	DartString *upper = dart_string_to_upper(concat);
	printf("Uppercase: %s\n", dart_string_cstr(upper));

	DartString *lower = dart_string_to_lower(concat);
	printf("Lowercase: %s\n", dart_string_cstr(lower));

	/* Replace */
	DartString *replaced = dart_string_replace(concat,
	                          dart_string_new("World"),
	                          dart_string_new("Dart"));
	printf("Replace 'World' with 'Dart': %s\n", dart_string_cstr(replaced));

	/* IndexOf */
	int index = dart_string_index_of(concat, dart_string_new("World"));
	printf("Index of 'World': %d\n", index);

	/* Trim */
	DartString *with_spaces = dart_string_new("   trimmed   ");
	DartString *trimmed = dart_string_trim(with_spaces);
	printf("Trimmed: '%s'\n", dart_string_cstr(trimmed));

	/* Split */
	printf("\n=== String Split ===\n");
	DartString *csv = dart_string_new("apple,banana,cherry,date");
	DartList *parts = dart_string_split(csv, dart_string_new(","));
	printf("Split '%s' by ','\n", dart_string_cstr(csv));
	printf("Parts (%zu):\n", dart_list_length(parts));
	for (size_t i = 0; i < dart_list_length(parts); i++) {
		DartString *part = (DartString *)dart_list_get(parts, i);
		printf("  [%zu]: %s\n", i, dart_string_cstr(part));
	}

	/* Cleanup */
	dart_object_release((DartObject *)str1);
	dart_object_release((DartObject *)str2);
	dart_object_release((DartObject *)concat);
	dart_object_release((DartObject *)sub);
	dart_object_release((DartObject *)upper);
	dart_object_release((DartObject *)lower);
	dart_object_release((DartObject *)replaced);
	dart_object_release((DartObject *)with_spaces);
	dart_object_release((DartObject *)trimmed);
	dart_object_release((DartObject *)csv);
	dart_object_release((DartObject *)parts);

	dart_runtime_cleanup();
	return 0;
}
