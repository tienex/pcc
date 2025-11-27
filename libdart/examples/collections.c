/*
 * Collections example using Dart runtime library
 */

#include "../dart.h"
#include <stdio.h>

int
main(void)
{
	dart_runtime_init();

	printf("=== List Example ===\n");

	/* Create a list */
	DartList *numbers = dart_list_new();

	/* Add some numbers */
	for (int i = 1; i <= 10; i++) {
		dart_list_add(numbers, dart_int_new(i * 10));
	}

	printf("List length: %zu\n", dart_list_length(numbers));

	/* Print list elements */
	printf("List elements: ");
	for (size_t i = 0; i < dart_list_length(numbers); i++) {
		DartObject *elem = dart_list_get(numbers, i);
		printf("%lld ", dart_int_value(elem));
	}
	printf("\n");

	printf("\n=== Map Example ===\n");

	/* Create a map */
	DartMap *person = dart_map_new();

	/* Add key-value pairs */
	dart_map_set(person,
	             (DartObject *)dart_string_new("name"),
	             (DartObject *)dart_string_new("Alice"));
	dart_map_set(person,
	             (DartObject *)dart_string_new("age"),
	             (DartObject *)dart_int_new(30));
	dart_map_set(person,
	             (DartObject *)dart_string_new("city"),
	             (DartObject *)dart_string_new("New York"));

	printf("Map size: %zu\n", dart_map_size(person));

	/* Retrieve values */
	DartObject *name = dart_map_get(person,
	                                (DartObject *)dart_string_new("name"));
	printf("Name: %s\n", dart_string_cstr((DartString *)name));

	DartObject *age = dart_map_get(person,
	                               (DartObject *)dart_string_new("age"));
	printf("Age: %lld\n", dart_int_value(age));

	printf("\n=== Set Example ===\n");

	/* Create a set */
	DartSet *unique = dart_set_new();

	/* Add elements (duplicates ignored) */
	dart_set_add(unique, (DartObject *)dart_string_new("apple"));
	dart_set_add(unique, (DartObject *)dart_string_new("banana"));
	dart_set_add(unique, (DartObject *)dart_string_new("apple")); /* duplicate */
	dart_set_add(unique, (DartObject *)dart_string_new("cherry"));

	printf("Set size: %zu\n", dart_set_size(unique));

	/* Check membership */
	bool has_apple = dart_set_contains(unique,
	                    (DartObject *)dart_string_new("apple"));
	printf("Contains 'apple': %s\n", has_apple ? "true" : "false");

	bool has_grape = dart_set_contains(unique,
	                    (DartObject *)dart_string_new("grape"));
	printf("Contains 'grape': %s\n", has_grape ? "true" : "false");

	/* Cleanup */
	dart_object_release((DartObject *)numbers);
	dart_object_release((DartObject *)person);
	dart_object_release((DartObject *)unique);

	dart_runtime_cleanup();
	return 0;
}
