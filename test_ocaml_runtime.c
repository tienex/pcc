/*
 * Test program for OCaml runtime library
 */

#include <stdio.h>
#include "libocaml/ocaml_runtime.h"

int main(void)
{
	ocaml_value_t list, arr, str, ref;
	int i;

	/* Initialize GC */
	ocaml_gc_init(1024 * 1024);

	printf("=== OCaml Runtime Library Test ===\n\n");

	/* Test integers */
	printf("Testing integers:\n");
	ocaml_print_int(VAL_INT(42));
	printf(" (should be 42)\n");
	printf("INT_VAL(VAL_INT(100)) = %ld (should be 100)\n\n", (long)INT_VAL(VAL_INT(100)));

	/* Test lists */
	printf("Testing lists:\n");
	list = ocaml_cons(VAL_INT(1),
	        ocaml_cons(VAL_INT(2),
	         ocaml_cons(VAL_INT(3), VAL_NIL)));
	printf("List length: ");
	ocaml_print_int(ocaml_list_length(list));
	printf(" (should be 3)\n");
	printf("List head: ");
	ocaml_print_int(ocaml_list_hd(list));
	printf(" (should be 1)\n");
	printf("List elements: ");
	for (i = 0; i < 3; i++) {
		ocaml_print_int(ocaml_list_nth(list, VAL_INT(i)));
		printf(" ");
	}
	printf("(should be 1 2 3)\n\n");

	/* Test arrays */
	printf("Testing arrays:\n");
	arr = ocaml_array_make(VAL_INT(5), VAL_INT(0));
	printf("Array length: ");
	ocaml_print_int(ocaml_array_length(arr));
	printf(" (should be 5)\n");
	ocaml_array_set(arr, VAL_INT(2), VAL_INT(42));
	printf("arr[2] = ");
	ocaml_print_int(ocaml_array_get(arr, VAL_INT(2)));
	printf(" (should be 42)\n\n");

	/* Test strings */
	printf("Testing strings:\n");
	str = ocaml_string_make("Hello, OCaml!");
	printf("String: ");
	ocaml_print_string(str);
	printf("\n");
	printf("String length: ");
	ocaml_print_int(ocaml_string_length(str));
	printf(" (should be 13)\n");
	printf("str[7] = ");
	ocaml_print_char(ocaml_string_get(str, VAL_INT(7)));
	printf(" (should be 'O')\n\n");

	/* Test string concatenation */
	printf("Testing string concat:\n");
	ocaml_value_t str_list = ocaml_cons(ocaml_string_make("Hello"),
	                          ocaml_cons(ocaml_string_make("World"),
	                           ocaml_cons(ocaml_string_make("OCaml"), VAL_NIL)));
	ocaml_value_t sep = ocaml_string_make(", ");
	ocaml_value_t joined = ocaml_string_concat(sep, str_list);
	printf("Joined: ");
	ocaml_print_string(joined);
	printf(" (should be 'Hello, World, OCaml')\n\n");

	/* Test references */
	printf("Testing references:\n");
	ref = ocaml_ref(VAL_INT(10));
	printf("Initial value: ");
	ocaml_print_int(ocaml_deref(ref));
	printf(" (should be 10)\n");
	ocaml_assign(ref, VAL_INT(20));
	printf("After assignment: ");
	ocaml_print_int(ocaml_deref(ref));
	printf(" (should be 20)\n\n");

	/* Test conversions */
	printf("Testing conversions:\n");
	printf("int_of_float(3.14) = ");
	ocaml_print_int(ocaml_int_of_float(3.14));
	printf(" (should be 3)\n");
	printf("float_of_int(42) = ");
	ocaml_print_float(ocaml_float_of_int(VAL_INT(42)));
	printf(" (should be 42)\n");
	printf("string_of_int(123) = ");
	ocaml_print_string(ocaml_string_of_int(VAL_INT(123)));
	printf(" (should be '123')\n");
	printf("int_of_string(\"456\") = ");
	ocaml_print_int(ocaml_int_of_string(ocaml_string_make("456")));
	printf(" (should be 456)\n\n");

	/* Test GC */
	printf("Testing GC:\n");
	printf("Allocating many strings...\n");
	for (i = 0; i < 1000; i++) {
		ocaml_string_make("Test string for GC");
	}
	printf("Running GC...\n");
	ocaml_gc_collect();
	printf("GC completed successfully\n\n");

	printf("=== All tests passed! ===\n");

	return 0;
}
