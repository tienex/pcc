/*
 * Integration test: Generic GC with OCaml runtime
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libocaml/ocaml_runtime.h"

int
main(void)
{
	ocaml_value_t list, arr, str, ref_val;
	ocaml_value_t temp;
	int i;

	printf("========================================\n");
	printf("OCaml Runtime + Generic GC Integration\n");
	printf("========================================\n");

	/* Test 1: Initialize OCaml runtime (uses generic GC) */
	printf("\n1. Initializing OCaml runtime...\n");
	ocaml_gc_init(16 * 1024 * 1024);
	printf("✓ OCaml runtime initialized with generic GC\n");

	/* Test 2: Create OCaml list */
	printf("\n2. Creating OCaml list...\n");
	list = ocaml_nil();
	ocaml_register_root(&list);

	for (i = 0; i < 10; i++) {
		list = ocaml_cons(VAL_INT(i), list);
	}

	int len = ocaml_list_length(list);
	printf("✓ Created list of length: %d\n", len);
	assert(len == 10);

	/* Verify list contents */
	temp = list;
	for (i = 9; i >= 0; i--) {
		ocaml_value_t head = ocaml_hd(temp);
		assert(IS_INT(head));
		assert(INT_VAL(head) == i);
		temp = ocaml_tl(temp);
	}
	printf("✓ List contents verified: [9,8,7,6,5,4,3,2,1,0]\n");

	/* Test 3: Create OCaml array */
	printf("\n3. Creating OCaml array...\n");
	arr = ocaml_array_make(VAL_INT(5), VAL_INT(0));
	ocaml_register_root(&arr);

	for (i = 0; i < 5; i++) {
		ocaml_array_set(arr, VAL_INT(i), VAL_INT(i * 10));
	}

	printf("✓ Created array of 5 elements\n");

	/* Verify array */
	for (i = 0; i < 5; i++) {
		ocaml_value_t elem = ocaml_array_get(arr, VAL_INT(i));
		assert(INT_VAL(elem) == i * 10);
	}
	printf("✓ Array contents verified: [0,10,20,30,40]\n");

	/* Test 4: Create OCaml string */
	printf("\n4. Creating OCaml string...\n");
	str = ocaml_string_make(VAL_INT(20), VAL_INT('A'));
	ocaml_register_root(&str);

	int str_len = INT_VAL(ocaml_string_length(str));
	printf("✓ Created string of length: %d\n", str_len);
	assert(str_len == 20);

	/* Verify string content */
	for (i = 0; i < 20; i++) {
		ocaml_value_t ch = ocaml_string_get(str, VAL_INT(i));
		assert(INT_VAL(ch) == 'A');
	}
	printf("✓ String contents verified (all 'A's)\n");

	/* Test 5: Create OCaml reference */
	printf("\n5. Creating OCaml reference...\n");
	ref_val = ocaml_ref(VAL_INT(42));
	ocaml_register_root(&ref_val);

	ocaml_value_t deref_val = ocaml_deref(ref_val);
	printf("✓ Created reference with value: %d\n", INT_VAL(deref_val));
	assert(INT_VAL(deref_val) == 42);

	/* Modify reference */
	ocaml_assign(ref_val, VAL_INT(100));
	deref_val = ocaml_deref(ref_val);
	printf("✓ Modified reference to: %d\n", INT_VAL(deref_val));
	assert(INT_VAL(deref_val) == 100);

	/* Test 6: Garbage collection */
	printf("\n6. Testing garbage collection...\n");

	/* Create temporary objects (garbage) */
	for (i = 0; i < 100; i++) {
		ocaml_cons(VAL_INT(i), ocaml_nil());
		ocaml_array_make(VAL_INT(3), VAL_INT(i));
		ocaml_ref(VAL_INT(i));
	}

	printf("✓ Created 300 temporary OCaml objects\n");

	/* Collect garbage */
	ocaml_gc_collect();
	printf("✓ Garbage collection completed\n");

	/* Verify rooted objects still valid */
	assert(ocaml_list_length(list) == 10);
	assert(INT_VAL(ocaml_array_get(arr, VAL_INT(2))) == 20);
	assert(INT_VAL(ocaml_string_length(str)) == 20);
	assert(INT_VAL(ocaml_deref(ref_val)) == 100);

	printf("✓ All rooted objects survived collection\n");

	/* Test 7: Nested structures */
	printf("\n7. Creating nested structures...\n");

	/* Create list of arrays */
	ocaml_value_t list_of_arrays = ocaml_nil();
	ocaml_register_root(&list_of_arrays);

	for (i = 0; i < 5; i++) {
		ocaml_value_t arr_elem = ocaml_array_make(VAL_INT(3), VAL_INT(i));
		list_of_arrays = ocaml_cons(arr_elem, list_of_arrays);
	}

	printf("✓ Created list of 5 arrays\n");

	/* Verify nested structure */
	temp = list_of_arrays;
	for (i = 4; i >= 0; i--) {
		ocaml_value_t arr_in_list = ocaml_hd(temp);
		ocaml_value_t elem = ocaml_array_get(arr_in_list, VAL_INT(0));
		assert(INT_VAL(elem) == i);
		temp = ocaml_tl(temp);
	}

	printf("✓ Nested structure verified\n");

	/* Test 8: Array of references */
	printf("\n8. Creating array of references...\n");

	ocaml_value_t ref_array = ocaml_array_make(VAL_INT(5), ocaml_ref(VAL_INT(0)));
	ocaml_register_root(&ref_array);

	for (i = 0; i < 5; i++) {
		ocaml_value_t ref_elem = ocaml_ref(VAL_INT(i * 5));
		ocaml_array_set(ref_array, VAL_INT(i), ref_elem);
	}

	printf("✓ Created array of 5 references\n");

	/* Verify and modify */
	for (i = 0; i < 5; i++) {
		ocaml_value_t ref_elem = ocaml_array_get(ref_array, VAL_INT(i));
		assert(INT_VAL(ocaml_deref(ref_elem)) == i * 5);
		ocaml_assign(ref_elem, VAL_INT(i * 5 + 1));
	}

	printf("✓ Array of references verified and modified\n");

	/* Test 9: Complex list operations */
	printf("\n9. Testing complex list operations...\n");

	ocaml_value_t list1 = ocaml_cons(VAL_INT(3), ocaml_cons(VAL_INT(2), ocaml_cons(VAL_INT(1), ocaml_nil())));
	ocaml_value_t list2 = ocaml_cons(VAL_INT(6), ocaml_cons(VAL_INT(5), ocaml_cons(VAL_INT(4), ocaml_nil())));
	ocaml_register_root(&list1);
	ocaml_register_root(&list2);

	/* Append lists (conceptually) */
	ocaml_value_t combined = ocaml_nil();
	ocaml_register_root(&combined);

	temp = list1;
	while (!IS_INT(temp)) {
		combined = ocaml_cons(ocaml_hd(temp), combined);
		temp = ocaml_tl(temp);
	}
	temp = list2;
	while (!IS_INT(temp)) {
		combined = ocaml_cons(ocaml_hd(temp), combined);
		temp = ocaml_tl(temp);
	}

	printf("✓ Combined two lists\n");

	/* Test 10: Multiple collections */
	printf("\n10. Running multiple GC cycles...\n");

	for (i = 0; i < 10; i++) {
		/* Create garbage */
		int j;
		for (j = 0; j < 50; j++) {
			ocaml_cons(VAL_INT(j), ocaml_nil());
		}
		ocaml_gc_collect();
	}

	printf("✓ Completed 10 GC cycles\n");

	/* Verify all objects still intact */
	assert(ocaml_list_length(list) == 10);
	assert(INT_VAL(ocaml_array_get(arr, VAL_INT(0))) == 0);
	assert(INT_VAL(ocaml_deref(ref_val)) == 100);

	printf("✓ All objects intact after multiple collections\n");

	/* Test 11: String operations */
	printf("\n11. Testing string operations...\n");

	ocaml_value_t str1 = ocaml_string_make(VAL_INT(5), VAL_INT('X'));
	ocaml_value_t str2 = ocaml_string_make(VAL_INT(5), VAL_INT('Y'));
	ocaml_register_root(&str1);
	ocaml_register_root(&str2);

	ocaml_value_t concat = ocaml_string_concat(str1, str2);
	ocaml_register_root(&concat);

	int concat_len = INT_VAL(ocaml_string_length(concat));
	assert(concat_len == 10);

	printf("✓ String concatenation: length = %d\n", concat_len);

	/* Verify concatenated string */
	for (i = 0; i < 5; i++) {
		assert(INT_VAL(ocaml_string_get(concat, VAL_INT(i))) == 'X');
	}
	for (i = 5; i < 10; i++) {
		assert(INT_VAL(ocaml_string_get(concat, VAL_INT(i))) == 'Y');
	}

	printf("✓ Concatenated string verified: XXXXXYYYY\n");

	/* Cleanup - unregister all roots */
	printf("\n12. Cleaning up...\n");
	ocaml_unregister_root(&list);
	ocaml_unregister_root(&arr);
	ocaml_unregister_root(&str);
	ocaml_unregister_root(&ref_val);
	ocaml_unregister_root(&list_of_arrays);
	ocaml_unregister_root(&ref_array);
	ocaml_unregister_root(&list1);
	ocaml_unregister_root(&list2);
	ocaml_unregister_root(&combined);
	ocaml_unregister_root(&str1);
	ocaml_unregister_root(&str2);
	ocaml_unregister_root(&concat);

	printf("✓ All roots unregistered\n");

	/* Final collection to clean everything */
	ocaml_gc_collect();

	/* Print statistics */
	printf("\n========================================\n");
	printf("Statistics:\n");
	printf("========================================\n");
	ocaml_gc_stats();

	printf("\n========================================\n");
	printf("✅ ALL INTEGRATION TESTS PASSED!\n");
	printf("========================================\n");
	printf("\nThe OCaml runtime successfully uses the\n");
	printf("generic GC with full functionality!\n");
	printf("========================================\n");

	return 0;
}
