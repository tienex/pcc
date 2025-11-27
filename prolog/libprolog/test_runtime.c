/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Test program for runtime library
 */

#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"

/* Test basic term creation and unification */
void test_unification(prolog_engine_t *eng) {
	printf("=== Test Unification ===\n");

	/* Create terms */
	word_t x = make_var(eng);
	word_t y = make_var(eng);
	word_t five = make_integer(eng, 5);
	word_t hello = make_atom(eng, "hello");

	/* Test unification */
	printf("Unify X with 5: ");
	if (unify(eng, x, five)) {
		printf("Success - X = ");
		print_term(eng, stdout, x);
		printf("\n");
	} else {
		printf("Failed\n");
	}

	printf("Unify Y with 'hello': ");
	if (unify(eng, y, hello)) {
		printf("Success - Y = ");
		print_term(eng, stdout, y);
		printf("\n");
	} else {
		printf("Failed\n");
	}

	/* Test compound term */
	word_t args[2];
	args[0] = five;
	args[1] = hello;
	functor_t *f = functor_create(eng, "test", 2);
	word_t compound = make_struct(eng, f, args);

	printf("Created compound term: ");
	print_term(eng, stdout, compound);
	printf("\n");

	printf("\n");
}

/* Test list operations */
void test_lists(prolog_engine_t *eng) {
	printf("=== Test Lists ===\n");

	/* Create list [1, 2, 3] */
	word_t nil = make_nil(eng);
	word_t three = make_integer(eng, 3);
	word_t two = make_integer(eng, 2);
	word_t one = make_integer(eng, 1);

	word_t list = make_list(eng, three, nil);
	list = make_list(eng, two, list);
	list = make_list(eng, one, list);

	printf("Created list: ");
	print_term(eng, stdout, list);
	printf("\n");

	printf("\n");
}

/* Test arithmetic */
void test_arithmetic(prolog_engine_t *eng) {
	printf("=== Test Arithmetic ===\n");

	/* Create expression 2 + 3 */
	word_t two = make_integer(eng, 2);
	word_t three = make_integer(eng, 3);

	word_t args[2];
	args[0] = two;
	args[1] = three;

	functor_t *plus = functor_create(eng, "+", 2);
	word_t expr = make_struct(eng, plus, args);

	printf("Expression: ");
	print_term(eng, stdout, expr);
	printf(" = ");

	intptr_t result;
	if (eval_arith(eng, expr, &result)) {
		printf("%ld\n", (long)result);
	} else {
		printf("evaluation failed\n");
	}

	/* Test float arithmetic */
	word_t pi = make_float(eng, 3.14159);
	word_t two_f = make_float(eng, 2.0);

	args[0] = pi;
	args[1] = two_f;

	functor_t *mult = functor_create(eng, "*", 2);
	word_t expr_f = make_struct(eng, mult, args);

	printf("Expression: ");
	print_term(eng, stdout, expr_f);
	printf(" = ");

	double result_f;
	if (eval_arith_float(eng, expr_f, &result_f)) {
		printf("%g\n", result_f);
	} else {
		printf("evaluation failed\n");
	}

	printf("\n");
}

/* Test built-in predicates */
void test_builtins(prolog_engine_t *eng) {
	printf("=== Test Built-ins ===\n");

	/* Test write/1 */
	printf("write('Hello from Prolog'): ");
	word_t hello = make_atom(eng, "Hello from Prolog");
	word_t args[1];
	args[0] = hello;
	builtin_write(eng, args);
	printf("\n");

	/* Test type checking */
	word_t x = make_var(eng);
	word_t five = make_integer(eng, 5);

	args[0] = x;
	printf("var(X): %s\n", builtin_var(eng, args) ? "true" : "false");
	args[0] = five;
	printf("var(5): %s\n", builtin_var(eng, args) ? "true" : "false");
	printf("integer(5): %s\n", builtin_integer(eng, args) ? "true" : "false");

	/* Test functor/3 */
	word_t fargs[2];
	fargs[0] = make_integer(eng, 1);
	fargs[1] = make_integer(eng, 2);
	functor_t *f = functor_create(eng, "foo", 2);
	word_t compound = make_struct(eng, f, fargs);

	word_t args3[3];
	args3[0] = compound;
	args3[1] = make_var(eng);
	args3[2] = make_var(eng);

	printf("functor(foo(1,2), F, A): ");
	if (builtin_functor(eng, args3)) {
		printf("F = ");
		print_term(eng, stdout, args3[1]);
		printf(", A = ");
		print_term(eng, stdout, args3[2]);
		printf("\n");
	} else {
		printf("failed\n");
	}

	printf("\n");
}

/* Test term comparison */
void test_comparison(prolog_engine_t *eng) {
	printf("=== Test Comparison ===\n");

	word_t one = make_integer(eng, 1);
	word_t two = make_integer(eng, 2);
	word_t three = make_integer(eng, 3);

	printf("1 @< 2: %s\n", term_compare(eng, one, two) < 0 ? "true" : "false");
	printf("3 @> 2: %s\n", term_compare(eng, three, two) > 0 ? "true" : "false");
	printf("2 == 2: %s\n", term_compare(eng, two, two) == 0 ? "true" : "false");

	word_t a = make_atom(eng, "apple");
	word_t b = make_atom(eng, "banana");

	printf("apple @< banana: %s\n",
	       term_compare(eng, a, b) < 0 ? "true" : "false");

	printf("\n");
}

/* Main test function */
int main(int argc, char **argv) {
	printf("PCC Prolog Runtime Library Test\n");
	printf("================================\n\n");

	/* Initialize engine */
	prolog_engine_t *eng = prolog_init();
	if (!eng) {
		fprintf(stderr, "Failed to initialize Prolog engine\n");
		return 1;
	}

	printf("Engine initialized successfully\n");
	printf("Heap size: %d words\n", HEAP_SIZE);
	printf("Stack size: %d words\n", STACK_SIZE);
	printf("Trail size: %d words\n\n", TRAIL_SIZE);

	/* Run tests */
	test_unification(eng);
	test_lists(eng);
	test_arithmetic(eng);
	test_builtins(eng);
	test_comparison(eng);

	/* Print statistics */
	printf("=== Statistics ===\n");
	prolog_statistics(eng, stdout);
	printf("\n");

	/* Cleanup */
	prolog_cleanup(eng);

	printf("All tests completed successfully!\n");

	return 0;
}
