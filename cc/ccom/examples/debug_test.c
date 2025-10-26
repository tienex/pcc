/*
 * Example program to test debug symbol generation
 * Compile with: pcc -g debug_test.c -o debug_test
 */

#include <stdio.h>

/* Enumeration */
enum Color {
	RED = 0,
	GREEN = 1,
	BLUE = 2
};

/* Structure */
struct Point {
	int x;
	int y;
};

/* Union */
union Data {
	int i;
	float f;
	char str[20];
};

/* Typedef */
typedef struct Point Point_t;

/* Global variable */
int global_var = 42;

/* Static variable */
static int static_var = 123;

/* Function with parameters */
int
add(int a, int b)
{
	int result;

	result = a + b;
	return result;
}

/* Function with local variables */
void
complex_function(void)
{
	int local1 = 10;
	float local2 = 3.14f;
	char local3 = 'A';
	struct Point p = {100, 200};
	enum Color c = RED;

	printf("Locals: %d, %f, %c\n", local1, local2, local3);
	printf("Point: (%d, %d)\n", p.x, p.y);
	printf("Color: %d\n", c);

	/* Nested block */
	{
		int nested_var = 999;
		printf("Nested: %d\n", nested_var);
	}
}

/* Function with array */
void
array_function(void)
{
	int array[10];
	int matrix[3][3];
	int i, j;

	for (i = 0; i < 10; i++)
		array[i] = i * 2;

	for (i = 0; i < 3; i++)
		for (j = 0; j < 3; j++)
			matrix[i][j] = i * 3 + j;

	printf("Array[5] = %d\n", array[5]);
	printf("Matrix[1][1] = %d\n", matrix[1][1]);
}

/* Function with pointers */
void
pointer_function(void)
{
	int value = 100;
	int *ptr = &value;
	int **pptr = &ptr;

	printf("Value: %d\n", value);
	printf("*ptr: %d\n", *ptr);
	printf("**pptr: %d\n", **pptr);
}

/* Recursive function */
int
factorial(int n)
{
	if (n <= 1)
		return 1;
	return n * factorial(n - 1);
}

/* Main function */
int
main(int argc, char *argv[])
{
	int result;

	printf("Debug Symbol Test Program\n");
	printf("=========================\n\n");

	/* Test basic function */
	result = add(10, 20);
	printf("add(10, 20) = %d\n", result);

	/* Test complex function */
	complex_function();

	/* Test array function */
	array_function();

	/* Test pointer function */
	pointer_function();

	/* Test recursive function */
	result = factorial(5);
	printf("factorial(5) = %d\n", result);

	printf("\nGlobal var: %d\n", global_var);
	printf("Static var: %d\n", static_var);

	return 0;
}
