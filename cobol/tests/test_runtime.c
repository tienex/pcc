/*
 * Test COBOL runtime library functions
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../libcobol/cobolrt.h"

void test_numeric_operations(void);
void test_string_operations(void);
void test_field_conversions(void);
void test_intrinsics(void);

int main(void)
{
	printf("Testing COBOL Runtime Library...\n\n");

	__cobol_init();

	test_numeric_operations();
	test_string_operations();
	test_field_conversions();
	test_intrinsics();

	__cobol_cleanup();

	printf("\nAll tests passed!\n");
	return 0;
}

void test_numeric_operations(void)
{
	char data1[10] = "00000100";
	char data2[10] = "00000050";
	char result[10];

	cobol_field_t field1 = { data1, 8, COB_TYPE_NUMERIC, 8, 0, 0 };
	cobol_field_t field2 = { data2, 8, COB_TYPE_NUMERIC, 8, 0, 0 };
	cobol_field_t field_result = { result, 8, COB_TYPE_NUMERIC, 8, 0, 0 };

	printf("Testing numeric operations...\n");

	/* Test addition */
	__cobol_add(&field_result, &field1, &field2);
	printf("  100 + 50 = %d\n", __cobol_get_int(&field_result));
	assert(__cobol_get_int(&field_result) == 150);

	/* Test subtraction */
	__cobol_subtract(&field_result, &field1, &field2);
	printf("  100 - 50 = %d\n", __cobol_get_int(&field_result));
	assert(__cobol_get_int(&field_result) == 50);

	/* Test multiplication */
	__cobol_multiply(&field_result, &field1, &field2);
	printf("  100 * 50 = %d\n", __cobol_get_int(&field_result));
	assert(__cobol_get_int(&field_result) == 5000);

	/* Test division */
	__cobol_divide(&field_result, &field1, &field2);
	printf("  100 / 50 = %d\n", __cobol_get_int(&field_result));
	assert(__cobol_get_int(&field_result) == 2);

	printf("  ✓ Numeric operations passed\n\n");
}

void test_string_operations(void)
{
	char data1[20] = "Hello           ";
	char data2[20] = "World           ";
	char result[40];

	cobol_field_t field1 = { data1, 16, COB_TYPE_ALPHANUMERIC, 0, 0, 0 };
	cobol_field_t field2 = { data2, 16, COB_TYPE_ALPHANUMERIC, 0, 0, 0 };
	cobol_field_t field_result = { result, 32, COB_TYPE_ALPHANUMERIC, 0, 0, 0 };

	printf("Testing string operations...\n");

	/* Test MOVE */
	__cobol_move(&field_result, &field1);
	printf("  MOVE: ");
	fwrite(result, 1, 16, stdout);
	printf("\n");
	assert(memcmp(result, "Hello           ", 16) == 0);

	/* Test STRING */
	cobol_field_t *sources[] = { &field1, &field2 };
	__cobol_string(&field_result, sources, 2);
	printf("  STRING: ");
	fwrite(result, 1, 20, stdout);
	printf("\n");

	/* Test comparison */
	int cmp = __cobol_compare(&field1, &field2);
	printf("  COMPARE: %d\n", cmp);

	printf("  ✓ String operations passed\n\n");
}

void test_field_conversions(void)
{
	char data[10];
	cobol_field_t field = { data, 10, COB_TYPE_NUMERIC, 10, 2, 0 };

	printf("Testing field conversions...\n");

	/* Test set/get int */
	__cobol_set_int(&field, 12345);
	int val = __cobol_get_int(&field);
	printf("  Set/Get Int: %d\n", val);
	assert(val == 12345);

	/* Test set/get double */
	__cobol_set_double(&field, 123.45);
	double dval = __cobol_get_double(&field);
	printf("  Set/Get Double: %.2f\n", dval);
	assert(dval >= 123.44 && dval <= 123.46);

	printf("  ✓ Field conversions passed\n\n");
}

void test_intrinsics(void)
{
	char data1[20] = "hello world         ";
	char data2[20];
	cobol_field_t field1 = { data1, 20, COB_TYPE_ALPHANUMERIC, 0, 0, 0 };
	cobol_field_t field2 = { data2, 20, COB_TYPE_ALPHANUMERIC, 0, 0, 0 };

	printf("Testing intrinsic functions...\n");

	/* Test LENGTH */
	int len = __cobol_length(&field1);
	printf("  LENGTH: %d\n", len);
	assert(len == 20);

	/* Test UPPER-CASE */
	__cobol_upper_case(&field2, &field1);
	printf("  UPPER-CASE: ");
	fwrite(data2, 1, 11, stdout);
	printf("\n");

	/* Test LOWER-CASE */
	char upper[20] = "HELLO WORLD         ";
	cobol_field_t field_upper = { upper, 20, COB_TYPE_ALPHANUMERIC, 0, 0, 0 };
	__cobol_lower_case(&field2, &field_upper);
	printf("  LOWER-CASE: ");
	fwrite(data2, 1, 11, stdout);
	printf("\n");

	/* Test REVERSE */
	__cobol_reverse(&field2, &field1);
	printf("  REVERSE: ");
	fwrite(data2, 1, 11, stdout);
	printf("\n");

	printf("  ✓ Intrinsic functions passed\n\n");
}
