/*
 * Test C# Runtime - String Functionality
 */

#include <stdio.h>
#include <assert.h>
#include "../include/csruntime.h"

void test_string_creation() {
	printf("Test: String creation...\n");

	CSString *str1 = CS_String_Create("Hello");
	assert(str1 != NULL);
	assert(CS_String_Length(str1) == 5);

	CSString *str2 = CS_String_CreateEmpty();
	assert(str2 != NULL);
	assert(CS_String_Length(str2) == 0);
	assert(CS_String_IsNullOrEmpty(str2) == 1);

	CS_Release((CSObject *)str1);
	CS_Release((CSObject *)str2);

	printf("  PASSED\n");
}

void test_string_concat() {
	printf("Test: String concatenation...\n");

	CSString *str1 = CS_String_Create("Hello");
	CSString *str2 = CS_String_Create(" World");
	CSString *result = CS_String_Concat(str1, str2);

	assert(result != NULL);
	assert(CS_String_Length(result) == 11);

	const char *utf8 = CS_String_ToUTF8(result);
	assert(utf8 != NULL);
	printf("  Result: '%s'\n", utf8);

	CS_Release((CSObject *)str1);
	CS_Release((CSObject *)str2);
	CS_Release((CSObject *)result);

	printf("  PASSED\n");
}

void test_string_comparison() {
	printf("Test: String comparison...\n");

	CSString *str1 = CS_String_Create("Hello");
	CSString *str2 = CS_String_Create("Hello");
	CSString *str3 = CS_String_Create("World");

	assert(CS_String_Equals(str1, str2) == 1);
	assert(CS_String_Equals(str1, str3) == 0);
	assert(CS_String_Compare(str1, str2) == 0);
	assert(CS_String_Compare(str1, str3) < 0);

	CS_Release((CSObject *)str1);
	CS_Release((CSObject *)str2);
	CS_Release((CSObject *)str3);

	printf("  PASSED\n");
}

void test_string_substring() {
	printf("Test: String substring...\n");

	CSString *str = CS_String_Create("Hello World");
	CSString *sub = CS_String_Substring(str, 6, 5);

	assert(sub != NULL);
	assert(CS_String_Length(sub) == 5);

	const char *utf8 = CS_String_ToUTF8(sub);
	assert(utf8 != NULL);
	printf("  Substring: '%s'\n", utf8);

	CS_Release((CSObject *)str);
	CS_Release((CSObject *)sub);

	printf("  PASSED\n");
}

void test_string_replace() {
	printf("Test: String replace...\n");

	CSString *str = CS_String_Create("Hello World");
	CSString *old = CS_String_Create("World");
	CSString *new = CS_String_Create("C#");
	CSString *result = CS_String_Replace(str, old, new);

	assert(result != NULL);

	const char *utf8 = CS_String_ToUTF8(result);
	assert(utf8 != NULL);
	printf("  Result: '%s'\n", utf8);

	CS_Release((CSObject *)str);
	CS_Release((CSObject *)old);
	CS_Release((CSObject *)new);
	CS_Release((CSObject *)result);

	printf("  PASSED\n");
}

void test_string_case() {
	printf("Test: String case conversion...\n");

	CSString *str = CS_String_Create("Hello World");
	CSString *upper = CS_String_ToUpper(str);
	CSString *lower = CS_String_ToLower(str);

	const char *upper_utf8 = CS_String_ToUTF8(upper);
	const char *lower_utf8 = CS_String_ToUTF8(lower);

	printf("  Upper: '%s'\n", upper_utf8);
	printf("  Lower: '%s'\n", lower_utf8);

	CS_Release((CSObject *)str);
	CS_Release((CSObject *)upper);
	CS_Release((CSObject *)lower);

	printf("  PASSED\n");
}

void test_string_conversion() {
	printf("Test: String conversion...\n");

	CSString *num_str = CS_String_Create("42");
	int32_t num = CS_String_ToInt32(num_str);
	assert(num == 42);

	CSString *from_int = CS_String_FromInt32(123);
	const char *utf8 = CS_String_ToUTF8(from_int);
	printf("  FromInt32(123): '%s'\n", utf8);

	CSString *from_bool = CS_String_FromBool(1);
	const char *bool_utf8 = CS_String_ToUTF8(from_bool);
	printf("  FromBool(true): '%s'\n", bool_utf8);

	CS_Release((CSObject *)num_str);
	CS_Release((CSObject *)from_int);
	CS_Release((CSObject *)from_bool);

	printf("  PASSED\n");
}

int main() {
	printf("\n========== C# Runtime String Tests ==========\n\n");

	/* Initialize runtime */
	struct cs_runtime_config config = {
		.initial_heap_size = 1024 * 1024,
		.max_heap_size = 1024 * 1024 * 1024,
		.enable_gc = 0,
		.enable_arc = 1,
		.gc_threshold = 0,
		.debug_mode = 0,
	};

	CSRuntime_Init(&config);

	/* Run tests */
	test_string_creation();
	test_string_concat();
	test_string_comparison();
	test_string_substring();
	test_string_replace();
	test_string_case();
	test_string_conversion();

	/* Shutdown */
	CSRuntime_Shutdown();

	printf("\n========== All String Tests Passed ==========\n\n");
	return 0;
}
