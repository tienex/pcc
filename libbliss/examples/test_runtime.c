/*
 * BLISS Runtime Library Test Program
 * Demonstrates the various features of libbliss
 */

#include "../blissrt.h"
#include <stdio.h>

static void test_io(void);
static void test_strings(void);
static void test_memory(void);
static void test_bits(void);
static void test_signals(void);

int main(void) {
	printf("=== BLISS Runtime Library Test ===\n");
	printf("Version: %s\n\n", bliss_runtime_version());

	bliss_runtime_init();

	test_io();
	test_strings();
	test_memory();
	test_bits();
	test_signals();

	bliss_runtime_cleanup();

	printf("\n=== All tests completed ===\n");
	return 0;
}

static void test_io(void) {
	printf("\n--- I/O Operations ---\n");

	bliss_puts("Basic string output: ");
	bliss_puts("Hello, BLISS!");
	bliss_putcrlf();

	bliss_puts("Decimal: ");
	bliss_put_decimal(42);
	bliss_putcrlf();

	bliss_puts("Hexadecimal: ");
	bliss_put_hex(255);
	bliss_putcrlf();

	bliss_puts("Octal: ");
	bliss_put_octal(64);
	bliss_putcrlf();

	bliss_string_t s = bliss_string_from_cstr("BLISS string output");
	bliss_puts("String: ");
	bliss_put_string(s);
	bliss_putcrlf();
	bliss_string_free(s);
}

static void test_strings(void) {
	printf("\n--- String Operations ---\n");

	bliss_string_t s1 = bliss_string_from_cstr("Hello");
	bliss_string_t s2 = bliss_string_from_cstr(" World");
	bliss_string_t s3 = bliss_string_concat(s1, s2);

	char *result = bliss_string_to_cstr(s3);
	printf("Concatenation: '%s'\n", result);
	bliss_free(result);

	int cmp = bliss_string_compare(s1, s2);
	printf("Compare 'Hello' vs ' World': %d\n", cmp);

	bliss_string_t sub = bliss_string_substr(s3, 0, 5);
	result = bliss_string_to_cstr(sub);
	printf("Substring [0:5]: '%s'\n", result);
	bliss_free(result);

	bliss_string_free(s1);
	bliss_string_free(s2);
	bliss_string_free(s3);
	bliss_string_free(sub);
}

static void test_memory(void) {
	printf("\n--- Memory Management ---\n");

	bliss_vector_t *vec = bliss_alloc_vector(10, sizeof(bliss_word_t));
	printf("Allocated vector: size=%zu, element_size=%zu\n",
	       vec->size, vec->element_size);

	// Fill vector
	for (size_t i = 0; i < vec->size; i++) {
		vec->data[i] = (bliss_word_t)(i * 10);
	}

	printf("Vector contents:\n");
	for (size_t i = 0; i < 5; i++) {
		printf("  vec[%zu] = %ld\n", i, (long)vec->data[i]);
	}

	bliss_free_vector(vec);
	printf("Vector freed\n");

	// Dynamic memory
	void *ptr = bliss_malloc(100);
	printf("Allocated 100 bytes at %p\n", ptr);
	bliss_free(ptr);
	printf("Memory freed\n");
}

static void test_bits(void) {
	printf("\n--- Bit Operations ---\n");

	bliss_word_t value = 0x12345678;
	printf("Original value: 0x%lX\n", (unsigned long)value);

	bliss_word_t field = bliss_field_extract(value, 8, 8);
	printf("Extract bits [8:15]: 0x%lX\n", (unsigned long)field);

	bliss_word_t new_value = bliss_field_insert(value, 0xFF, 8, 8);
	printf("Insert 0xFF at bits [8:15]: 0x%lX\n", (unsigned long)new_value);

	bliss_uword_t test = 0x00FF0000;
	printf("\nBit counting for 0x%lX:\n", (unsigned long)test);
	printf("  Leading zeros: %d\n", bliss_count_leading_zeros(test));
	printf("  Trailing zeros: %d\n", bliss_count_trailing_zeros(test));
	printf("  Population count: %d\n", bliss_popcount(test));
}

static int test_signal_handler(bliss_signal_t condition, void *arg) {
	const char *cond_str;
	switch (condition) {
	case BLISS_SIG_SUCCESS: cond_str = "SUCCESS"; break;
	case BLISS_SIG_ERROR: cond_str = "ERROR"; break;
	case BLISS_SIG_WARNING: cond_str = "WARNING"; break;
	case BLISS_SIG_INFO: cond_str = "INFO"; break;
	default: cond_str = "UNKNOWN"; break;
	}

	printf("Custom handler received: %s (arg=%p)\n", cond_str, arg);
	return (condition != BLISS_SIG_ERROR);
}

static void test_signals(void) {
	printf("\n--- Signal Handling ---\n");

	bliss_set_signal_handler(test_signal_handler);

	bliss_signal(BLISS_SIG_SUCCESS, NULL);
	bliss_signal(BLISS_SIG_INFO, (void *)0x1234);
	bliss_signal(BLISS_SIG_WARNING, NULL);
}
