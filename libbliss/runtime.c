/*
 * Copyright (c) 2025 PCC BLISS Runtime Library
 *
 * Runtime Support Implementation
 * Signal handling, bit operations, and utilities
 */

#include "blissrt.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Runtime version */
#define BLISSRT_VERSION "1.0.0"

/* Global state */
static bliss_signal_handler_t current_handler = NULL;
static int debug_enabled = 0;

/* =================================================================
 * Bit and Field Operations
 * ================================================================= */

/*
 * Extract a bit field
 */
bliss_word_t bliss_field_extract(bliss_word_t value, int position, int size)
{
	bliss_word_t mask;

	if (size <= 0 || size > (int)(sizeof(bliss_word_t) * 8))
		return 0;

	if (position < 0 || position >= (int)(sizeof(bliss_word_t) * 8))
		return 0;

	/* Create mask with 'size' bits set */
	mask = (size == (int)(sizeof(bliss_word_t) * 8)) ?
		~(bliss_word_t)0 : ((bliss_word_t)1 << size) - 1;

	/* Extract field */
	return (value >> position) & mask;
}

/*
 * Insert a bit field
 */
bliss_word_t bliss_field_insert(bliss_word_t dest, bliss_word_t field,
                                int position, int size)
{
	bliss_word_t mask;

	if (size <= 0 || size > (int)(sizeof(bliss_word_t) * 8))
		return dest;

	if (position < 0 || position >= (int)(sizeof(bliss_word_t) * 8))
		return dest;

	/* Create mask with 'size' bits set */
	mask = (size == (int)(sizeof(bliss_word_t) * 8)) ?
		~(bliss_word_t)0 : ((bliss_word_t)1 << size) - 1;

	/* Clear destination bits */
	dest &= ~(mask << position);

	/* Insert field */
	dest |= (field & mask) << position;

	return dest;
}

/*
 * Count leading zeros
 */
int bliss_count_leading_zeros(bliss_uword_t value)
{
	int count = 0;

	if (value == 0)
		return sizeof(bliss_uword_t) * 8;

#if defined(__GNUC__) || defined(__clang__)
	/* Use compiler builtin if available */
	if (sizeof(bliss_uword_t) == sizeof(unsigned long long))
		return __builtin_clzll(value);
	else if (sizeof(bliss_uword_t) == sizeof(unsigned long))
		return __builtin_clzl(value);
	else
		return __builtin_clz(value);
#else
	/* Manual implementation */
	bliss_uword_t mask = (bliss_uword_t)1 << (sizeof(bliss_uword_t) * 8 - 1);
	while ((value & mask) == 0 && count < (int)(sizeof(bliss_uword_t) * 8)) {
		count++;
		mask >>= 1;
	}
	return count;
#endif
}

/*
 * Count trailing zeros
 */
int bliss_count_trailing_zeros(bliss_uword_t value)
{
	int count = 0;

	if (value == 0)
		return sizeof(bliss_uword_t) * 8;

#if defined(__GNUC__) || defined(__clang__)
	/* Use compiler builtin if available */
	if (sizeof(bliss_uword_t) == sizeof(unsigned long long))
		return __builtin_ctzll(value);
	else if (sizeof(bliss_uword_t) == sizeof(unsigned long))
		return __builtin_ctzl(value);
	else
		return __builtin_ctz(value);
#else
	/* Manual implementation */
	while ((value & 1) == 0 && count < (int)(sizeof(bliss_uword_t) * 8)) {
		count++;
		value >>= 1;
	}
	return count;
#endif
}

/*
 * Count set bits (population count)
 */
int bliss_popcount(bliss_uword_t value)
{
#if defined(__GNUC__) || defined(__clang__)
	/* Use compiler builtin if available */
	if (sizeof(bliss_uword_t) == sizeof(unsigned long long))
		return __builtin_popcountll(value);
	else if (sizeof(bliss_uword_t) == sizeof(unsigned long))
		return __builtin_popcountl(value);
	else
		return __builtin_popcount(value);
#else
	/* Manual implementation (Brian Kernighan's algorithm) */
	int count = 0;
	while (value) {
		value &= value - 1;
		count++;
	}
	return count;
#endif
}

/* =================================================================
 * Signal Handling
 * ================================================================= */

/*
 * Default signal handler
 */
static int default_signal_handler(bliss_signal_t condition, void *arg)
{
	const char *cond_str;

	switch (condition) {
	case BLISS_SIG_SUCCESS:
		cond_str = "SUCCESS";
		break;
	case BLISS_SIG_ERROR:
		cond_str = "ERROR";
		break;
	case BLISS_SIG_WARNING:
		cond_str = "WARNING";
		break;
	case BLISS_SIG_INFO:
		cond_str = "INFO";
		break;
	default:
		cond_str = "UNKNOWN";
		break;
	}

	fprintf(stderr, "BLISS SIGNAL: %s (arg=%p)\n", cond_str, arg);

	/* Continue for non-error conditions */
	return (condition != BLISS_SIG_ERROR);
}

/*
 * Signal a condition
 */
void bliss_signal(bliss_signal_t condition, void *arg)
{
	bliss_signal_handler_t handler = current_handler ?
		current_handler : default_signal_handler;

	if (!handler(condition, arg)) {
		/* Handler returned false, abort */
		bliss_abort("Signal handler aborted execution");
	}
}

/*
 * Set signal handler
 */
void bliss_set_signal_handler(bliss_signal_handler_t handler)
{
	current_handler = handler;
}

/*
 * Get current signal handler
 */
bliss_signal_handler_t bliss_get_signal_handler(void)
{
	return current_handler;
}

/* =================================================================
 * Runtime Utilities
 * ================================================================= */

/*
 * Initialize BLISS runtime
 */
void bliss_runtime_init(void)
{
	current_handler = NULL;
	debug_enabled = 0;

	if (debug_enabled) {
		fprintf(stderr, "BLISS Runtime %s initialized\n", BLISSRT_VERSION);
	}
}

/*
 * Cleanup BLISS runtime
 */
void bliss_runtime_cleanup(void)
{
	if (debug_enabled) {
		fprintf(stderr, "BLISS Runtime cleanup\n");
	}
}

/*
 * Abort program with error message
 */
void bliss_abort(const char *msg)
{
	fprintf(stderr, "BLISS ABORT: %s\n", msg);
	exit(1);
}

/*
 * Get runtime version
 */
const char *bliss_runtime_version(void)
{
	return BLISSRT_VERSION;
}

/* =================================================================
 * Debugging Support
 * ================================================================= */

/*
 * Dump a vector (for debugging)
 */
void bliss_dump_vector(bliss_vector_t *vec)
{
	size_t i;

	if (vec == NULL) {
		printf("Vector: NULL\n");
		return;
	}

	printf("Vector: size=%zu, element_size=%zu\n", vec->size, vec->element_size);
	printf("Data at %p:\n", (void *)vec->data);

	if (vec->data == NULL) {
		printf("  (null data)\n");
		return;
	}

	for (i = 0; i < vec->size && i < 16; i++) {
		printf("  [%zu] = %ld (0x%lX)\n", i,
		       (long)vec->data[i], (unsigned long)vec->data[i]);
	}

	if (vec->size > 16)
		printf("  ... (%zu more elements)\n", vec->size - 16);
}

/*
 * Dump a string (for debugging)
 */
void bliss_dump_string(bliss_string_t str)
{
	size_t i;

	printf("String: length=%zu\n", str.length);
	printf("Data at %p:\n", (void *)str.data);

	if (str.data == NULL) {
		printf("  (null data)\n");
		return;
	}

	printf("  \"");
	for (i = 0; i < str.length && i < 80; i++) {
		if (str.data[i] >= 32 && str.data[i] < 127)
			putchar(str.data[i]);
		else
			printf("\\x%02X", (unsigned char)str.data[i]);
	}
	if (str.length > 80)
		printf("... (%zu more)", str.length - 80);
	printf("\"\n");
}

/*
 * Enable debug tracing
 */
void bliss_set_debug(int enable)
{
	debug_enabled = enable;
	if (enable) {
		fprintf(stderr, "BLISS debug tracing enabled\n");
	}
}
