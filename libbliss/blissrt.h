/*
 * Copyright (c) 2025 PCC BLISS Runtime Library
 *
 * BLISS Runtime Library Header
 * Provides essential runtime support for BLISS programs
 */

#ifndef BLISSRT_H
#define BLISSRT_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Basic Types
 * BLISS uses fullword (machine word) as the default type
 */
typedef intptr_t bliss_word_t;
typedef uintptr_t bliss_uword_t;
typedef int32_t bliss_long_t;
typedef uint32_t bliss_ulong_t;

/*
 * Vector (array) descriptor
 */
typedef struct {
	bliss_word_t *data;     /* Pointer to data */
	size_t size;            /* Number of elements */
	size_t element_size;    /* Size of each element in bytes */
} bliss_vector_t;

/*
 * String descriptor (counted string)
 */
typedef struct {
	char *data;             /* Pointer to string data */
	size_t length;          /* Length of string */
} bliss_string_t;

/*
 * Signal condition codes
 */
typedef enum {
	BLISS_SIG_SUCCESS = 0,
	BLISS_SIG_ERROR = 1,
	BLISS_SIG_WARNING = 2,
	BLISS_SIG_INFO = 3
} bliss_signal_t;

/* =================================================================
 * Memory Management
 * ================================================================= */

/**
 * Allocate memory for a vector
 * @param size Number of elements
 * @param element_size Size of each element in bytes
 * @return Pointer to allocated vector descriptor, or NULL on failure
 */
bliss_vector_t *bliss_alloc_vector(size_t size, size_t element_size);

/**
 * Free a vector
 * @param vec Vector to free
 */
void bliss_free_vector(bliss_vector_t *vec);

/**
 * Allocate memory
 * @param size Number of bytes to allocate
 * @return Pointer to allocated memory, or NULL on failure
 */
void *bliss_malloc(size_t size);

/**
 * Free allocated memory
 * @param ptr Pointer to free
 */
void bliss_free(void *ptr);

/**
 * Reallocate memory
 * @param ptr Pointer to existing memory
 * @param size New size in bytes
 * @return Pointer to reallocated memory, or NULL on failure
 */
void *bliss_realloc(void *ptr, size_t size);

/* =================================================================
 * String Operations
 * ================================================================= */

/**
 * Create a BLISS string from C string
 * @param str C string
 * @return BLISS string descriptor
 */
bliss_string_t bliss_string_from_cstr(const char *str);

/**
 * Create a C string from BLISS string
 * @param str BLISS string
 * @return Newly allocated C string (must be freed with bliss_free)
 */
char *bliss_string_to_cstr(bliss_string_t str);

/**
 * Compare two BLISS strings
 * @param s1 First string
 * @param s2 Second string
 * @return <0 if s1 < s2, 0 if equal, >0 if s1 > s2
 */
int bliss_string_compare(bliss_string_t s1, bliss_string_t s2);

/**
 * Concatenate two BLISS strings
 * @param s1 First string
 * @param s2 Second string
 * @return New string (must be freed)
 */
bliss_string_t bliss_string_concat(bliss_string_t s1, bliss_string_t s2);

/**
 * Get substring
 * @param str Source string
 * @param start Start position (0-based)
 * @param length Length of substring
 * @return New substring (must be freed)
 */
bliss_string_t bliss_string_substr(bliss_string_t str, size_t start, size_t length);

/**
 * Free a BLISS string
 * @param str String to free
 */
void bliss_string_free(bliss_string_t str);

/* =================================================================
 * I/O Operations
 * ================================================================= */

/**
 * Output a character
 * @param ch Character to output
 */
void bliss_putchar(int ch);

/**
 * Output a string
 * @param str C string to output
 */
void bliss_puts(const char *str);

/**
 * Output a BLISS string
 * @param str BLISS string to output
 */
void bliss_put_string(bliss_string_t str);

/**
 * Output a newline
 */
void bliss_putcrlf(void);

/**
 * Output an integer in decimal
 * @param value Integer value
 */
void bliss_put_decimal(bliss_word_t value);

/**
 * Output an integer in hexadecimal
 * @param value Integer value
 */
void bliss_put_hex(bliss_word_t value);

/**
 * Output an integer in octal
 * @param value Integer value
 */
void bliss_put_octal(bliss_word_t value);

/**
 * Input a character
 * @return Character read, or -1 on EOF
 */
int bliss_getchar(void);

/**
 * Input a line
 * @param buffer Buffer to store line
 * @param size Size of buffer
 * @return Number of characters read, or -1 on error
 */
int bliss_getline(char *buffer, size_t size);

/* =================================================================
 * Bit and Field Operations
 * ================================================================= */

/**
 * Extract a bit field
 * @param value Source value
 * @param position Starting bit position (0 = LSB)
 * @param size Number of bits
 * @return Extracted field value
 */
bliss_word_t bliss_field_extract(bliss_word_t value, int position, int size);

/**
 * Insert a bit field
 * @param dest Destination value
 * @param field Field value to insert
 * @param position Starting bit position (0 = LSB)
 * @param size Number of bits
 * @return New value with field inserted
 */
bliss_word_t bliss_field_insert(bliss_word_t dest, bliss_word_t field,
                                int position, int size);

/**
 * Count leading zeros
 * @param value Value to examine
 * @return Number of leading zero bits
 */
int bliss_count_leading_zeros(bliss_uword_t value);

/**
 * Count trailing zeros
 * @param value Value to examine
 * @return Number of trailing zero bits
 */
int bliss_count_trailing_zeros(bliss_uword_t value);

/**
 * Count set bits (population count)
 * @param value Value to examine
 * @return Number of 1 bits
 */
int bliss_popcount(bliss_uword_t value);

/* =================================================================
 * Signal Handling (BLISS-style exceptions)
 * ================================================================= */

/**
 * Signal handler function type
 * @param condition Signal condition code
 * @param arg Argument data
 * @return Non-zero to continue, zero to stop
 */
typedef int (*bliss_signal_handler_t)(bliss_signal_t condition, void *arg);

/**
 * Signal a condition
 * @param condition Condition code
 * @param arg Argument data
 */
void bliss_signal(bliss_signal_t condition, void *arg);

/**
 * Set signal handler
 * @param handler Handler function
 */
void bliss_set_signal_handler(bliss_signal_handler_t handler);

/**
 * Get current signal handler
 * @return Current handler function
 */
bliss_signal_handler_t bliss_get_signal_handler(void);

/* =================================================================
 * Runtime Utilities
 * ================================================================= */

/**
 * Initialize BLISS runtime
 * Call this at program startup
 */
void bliss_runtime_init(void);

/**
 * Cleanup BLISS runtime
 * Call this at program exit
 */
void bliss_runtime_cleanup(void);

/**
 * Abort program with error message
 * @param msg Error message
 */
void bliss_abort(const char *msg) __attribute__((noreturn));

/**
 * Get runtime version
 * @return Version string
 */
const char *bliss_runtime_version(void);

/* =================================================================
 * Debugging Support
 * ================================================================= */

/**
 * Dump a vector (for debugging)
 * @param vec Vector to dump
 */
void bliss_dump_vector(bliss_vector_t *vec);

/**
 * Dump a string (for debugging)
 * @param str String to dump
 */
void bliss_dump_string(bliss_string_t str);

/**
 * Enable debug tracing
 * @param enable Non-zero to enable, zero to disable
 */
void bliss_set_debug(int enable);

#ifdef __cplusplus
}
#endif

#endif /* BLISSRT_H */
