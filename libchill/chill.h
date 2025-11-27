/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL runtime library header
 * CCITT High Level Language (Z.200) runtime support
 */

#ifndef CHILL_H
#define CHILL_H

#include <stdint.h>
#include <stddef.h>

/*
 * CHILL basic types
 */
typedef int32_t chill_int_t;
typedef uint32_t chill_uint_t;
typedef int64_t chill_long_t;
typedef uint64_t chill_ulong_t;
typedef uint8_t chill_bool_t;
typedef char chill_char_t;
typedef float chill_real_t;
typedef double chill_long_real_t;

/* Boolean values */
#define CHILL_TRUE  1
#define CHILL_FALSE 0
#define CHILL_NULL  ((void *)0)

/*
 * String types
 */
typedef struct {
	char *data;
	size_t length;
	size_t capacity;
} chill_chars_t;

typedef struct {
	uint8_t *data;
	size_t length;
	size_t capacity;
} chill_bits_t;

/*
 * Time types
 */
typedef int64_t chill_duration_t;  /* Duration in microseconds */
typedef int64_t chill_time_t;      /* Absolute time */

/*
 * I/O Functions
 */
void chill_read_int(chill_int_t *value);
void chill_read_real(chill_real_t *value);
void chill_read_char(chill_char_t *value);
void chill_read_chars(chill_chars_t *str);

void chill_write_int(chill_int_t value);
void chill_write_real(chill_real_t value);
void chill_write_char(chill_char_t value);
void chill_write_chars(const chill_chars_t *str);
void chill_write_string(const char *str);
void chill_writeln(void);

/*
 * String Functions
 */
chill_chars_t *chill_chars_create(const char *str);
chill_chars_t *chill_chars_alloc(size_t length);
void chill_chars_free(chill_chars_t *str);
size_t chill_chars_length(const chill_chars_t *str);
chill_chars_t *chill_chars_concat(const chill_chars_t *s1, const chill_chars_t *s2);
chill_chars_t *chill_chars_substr(const chill_chars_t *str, size_t start, size_t len);
chill_chars_t *chill_chars_upper(const chill_chars_t *str);
chill_chars_t *chill_chars_lower(const chill_chars_t *str);
int chill_chars_compare(const chill_chars_t *s1, const chill_chars_t *s2);

/*
 * Mathematical Functions
 */
chill_int_t chill_abs_int(chill_int_t x);
chill_real_t chill_abs_real(chill_real_t x);
chill_int_t chill_sign_int(chill_int_t x);
chill_real_t chill_sign_real(chill_real_t x);
chill_int_t chill_max_int(chill_int_t a, chill_int_t b);
chill_int_t chill_min_int(chill_int_t a, chill_int_t b);
chill_real_t chill_max_real(chill_real_t a, chill_real_t b);
chill_real_t chill_min_real(chill_real_t a, chill_real_t b);

/* Trigonometric functions */
chill_real_t chill_sin(chill_real_t x);
chill_real_t chill_cos(chill_real_t x);
chill_real_t chill_tan(chill_real_t x);
chill_real_t chill_arcsin(chill_real_t x);
chill_real_t chill_arccos(chill_real_t x);
chill_real_t chill_arctan(chill_real_t x);

/* Exponential and logarithmic functions */
chill_real_t chill_sqrt(chill_real_t x);
chill_real_t chill_exp(chill_real_t x);
chill_real_t chill_ln(chill_real_t x);
chill_real_t chill_log10(chill_real_t x);
chill_real_t chill_pow(chill_real_t base, chill_real_t exp);

/*
 * Type Conversion Functions
 */
chill_int_t chill_num(chill_char_t ch);
chill_char_t chill_char(chill_int_t n);
chill_int_t chill_pred(chill_int_t x);
chill_int_t chill_succ(chill_int_t x);
chill_int_t chill_real_to_int(chill_real_t x);
chill_real_t chill_int_to_real(chill_int_t x);

/*
 * Memory Management
 */
void *chill_allocate(size_t size);
void chill_free(void *ptr);

/*
 * Process/Concurrency Support (stubs for basic implementation)
 */
typedef struct chill_process chill_process_t;

chill_process_t *chill_process_create(void (*func)(void *), void *arg);
void chill_process_start(chill_process_t *proc);
void chill_process_stop(chill_process_t *proc);
void chill_delay(chill_duration_t duration);

/* Signal/message passing (simplified) */
typedef struct chill_signal chill_signal_t;

chill_signal_t *chill_signal_create(const char *name);
void chill_send(chill_signal_t *sig, void *data, size_t size);
int chill_receive(chill_signal_t *sig, void *buffer, size_t size, chill_duration_t timeout);

/*
 * Set Operations (for POWERSET type)
 */
typedef struct {
	uint64_t *bits;
	size_t size;
} chill_powerset_t;

chill_powerset_t *chill_powerset_create(size_t max_elem);
void chill_powerset_free(chill_powerset_t *set);
void chill_powerset_add(chill_powerset_t *set, chill_int_t elem);
void chill_powerset_remove(chill_powerset_t *set, chill_int_t elem);
chill_bool_t chill_powerset_contains(const chill_powerset_t *set, chill_int_t elem);
chill_powerset_t *chill_powerset_union(const chill_powerset_t *s1, const chill_powerset_t *s2);
chill_powerset_t *chill_powerset_intersection(const chill_powerset_t *s1, const chill_powerset_t *s2);
chill_powerset_t *chill_powerset_difference(const chill_powerset_t *s1, const chill_powerset_t *s2);

/*
 * Exception Handling (simplified)
 */
typedef enum {
	CHILL_EXC_NONE = 0,
	CHILL_EXC_RANGEFAIL,
	CHILL_EXC_EMPTY,
	CHILL_EXC_OVERFLOW,
	CHILL_EXC_ZERODIVIDE,
	CHILL_EXC_INVALIDOP,
	CHILL_EXC_OUTOFMEM,
	CHILL_EXC_IOERROR
} chill_exception_t;

void chill_raise(chill_exception_t exc, const char *msg);
const char *chill_exception_name(chill_exception_t exc);

/*
 * Runtime initialization/cleanup
 */
void chill_runtime_init(void);
void chill_runtime_cleanup(void);

#endif /* CHILL_H */
