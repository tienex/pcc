/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * Runtime library header for PL/I programs
 */

#ifndef PLI_RUNTIME_H
#define PLI_RUNTIME_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

/* PL/I data types */
typedef int32_t pli_fixed_t;
typedef int64_t pli_fixed_long_t;
typedef float pli_float_t;
typedef double pli_float_long_t;
typedef uint8_t pli_bit_t;
typedef char pli_char_t;

/* String descriptor for VARYING strings */
typedef struct {
	uint16_t length;
	uint16_t max_length;
	char *data;
} pli_string_t;

/* Array descriptor */
typedef struct {
	void *data;
	int32_t ndims;
	int32_t *lower_bounds;
	int32_t *upper_bounds;
	size_t elem_size;
} pli_array_t;

/* File descriptor for PL/I files */
typedef struct {
	FILE *fp;
	char *filename;
	int mode;  /* 0=closed, 1=input, 2=output, 3=update */
	int type;  /* 0=stream, 1=record */
	int is_open;
} pli_file_t;

/* Condition handling */
typedef struct {
	const char *name;
	void (*handler)(void);
	int enabled;
} pli_condition_t;

/* Global state */
extern pli_file_t *pli_sysin;
extern pli_file_t *pli_sysprint;
extern int pli_error_code;
extern char pli_error_msg[256];

/* Startup/shutdown */
void pli_init(void);
void pli_finish(void);
void pli_stop(void);
void pli_exit(int code);

/* I/O functions */
void pli_put_skip(void);
void pli_put_page(void);
void pli_put_line(void);
void pli_put_string(const char *s);
void pli_put_fixed(pli_fixed_t n);
void pli_put_float(pli_float_long_t f);
void pli_put_char(char c);

void pli_get_skip(void);
void pli_get_page(void);
void pli_get_string(char *buf, size_t maxlen);
pli_fixed_t pli_get_fixed(void);
pli_float_long_t pli_get_float(void);
char pli_get_char(void);

pli_file_t *pli_file_open(const char *filename, int mode, int type);
void pli_file_close(pli_file_t *f);
void pli_file_read(pli_file_t *f, void *buf, size_t len);
void pli_file_write(pli_file_t *f, const void *buf, size_t len);

/* Mathematical functions */
pli_fixed_t pli_abs_fixed(pli_fixed_t x);
pli_float_long_t pli_abs_float(pli_float_long_t x);
pli_fixed_t pli_ceil(pli_float_long_t x);
pli_fixed_t pli_floor(pli_float_long_t x);
pli_fixed_t pli_max_fixed(pli_fixed_t a, pli_fixed_t b);
pli_fixed_t pli_min_fixed(pli_fixed_t a, pli_fixed_t b);
pli_float_long_t pli_max_float(pli_float_long_t a, pli_float_long_t b);
pli_float_long_t pli_min_float(pli_float_long_t a, pli_float_long_t b);
pli_fixed_t pli_mod(pli_fixed_t a, pli_fixed_t b);
pli_fixed_t pli_round(pli_float_long_t x);
pli_fixed_t pli_sign(pli_fixed_t x);
pli_fixed_t pli_trunc(pli_float_long_t x);

/* Trigonometric functions */
pli_float_long_t pli_acos(pli_float_long_t x);
pli_float_long_t pli_asin(pli_float_long_t x);
pli_float_long_t pli_atan(pli_float_long_t x);
pli_float_long_t pli_atanh(pli_float_long_t x);
pli_float_long_t pli_cos(pli_float_long_t x);
pli_float_long_t pli_cosh(pli_float_long_t x);
pli_float_long_t pli_sin(pli_float_long_t x);
pli_float_long_t pli_sinh(pli_float_long_t x);
pli_float_long_t pli_tan(pli_float_long_t x);
pli_float_long_t pli_tanh(pli_float_long_t x);

/* Exponential and logarithmic */
pli_float_long_t pli_exp(pli_float_long_t x);
pli_float_long_t pli_log(pli_float_long_t x);
pli_float_long_t pli_log10(pli_float_long_t x);
pli_float_long_t pli_log2(pli_float_long_t x);
pli_float_long_t pli_sqrt(pli_float_long_t x);

/* String functions */
pli_fixed_t pli_index(const char *haystack, const char *needle);
pli_fixed_t pli_length(const char *s);
void pli_substr(char *dest, const char *src, pli_fixed_t start, pli_fixed_t len);
void pli_repeat(char *dest, const char *s, pli_fixed_t count);
void pli_trim(char *dest, const char *src);
pli_fixed_t pli_verify(const char *s, const char *set);
void pli_concat(char *dest, const char *s1, const char *s2);

/* String creation/destruction */
pli_string_t *pli_string_create(size_t max_len);
void pli_string_destroy(pli_string_t *s);
void pli_string_assign(pli_string_t *dest, const char *src);
void pli_string_concat(pli_string_t *dest, const char *s1, const char *s2);

/* Bit string functions */
pli_bit_t pli_bool(pli_bit_t a, pli_bit_t b, pli_bit_t op);
pli_bit_t pli_high(size_t n);
pli_bit_t pli_low(size_t n);

/* Memory management */
void *pli_allocate(size_t size);
void pli_free(void *ptr);
void *pli_addr(void *var);
void *pli_null(void);
size_t pli_size(void *ptr);
pli_fixed_t pli_allocation(void *ptr);

/* Array functions */
pli_fixed_t pli_dim(pli_array_t *arr, pli_fixed_t dimension);
pli_fixed_t pli_hbound(pli_array_t *arr, pli_fixed_t dimension);
pli_fixed_t pli_lbound(pli_array_t *arr, pli_fixed_t dimension);
pli_fixed_t pli_sum(pli_array_t *arr);
pli_fixed_t pli_prod(pli_array_t *arr);

pli_array_t *pli_array_create(int32_t ndims, int32_t *lower, int32_t *upper, size_t elem_size);
void pli_array_destroy(pli_array_t *arr);

/* Type conversion functions */
pli_fixed_t pli_binary(const char *s);
void pli_bit(char *dest, pli_fixed_t value, size_t len);
void pli_char(char *dest, pli_fixed_t value);
pli_fixed_t pli_decimal(const char *s);
pli_fixed_t pli_fixed(pli_float_long_t x);
pli_float_long_t pli_float(pli_fixed_t x);
void pli_unspec(pli_bit_t *dest, const void *src, size_t len);

/* Condition handling */
void pli_on(const char *condition, void (*handler)(void));
void pli_signal(const char *condition);
void pli_revert(const char *condition);

/* Standard conditions */
void pli_condition_error(void);
void pli_condition_endfile(void);
void pli_condition_conversion(void);
void pli_condition_fixedoverflow(void);
void pli_condition_zerodivide(void);

/* Condition query functions */
pli_fixed_t pli_oncode(void);
void pli_onchar(char *dest);
void pli_onsource(char *dest);
void pli_onfile(char *dest);
void pli_onkey(char *dest);

/* PL/M specific functions */
uint8_t pli_input(uint16_t port);
void pli_output(uint16_t port, uint8_t value);
uint8_t pli_shl(uint8_t value, uint8_t count);
uint8_t pli_shr(uint8_t value, uint8_t count);
uint8_t pli_rol(uint8_t value, uint8_t count);
uint8_t pli_ror(uint8_t value, uint8_t count);
uint8_t pli_high_byte(uint16_t value);
uint8_t pli_low_byte(uint16_t value);
uint16_t pli_double(uint8_t low, uint8_t high);
uint8_t pli_last(void *array);
void pli_move(void *dest, const void *src, size_t count);

/* Time/date functions */
void pli_date(char *dest);
void pli_time(char *dest);
void pli_datetime(char *dest);

#endif /* PLI_RUNTIME_H */
