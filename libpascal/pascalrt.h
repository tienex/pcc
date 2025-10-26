/*
 * Copyright (c) 2025 PCC Pascal Runtime Library
 *
 * Pascal Runtime Support Library - Public Header
 *
 * This library provides runtime support for Pascal programs compiled with PCC.
 * It includes I/O operations, string handling, mathematical functions,
 * memory management, and runtime error handling.
 */

#ifndef _PASCALRT_H_
#define _PASCALRT_H_

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Pascal String Types
 *
 * Pascal strings are length-prefixed. We support both traditional
 * short strings (255 char max) and long strings (dynamic).
 */

/* Short string (Turbo Pascal/Delphi style) - max 255 chars */
typedef struct {
	uint8_t length;
	char data[256];
} PascalString;

/* Long string (dynamic, Delphi style) */
typedef struct {
	size_t length;
	size_t capacity;
	char *data;
} PascalLongString;

/* Text file type */
typedef struct {
	FILE *fp;
	char *name;
	int mode;        /* 0=closed, 1=read, 2=write */
	int eof_flag;
	int eoln_flag;
	char line_buffer[1024];
	size_t line_pos;
} PascalTextFile;

/* Binary file type */
typedef struct {
	FILE *fp;
	char *name;
	int mode;        /* 0=closed, 1=read, 2=write */
	size_t element_size;
} PascalFile;

/*
 * Runtime Error Codes
 */
typedef enum {
	PASCAL_ERR_NONE = 0,
	PASCAL_ERR_RANGE_CHECK = 1,      /* Value out of range */
	PASCAL_ERR_STACK_OVERFLOW = 2,   /* Stack overflow */
	PASCAL_ERR_HEAP_OVERFLOW = 3,    /* Heap overflow */
	PASCAL_ERR_INTEGER_OVERFLOW = 4, /* Integer overflow */
	PASCAL_ERR_DIVIDE_BY_ZERO = 5,   /* Division by zero */
	PASCAL_ERR_INVALID_OPERATION = 6,/* Invalid operation */
	PASCAL_ERR_FILE_NOT_FOUND = 7,   /* File not found */
	PASCAL_ERR_FILE_NOT_OPEN = 8,    /* File not open */
	PASCAL_ERR_DISK_FULL = 9,        /* Disk full */
	PASCAL_ERR_READ_ERROR = 10,      /* Read error */
	PASCAL_ERR_WRITE_ERROR = 11,     /* Write error */
	PASCAL_ERR_NIL_POINTER = 12,     /* Nil pointer dereference */
	PASCAL_ERR_INVALID_POINTER = 13, /* Invalid pointer */
} PascalError;

/*
 * Runtime Initialization and Cleanup
 */
void pascal_init_runtime(int argc, char **argv);
void pascal_fini_runtime(void);

/*
 * Error Handling
 */
void pascal_runtime_error(PascalError error, const char *message);
void pascal_set_error_handler(void (*handler)(PascalError, const char *));

/*
 * I/O Operations - Text Files
 */

/* Standard I/O */
void pascal_writeln(void);
void pascal_write_integer(int value);
void pascal_write_real(double value);
void pascal_write_char(char value);
void pascal_write_string(const char *str);
void pascal_write_boolean(int value);

void pascal_read_integer(int *value);
void pascal_read_real(double *value);
void pascal_read_char(char *value);
void pascal_read_string(char *str, size_t maxlen);
void pascal_readln(void);

/* File operations */
void pascal_assign(PascalTextFile *f, const char *filename);
void pascal_reset_text(PascalTextFile *f);
void pascal_rewrite_text(PascalTextFile *f);
void pascal_close_text(PascalTextFile *f);
void pascal_flush_text(PascalTextFile *f);

int pascal_eof_text(PascalTextFile *f);
int pascal_eoln_text(PascalTextFile *f);

void pascal_write_text_integer(PascalTextFile *f, int value);
void pascal_write_text_real(PascalTextFile *f, double value);
void pascal_write_text_char(PascalTextFile *f, char value);
void pascal_write_text_string(PascalTextFile *f, const char *str);
void pascal_writeln_text(PascalTextFile *f);

void pascal_read_text_integer(PascalTextFile *f, int *value);
void pascal_read_text_real(PascalTextFile *f, double *value);
void pascal_read_text_char(PascalTextFile *f, char *value);
void pascal_read_text_string(PascalTextFile *f, char *str, size_t maxlen);
void pascal_readln_text(PascalTextFile *f);

/*
 * I/O Operations - Binary Files
 */
void pascal_reset_file(PascalFile *f, size_t element_size);
void pascal_rewrite_file(PascalFile *f, size_t element_size);
void pascal_close_file(PascalFile *f);
int pascal_eof_file(PascalFile *f);
void pascal_read_file(PascalFile *f, void *buffer);
void pascal_write_file(PascalFile *f, const void *buffer);
void pascal_seek_file(PascalFile *f, long position);
long pascal_filepos(PascalFile *f);
long pascal_filesize(PascalFile *f);

/*
 * File Management
 */
void pascal_erase(const char *filename);
void pascal_rename_file(const char *oldname, const char *newname);

/*
 * String Operations
 */

/* Short string operations */
void pascal_str_assign(PascalString *dest, const char *src);
void pascal_str_concat(PascalString *dest, const PascalString *s1, const PascalString *s2);
void pascal_str_copy(PascalString *dest, const PascalString *src, int index, int count);
int pascal_str_pos(const PascalString *substr, const PascalString *str);
int pascal_str_length(const PascalString *str);
void pascal_str_upcase(PascalString *str);
void pascal_str_lowercase(PascalString *str);
void pascal_str_trim(PascalString *str);
void pascal_str_insert(PascalString *str, const PascalString *substr, int index);
void pascal_str_delete(PascalString *str, int index, int count);
int pascal_str_compare(const PascalString *s1, const PascalString *s2);
int pascal_str_compare_nocase(const PascalString *s1, const PascalString *s2);

/* Long string operations */
PascalLongString *pascal_lstr_create(const char *src);
void pascal_lstr_free(PascalLongString *str);
void pascal_lstr_assign(PascalLongString *dest, const char *src);
void pascal_lstr_concat(PascalLongString *dest, const PascalLongString *s1, const PascalLongString *s2);
void pascal_lstr_setlength(PascalLongString *str, size_t newlen);

/*
 * Mathematical Functions
 */
int pascal_abs_int(int x);
double pascal_abs_real(double x);
int pascal_sqr_int(int x);
double pascal_sqr_real(double x);
double pascal_sqrt(double x);
double pascal_sin(double x);
double pascal_cos(double x);
double pascal_arctan(double x);
double pascal_exp(double x);
double pascal_ln(double x);
int pascal_round(double x);
int pascal_trunc(double x);
int pascal_odd(int x);

/*
 * Ordinal Operations
 */
int pascal_ord_char(char c);
int pascal_ord_bool(int b);
char pascal_chr(int x);
int pascal_succ_int(int x);
int pascal_pred_int(int x);
char pascal_succ_char(char c);
char pascal_pred_char(char c);

/*
 * Memory Management
 */
void *pascal_new(size_t size);
void pascal_dispose(void *ptr);
void *pascal_getmem(size_t size);
void pascal_freemem(void *ptr, size_t size);
void pascal_fillchar(void *dest, size_t count, uint8_t value);
void pascal_move(const void *src, void *dest, size_t count);

/*
 * Type Conversion
 */
void pascal_int_to_str(int value, char *str);
void pascal_real_to_str(double value, char *str);
int pascal_str_to_int(const char *str, int *error);
double pascal_str_to_real(const char *str, int *error);

/*
 * Utility Functions (Turbo/Delphi extensions)
 */
void pascal_inc_int(int *x, int delta);
void pascal_dec_int(int *x, int delta);
uint8_t pascal_hi_byte(uint16_t x);
uint8_t pascal_lo_byte(uint16_t x);
uint16_t pascal_swap_word(uint16_t x);

/*
 * Set Operations (for Pascal set type)
 */
typedef uint32_t PascalSet[8];  /* Supports sets of 0..255 */

void pascal_set_init(PascalSet s);
void pascal_set_add(PascalSet s, int element);
void pascal_set_remove(PascalSet s, int element);
int pascal_set_contains(const PascalSet s, int element);
void pascal_set_union(PascalSet result, const PascalSet s1, const PascalSet s2);
void pascal_set_intersection(PascalSet result, const PascalSet s1, const PascalSet s2);
void pascal_set_difference(PascalSet result, const PascalSet s1, const PascalSet s2);
int pascal_set_equal(const PascalSet s1, const PascalSet s2);
int pascal_set_subset(const PascalSet s1, const PascalSet s2);

/*
 * Range Checking
 */
int pascal_range_check_int(int value, int min, int max);
char pascal_range_check_char(int value);

/*
 * Standard Input/Output File Handles
 */
extern PascalTextFile pascal_input;
extern PascalTextFile pascal_output;

#ifdef __cplusplus
}
#endif

#endif /* _PASCALRT_H_ */
