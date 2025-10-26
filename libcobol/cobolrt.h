/*
 * COBOL Runtime Library - Internal Header
 * Supports OO COBOL with multi-vendor dialects
 */

#ifndef _COBOLRT_H
#define _COBOLRT_H

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

/* COBOL data types */
typedef struct cobol_field {
	void *data;           /* Pointer to data */
	size_t size;          /* Size in bytes */
	int type;             /* Field type */
	int digits;           /* Total digits */
	int scale;            /* Decimal places */
	int sign;             /* Signed flag */
} cobol_field_t;

/* Field types */
#define COB_TYPE_NUMERIC        1
#define COB_TYPE_ALPHABETIC     2
#define COB_TYPE_ALPHANUMERIC   3
#define COB_TYPE_EDITED         4
#define COB_TYPE_POINTER        5
#define COB_TYPE_OBJECT         6

/* File organization types */
#define COB_ORG_SEQUENTIAL      1
#define COB_ORG_INDEXED         2
#define COB_ORG_RELATIVE        3

/* File access modes */
#define COB_ACCESS_SEQUENTIAL   1
#define COB_ACCESS_RANDOM       2
#define COB_ACCESS_DYNAMIC      3

/* File open modes */
#define COB_OPEN_INPUT   1
#define COB_OPEN_OUTPUT  2
#define COB_OPEN_IO      3
#define COB_OPEN_EXTEND  4

/* File handle */
typedef struct cobol_file {
	FILE *fp;
	char *filename;
	int organization;
	int access_mode;
	int open_mode;
	int status;
	void *key_field;
	size_t record_size;
	long current_record;
} cobol_file_t;

/* OO COBOL object */
typedef struct cobol_object {
	void *vtable;         /* Virtual method table */
	void *data;           /* Object data */
	char *class_name;     /* Class name */
	struct cobol_object *parent; /* Parent object */
	int ref_count;        /* Reference count */
} cobol_object_t;

/* Decimal number (for packed decimal) */
typedef struct cobol_decimal {
	uint8_t digits[31];   /* BCD digits */
	int length;           /* Number of digits */
	int scale;            /* Decimal places */
	int sign;             /* Sign (0=positive, 1=negative) */
} cobol_decimal_t;

/* Error codes */
#define COB_STATUS_SUCCESS      0
#define COB_STATUS_EOF          10
#define COB_STATUS_NO_RECORD    23
#define COB_STATUS_KEY_EXISTS   22
#define COB_STATUS_KEY_INVALID  23
#define COB_STATUS_FILE_OPEN    41
#define COB_STATUS_FILE_CLOSED  42

/* I/O functions */
void __cobol_accept(cobol_field_t *field);
void __cobol_display(cobol_field_t *field);
void __cobol_display_line(cobol_field_t *fields[], int count);

/* Numeric operations */
void __cobol_add(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2);
void __cobol_subtract(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2);
void __cobol_multiply(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2);
void __cobol_divide(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2);
void __cobol_compute(cobol_field_t *result, double value);

/* Numeric conversions */
int __cobol_get_int(cobol_field_t *field);
long long __cobol_get_long(cobol_field_t *field);
double __cobol_get_double(cobol_field_t *field);
void __cobol_set_int(cobol_field_t *field, int value);
void __cobol_set_long(cobol_field_t *field, long long value);
void __cobol_set_double(cobol_field_t *field, double value);

/* Packed decimal operations */
void __cobol_pack(cobol_decimal_t *dec, const char *str);
void __cobol_unpack(const cobol_decimal_t *dec, char *str);
void __cobol_decimal_add(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2);
void __cobol_decimal_subtract(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2);
void __cobol_decimal_multiply(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2);
void __cobol_decimal_divide(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2);

/* String operations */
void __cobol_move(cobol_field_t *dst, cobol_field_t *src);
void __cobol_move_all(cobol_field_t *dst, cobol_field_t *src);
void __cobol_string(cobol_field_t *dst, cobol_field_t *sources[], int count);
void __cobol_unstring(cobol_field_t *src, cobol_field_t *dests[], int count, const char *delim);
void __cobol_inspect_replacing(cobol_field_t *field, const char *pattern, const char *replacement);
int __cobol_compare(cobol_field_t *f1, cobol_field_t *f2);

/* File I/O */
cobol_file_t *__cobol_file_open(const char *filename, int mode, int org, int access);
int __cobol_file_close(cobol_file_t *file);
int __cobol_file_read(cobol_file_t *file, cobol_field_t *record);
int __cobol_file_write(cobol_file_t *file, cobol_field_t *record);
int __cobol_file_rewrite(cobol_file_t *file, cobol_field_t *record);
int __cobol_file_delete(cobol_file_t *file);
int __cobol_file_start(cobol_file_t *file, cobol_field_t *key, int operator);

/* OO support */
cobol_object_t *__cobol_object_new(const char *class_name, size_t size);
void __cobol_object_free(cobol_object_t *obj);
void __cobol_object_retain(cobol_object_t *obj);
void __cobol_object_release(cobol_object_t *obj);
void *__cobol_invoke(cobol_object_t *obj, const char *method_name, void **args);

/* Intrinsic functions */
int __cobol_length(cobol_field_t *field);
void __cobol_upper_case(cobol_field_t *dst, cobol_field_t *src);
void __cobol_lower_case(cobol_field_t *dst, cobol_field_t *src);
void __cobol_reverse(cobol_field_t *dst, cobol_field_t *src);
int __cobol_numval(cobol_field_t *field);
void __cobol_current_date(cobol_field_t *field);

/* Memory management */
void *__cobol_malloc(size_t size);
void __cobol_free(void *ptr);
void *__cobol_realloc(void *ptr, size_t size);

/* Error handling */
void __cobol_set_exception(int code, const char *message);
int __cobol_get_exception(void);
const char *__cobol_get_exception_message(void);
void __cobol_fatal_error(const char *message);

/* Utility functions */
void __cobol_init(void);
void __cobol_cleanup(void);
void __cobol_set_trace(int enabled);

#endif /* _COBOLRT_H */
