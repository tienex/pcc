/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * Main header file for Xbase++ runtime library
 */

#ifndef XBRT_H
#define XBRT_H

#include <stdint.h>
#include <stddef.h>
#include <time.h>

/* Xbase++ value types */
typedef enum {
	XB_NULL = 0,
	XB_NUMERIC,
	XB_CHARACTER,
	XB_DATE,
	XB_LOGICAL,
	XB_ARRAY,
	XB_OBJECT,
	XB_CODEBLOCK,
	XB_MEMO,
	XB_NIL
} xb_type_t;

/* Forward declarations */
typedef struct xb_value xb_value_t;
typedef struct xb_array xb_array_t;
typedef struct xb_object xb_object_t;
typedef struct xb_codeblock xb_codeblock_t;

/* Xbase++ value structure */
struct xb_value {
	xb_type_t type;
	union {
		double numeric;         /* Numeric value */
		char *string;           /* Character string */
		int32_t date;           /* Date value (Julian day number) */
		int logical;            /* Logical value (0 = .F., 1 = .T.) */
		xb_array_t *array;      /* Array pointer */
		xb_object_t *object;    /* Object pointer */
		xb_codeblock_t *block;  /* Code block pointer */
	} data;
};

/* Array structure */
struct xb_array {
	size_t length;          /* Number of elements */
	size_t capacity;        /* Allocated capacity */
	xb_value_t *elements;   /* Array elements */
};

/* Object structure */
struct xb_object {
	char *class_name;       /* Class name */
	size_t field_count;     /* Number of fields */
	char **field_names;     /* Field names */
	xb_value_t *fields;     /* Field values */
};

/* Code block structure */
struct xb_codeblock {
	void *code;             /* Compiled code pointer */
	size_t param_count;     /* Number of parameters */
	xb_value_t *locals;     /* Local variables */
};

/* Value creation and manipulation */
xb_value_t *xb_value_new(xb_type_t type);
xb_value_t *xb_value_new_numeric(double value);
xb_value_t *xb_value_new_string(const char *str);
xb_value_t *xb_value_new_date(int32_t julian);
xb_value_t *xb_value_new_logical(int value);
xb_value_t *xb_value_new_array(size_t size);
xb_value_t *xb_value_new_nil(void);
void xb_value_free(xb_value_t *val);
xb_value_t *xb_value_clone(const xb_value_t *val);

/* Type conversion */
double xb_value_to_numeric(const xb_value_t *val);
char *xb_value_to_string(const xb_value_t *val);
int xb_value_to_logical(const xb_value_t *val);
const char *xb_value_type_name(const xb_value_t *val);

/* String functions */
int xb_len(const char *str);
char *xb_substr(const char *str, int start, int len);
char *xb_left(const char *str, int len);
char *xb_right(const char *str, int len);
char *xb_upper(const char *str);
char *xb_lower(const char *str);
char *xb_trim(const char *str);
char *xb_ltrim(const char *str);
char *xb_rtrim(const char *str);
char *xb_alltrim(const char *str);
char *xb_space(int n);
char *xb_replicate(const char *str, int n);
char *xb_stuff(const char *str, int pos, int del, const char *ins);
int xb_at(const char *needle, const char *haystack);
int xb_rat(const char *needle, const char *haystack);
char *xb_strtran(const char *str, const char *find, const char *replace);
char *xb_chr(int code);
int xb_asc(const char *str);
int xb_isalpha(int c);
int xb_isdigit(int c);

/* Numeric functions */
double xb_abs(double x);
double xb_int(double x);
double xb_round(double x, int decimals);
double xb_sqrt(double x);
double xb_exp(double x);
double xb_log(double x);
double xb_log10(double x);
double xb_sin(double x);
double xb_cos(double x);
double xb_tan(double x);
double xb_min(double a, double b);
double xb_max(double a, double b);
double xb_mod(double a, double b);
double xb_rand(void);

/* Array functions */
int xb_alen(const xb_array_t *arr);
void xb_asize(xb_array_t *arr, int new_size);
void xb_aadd(xb_array_t *arr, const xb_value_t *val);
void xb_ains(xb_array_t *arr, int pos);
void xb_adel(xb_array_t *arr, int pos);
void xb_asort(xb_array_t *arr);
int xb_ascan(const xb_array_t *arr, const xb_value_t *val);
void xb_afill(xb_array_t *arr, const xb_value_t *val, int start, int count);
xb_array_t *xb_aclone(const xb_array_t *arr);

/* Date/Time functions */
int32_t xb_date(void);
int xb_year(int32_t date);
int xb_month(int32_t date);
int xb_day(int32_t date);
int xb_dow(int32_t date);
char *xb_cdow(int32_t date);
char *xb_cmonth(int32_t date);
int32_t xb_ctod(const char *str);
char *xb_dtoc(int32_t date);
char *xb_dtos(int32_t date);
int32_t xb_stod(const char *str);

/* Type conversion functions */
char *xb_str(double num, int width, int decimals);
double xb_val(const char *str);
char *xb_transform(const xb_value_t *val, const char *format);

/* I/O functions */
int xb_fopen(const char *filename, int mode);
int xb_fcreate(const char *filename, int attr);
void xb_fclose(int handle);
int xb_fread(int handle, char *buffer, int bytes);
int xb_fwrite(int handle, const char *buffer, int bytes);
long xb_fseek(int handle, long offset, int whence);
int xb_ferror(void);
int xb_file(const char *filename);
int xb_ferase(const char *filename);
int xb_frename(const char *oldname, const char *newname);
char *xb_memoread(const char *filename);
int xb_memowrit(const char *filename, const char *content);

/* Type checking functions */
char xb_valtype(const xb_value_t *val);
int xb_isnil(const xb_value_t *val);
int xb_isnumber(const xb_value_t *val);
int xb_ischaracter(const xb_value_t *val);
int xb_islogical(const xb_value_t *val);
int xb_isdate(const xb_value_t *val);
int xb_isarray(const xb_value_t *val);
int xb_isobject(const xb_value_t *val);
int xb_isblock(const xb_value_t *val);

/* Misc functions */
int xb_empty(const xb_value_t *val);
void xb_qout(const xb_value_t *val);

/* Utility functions */
int32_t xb_julian_from_ymd(int year, int month, int day);
void xb_ymd_from_julian(int32_t julian, int *year, int *month, int *day);

/* OOP Support */
typedef struct class_def CLASS_DEF;
CLASS_DEF *xb_register_class(const char *name, const char *parent_name);
void xb_class_add_field(CLASS_DEF *cls, const char *name);
void xb_class_add_method(CLASS_DEF *cls, const char *name, void (*func)(void));
xb_object_t *xb_object_new(const char *class_name);
xb_value_t *xb_object_get_field(xb_object_t *obj, const char *name);
void xb_object_set_field(xb_object_t *obj, const char *name, const xb_value_t *val);
xb_value_t *xb_object_call_method(xb_object_t *obj, const char *method_name, xb_value_t **args, int arg_count);
void xb_object_free(xb_object_t *obj);

/* ARC (Automatic Reference Counting) */
void *xb_arc_alloc(size_t size, void (*destructor)(void*));
void *xb_arc_retain(void *ptr);
void xb_arc_release(void *ptr);
size_t xb_arc_ref_count(void *ptr);

typedef struct autorelease_pool AUTORELEASE_POOL;
AUTORELEASE_POOL *xb_arc_pool_create(void);
void *xb_arc_autorelease(void *ptr);
void xb_arc_pool_drain(AUTORELEASE_POOL *pool);
xb_value_t *xb_value_new_arc(xb_type_t type);

#endif /* XBRT_H */
