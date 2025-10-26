/*
 * Copyright (c) 2025 PCC ALGOL 60+ Runtime Library
 *
 * Runtime library header for ALGOL 60+ programs
 * Provides I/O, mathematical functions, and runtime support
 */

#ifndef ALGOL60_H
#define ALGOL60_H

#include <stddef.h>
#include <stdint.h>

/* ALGOL 60 types */
typedef int32_t algol_integer;
typedef double algol_real;
typedef int32_t algol_boolean;
typedef char *algol_string;

/* Boolean constants */
#define ALGOL_TRUE  1
#define ALGOL_FALSE 0

/* Array descriptor for multi-dimensional arrays */
typedef struct {
	void *data;          /* Pointer to array data */
	int ndims;           /* Number of dimensions */
	int *lower_bounds;   /* Lower bounds for each dimension */
	int *upper_bounds;   /* Upper bounds for each dimension */
	size_t elem_size;    /* Size of each element */
	size_t total_size;   /* Total allocated size */
} algol_array_t;

/*
 * Runtime initialization and cleanup
 */
void algol_init(int argc, char **argv);
void algol_fini(void);

/*
 * I/O Functions
 */

/* Output functions */
void algol_write_integer(algol_integer value);
void algol_write_real(algol_real value);
void algol_write_boolean(algol_boolean value);
void algol_write_string(const char *str);
void algol_write_newline(void);

/* Print is an alias for write */
#define algol_print_integer algol_write_integer
#define algol_print_real algol_write_real
#define algol_print_boolean algol_write_boolean
#define algol_print_string algol_write_string

/* Input functions */
algol_integer algol_read_integer(void);
algol_real algol_read_real(void);
algol_boolean algol_read_boolean(void);
algol_string algol_read_string(void);

/*
 * Mathematical Functions (ALGOL 60 transfer functions)
 */

/* Absolute value */
algol_integer algol_abs_integer(algol_integer x);
algol_real algol_abs_real(algol_real x);

/* Sign function: returns -1, 0, or 1 */
algol_integer algol_sign_integer(algol_integer x);
algol_integer algol_sign_real(algol_real x);

/* Square root */
algol_real algol_sqrt(algol_real x);

/* Trigonometric functions */
algol_real algol_sin(algol_real x);
algol_real algol_cos(algol_real x);
algol_real algol_arctan(algol_real x);
algol_real algol_tan(algol_real x);

/* Exponential and logarithm */
algol_real algol_exp(algol_real x);
algol_real algol_ln(algol_real x);
algol_real algol_log10(algol_real x);

/* Entier function (floor - greatest integer <= x) */
algol_integer algol_entier(algol_real x);

/* Power function */
algol_real algol_power(algol_real base, algol_real exponent);
algol_integer algol_power_int(algol_integer base, algol_integer exponent);

/*
 * String Operations
 */

/* String creation and destruction */
algol_string algol_string_create(const char *str);
algol_string algol_string_create_n(const char *str, size_t len);
void algol_string_free(algol_string str);

/* String operations */
size_t algol_string_length(algol_string str);
algol_string algol_string_concat(algol_string s1, algol_string s2);
algol_string algol_string_substring(algol_string str, int start, int length);
int algol_string_compare(algol_string s1, algol_string s2);

/*
 * Array Operations
 */

/* Array allocation and deallocation */
algol_array_t *algol_array_create(int ndims, int *lower_bounds, int *upper_bounds, size_t elem_size);
void algol_array_free(algol_array_t *array);

/* Array element access - returns pointer to element */
void *algol_array_element(algol_array_t *array, int *indices);

/* Array bounds checking */
int algol_array_bounds_check(algol_array_t *array, int dim, int index);

/*
 * Runtime Error Handling
 */

/* Error codes */
#define ALGOL_ERR_NONE           0
#define ALGOL_ERR_BOUNDS         1  /* Array bounds error */
#define ALGOL_ERR_DOMAIN         2  /* Math domain error (e.g., sqrt(-1)) */
#define ALGOL_ERR_RANGE          3  /* Math range error (overflow) */
#define ALGOL_ERR_DIV_ZERO       4  /* Division by zero */
#define ALGOL_ERR_NULL_PTR       5  /* Null pointer dereference */
#define ALGOL_ERR_MEMORY         6  /* Memory allocation failure */
#define ALGOL_ERR_IO             7  /* I/O error */
#define ALGOL_ERR_TYPE           8  /* Type error */

/* Error reporting */
void algol_error(int code, const char *message, ...);
void algol_fatal_error(int code, const char *message, ...);

/*
 * Type Conversion
 */

/* Integer to real */
algol_real algol_int_to_real(algol_integer i);

/* Real to integer (truncation) */
algol_integer algol_real_to_int(algol_real r);

/* Boolean to integer */
algol_integer algol_bool_to_int(algol_boolean b);

/* Integer to boolean */
algol_boolean algol_int_to_bool(algol_integer i);

/*
 * Utility Functions
 */

/* Random number generation (extension) */
algol_real algol_random(void);
algol_integer algol_random_int(algol_integer min, algol_integer max);

/* Time functions (extension) */
algol_real algol_time(void);  /* Returns time in seconds since epoch */

/*
 * Memory Management
 */

/* General allocation */
void *algol_malloc(size_t size);
void *algol_calloc(size_t nmemb, size_t size);
void *algol_realloc(void *ptr, size_t size);
void algol_free(void *ptr);

/* Memory statistics (for debugging) */
size_t algol_memory_allocated(void);
size_t algol_memory_peak(void);

/*
 * Formatted I/O (extensions)
 */

/* Formatted output (similar to printf) */
void algol_writef(const char *format, ...);

/* Formatted input (similar to scanf) */
int algol_readf(const char *format, ...);

/*
 * Procedure/Function Call Support
 */

/* Thunk structure for call-by-name (Jensen's device) */
typedef struct algol_thunk {
	void *(*evaluate)(void *context);  /* Evaluation function */
	void *context;                      /* Context data */
} algol_thunk_t;

/* Evaluate a thunk */
void *algol_thunk_eval(algol_thunk_t *thunk);

/*
 * Exception Handling (extensions)
 */

typedef void (*algol_exception_handler_t)(int code, const char *message);

/* Set custom exception handler */
void algol_set_exception_handler(algol_exception_handler_t handler);

/* Get current exception handler */
algol_exception_handler_t algol_get_exception_handler(void);

#endif /* ALGOL60_H */
