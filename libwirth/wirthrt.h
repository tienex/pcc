/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Unified Runtime Library for Wirth Languages and Ada
 *
 * This library provides runtime support for:
 * - Pascal (ISO 7185, ISO 10206, UCSD, Turbo, Borland, Delphi, Free Pascal)
 * - Modula-2 (PIM2, PIM3, PIM4, ISO, GNU)
 * - Modula-3
 * - Oberon (Oberon, Oberon-2, Oberon-07, Component Pascal, Active Oberon)
 * - Ada (Ada 83, Ada 95, Ada 2005, Ada 2012, Ada 2022)
 *
 * Provides full interoperability between all supported languages.
 */

#ifndef _WIRTHRT_H_
#define _WIRTHRT_H_

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Common Types for All Languages
 */

/* Integer types (common across all languages) */
typedef int8_t   WIRTH_INT8;
typedef int16_t  WIRTH_INT16;
typedef int32_t  WIRTH_INT32;
typedef int64_t  WIRTH_INT64;
typedef uint8_t  WIRTH_CARD8;
typedef uint16_t WIRTH_CARD16;
typedef uint32_t WIRTH_CARD32;
typedef uint64_t WIRTH_CARD64;

/* Real types */
typedef float    WIRTH_REAL32;
typedef double   WIRTH_REAL64;
typedef long double WIRTH_REAL80;

/* Character types */
typedef char     WIRTH_CHAR;
typedef uint16_t WIRTH_WIDECHAR;  /* Unicode character */

/* Boolean type */
typedef int WIRTH_BOOLEAN;
#define WIRTH_TRUE  1
#define WIRTH_FALSE 0

/* Address/pointer type */
typedef void* WIRTH_ADDRESS;

/*
 * String Types (supporting all language variants)
 */

/* Pascal short string (255 max) */
typedef struct {
	uint8_t length;
	char data[256];
} WirthShortString;

/* Pascal long string (dynamic) */
typedef struct {
	size_t length;
	size_t capacity;
	char *data;
} WirthLongString;

/* Modula-2/Oberon ARRAY OF CHAR (null-terminated) */
typedef char* WirthArrayString;

/* Modula-3 TEXT (reference-counted) */
typedef struct {
	size_t length;
	size_t refcount;
	char *data;
} WirthText;

/* Ada String (constrained array) */
typedef struct {
	size_t first;
	size_t last;
	char *data;
} WirthAdaString;

/* Unicode/Wide string support */
typedef struct {
	size_t length;
	size_t capacity;
	uint16_t *data;
} WirthWideString;

/*
 * File Types
 */

typedef struct {
	FILE *fp;
	char *name;
	int mode;        /* 0=closed, 1=read, 2=write, 3=append */
	int eof_flag;
	int eoln_flag;
	char line_buffer[4096];
	size_t line_pos;
	int lang_variant; /* Language-specific behavior */
} WirthTextFile;

typedef struct {
	FILE *fp;
	char *name;
	int mode;
	size_t element_size;
} WirthBinaryFile;

/*
 * Module/Package System (for Modula-2/3, Oberon, Ada)
 */

typedef struct WirthModule {
	const char *name;
	int initialized;
	void (*init_proc)(void);
	void (*fini_proc)(void);
	struct WirthModule *next;
	int import_count;
	struct WirthModule **imports;
} WirthModule;

/*
 * Exception/Error Handling (unified across all languages)
 */

typedef enum {
	WIRTH_ERR_NONE = 0,
	/* Range and type errors */
	WIRTH_ERR_RANGE_CHECK = 1,
	WIRTH_ERR_INDEX_CHECK = 2,
	WIRTH_ERR_CASE_SELECT = 3,
	WIRTH_ERR_TYPE_GUARD = 4,
	/* Arithmetic errors */
	WIRTH_ERR_INTEGER_OVERFLOW = 5,
	WIRTH_ERR_INTEGER_DIVIDE = 6,
	WIRTH_ERR_REAL_OVERFLOW = 7,
	WIRTH_ERR_REAL_UNDERFLOW = 8,
	WIRTH_ERR_REAL_DIVIDE = 9,
	/* Memory errors */
	WIRTH_ERR_STACK_OVERFLOW = 10,
	WIRTH_ERR_HEAP_OVERFLOW = 11,
	WIRTH_ERR_NIL_POINTER = 12,
	WIRTH_ERR_INVALID_POINTER = 13,
	/* File errors */
	WIRTH_ERR_FILE_NOT_FOUND = 14,
	WIRTH_ERR_FILE_NOT_OPEN = 15,
	WIRTH_ERR_FILE_READ = 16,
	WIRTH_ERR_FILE_WRITE = 17,
	WIRTH_ERR_END_OF_FILE = 18,
	/* Module/package errors */
	WIRTH_ERR_MODULE_NOT_FOUND = 19,
	WIRTH_ERR_CIRCULAR_IMPORT = 20,
	/* Ada-specific errors */
	WIRTH_ERR_CONSTRAINT_ERROR = 21,
	WIRTH_ERR_PROGRAM_ERROR = 22,
	WIRTH_ERR_STORAGE_ERROR = 23,
	WIRTH_ERR_TASKING_ERROR = 24,
	/* Modula-3 exceptions */
	WIRTH_ERR_THREAD_ALERTED = 25,
	/* General errors */
	WIRTH_ERR_ASSERT_FAILED = 26,
	WIRTH_ERR_INVALID_OPERATION = 27,
} WirthError;

/* Exception context for try/except */
typedef struct {
	jmp_buf env;
	WirthError error;
	char message[256];
	struct WirthExceptionContext *prev;
} WirthExceptionContext;

/*
 * Record/Object Type Support (for OOP in Oberon-2, Component Pascal, Ada)
 */

typedef struct {
	const char *type_name;
	size_t type_size;
	struct WirthTypeDescriptor *base_type;
	int method_count;
	void **method_table;
} WirthTypeDescriptor;

/*
 * Process/Thread Support (for Modula-2, Modula-3, Ada tasks)
 */

typedef struct WirthProcess WirthProcess;
typedef void (*WirthProcessProc)(void *arg);

/*
 * Coroutine Support (Modula-2)
 */

typedef struct {
	void *stack;
	size_t stack_size;
	jmp_buf context;
	int active;
} WirthCoroutine;

/*
 * Set Types (Pascal, Modula-2, Oberon)
 */

typedef uint32_t WirthSet32;
typedef uint64_t WirthSet64;
typedef uint32_t WirthSet256[8];  /* Sets of 0..255 */

/*
 * Runtime Initialization
 */

void wirth_init_runtime(int argc, char **argv);
void wirth_fini_runtime(void);
void wirth_set_language(const char *lang);

/*
 * Module/Package Management
 */

WirthModule *wirth_module_register(const char *name,
                                     void (*init)(void),
                                     void (*fini)(void));
void wirth_module_import(WirthModule *module, WirthModule *import);
void wirth_module_init(WirthModule *module);
void wirth_module_fini(WirthModule *module);

/*
 * Exception Handling
 */

void wirth_runtime_error(WirthError error, const char *message);
void wirth_set_error_handler(void (*handler)(WirthError, const char *));
WirthExceptionContext *wirth_exception_push(void);
void wirth_exception_pop(void);
void wirth_exception_raise(WirthError error, const char *message);

#define WIRTH_TRY(ctx) if (setjmp((ctx)->env) == 0)
#define WIRTH_RAISE(err, msg) wirth_exception_raise(err, msg)

/*
 * I/O Operations (unified for all languages)
 */

/* Text I/O */
void wirth_write_int(int value, int width);
void wirth_write_card(unsigned int value, int width);
void wirth_write_real(double value, int width, int precision);
void wirth_write_char(char c);
void wirth_write_string(const char *str);
void wirth_write_bool(int value);
void wirth_write_hex(unsigned int value, int width);
void wirth_write_ln(void);

void wirth_read_int(int *value);
void wirth_read_card(unsigned int *value);
void wirth_read_real(double *value);
void wirth_read_char(char *c);
void wirth_read_string(char *str, size_t maxlen);
void wirth_read_ln(void);

/* File operations */
WirthTextFile *wirth_file_open_read(const char *filename);
WirthTextFile *wirth_file_open_write(const char *filename);
WirthTextFile *wirth_file_open_append(const char *filename);
void wirth_file_close(WirthTextFile *f);
int wirth_file_eof(WirthTextFile *f);

/*
 * String Operations (supporting all language variants)
 */

/* Short string (Pascal) */
void wirth_str_assign(WirthShortString *dest, const char *src);
void wirth_str_concat(WirthShortString *dest, const WirthShortString *s1, const WirthShortString *s2);
int wirth_str_compare(const WirthShortString *s1, const WirthShortString *s2);
int wirth_str_length(const WirthShortString *s);

/* Long string (Delphi) */
WirthLongString *wirth_lstr_create(const char *src);
void wirth_lstr_free(WirthLongString *s);
void wirth_lstr_assign(WirthLongString *dest, const char *src);

/* Modula-2/Oberon ARRAY OF CHAR operations */
void wirth_array_assign(char *dest, const char *src, size_t maxlen);
int wirth_array_compare(const char *s1, const char *s2);
void wirth_array_concat(char *dest, const char *s1, const char *s2, size_t maxlen);
size_t wirth_array_length(const char *s);

/* Modula-3 TEXT operations */
WirthText *wirth_text_create(const char *src);
void wirth_text_free(WirthText *t);
WirthText *wirth_text_concat(const WirthText *t1, const WirthText *t2);
int wirth_text_compare(const WirthText *t1, const WirthText *t2);
WirthText *wirth_text_addref(WirthText *t);
void wirth_text_release(WirthText *t);

/* Ada String operations */
void wirth_ada_str_assign(WirthAdaString *dest, const char *src, size_t first, size_t last);
int wirth_ada_str_compare(const WirthAdaString *s1, const WirthAdaString *s2);
WirthAdaString *wirth_ada_str_slice(const WirthAdaString *s, size_t first, size_t last);

/* Wide/Unicode string operations */
WirthWideString *wirth_wstr_create(const uint16_t *src, size_t len);
void wirth_wstr_free(WirthWideString *s);

/*
 * Interoperability: String Conversions
 */

void wirth_convert_pascal_to_modula(WirthShortString *pas, char *mod, size_t maxlen);
void wirth_convert_modula_to_pascal(const char *mod, WirthShortString *pas);
WirthText *wirth_convert_pascal_to_text(const WirthShortString *pas);
void wirth_convert_text_to_pascal(const WirthText *text, WirthShortString *pas);
void wirth_convert_ada_to_modula(const WirthAdaString *ada, char *mod, size_t maxlen);
void wirth_convert_modula_to_ada(const char *mod, WirthAdaString *ada, size_t first, size_t last);

/*
 * Mathematical Functions (common across languages)
 */

/* Standard functions */
int wirth_abs_int(int x);
double wirth_abs_real(double x);
int wirth_odd(int x);
double wirth_sqrt(double x);
double wirth_sin(double x);
double wirth_cos(double x);
double wirth_tan(double x);
double wirth_arctan(double x);
double wirth_arctan2(double y, double x);
double wirth_exp(double x);
double wirth_ln(double x);
double wirth_log10(double x);
double wirth_power(double x, double y);

/* Rounding and truncation */
int wirth_floor(double x);
int wirth_ceil(double x);
int wirth_round(double x);
int wirth_trunc(double x);

/* Modula-2 specific */
int wirth_entier(double x);  /* Same as floor */

/* Bit operations (Modula-2, Oberon, Ada) */
unsigned int wirth_bit_and(unsigned int a, unsigned int b);
unsigned int wirth_bit_or(unsigned int a, unsigned int b);
unsigned int wirth_bit_xor(unsigned int a, unsigned int b);
unsigned int wirth_bit_not(unsigned int a);
unsigned int wirth_lsh(unsigned int value, int shift);  /* Logical shift */
unsigned int wirth_rot(unsigned int value, int shift);  /* Rotation */

/*
 * Memory Management
 */

/* Generic allocation (works for all languages) */
void *wirth_new(size_t size);
void wirth_dispose(void *ptr);
void *wirth_allocate(size_t size);
void wirth_deallocate(void *ptr, size_t size);

/* Modula-3 traced/untraced heaps */
void *wirth_new_traced(size_t size, WirthTypeDescriptor *type);
void *wirth_new_untraced(size_t size);

/* Memory operations */
void wirth_fill(void *dest, size_t count, uint8_t value);
void wirth_copy(const void *src, void *dest, size_t count);
int wirth_compare_mem(const void *s1, const void *s2, size_t count);

/*
 * Type System Support (for Oberon-2, Component Pascal, Ada)
 */

int wirth_is_type(void *obj, WirthTypeDescriptor *type);
void *wirth_type_guard(void *obj, WirthTypeDescriptor *type);
WirthTypeDescriptor *wirth_type_of(void *obj);

/*
 * Set Operations (Pascal, Modula-2, Oberon)
 */

/* 32-bit sets */
WirthSet32 wirth_set32_empty(void);
WirthSet32 wirth_set32_singleton(int elem);
WirthSet32 wirth_set32_range(int low, int high);
WirthSet32 wirth_set32_union(WirthSet32 a, WirthSet32 b);
WirthSet32 wirth_set32_intersection(WirthSet32 a, WirthSet32 b);
WirthSet32 wirth_set32_difference(WirthSet32 a, WirthSet32 b);
WirthSet32 wirth_set32_sym_diff(WirthSet32 a, WirthSet32 b);
int wirth_set32_contains(WirthSet32 s, int elem);
int wirth_set32_equal(WirthSet32 a, WirthSet32 b);
int wirth_set32_subset(WirthSet32 a, WirthSet32 b);
void wirth_set32_incl(WirthSet32 *s, int elem);
void wirth_set32_excl(WirthSet32 *s, int elem);

/* 256-element sets */
void wirth_set256_init(WirthSet256 s);
void wirth_set256_incl(WirthSet256 s, int elem);
void wirth_set256_excl(WirthSet256 s, int elem);
int wirth_set256_contains(WirthSet256 s, int elem);

/*
 * Ordinal Operations
 */

int wirth_ord(char c);
char wirth_chr(int x);
int wirth_succ(int x);
int wirth_pred(int x);
int wirth_max_int(int a, int b);
int wirth_min_int(int a, int b);
double wirth_max_real(double a, double b);
double wirth_min_real(double a, double b);

/*
 * Range Checking
 */

int wirth_range_check(int value, int min, int max, const char *name);
void wirth_index_check(int index, int length, const char *name);
void wirth_nil_check(void *ptr, const char *name);

/*
 * Coroutine Support (Modula-2)
 */

WirthCoroutine *wirth_coroutine_create(WirthProcessProc proc, void *arg, size_t stack_size);
void wirth_coroutine_transfer(WirthCoroutine *from, WirthCoroutine *to);
void wirth_coroutine_destroy(WirthCoroutine *co);

/*
 * Process/Thread Support (Modula-2, Modula-3, Ada)
 */

WirthProcess *wirth_process_create(WirthProcessProc proc, void *arg, size_t stack_size, int priority);
void wirth_process_start(WirthProcess *p);
void wirth_process_suspend(WirthProcess *p);
void wirth_process_resume(WirthProcess *p);
void wirth_process_yield(void);
void wirth_process_sleep(unsigned int milliseconds);

/* Synchronization primitives */
typedef struct WirthMutex WirthMutex;
typedef struct WirthCondition WirthCondition;
typedef struct WirthSemaphore WirthSemaphore;

WirthMutex *wirth_mutex_create(void);
void wirth_mutex_lock(WirthMutex *m);
void wirth_mutex_unlock(WirthMutex *m);
void wirth_mutex_destroy(WirthMutex *m);

WirthCondition *wirth_condition_create(void);
void wirth_condition_wait(WirthCondition *c, WirthMutex *m);
void wirth_condition_signal(WirthCondition *c);
void wirth_condition_broadcast(WirthCondition *c);
void wirth_condition_destroy(WirthCondition *c);

WirthSemaphore *wirth_semaphore_create(int initial);
void wirth_semaphore_wait(WirthSemaphore *s);
void wirth_semaphore_signal(WirthSemaphore *s);
void wirth_semaphore_destroy(WirthSemaphore *s);

/*
 * Standard Input/Output File Handles
 */

extern WirthTextFile wirth_stdin;
extern WirthTextFile wirth_stdout;
extern WirthTextFile wirth_stderr;

/*
 * Language-Specific Compatibility Macros
 */

/* Pascal compatibility */
#define PASCAL_WRITE(x)     wirth_write_int(x, 0)
#define PASCAL_WRITELN()    wirth_write_ln()
#define PASCAL_READ(x)      wirth_read_int(x)

/* Modula-2 compatibility */
#define M2_WriteInt(x, w)   wirth_write_int(x, w)
#define M2_WriteCard(x, w)  wirth_write_card(x, w)
#define M2_WriteLn()        wirth_write_ln()
#define M2_ReadInt(x)       wirth_read_int(x)

/* Oberon compatibility */
#define OBERON_WriteInt(x, w)  wirth_write_int(x, w)
#define OBERON_WriteLn()       wirth_write_ln()

/* Ada compatibility */
#define ADA_PUT(x)          wirth_write_int(x, 0)
#define ADA_PUT_LINE(s)     do { wirth_write_string(s); wirth_write_ln(); } while(0)
#define ADA_NEW_LINE()      wirth_write_ln()

#ifdef __cplusplus
}
#endif

#endif /* _WIRTHRT_H_ */
