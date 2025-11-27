/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * Main runtime library header for PAL/ObjectPAL programs
 * Provides all built-in functions and types
 */

#ifndef PALRT_H
#define PALRT_H

#include <stdint.h>
#include <stddef.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* PAL data types */
typedef int8_t   PAL_ShortInt;
typedef int16_t  PAL_SmallInt;
typedef int32_t  PAL_LongInt;
typedef uint8_t  PAL_Byte;
typedef uint16_t PAL_Word;
typedef uint32_t PAL_DWord;
typedef int8_t   PAL_Logical;
typedef double   PAL_Number;      /* BCD emulated as double */
typedef double   PAL_Currency;    /* Fixed-point emulated as double */

/* String type - Pascal-style with length prefix */
typedef struct {
	uint16_t length;
	uint16_t capacity;
	char *data;
} PAL_String;

/* Date/Time types */
typedef struct {
	int16_t year;
	int8_t  month;
	int8_t  day;
} PAL_Date;

typedef struct {
	int8_t hour;
	int8_t minute;
	int8_t second;
	int16_t millisecond;
} PAL_Time;

typedef struct {
	PAL_Date date;
	PAL_Time time;
} PAL_DateTime;

typedef time_t PAL_TimeStamp;

/* Variant type - can hold any PAL type */
typedef enum {
	PAL_VT_NULL = 0,
	PAL_VT_SHORTINT,
	PAL_VT_SMALLINT,
	PAL_VT_LONGINT,
	PAL_VT_NUMBER,
	PAL_VT_CURRENCY,
	PAL_VT_LOGICAL,
	PAL_VT_STRING,
	PAL_VT_DATE,
	PAL_VT_TIME,
	PAL_VT_DATETIME,
	PAL_VT_TIMESTAMP,
	PAL_VT_ARRAY,
	PAL_VT_OBJECT
} PAL_VariantType;

typedef struct {
	PAL_VariantType vtype;
	union {
		PAL_ShortInt  v_shortint;
		PAL_SmallInt  v_smallint;
		PAL_LongInt   v_longint;
		PAL_Number    v_number;
		PAL_Currency  v_currency;
		PAL_Logical   v_logical;
		PAL_String    v_string;
		PAL_Date      v_date;
		PAL_Time      v_time;
		PAL_DateTime  v_datetime;
		PAL_TimeStamp v_timestamp;
		void         *v_ptr;
	} value;
} PAL_Variant;

/* Array descriptor */
typedef struct {
	void *data;
	size_t elem_size;
	int32_t length;
	int32_t capacity;
} PAL_Array;

/* Boolean constants */
#define PAL_TRUE  ((PAL_Logical)1)
#define PAL_FALSE ((PAL_Logical)0)

/*
 * String Functions
 */

/* Create a new string from C string */
PAL_String *pal_string_new(const char *str);

/* Create a new empty string with capacity */
PAL_String *pal_string_alloc(size_t capacity);

/* Free a string */
void pal_string_free(PAL_String *str);

/* Get C string pointer (null-terminated) */
const char *pal_string_cstr(PAL_String *str);

/* String length */
int32_t pal_strlen(PAL_String *str);

/* Convert string to uppercase */
PAL_String *pal_upper(PAL_String *str);

/* Convert string to lowercase */
PAL_String *pal_lower(PAL_String *str);

/* Extract substring (1-based indexing like Pascal) */
PAL_String *pal_substr(PAL_String *str, int32_t start, int32_t length);

/* Trim whitespace from both ends */
PAL_String *pal_trim(PAL_String *str);

/* Trim whitespace from left */
PAL_String *pal_ltrim(PAL_String *str);

/* Trim whitespace from right */
PAL_String *pal_rtrim(PAL_String *str);

/* Find substring position (1-based, 0 if not found) */
int32_t pal_strpos(PAL_String *haystack, PAL_String *needle);

/* Concatenate strings */
PAL_String *pal_concat(PAL_String *s1, PAL_String *s2);

/* Get character at position (1-based) */
char pal_chr(int32_t code);

/* Get ASCII code of character */
int32_t pal_asc(char c);

/* Format number as string */
PAL_String *pal_format(PAL_Number value, const char *fmt);

/* Fill string with character */
PAL_String *pal_fill(char c, int32_t count);

/* String comparison (case-insensitive) */
int32_t pal_strcmp(PAL_String *s1, PAL_String *s2);

/*
 * Math Functions
 */

/* Absolute value */
PAL_Number pal_abs(PAL_Number x);

/* Square root */
PAL_Number pal_sqrt(PAL_Number x);

/* Sine */
PAL_Number pal_sin(PAL_Number x);

/* Cosine */
PAL_Number pal_cos(PAL_Number x);

/* Tangent */
PAL_Number pal_tan(PAL_Number x);

/* Exponential */
PAL_Number pal_exp(PAL_Number x);

/* Natural logarithm */
PAL_Number pal_ln(PAL_Number x);

/* Base-10 logarithm */
PAL_Number pal_log(PAL_Number x);

/* Round to nearest integer */
PAL_Number pal_round(PAL_Number x);

/* Truncate to integer */
PAL_Number pal_trunc(PAL_Number x);

/* Integer part */
PAL_LongInt pal_int(PAL_Number x);

/* Fractional part */
PAL_Number pal_frac(PAL_Number x);

/* Random number [0, 1) */
PAL_Number pal_random(void);

/* Maximum of two numbers */
PAL_Number pal_max(PAL_Number a, PAL_Number b);

/* Minimum of two numbers */
PAL_Number pal_min(PAL_Number a, PAL_Number b);

/* Power */
PAL_Number pal_power(PAL_Number base, PAL_Number exponent);

/* Modulo */
PAL_LongInt pal_mod(PAL_LongInt a, PAL_LongInt b);

/*
 * Date/Time Functions
 */

/* Get current date */
PAL_Date pal_today(void);

/* Get current date and time */
PAL_DateTime pal_now(void);

/* Extract year from date */
int32_t pal_year(PAL_Date date);

/* Extract month from date */
int32_t pal_month(PAL_Date date);

/* Extract day from date */
int32_t pal_day(PAL_Date date);

/* Extract hour from time */
int32_t pal_hour(PAL_Time time);

/* Extract minute from time */
int32_t pal_minute(PAL_Time time);

/* Extract second from time */
int32_t pal_second(PAL_Time time);

/* Create date from components */
PAL_Date pal_date(int32_t year, int32_t month, int32_t day);

/* Create time from components */
PAL_Time pal_time(int32_t hour, int32_t minute, int32_t second);

/* Create datetime from components */
PAL_DateTime pal_datetime(int32_t year, int32_t month, int32_t day,
                          int32_t hour, int32_t minute, int32_t second);

/* Convert date to string */
PAL_String *pal_datetostr(PAL_Date date, const char *format);

/* Parse string to date */
PAL_Date pal_strtodate(const char *str, const char *format);

/* Add days to date */
PAL_Date pal_adddays(PAL_Date date, int32_t days);

/* Difference between dates in days */
int32_t pal_datediff(PAL_Date date1, PAL_Date date2);

/*
 * Type Conversion Functions
 */

/* Convert string to number */
PAL_Number pal_numval(PAL_String *str);

/* Convert number to string */
PAL_String *pal_strval(PAL_Number value);

/* Convert to logical */
PAL_Logical pal_logical(PAL_Variant *var);

/* Convert number to integer */
PAL_LongInt pal_toint(PAL_Number value);

/* Convert integer to number */
PAL_Number pal_tonum(PAL_LongInt value);

/*
 * Array Functions
 */

/* Create new array */
PAL_Array *pal_array_new(size_t elem_size, int32_t length);

/* Free array */
void pal_array_free(PAL_Array *arr);

/* Get array size */
int32_t pal_arraysize(PAL_Array *arr);

/* Get array element */
void *pal_arrayget(PAL_Array *arr, int32_t index);

/* Set array element */
void pal_arrayset(PAL_Array *arr, int32_t index, const void *value);

/* Insert element at position */
int pal_arrayinsert(PAL_Array *arr, int32_t index, const void *value);

/* Delete element at position */
int pal_arraydelete(PAL_Array *arr, int32_t index);

/* Resize array */
int pal_arrayresize(PAL_Array *arr, int32_t new_size);

/*
 * UI Functions
 */

/* Display information message */
void pal_msginfo(const char *title, const char *message);

/* Display warning message */
void pal_msgwarning(const char *title, const char *message);

/* Display error message */
void pal_msgerror(const char *title, const char *message);

/* Display question and return yes/no */
PAL_Logical pal_msgquestion(const char *title, const char *message);

/* Display OK message */
void pal_msgok(const char *title, const char *message);

/* Display yes/no question */
PAL_Logical pal_msgyesno(const char *title, const char *message);

/* Display OK/Cancel question */
PAL_Logical pal_msgokcancel(const char *title, const char *message);

/* Beep */
void pal_beep(void);

/* Sleep for milliseconds */
void pal_sleep(int32_t milliseconds);

/* Wait for user input */
void pal_wait(void);

/*
 * System Functions
 */

/* Execute command */
int32_t pal_execute(const char *command);

/* Run shell command */
int32_t pal_shell(const char *command);

/* Get environment variable */
PAL_String *pal_getenv(const char *name);

/* Set environment variable */
int pal_setenv(const char *name, const char *value);

/* Check if file exists */
PAL_Logical pal_fileexists(const char *path);

/* Copy file */
int pal_filecopy(const char *src, const char *dst);

/* Delete file */
int pal_filedelete(const char *path);

/* Rename file */
int pal_filerename(const char *old_path, const char *new_path);

/*
 * Database Functions (Stubs for now)
 */

/* Move to record */
int pal_moveto(const char *table, int32_t record_num);

/* Locate record */
int pal_locate(const char *table, const char *condition);

/* Lock record */
int pal_lockrecord(const char *table);

/* Unlock record */
int pal_unlockrecord(const char *table);

/* Post changes */
int pal_post(void);

/* Resync table */
int pal_resync(const char *table);

/*
 * Memory Management
 */

/* Allocate memory */
void *pal_malloc(size_t size);

/* Reallocate memory */
void *pal_realloc(void *ptr, size_t size);

/* Free memory */
void pal_free(void *ptr);

/*
 * Exception Handling
 */

/* Exception structure */
typedef struct {
	int code;
	char message[256];
	const char *file;
	int line;
} PAL_Exception;

/* Get last exception */
PAL_Exception *pal_get_exception(void);

/* Set exception */
void pal_set_exception(int code, const char *message, const char *file, int line);

/* Clear exception */
void pal_clear_exception(void);

/*
 * Runtime Initialization
 */

/* Initialize runtime library */
void pal_runtime_init(void);

/* Cleanup runtime library */
void pal_runtime_cleanup(void);

#ifdef __cplusplus
}
#endif

#endif /* PALRT_H */
