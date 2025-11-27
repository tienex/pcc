/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * BASIC Runtime Library Header
 * Provides runtime support for BASIC programs
 */

#ifndef BASICRT_H
#define BASICRT_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

/* String type for BASIC strings */
typedef struct {
	char *data;
	int length;
	int allocated;
} basic_string_t;

/* I/O Functions */
void basic_print_int(int value);
void basic_print_long(long value);
void basic_print_float(float value);
void basic_print_double(double value);
void basic_print_string(const char *str);
void basic_print_newline(void);
void basic_input_int(const char *prompt, int *value);
void basic_input_string(const char *prompt, basic_string_t *str);

/* String Functions */
basic_string_t *basic_string_new(const char *str);
void basic_string_free(basic_string_t *str);
basic_string_t *basic_left(basic_string_t *str, int n);
basic_string_t *basic_right(basic_string_t *str, int n);
basic_string_t *basic_mid(basic_string_t *str, int start, int length);
int basic_len(basic_string_t *str);
basic_string_t *basic_concat(basic_string_t *s1, basic_string_t *s2);
int basic_instr(basic_string_t *haystack, basic_string_t *needle);
basic_string_t *basic_chr(int code);
int basic_asc(basic_string_t *str);
basic_string_t *basic_str(double value);
double basic_val(basic_string_t *str);
basic_string_t *basic_ucase(basic_string_t *str);
basic_string_t *basic_lcase(basic_string_t *str);
basic_string_t *basic_ltrim(basic_string_t *str);
basic_string_t *basic_rtrim(basic_string_t *str);
basic_string_t *basic_space(int n);
basic_string_t *basic_string_repeat(int n, const char *ch);

/* Math Functions */
double basic_abs_d(double x);
int basic_abs_i(int x);
double basic_sin(double x);
double basic_cos(double x);
double basic_tan(double x);
double basic_atn(double x);  /* ATAN */
double basic_exp(double x);
double basic_log(double x);
double basic_sqr(double x);  /* SQRT */
int basic_sgn(double x);     /* Sign function */
int basic_fix(double x);     /* Truncate */
int basic_int(double x);     /* Floor */
double basic_rnd(void);      /* Random number [0,1) */
void basic_randomize(void);  /* Seed RNG */

/* Type Conversion Functions */
int basic_cint(double x);    /* Convert to integer */
long basic_clng(double x);   /* Convert to long */
float basic_csng(double x);  /* Convert to single */
double basic_cdbl(float x);  /* Convert to double */

/* File I/O Functions */
typedef struct {
	FILE *fp;
	int file_num;
	int mode;  /* 0=input, 1=output, 2=append, 3=random, 4=binary */
	int is_open;
} basic_file_t;

#define MAX_FILES 16
extern basic_file_t basic_files[MAX_FILES];

void basic_open(const char *filename, int file_num, const char *mode);
void basic_close(int file_num);
void basic_print_file(int file_num, const char *str);
void basic_input_file(int file_num, basic_string_t *str);
int basic_eof(int file_num);
int basic_lof(int file_num);
int basic_loc(int file_num);

/* System Functions */
void basic_end(void);
void basic_stop(void);
void basic_system(const char *cmd);
long basic_timer(void);  /* Seconds since midnight */
void basic_sleep(int seconds);
void basic_beep(void);

/* Global runtime state */
extern int rnd_initialized;

/* Runtime initialization */
void basic_runtime_init(void);
void basic_runtime_cleanup(void);

#endif /* BASICRT_H */
