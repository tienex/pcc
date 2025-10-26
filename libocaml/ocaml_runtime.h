/*
 * Copyright (c) 2025 PCC OCaml Runtime Library
 *
 * OCaml runtime library header
 * Provides memory management, data structures, and runtime support
 */

#ifndef OCAML_RUNTIME_H
#define OCAML_RUNTIME_H

#include <stddef.h>
#include <stdint.h>

/* Value representation
 * OCaml uses tagged values:
 * - Integers: lowest bit = 1, value shifted left by 1
 * - Pointers: lowest bit = 0, points to heap-allocated data
 */
typedef intptr_t ocaml_value_t;

/* Tag for block headers */
#define OCAML_TAG_BITS 8
#define OCAML_TAG_MASK ((1 << OCAML_TAG_BITS) - 1)

/* Block tags */
#define TAG_CLOSURE     247
#define TAG_STRING      252
#define TAG_DOUBLE      253
#define TAG_ARRAY       254
#define TAG_CUSTOM      255

/* List constructor tags */
#define TAG_CONS        0  /* :: constructor */
#define TAG_NIL         1  /* [] constructor */

/* Value predicates */
#define IS_INT(v)       (((v) & 1) == 1)
#define IS_BLOCK(v)     (((v) & 1) == 0)

/* Integer conversion */
#define INT_VAL(v)      ((intptr_t)(v) >> 1)
#define VAL_INT(i)      (((intptr_t)(i) << 1) | 1)

/* Block header structure */
typedef struct {
	size_t size;    /* Size in words */
	uint8_t tag;    /* Block tag */
	uint8_t color;  /* GC color (white/gray/black) */
} ocaml_header_t;

/* Block access */
#define HEADER(v)       (((ocaml_header_t *)((v) - sizeof(ocaml_header_t))))
#define FIELD(v, i)     (((ocaml_value_t *)(v))[i])
#define TAG(v)          (HEADER(v)->tag)
#define SIZE(v)         (HEADER(v)->size)

/* Constants */
#define VAL_UNIT        VAL_INT(0)
#define VAL_TRUE        VAL_INT(1)
#define VAL_FALSE       VAL_INT(0)
#define VAL_NIL         VAL_INT(0)  /* Empty list */

/* Memory management */
ocaml_value_t ocaml_alloc(size_t size, uint8_t tag);
void ocaml_gc_init(size_t heap_size);
void ocaml_gc_collect(void);
void ocaml_gc_register_root(ocaml_value_t *root);

/* List operations */
ocaml_value_t ocaml_cons(ocaml_value_t head, ocaml_value_t tail);
ocaml_value_t ocaml_list_hd(ocaml_value_t list);
ocaml_value_t ocaml_list_tl(ocaml_value_t list);
ocaml_value_t ocaml_list_length(ocaml_value_t list);
ocaml_value_t ocaml_list_nth(ocaml_value_t list, ocaml_value_t n);

/* Array operations */
ocaml_value_t ocaml_array_make(ocaml_value_t size, ocaml_value_t init);
ocaml_value_t ocaml_array_length(ocaml_value_t array);
ocaml_value_t ocaml_array_get(ocaml_value_t array, ocaml_value_t index);
void ocaml_array_set(ocaml_value_t array, ocaml_value_t index, ocaml_value_t val);

/* String operations */
ocaml_value_t ocaml_string_make(const char *str);
ocaml_value_t ocaml_string_length(ocaml_value_t str);
ocaml_value_t ocaml_string_get(ocaml_value_t str, ocaml_value_t index);
ocaml_value_t ocaml_string_concat(ocaml_value_t sep, ocaml_value_t list);
const char *ocaml_string_data(ocaml_value_t str);

/* I/O operations */
void ocaml_print_int(ocaml_value_t val);
void ocaml_print_float(double val);
void ocaml_print_char(ocaml_value_t val);
void ocaml_print_string(ocaml_value_t val);
void ocaml_print_newline(void);
ocaml_value_t ocaml_read_int(void);
ocaml_value_t ocaml_read_line(void);

/* Conversion functions */
ocaml_value_t ocaml_int_of_float(double f);
double ocaml_float_of_int(ocaml_value_t i);
ocaml_value_t ocaml_char_of_int(ocaml_value_t i);
ocaml_value_t ocaml_int_of_char(ocaml_value_t c);
ocaml_value_t ocaml_string_of_int(ocaml_value_t i);
ocaml_value_t ocaml_int_of_string(ocaml_value_t s);

/* Exception handling */
typedef struct ocaml_exception {
	ocaml_value_t tag;
	ocaml_value_t arg;
} ocaml_exception_t;

void ocaml_raise(ocaml_value_t exn);
ocaml_value_t ocaml_try(ocaml_value_t (*fn)(void *), void *arg,
                        ocaml_value_t (*handler)(ocaml_exception_t *));

/* Reference operations */
ocaml_value_t ocaml_ref(ocaml_value_t val);
ocaml_value_t ocaml_deref(ocaml_value_t ref);
void ocaml_assign(ocaml_value_t ref, ocaml_value_t val);

/* Math operations */
double ocaml_sqrt(double x);
double ocaml_sin(double x);
double ocaml_cos(double x);
double ocaml_exp(double x);
double ocaml_log(double x);

#endif /* OCAML_RUNTIME_H */
