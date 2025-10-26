/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Runtime support for Go programs compiled with PCC
 * Provides memory management, goroutines, channels, and built-in functions
 */

#ifndef GO_RUNTIME_H
#define GO_RUNTIME_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Runtime types */
typedef int8_t   go_int8;
typedef uint8_t  go_uint8;
typedef int16_t  go_int16;
typedef uint16_t go_uint16;
typedef int32_t  go_int32;
typedef uint32_t go_uint32;
typedef int64_t  go_int64;
typedef uint64_t go_uint64;
typedef float    go_float32;
typedef double   go_float64;

#if defined(__LP64__) || defined(_WIN64)
typedef int64_t  go_int;
typedef uint64_t go_uint;
typedef uint64_t go_uintptr;
#else
typedef int32_t  go_int;
typedef uint32_t go_uint;
typedef uint32_t go_uintptr;
#endif

typedef bool go_bool;

/* String representation */
typedef struct {
	const char *data;
	go_int len;
} go_string;

/* Slice representation */
typedef struct {
	void *data;
	go_int len;
	go_int cap;
} go_slice;

/* Interface representation */
typedef struct {
	void *type;    /* Type descriptor */
	void *data;    /* Actual data */
} go_interface;

/* Map representation (opaque pointer to hashmap) */
typedef void* go_map;

/* Channel representation */
typedef struct go_channel go_channel;

/* Type descriptor */
typedef struct go_type {
	size_t size;
	size_t align;
	uint32_t hash;
	uint8_t kind;
	const char *name;
} go_type;

/* Function type */
typedef void (*go_func)(void);

/* =================================================================
 * Runtime Initialization
 * ================================================================= */

void go_runtime_init(int argc, char **argv);
void go_runtime_exit(int code);

/* =================================================================
 * Memory Management
 * ================================================================= */

void *go_malloc(size_t size);
void *go_calloc(size_t nmemb, size_t size);
void *go_realloc(void *ptr, size_t size);
void go_free(void *ptr);

/* Garbage collection interface */
void go_gc_init(void);
void go_gc_run(void);
void go_gc_register(void *ptr, size_t size);
void go_gc_unregister(void *ptr);

/* =================================================================
 * String Operations
 * ================================================================= */

go_string go_string_new(const char *str);
go_string go_string_from_bytes(const char *data, go_int len);
go_string go_string_concat(go_string s1, go_string s2);
go_int go_string_compare(go_string s1, go_string s2);
go_bool go_string_equal(go_string s1, go_string s2);
go_string go_string_slice(go_string s, go_int low, go_int high);
const char *go_string_cstr(go_string s);  /* NULL-terminated copy */

/* String conversions */
go_string go_int_to_string(go_int64 val);
go_string go_uint_to_string(go_uint64 val);
go_string go_float_to_string(go_float64 val);
go_string go_bool_to_string(go_bool val);

/* =================================================================
 * Slice Operations
 * ================================================================= */

go_slice go_slice_new(go_int len, go_int cap, size_t elem_size);
go_slice go_slice_make(go_int len, go_int cap, size_t elem_size);
go_slice go_slice_append(go_slice s, void *elem, size_t elem_size);
go_slice go_slice_copy(go_slice dst, go_slice src, size_t elem_size);
go_slice go_slice_sub(go_slice s, go_int low, go_int high, size_t elem_size);
void *go_slice_index(go_slice s, go_int i, size_t elem_size);

/* =================================================================
 * Map Operations
 * ================================================================= */

go_map go_map_new(size_t key_size, size_t val_size, size_t initial_cap);
void go_map_insert(go_map m, void *key, void *val);
void *go_map_lookup(go_map m, void *key);
bool go_map_delete(go_map m, void *key);
go_int go_map_len(go_map m);
void go_map_clear(go_map m);
void go_map_free(go_map m);

/* Map iteration */
typedef struct go_map_iter go_map_iter;
go_map_iter *go_map_iter_new(go_map m);
bool go_map_iter_next(go_map_iter *it, void *key, void *val);
void go_map_iter_free(go_map_iter *it);

/* =================================================================
 * Channel Operations
 * ================================================================= */

go_channel *go_chan_new(size_t elem_size, go_int buffer_size);
void go_chan_send(go_channel *ch, void *elem);
bool go_chan_recv(go_channel *ch, void *elem);
bool go_chan_try_send(go_channel *ch, void *elem);
bool go_chan_try_recv(go_channel *ch, void *elem);
void go_chan_close(go_channel *ch);
bool go_chan_is_closed(go_channel *ch);
void go_chan_free(go_channel *ch);

/* =================================================================
 * Goroutine Support
 * ================================================================= */

typedef uint64_t go_routine_id;

go_routine_id go_routine_create(go_func fn, void *arg);
void go_routine_yield(void);
void go_routine_exit(void);
go_routine_id go_routine_self(void);
void go_sched_init(void);
void go_sched_run(void);

/* =================================================================
 * Panic/Recover Mechanism
 * ================================================================= */

typedef struct {
	go_interface value;
	void *pc;          /* Program counter */
	const char *file;
	int line;
} go_panic_info;

void go_panic(go_interface value);
go_interface go_recover(void);
bool go_has_panic(void);

/* Defer mechanism */
typedef void (*go_defer_func)(void *arg);
void go_defer(go_defer_func fn, void *arg);
void go_defer_run(void);

/* =================================================================
 * Interface Operations
 * ================================================================= */

go_interface go_iface_new(go_type *typ, void *data);
bool go_iface_is_nil(go_interface iface);
bool go_iface_type_assert(go_interface iface, go_type *typ);
void *go_iface_data(go_interface iface);
go_type *go_iface_type(go_interface iface);

/* Type assertions */
bool go_type_assert(go_interface iface, go_type *typ, void *result);
bool go_type_assert_ok(go_interface iface, go_type *typ);

/* =================================================================
 * Built-in Functions
 * ================================================================= */

/* make - allocate and initialize slices, maps, channels */
go_slice go_make_slice(go_type *elem_type, go_int len, go_int cap);
go_map go_make_map(go_type *key_type, go_type *val_type, go_int hint);
go_channel *go_make_chan(go_type *elem_type, go_int buffer);

/* new - allocate zeroed memory */
void *go_new(go_type *typ);

/* len - length of string, slice, array, map, channel */
go_int go_len_string(go_string s);
go_int go_len_slice(go_slice s);
go_int go_len_map(go_map m);
go_int go_len_chan(go_channel *ch);

/* cap - capacity of slice, array, channel */
go_int go_cap_slice(go_slice s);
go_int go_cap_chan(go_channel *ch);

/* append - append elements to slice */
go_slice go_append(go_slice s, void *elems, go_int count, size_t elem_size);

/* copy - copy slice elements */
go_int go_copy_slice(go_slice dst, go_slice src, size_t elem_size);

/* delete - delete map entry */
void go_delete_map(go_map m, void *key);

/* close - close channel */
void go_close_chan(go_channel *ch);

/* print/println - output functions */
void go_print(go_int nargs, ...);
void go_println(go_int nargs, ...);
void go_print_bool(go_bool val);
void go_print_int(go_int64 val);
void go_print_uint(go_uint64 val);
void go_print_float(go_float64 val);
void go_print_string(go_string s);
void go_print_newline(void);

/* =================================================================
 * Type Reflection
 * ================================================================= */

go_type *go_typeof(go_interface iface);
go_string go_type_name(go_type *typ);
size_t go_type_size(go_type *typ);

/* Predefined type descriptors */
extern go_type go_type_bool;
extern go_type go_type_int8;
extern go_type go_type_uint8;
extern go_type go_type_int16;
extern go_type go_type_uint16;
extern go_type go_type_int32;
extern go_type go_type_uint32;
extern go_type go_type_int64;
extern go_type go_type_uint64;
extern go_type go_type_int;
extern go_type go_type_uint;
extern go_type go_type_uintptr;
extern go_type go_type_float32;
extern go_type go_type_float64;
extern go_type go_type_string;

/* =================================================================
 * Select Statement Support
 * ================================================================= */

typedef enum {
	GO_SELECT_SEND,
	GO_SELECT_RECV,
	GO_SELECT_DEFAULT
} go_select_op;

typedef struct {
	go_select_op op;
	go_channel *ch;
	void *data;
} go_select_case;

int go_select(go_select_case *cases, int ncase);

/* =================================================================
 * Atomic Operations
 * ================================================================= */

int32_t go_atomic_add_int32(int32_t *ptr, int32_t delta);
int64_t go_atomic_add_int64(int64_t *ptr, int64_t delta);
uint32_t go_atomic_add_uint32(uint32_t *ptr, uint32_t delta);
uint64_t go_atomic_add_uint64(uint64_t *ptr, uint64_t delta);

bool go_atomic_cas_int32(int32_t *ptr, int32_t old, int32_t new);
bool go_atomic_cas_int64(int64_t *ptr, int64_t old, int64_t new);
bool go_atomic_cas_ptr(void **ptr, void *old, void *new);

int32_t go_atomic_load_int32(int32_t *ptr);
int64_t go_atomic_load_int64(int64_t *ptr);
void *go_atomic_load_ptr(void **ptr);

void go_atomic_store_int32(int32_t *ptr, int32_t val);
void go_atomic_store_int64(int64_t *ptr, int64_t val);
void go_atomic_store_ptr(void **ptr, void *val);

/* =================================================================
 * Utilities
 * ================================================================= */

/* Range iteration helpers */
typedef struct {
	go_int index;
	go_int len;
} go_range_iter;

go_range_iter go_range_init(go_int len);
bool go_range_next(go_range_iter *it);

/* Hash functions */
uint32_t go_hash_string(go_string s);
uint32_t go_hash_int(int64_t val);
uint32_t go_hash_ptr(void *ptr);
uint32_t go_hash_bytes(const void *data, size_t len);

#endif /* GO_RUNTIME_H */
