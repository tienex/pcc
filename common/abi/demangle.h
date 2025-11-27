/*
 * Copyright (c) 2025 PCC Project
 *
 * Generic C++ name demangling interface
 *
 * Supports demangling of mangled names from:
 * - Itanium C++ ABI (GCC, Clang)
 * - Microsoft Visual C++ ABI
 * - Watcom C++ ABI
 * - Sun/Oracle C++ ABI
 * - Intel C++ Compiler ABI
 * - IBM XL C++ ABI
 * - HP aCC ABI
 */

#ifndef DEMANGLE_H
#define DEMANGLE_H

#include <stddef.h>
#include "abi.h"

/* Demangling options */
typedef enum {
	DEMANGLE_OPT_NONE = 0,
	DEMANGLE_OPT_PARAMS = (1 << 0),     /* Include parameter types */
	DEMANGLE_OPT_RETURN = (1 << 1),     /* Include return type */
	DEMANGLE_OPT_QUALIFIERS = (1 << 2), /* Include const/volatile */
	DEMANGLE_OPT_VERBOSE = (1 << 3),    /* Verbose output */
	DEMANGLE_OPT_TEMPLATES = (1 << 4),  /* Expand template args */
	DEMANGLE_OPT_ALL = 0xFF             /* All options */
} demangle_options_t;

/* Demangling result */
typedef struct demangle_result {
	char *demangled;          /* Demangled name */
	abi_kind_t abi;           /* Detected ABI */
	int success;              /* 1 if successful, 0 otherwise */
	const char *error;        /* Error message if failed */

	/* Parsed components (optional) */
	char *function_name;      /* Just the function name */
	char *class_name;         /* Class name (if method) */
	char *namespace_name;     /* Namespace (if any) */
	char *return_type;        /* Return type string */
	char **param_types;       /* Parameter type strings */
	int param_count;          /* Number of parameters */

	unsigned int is_const : 1;
	unsigned int is_volatile : 1;
	unsigned int is_static : 1;
	unsigned int is_virtual : 1;
	unsigned int is_constructor : 1;
	unsigned int is_destructor : 1;
} demangle_result_t;

/* Demangling context */
typedef struct demangle_ctx {
	const char *input;        /* Input mangled name */
	const char *pos;          /* Current position */
	char *output;             /* Output buffer */
	size_t output_size;       /* Output buffer size */
	size_t output_pos;        /* Current output position */

	demangle_options_t options;
	abi_kind_t abi;

	/* Substitution table (Itanium) */
	char **substitutions;
	int subst_count;
	int subst_capacity;

	/* Template argument stack */
	char **template_args;
	int template_depth;

	/* Error handling */
	const char *error_message;
	int error_pos;
} demangle_ctx_t;

/*
 * Public API
 */

/* Auto-detect ABI and demangle */
char *demangle(const char *mangled_name);
char *demangle_with_options(const char *mangled_name, demangle_options_t opts);

/* Demangle with specific ABI */
char *demangle_itanium(const char *mangled_name, demangle_options_t opts);
char *demangle_msvc(const char *mangled_name, demangle_options_t opts);
char *demangle_watcom(const char *mangled_name, demangle_options_t opts);
char *demangle_sun(const char *mangled_name, demangle_options_t opts);
char *demangle_intel(const char *mangled_name, demangle_options_t opts);
char *demangle_ibm(const char *mangled_name, demangle_options_t opts);
char *demangle_hp(const char *mangled_name, demangle_options_t opts);

/* Detailed demangling with component extraction */
demangle_result_t *demangle_detailed(const char *mangled_name);
void demangle_result_free(demangle_result_t *result);

/* Auto-detect which ABI a mangled name uses */
abi_kind_t demangle_detect_abi(const char *mangled_name);

/* Check if a name is mangled */
int is_mangled_name(const char *name);
int is_itanium_mangled(const char *name);
int is_msvc_mangled(const char *name);
int is_watcom_mangled(const char *name);

/* Demangler initialization */
demangle_ctx_t *demangle_ctx_create(const char *mangled, abi_kind_t abi);
void demangle_ctx_destroy(demangle_ctx_t *ctx);

/* Internal demangling operations */
int demangle_name(demangle_ctx_t *ctx);
int demangle_type(demangle_ctx_t *ctx);
int demangle_nested_name(demangle_ctx_t *ctx);
int demangle_template_args(demangle_ctx_t *ctx);
int demangle_operator(demangle_ctx_t *ctx);
int demangle_special_name(demangle_ctx_t *ctx);

/* Output buffer management */
void demangle_append(demangle_ctx_t *ctx, const char *str);
void demangle_append_char(demangle_ctx_t *ctx, char c);
void demangle_append_number(demangle_ctx_t *ctx, int n);

/* Substitution management (Itanium) */
void demangle_add_substitution(demangle_ctx_t *ctx, const char *str);
const char *demangle_get_substitution(demangle_ctx_t *ctx, int index);

/* Utility functions */
int demangle_parse_number(demangle_ctx_t *ctx);
int demangle_parse_seq_id(demangle_ctx_t *ctx);
char demangle_peek(demangle_ctx_t *ctx);
char demangle_next(demangle_ctx_t *ctx);
int demangle_match(demangle_ctx_t *ctx, const char *str);

/* Error handling */
void demangle_error(demangle_ctx_t *ctx, const char *msg);
const char *demangle_get_error(demangle_ctx_t *ctx);

/* Pretty-printing */
char *demangle_pretty_function(const char *mangled);
char *demangle_pretty_type(const char *mangled);

/* Batch demangling */
char **demangle_multiple(const char **mangled_names, int count);
void demangle_free_multiple(char **demangled_names, int count);

#endif /* DEMANGLE_H */
