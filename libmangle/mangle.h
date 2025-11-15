/*
 * Copyright (c) 2025 PCC Project
 *
 * Symbol Mangler and Demangler Library
 *
 * Supports multiple name mangling schemes:
 * - C (no mangling)
 * - C++ (Itanium ABI)
 * - Pascal/Delphi
 * - Modula-2/Modula-3
 * - Oberon
 * - Ada
 * - PCC custom scheme (cross-language)
 *
 * This library is reusable across different compiler projects.
 */

#ifndef _PCC_MANGLE_H_
#define _PCC_MANGLE_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Mangling Schemes
 */
typedef enum {
	MANGLE_NONE = 0,        /* C naming - no mangling */
	MANGLE_C,               /* Explicit C naming */
	MANGLE_CXX_ITANIUM,     /* C++ Itanium ABI */
	MANGLE_CXX_MSVC,        /* MSVC C++ mangling */
	MANGLE_PASCAL,          /* Pascal (uppercase, length prefix) */
	MANGLE_DELPHI,          /* Delphi (unit.name) */
	MANGLE_MODULA2,         /* Modula-2 (module_name) */
	MANGLE_MODULA3,         /* Modula-3 (module__name) */
	MANGLE_OBERON,          /* Oberon (Module.Name) */
	MANGLE_ADA,             /* Ada (package__subpackage__name) */
	MANGLE_PCC              /* PCC cross-language scheme */
} mangle_scheme_t;

/*
 * Symbol Types
 */
typedef enum {
	SYM_FUNCTION = 0,
	SYM_VARIABLE,
	SYM_TYPE,
	SYM_CONSTANT,
	SYM_MODULE,
	SYM_NAMESPACE,
	SYM_CLASS,
	SYM_METHOD,
	SYM_CONSTRUCTOR,
	SYM_DESTRUCTOR,
	SYM_OPERATOR,
	SYM_VTABLE,
	SYM_TYPEINFO
} symbol_type_t;

/*
 * Type Information for Mangling
 */
typedef enum {
	TYPE_VOID = 0,
	TYPE_BOOL,
	TYPE_CHAR,
	TYPE_SCHAR,
	TYPE_UCHAR,
	TYPE_SHORT,
	TYPE_USHORT,
	TYPE_INT,
	TYPE_UINT,
	TYPE_LONG,
	TYPE_ULONG,
	TYPE_LONGLONG,
	TYPE_ULONGLONG,
	TYPE_INT8,
	TYPE_UINT8,
	TYPE_INT16,
	TYPE_UINT16,
	TYPE_INT32,
	TYPE_UINT32,
	TYPE_INT64,
	TYPE_UINT64,
	TYPE_FLOAT,
	TYPE_DOUBLE,
	TYPE_LONGDOUBLE,
	TYPE_POINTER,
	TYPE_REFERENCE,
	TYPE_RVALUE_REF,
	TYPE_ARRAY,
	TYPE_FUNCTION,
	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_CLASS,
	TYPE_ENUM,
	TYPE_TEMPLATE,
	TYPE_CUSTOM
} type_kind_t;

/*
 * Type qualifiers
 */
typedef enum {
	QUAL_NONE = 0,
	QUAL_CONST = 1,
	QUAL_VOLATILE = 2,
	QUAL_RESTRICT = 4,
	QUAL_ATOMIC = 8
} type_qualifier_t;

/*
 * Function calling conventions
 */
typedef enum {
	CALL_CDECL = 0,
	CALL_STDCALL,
	CALL_FASTCALL,
	CALL_THISCALL,
	CALL_VECTORCALL,
	CALL_PASCAL,
	CALL_FORTRAN,
	CALL_SYSCALL
} calling_convention_t;

/*
 * Type descriptor for mangling
 */
typedef struct mangle_type {
	type_kind_t kind;
	type_qualifier_t qualifiers;
	const char *name;           /* For named types */
	struct mangle_type *base;   /* For pointers, references, arrays */
	struct mangle_type **params; /* For functions */
	int param_count;
	size_t array_size;          /* For arrays */
} mangle_type_t;

/*
 * Symbol descriptor
 */
typedef struct {
	const char *name;           /* Unmangled name */
	const char *scope;          /* Scope/namespace/module */
	symbol_type_t sym_type;
	mangle_type_t *type;        /* Type information */
	calling_convention_t call_conv;
	int is_const;
	int is_static;
	int is_extern;
	int is_inline;
	int is_virtual;
	int is_template;
	const char **template_params; /* Template parameter names */
	int template_param_count;
} symbol_info_t;

/*
 * Demangled symbol information
 */
typedef struct {
	char *full_name;            /* Complete demangled name */
	char *name;                 /* Base name */
	char *scope;                /* Scope/namespace */
	char *return_type;          /* For functions */
	char **param_types;         /* For functions */
	int param_count;
	symbol_type_t sym_type;
	mangle_scheme_t detected_scheme;
} demangle_info_t;

/*
 * Mangling Functions
 */

/* Mangle a symbol according to the specified scheme */
char *mangle_symbol(const symbol_info_t *info, mangle_scheme_t scheme);

/* Mangle a function name with parameters */
char *mangle_function(const char *name, const char *scope,
                      mangle_type_t *return_type,
                      mangle_type_t **param_types, int param_count,
                      mangle_scheme_t scheme);

/* Mangle a variable name */
char *mangle_variable(const char *name, const char *scope,
                      mangle_type_t *type, mangle_scheme_t scheme);

/* Mangle a type name */
char *mangle_type_name(const char *name, const char *scope,
                       mangle_scheme_t scheme);

/* Create PCC universal mangled name (works across all languages) */
char *mangle_pcc_universal(const symbol_info_t *info);

/*
 * Demangling Functions
 */

/* Demangle a symbol (auto-detects scheme) */
demangle_info_t *demangle_symbol(const char *mangled_name);

/* Demangle with specific scheme */
demangle_info_t *demangle_symbol_scheme(const char *mangled_name,
                                         mangle_scheme_t scheme);

/* Get just the base name from a mangled symbol */
char *demangle_name_only(const char *mangled_name);

/* Detect mangling scheme from a symbol */
mangle_scheme_t detect_mangle_scheme(const char *symbol);

/* Check if a symbol is mangled */
int is_mangled(const char *symbol);

/*
 * Type Construction Helpers
 */

/* Create a basic type descriptor */
mangle_type_t *mangle_type_create(type_kind_t kind, const char *name);

/* Create a pointer type */
mangle_type_t *mangle_type_pointer(mangle_type_t *base, type_qualifier_t quals);

/* Create a reference type */
mangle_type_t *mangle_type_reference(mangle_type_t *base);

/* Create an array type */
mangle_type_t *mangle_type_array(mangle_type_t *elem, size_t size);

/* Create a function type */
mangle_type_t *mangle_type_function(mangle_type_t *ret,
                                     mangle_type_t **params, int count);

/* Free type descriptor */
void mangle_type_free(mangle_type_t *type);

/*
 * Symbol Info Helpers
 */

/* Create symbol info structure */
symbol_info_t *symbol_info_create(const char *name, const char *scope);

/* Free symbol info */
void symbol_info_free(symbol_info_t *info);

/* Free demangle info */
void demangle_info_free(demangle_info_t *info);

/*
 * Utility Functions
 */

/* Convert mangled name from one scheme to another */
char *mangle_convert(const char *mangled_name,
                     mangle_scheme_t from_scheme,
                     mangle_scheme_t to_scheme);

/* Get a human-readable name for a mangling scheme */
const char *mangle_scheme_name(mangle_scheme_t scheme);

/* Validate a mangled name */
int mangle_validate(const char *mangled_name, mangle_scheme_t scheme);

/* Get the mangled length estimate for a symbol */
size_t mangle_estimate_length(const symbol_info_t *info, mangle_scheme_t scheme);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_MANGLE_H_ */
