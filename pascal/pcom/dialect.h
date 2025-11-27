/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Pascal dialect support
 * Handles differences between ISO Pascal, Borland, Delphi, FreePascal, etc.
 */

#ifndef DIALECT_H
#define DIALECT_H

/* Pascal dialect types */
typedef enum {
	DIALECT_ISO,            /* ISO 7185 Standard Pascal */
	DIALECT_ISO_EXTENDED,   /* ISO 10206 Extended Pascal */
	DIALECT_MICROSOFT,      /* Microsoft Pascal 4.0 */
	DIALECT_CLASCAL,        /* Apple Clascal (Object Pascal for classic Mac) */
	DIALECT_MACPASCAL,      /* Apple MacPascal / MPW Pascal */
	DIALECT_BORLAND,        /* Borland Pascal / Turbo Pascal 7.0 */
	DIALECT_DELPHI,         /* Delphi Object Pascal */
	DIALECT_FREEPASCAL,     /* Free Pascal (default mode) */
	DIALECT_AUTO            /* Auto-detect from source */
} pascal_dialect_t;

/* Dialect feature flags */
typedef struct dialect_features {
	/* Language extensions */
	int allow_nested_comments;      /* (* (* nested *) *) */
	int allow_cpp_comments;         /* // C++-style comments */
	int allow_dollar_ident;         /* $-prefixed identifiers */
	int allow_underscores;          /* Underscores in identifiers */
	int allow_inline_assembly;      /* asm...end blocks */
	int allow_goto;                 /* goto statement */

	/* String handling */
	int allow_cstring_literals;     /* C-style "string" literals */
	int allow_string_concat;        /* Automatic string concatenation */
	int allow_escape_sequences;     /* \n, \t, etc. in strings */
	int pstring_literals;           /* Pascal-style 'string' */

	/* Type system extensions */
	int allow_unsigned_types;       /* unsigned, byte, word, dword */
	int allow_int64;                /* int64, qword */
	int allow_currency;             /* currency type (Delphi) */
	int allow_variant;              /* variant type */
	int allow_dynamic_arrays;       /* array of T */
	int allow_open_arrays;          /* open array parameters */
	int allow_set_operators;        /* +, -, *, in for sets */
	int allow_string_type;          /* string as built-in type */

	/* Object-oriented features */
	int allow_objects;              /* object type (Turbo Pascal) */
	int allow_classes;              /* class type (Delphi) */
	int allow_interfaces;           /* interface type */
	int allow_properties;           /* property declarations */
	int allow_operator_overload;    /* operator overloading */

	/* Procedure/function extensions */
	int allow_default_params;       /* Default parameter values */
	int allow_overloading;          /* Function overloading */
	int allow_inline_directive;     /* inline directive */
	int allow_forward_directive;    /* forward directive */
	int allow_external_directive;   /* external directive */
	int allow_interrupt_directive;  /* interrupt directive */
	int allow_varargs;              /* Variadic functions */

	/* Units and modules */
	int allow_units;                /* unit...interface...implementation */
	int allow_namespaces;           /* namespace support (FreePascal) */
	int allow_uses_clause;          /* uses clause for importing */

	/* Control flow */
	int allow_break_continue;       /* break, continue statements */
	int allow_exit_function;        /* exit with return value */
	int allow_case_else;            /* else clause in case */

	/* Miscellaneous */
	int allow_absolute;             /* absolute variable addressing */
	int allow_packed;               /* packed directive */
	int allow_far_near;             /* far/near pointer modifiers */
	int allow_interrupt;            /* interrupt keyword */
	int allow_assembler;            /* assembler keyword */
	int allow_exports;              /* exports section */
	int allow_resourcestring;       /* resourcestring section */
	int allow_threadvar;            /* threadvar section */
	int allow_generic;              /* generic programming (FreePascal) */

	/* Case sensitivity */
	int case_sensitive;             /* Identifiers are case-sensitive */

	/* Standard Pascal constraints */
	int strict_iso;                 /* Enforce strict ISO compliance */
	int require_program_header;     /* Require "program" declaration */
	int require_forward_decl;       /* Require forward declarations */

	/* Numeric handling */
	int default_int_size;           /* Default integer size (16 or 32 bit) */
	int default_real_type;          /* 0=real, 1=double, 2=extended */
} dialect_features_t;

/* Global current dialect */
extern pascal_dialect_t current_dialect;
extern dialect_features_t *dialect_features;

/* Initialize dialect system */
void dialect_init(void);

/* Set dialect and configure features */
void set_dialect(pascal_dialect_t dialect);

/* Get dialect name as string */
const char *get_dialect_name(pascal_dialect_t dialect);

/* Parse dialect from command-line option */
pascal_dialect_t parse_dialect(const char *name);

/* Check if a feature is enabled in current dialect */
int feature_enabled(int feature);

/* Feature check macros for common patterns */
#define ALLOW_NESTED_COMMENTS()   (dialect_features->allow_nested_comments)
#define ALLOW_CPP_COMMENTS()      (dialect_features->allow_cpp_comments)
#define ALLOW_UNDERSCORES()       (dialect_features->allow_underscores)
#define ALLOW_INLINE_ASM()        (dialect_features->allow_inline_assembly)
#define ALLOW_UNITS()             (dialect_features->allow_units)
#define ALLOW_CLASSES()           (dialect_features->allow_classes)
#define ALLOW_OBJECTS()           (dialect_features->allow_objects)
#define ALLOW_OVERLOADING()       (dialect_features->allow_overloading)
#define IS_CASE_SENSITIVE()       (dialect_features->case_sensitive)

/* Predefined dialect feature sets */
extern dialect_features_t iso_features;
extern dialect_features_t iso_extended_features;
extern dialect_features_t microsoft_features;
extern dialect_features_t clascal_features;
extern dialect_features_t macpascal_features;
extern dialect_features_t borland_features;
extern dialect_features_t delphi_features;
extern dialect_features_t freepascal_features;

#endif /* DIALECT_H */
