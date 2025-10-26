/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Pascal dialect support
 * Handles differences between ISO Pascal, Borland, Delphi, FreePascal, etc.
 */

#ifndef DIALECT_H
#define DIALECT_H

/* Wirth Language Family and Ada dialect types */
typedef enum {
	/* Pascal dialects */
	DIALECT_ISO,            /* ISO 7185 Standard Pascal */
	DIALECT_ISO_EXTENDED,   /* ISO 10206 Extended Pascal */
	DIALECT_UCSD,           /* UCSD Pascal (P-System) */
	DIALECT_MICROSOFT,      /* Microsoft Pascal 4.0 */
	DIALECT_CLASCAL,        /* Apple Clascal (Object Pascal for classic Mac) */
	DIALECT_MACPASCAL,      /* Apple MacPascal / MPW Pascal */
	DIALECT_THINK,          /* Think Pascal (Symantec/Lightspeed) */
	DIALECT_BORLAND,        /* Borland Pascal / Turbo Pascal 7.0 */
	DIALECT_DELPHI,         /* Delphi Object Pascal */
	DIALECT_FREEPASCAL,     /* Free Pascal (default mode) */

	/* Modula-2 dialects */
	DIALECT_MODULA2_PIM2,   /* Modula-2 PIM 2nd edition */
	DIALECT_MODULA2_PIM3,   /* Modula-2 PIM 3rd edition */
	DIALECT_MODULA2_PIM4,   /* Modula-2 PIM 4th edition */
	DIALECT_MODULA2_ISO,    /* ISO Modula-2 (ISO/IEC 10514) */
	DIALECT_MODULA2_GNU,    /* GNU Modula-2 */
	DIALECT_MODULA2_R10,    /* Modula-2 R10 (modern revision) */

	/* Modula-3 */
	DIALECT_MODULA3,        /* Modula-3 (DEC SRC) */
	DIALECT_MODULA3_CM,     /* Critical Mass Modula-3 */
	DIALECT_MODULA3_PM,     /* Polytechnique Modula-3 */

	/* Oberon dialects */
	DIALECT_OBERON,         /* Oberon (original) */
	DIALECT_OBERON2,        /* Oberon-2 (with type-bound procedures) */
	DIALECT_OBERON07,       /* Oberon-07 (revised Oberon) */
	DIALECT_COMPONENT_PASCAL, /* Component Pascal (BlackBox) */
	DIALECT_ACTIVE_OBERON,  /* Active Oberon (multithreading) */
	DIALECT_OBERON_A2,      /* A2 Oberon (Bluebottle) */

	/* Ada dialects */
	DIALECT_ADA83,          /* Ada 83 (ANSI/MIL-STD-1815A) */
	DIALECT_ADA95,          /* Ada 95 (ISO/IEC 8652:1995) */
	DIALECT_ADA2005,        /* Ada 2005 (ISO/IEC 8652:1995/Amd 1:2007) */
	DIALECT_ADA2012,        /* Ada 2012 (ISO/IEC 8652:2012) */
	DIALECT_ADA2022,        /* Ada 2022 (ISO/IEC 8652:2023) */
	DIALECT_SPARK,          /* SPARK Ada (verification subset) */

	DIALECT_AUTO            /* Auto-detect from source */
} pascal_dialect_t;

/* Dialect feature flags */
typedef struct dialect_features {
	/* Language family */
	int language_family;            /* 0=Pascal, 1=Modula-2, 2=Modula-3, 3=Oberon, 4=Ada */

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
	int null_terminated_strings;    /* Modula-2/Oberon null-terminated */

	/* Type system extensions */
	int allow_unsigned_types;       /* unsigned, byte, word, dword, CARDINAL */
	int allow_int64;                /* int64, qword, LONGINT */
	int allow_currency;             /* currency type (Delphi) */
	int allow_variant;              /* variant type */
	int allow_dynamic_arrays;       /* array of T */
	int allow_open_arrays;          /* open array parameters */
	int allow_set_operators;        /* +, -, *, in for sets */
	int allow_string_type;          /* string as built-in type */
	int allow_subranges;            /* subrange types [low..high] */
	int allow_opaque_types;         /* Modula-2 opaque types */
	int allow_type_extension;       /* Oberon type extension */
	int allow_record_variants;      /* variant records */

	/* Object-oriented features */
	int allow_objects;              /* object type (Turbo Pascal) */
	int allow_classes;              /* class type (Delphi) */
	int allow_interfaces;           /* interface type */
	int allow_properties;           /* property declarations */
	int allow_operator_overload;    /* operator overloading */
	int allow_type_bound_procs;     /* Oberon-2 type-bound procedures */
	int allow_tagged_types;         /* Ada tagged types */

	/* Procedure/function extensions */
	int allow_default_params;       /* Default parameter values */
	int allow_overloading;          /* Function overloading */
	int allow_inline_directive;     /* inline directive */
	int allow_forward_directive;    /* forward directive */
	int allow_external_directive;   /* external directive */
	int allow_interrupt_directive;  /* interrupt directive */
	int allow_varargs;              /* Variadic functions */
	int allow_function_types;       /* PROCEDURE types (Modula-2) */

	/* Modules and packages */
	int allow_units;                /* unit...interface...implementation */
	int allow_modules;              /* DEFINITION/IMPLEMENTATION MODULE */
	int allow_packages;             /* Ada packages */
	int allow_namespaces;           /* namespace support */
	int allow_uses_clause;          /* uses clause for importing */
	int allow_import_clause;        /* IMPORT clause (Modula/Oberon) */
	int require_module_header;      /* Require MODULE declaration */

	/* Control flow */
	int allow_break_continue;       /* break, continue statements */
	int allow_exit_function;        /* exit with return value */
	int allow_loop_statement;       /* LOOP...EXIT...END (Modula-2) */
	int allow_with_statement;       /* WITH statement */
	int allow_case_else;            /* else clause in case */
	int allow_return_statement;     /* RETURN statement (Modula/Oberon) */

	/* Concurrency (Modula-2, Modula-3, Ada) */
	int allow_coroutines;           /* Modula-2 coroutines */
	int allow_processes;            /* Modula-2 PROCESS type */
	int allow_threads;              /* Modula-3/Ada tasks */
	int allow_protected_types;      /* Ada protected types */

	/* Memory management */
	int allow_pointers;             /* Pointer types */
	int allow_nil;                  /* NIL constant */
	int allow_address_type;         /* ADDRESS type (Modula-2) */
	int allow_unsafe;               /* Modula-3 UNSAFE operations */
	int allow_unchecked;            /* Ada Unchecked_* operations */

	/* Exception handling */
	int allow_exceptions;           /* Modula-3/Ada exceptions */
	int allow_try_except;           /* TRY...EXCEPT (Modula-3) */
	int allow_try_finally;          /* TRY...FINALLY */

	/* Miscellaneous */
	int allow_absolute;             /* absolute variable addressing */
	int allow_packed;               /* packed directive */
	int allow_far_near;             /* far/near pointer modifiers */
	int allow_interrupt;            /* interrupt keyword */
	int allow_assembler;            /* assembler keyword */
	int allow_exports;              /* exports section */
	int allow_resourcestring;       /* resourcestring section */
	int allow_threadvar;            /* threadvar section */
	int allow_generic;              /* generic programming */
	int allow_pragma;               /* Ada pragma / Modula-2 pragma */
	int allow_separate;             /* Ada separate compilation */

	/* Case sensitivity */
	int case_sensitive;             /* Identifiers are case-sensitive */

	/* Language constraints */
	int strict_iso;                 /* Enforce strict ISO compliance */
	int require_program_header;     /* Require "program" declaration */
	int require_forward_decl;       /* Require forward declarations */
	int require_type_declarations;  /* Ada-style type declarations */

	/* Numeric handling */
	int default_int_size;           /* Default integer size (16 or 32 bit) */
	int default_real_type;          /* 0=real, 1=double, 2=extended */

	/* Syntax preferences */
	int use_begin_end;              /* BEGIN...END blocks */
	int use_do_end;                 /* DO...END blocks (Modula) */
	int assignment_operator;        /* 0=:=, 1== (some Oberons) */
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

/* Pascal family */
extern dialect_features_t iso_features;
extern dialect_features_t iso_extended_features;
extern dialect_features_t ucsd_features;
extern dialect_features_t microsoft_features;
extern dialect_features_t clascal_features;
extern dialect_features_t macpascal_features;
extern dialect_features_t think_features;
extern dialect_features_t borland_features;
extern dialect_features_t delphi_features;
extern dialect_features_t freepascal_features;

/* Modula-2 family */
extern dialect_features_t modula2_pim2_features;
extern dialect_features_t modula2_pim3_features;
extern dialect_features_t modula2_pim4_features;
extern dialect_features_t modula2_iso_features;
extern dialect_features_t modula2_gnu_features;
extern dialect_features_t modula2_r10_features;

/* Modula-3 family */
extern dialect_features_t modula3_features;
extern dialect_features_t modula3_cm_features;
extern dialect_features_t modula3_pm_features;

/* Oberon family */
extern dialect_features_t oberon_features;
extern dialect_features_t oberon2_features;
extern dialect_features_t oberon07_features;
extern dialect_features_t component_pascal_features;
extern dialect_features_t active_oberon_features;
extern dialect_features_t oberon_a2_features;

/* Ada family */
extern dialect_features_t ada83_features;
extern dialect_features_t ada95_features;
extern dialect_features_t ada2005_features;
extern dialect_features_t ada2012_features;
extern dialect_features_t ada2022_features;
extern dialect_features_t spark_features;

#endif /* DIALECT_H */
