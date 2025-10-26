/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * BASIC dialect support
 * Handles differences between GW-BASIC, Turbo BASIC, VB-DOS, PDS, PowerBASIC, FreeBASIC, ISO BASIC
 */

#ifndef DIALECT_H
#define DIALECT_H

/* BASIC dialect types */
typedef enum {
	DIALECT_GWBASIC,        /* GW-BASIC */
	DIALECT_TURBO,          /* Turbo BASIC */
	DIALECT_VBDOS,          /* Visual Basic for DOS */
	DIALECT_PDS,            /* Microsoft BASIC PDS 7.1 */
	DIALECT_POWERBASIC,     /* PowerBASIC */
	DIALECT_FREEBASIC,      /* FreeBASIC */
	DIALECT_ISO,            /* ISO BASIC Advanced */
	DIALECT_QUICKBASIC,     /* Macintosh QuickBASIC */
	DIALECT_ATARI_ST,       /* Atari ST BASIC */
	DIALECT_AMIGA,          /* Amiga BASIC */
	DIALECT_DEC,            /* DEC BASIC-PLUS/BASIC-PLUS-2 */
	DIALECT_HP,             /* HP BASIC */
	DIALECT_IBM,            /* IBM BASIC (PC BASIC, BASICA) */
	DIALECT_AUTO            /* Auto-detect from source */
} basic_dialect_t;

/* Dialect feature flags */
typedef struct dialect_features {
	/* Line number handling */
	int require_line_numbers;       /* Require line numbers on all lines */
	int allow_line_labels;          /* Allow alphanumeric labels */
	int allow_multistatement;       /* Allow multiple statements per line with : */

	/* Comments */
	int allow_rem_statement;        /* REM comments */
	int allow_quote_comment;        /* ' style comments */
	int allow_inline_comments;      /* Comments after statements */

	/* Variable naming */
	int allow_long_names;           /* Variable names > 2 chars */
	int allow_underscores;          /* Underscores in identifiers */
	int require_type_suffix;        /* Require $,%,&,!,# suffixes */
	int allow_explicit_types;       /* Allow AS INTEGER, AS STRING, etc. */
	int case_sensitive;             /* Case-sensitive identifiers */

	/* String handling */
	int allow_string_functions;     /* LEFT$, RIGHT$, MID$, etc. */
	int allow_string_concat;        /* + for string concatenation */
	int allow_fixed_strings;        /* STRING * length */
	int max_string_length;          /* Maximum string length (0 = unlimited) */

	/* Type system */
	int allow_integer;              /* INTEGER type (% suffix) */
	int allow_long;                 /* LONG type (& suffix) */
	int allow_single;               /* SINGLE type (! suffix) */
	int allow_double;               /* DOUBLE type (# suffix) */
	int allow_string;               /* STRING type ($ suffix) */
	int allow_currency;             /* CURRENCY type */
	int allow_byte;                 /* BYTE type */
	int allow_boolean;              /* BOOLEAN type */
	int allow_variant;              /* VARIANT type (VB-style) */
	int allow_udt;                  /* User-defined types (TYPE...END TYPE) */

	/* Array features */
	int allow_dynamic_arrays;       /* REDIM statement */
	int allow_multidim_arrays;      /* Multi-dimensional arrays */
	int default_array_base;         /* Default OPTION BASE (0 or 1) */
	int allow_option_base;          /* OPTION BASE statement */

	/* Control flow */
	int allow_structured_if;        /* IF...THEN...ELSE...END IF */
	int allow_select_case;          /* SELECT CASE statement */
	int allow_do_loop;              /* DO...LOOP */
	int allow_while_wend;           /* WHILE...WEND */
	int allow_for_next;             /* FOR...NEXT */
	int allow_for_step;             /* STEP clause in FOR */
	int allow_exit_statement;       /* EXIT FOR, EXIT DO, EXIT SUB, etc. */

	/* Subroutines and functions */
	int allow_gosub;                /* GOSUB...RETURN */
	int allow_def_fn;               /* DEF FN single-line functions */
	int allow_sub_procedures;       /* SUB...END SUB */
	int allow_function_procedures;  /* FUNCTION...END FUNCTION */
	int allow_byref_byval;          /* BYREF/BYVAL parameter passing */
	int allow_optional_params;      /* OPTIONAL parameters */
	int allow_param_arrays;         /* ParamArray (VB-style) */

	/* Scope and visibility */
	int allow_static;               /* STATIC keyword */
	int allow_shared;               /* SHARED keyword */
	int allow_common;               /* COMMON statement */
	int allow_local;                /* Local variables in SUB/FUNCTION */

	/* I/O features */
	int allow_print_using;          /* PRINT USING formatted output */
	int allow_input_prompt;         /* INPUT with prompt */
	int allow_line_input;           /* LINE INPUT */
	int allow_file_io;              /* OPEN, CLOSE, GET, PUT, etc. */
	int allow_binary_files;         /* Binary file access */
	int allow_random_files;         /* Random file access */

	/* Graphics and sound */
	int allow_graphics;             /* PSET, LINE, CIRCLE, etc. */
	int allow_screen_modes;         /* SCREEN statement */
	int allow_sound;                /* SOUND, PLAY statements */

	/* Memory and system */
	int allow_peek_poke;            /* PEEK, POKE statements */
	int allow_inline_asm;           /* Inline assembly (PowerBASIC) */
	int allow_pointer_ops;          /* Pointer operations (FreeBASIC) */
	int allow_oop;                  /* Object-oriented features (FreeBASIC) */

	/* Meta-programming */
	int allow_meta_commands;        /* $INCLUDE, $DYNAMIC, etc. */
	int allow_option_explicit;      /* OPTION EXPLICIT */
	int allow_option_compare;       /* OPTION COMPARE */

	/* Modern features */
	int allow_overloading;          /* Function overloading */
	int allow_operator_overload;    /* Operator overloading */
	int allow_namespaces;           /* Namespace support */
	int allow_classes;              /* CLASS definitions */

	/* Numeric precision */
	int default_int_size;           /* 16 or 32 bit default integer */
	int default_real_type;          /* 0=single, 1=double */
} dialect_features_t;

/* Global current dialect */
extern basic_dialect_t current_dialect;
extern dialect_features_t *dialect_features;

/* Initialize dialect system */
void dialect_init(void);

/* Set dialect and configure features */
void set_dialect(basic_dialect_t dialect);

/* Get dialect name as string */
const char *get_dialect_name(basic_dialect_t dialect);

/* Parse dialect from command-line option */
basic_dialect_t parse_dialect(const char *name);

/* Feature check macros for common patterns */
#define ALLOW_LINE_NUMBERS()      (dialect_features->require_line_numbers)
#define ALLOW_LONG_NAMES()        (dialect_features->allow_long_names)
#define ALLOW_UNDERSCORES()       (dialect_features->allow_underscores)
#define ALLOW_SUB_PROCEDURES()    (dialect_features->allow_sub_procedures)
#define ALLOW_UDT()               (dialect_features->allow_udt)
#define ALLOW_OOP()               (dialect_features->allow_oop)
#define IS_CASE_SENSITIVE()       (dialect_features->case_sensitive)

/* Predefined dialect feature sets */
extern dialect_features_t gwbasic_features;
extern dialect_features_t turbo_features;
extern dialect_features_t vbdos_features;
extern dialect_features_t pds_features;
extern dialect_features_t powerbasic_features;
extern dialect_features_t freebasic_features;
extern dialect_features_t iso_features;
extern dialect_features_t quickbasic_features;
extern dialect_features_t atari_st_features;
extern dialect_features_t amiga_features;
extern dialect_features_t dec_features;
extern dialect_features_t hp_features;
extern dialect_features_t ibm_features;

#endif /* DIALECT_H */
