/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * PL/I dialect support
 * Handles PL/I, PL/M, PL/C, and other PL dialect variations
 */

#ifndef DIALECT_H
#define DIALECT_H

/* PL/I dialect types */
typedef enum {
	DIALECT_PLI,            /* Standard PL/I (IBM) */
	DIALECT_PLI_SUBSET,     /* PL/I Subset G */
	DIALECT_PLI_OPTIMIZING, /* Optimizing PL/I */
	DIALECT_PLM,            /* Intel PL/M (8080, 8086, 80286, 80386, 80486) */
	DIALECT_PLM86,          /* PL/M-86 specifically */
	DIALECT_PLM386,         /* PL/M-386 specifically */
	DIALECT_PLC,            /* PL/C (teaching dialect) */
	DIALECT_PL1,            /* PL/1 (alternate name) */
	DIALECT_PLSQL,          /* Oracle PL/SQL (limited support) */
	DIALECT_AUTO            /* Auto-detect from source */
} pli_dialect_t;

/* Dialect feature flags */
typedef struct dialect_features {
	/* Basic language features */
	int allow_abbreviated_keywords;  /* Abbrev keywords (DCL, PROC, etc.) */
	int allow_dollar_ident;          /* $ in identifiers */
	int allow_underscores;           /* _ in identifiers */
	int min_keyword_length;          /* Minimum keyword length (2-4) */
	int case_sensitive;              /* Case-sensitive identifiers */

	/* Comment styles */
	int allow_c_comments;            /* /* */ style */
	int allow_cpp_comments;          /* // style */
	int allow_nested_comments;       /* Nested /* */ */

	/* Data types */
	int allow_picture;               /* PICTURE declarations */
	int allow_area;                  /* AREA data type */
	int allow_task;                  /* TASK data type */
	int allow_event;                 /* EVENT data type */
	int allow_ordinal;               /* ORDINAL attribute */
	int allow_generic;               /* GENERIC attribute */
	int allow_builtin;               /* BUILTIN attribute */
	int allow_defined;               /* DEFINED attribute */
	int allow_based;                 /* BASED attribute */
	int allow_controlled;            /* CONTROLLED storage */
	int allow_static;                /* STATIC storage */

	/* Numeric types */
	int default_fixed_binary;        /* FIXED BIN is default integer */
	int default_fixed_decimal;       /* FIXED DEC is default */
	int default_float_binary;        /* FLOAT BIN is default real */
	int max_fixed_precision;         /* Max FIXED precision (15-31) */
	int max_float_precision;         /* Max FLOAT precision */

	/* String handling */
	int allow_varying_strings;       /* VARYING attribute */
	int max_char_length;             /* Max CHARACTER length */
	int max_bit_length;              /* Max BIT length */
	int allow_graphic;               /* GRAPHIC (DBCS) type */
	int allow_widechar;              /* WIDECHAR type */

	/* Structures */
	int allow_level_numbers;         /* Level numbers (1-255) */
	int allow_factored_declares;     /* Factored DECLARE */
	int allow_union;                 /* UNION attribute */
	int max_structure_depth;         /* Max nesting depth */

	/* Arrays */
	int allow_asterisk_extent;       /* * for extent */
	int allow_refer_option;          /* REFER option */
	int allow_hbound_lbound;         /* HBOUND/LBOUND functions */
	int max_array_dimensions;        /* Max dimensions (usually 32) */
	int arrays_start_at_one;         /* Arrays start at 1 (0 otherwise) */

	/* Procedures */
	int allow_recursive;             /* RECURSIVE attribute */
	int allow_reentrant;             /* REENTRANT attribute */
	int allow_main_procedure;        /* MAIN attribute */
	int allow_options_main;          /* OPTIONS(MAIN) */
	int allow_entry_points;          /* ENTRY statements */
	int allow_generic_entry;         /* GENERIC entry names */
	int allow_returns;               /* RETURNS attribute */
	int allow_external;              /* EXTERNAL attribute */
	int allow_internal;              /* INTERNAL attribute */

	/* PL/M specific features */
	int is_plm;                      /* This is PL/M */
	int allow_at_clause;             /* AT address clause (PL/M) */
	int allow_data_clause;           /* DATA initialization (PL/M) */
	int allow_literally;             /* LITERALLY macro (PL/M) */
	int allow_public_external;       /* PUBLIC keyword (PL/M) */
	int allow_interrupt;             /* INTERRUPT procedures (PL/M) */
	int allow_byte_word_types;       /* BYTE, WORD, DWORD, ADDRESS */
	int allow_structure_assign;      /* Structure assignment (PL/M-386+) */

	/* Control structures */
	int allow_do_case;               /* DO CASE statement (PL/M) */
	int allow_do_while;              /* DO WHILE */
	int allow_do_until;              /* DO UNTIL */
	int allow_do_to_by;              /* DO var = start TO end BY inc */
	int allow_leave;                 /* LEAVE statement */
	int allow_iterate;               /* ITERATE statement */
	int allow_select;                /* SELECT statement */
	int allow_otherwise;             /* OTHERWISE in SELECT */

	/* I/O */
	int allow_get_put;               /* GET/PUT statements */
	int allow_read_write;            /* READ/WRITE statements */
	int allow_stream_io;             /* STREAM I/O */
	int allow_record_io;             /* RECORD I/O */
	int allow_locate_mode;           /* LOCATE mode I/O */
	int allow_keyed_io;              /* KEYED attribute */
	int allow_sequential;            /* SEQUENTIAL attribute */
	int allow_direct;                /* DIRECT attribute */

	/* Exception handling */
	int allow_on_conditions;         /* ON conditions */
	int allow_signal_revert;         /* SIGNAL/REVERT */
	int allow_error_condition;       /* ERROR condition */
	int allow_endfile_condition;     /* ENDFILE condition */
	int allow_conversion_condition;  /* CONVERSION condition */
	int allow_fixedoverflow;         /* FIXEDOVERFLOW condition */
	int allow_zerodivide;            /* ZERODIVIDE condition */

	/* Compile-time facilities */
	int allow_percent_statements;    /* %DECLARE, %ACTIVATE, etc. */
	int allow_preprocessor;          /* Full preprocessor */
	int allow_include;               /* %INCLUDE */
	int allow_replace;               /* %REPLACE (like #define) */

	/* Storage management */
	int allow_allocate_free;         /* ALLOCATE/FREE statements */
	int allow_pointer_arithmetic;    /* Pointer arithmetic */
	int allow_addr_function;         /* ADDR built-in */
	int allow_null_builtin;          /* NULL built-in */

	/* Multi-tasking */
	int allow_multitasking;          /* TASK, EVENT, etc. */
	int allow_priority;              /* PRIORITY attribute */

	/* Miscellaneous */
	int allow_goto;                  /* GOTO statement */
	int allow_format;                /* FORMAT statements */
	int allow_return_expr;           /* RETURN(expr) */
	int allow_stop_exit;             /* STOP/EXIT statements */
	int allow_attributes_in_dcl;     /* All attributes in DECLARE */
	int allow_implicit_declare;      /* Implicit declarations */
	int require_declare;             /* All vars must be declared */

	/* Default settings */
	int default_aligned;             /* ALIGNED is default */
	int default_signed;              /* SIGNED is default */
} dialect_features_t;

/* Global current dialect */
extern pli_dialect_t current_dialect;
extern dialect_features_t *dialect_features;

/* Initialize dialect system */
void dialect_init(void);

/* Set dialect and configure features */
void set_dialect(pli_dialect_t dialect);

/* Get dialect name */
const char *get_dialect_name(pli_dialect_t dialect);

/* Parse dialect from command-line */
pli_dialect_t parse_dialect(const char *name);

/* Check if feature is enabled */
int feature_enabled(int feature);

/* Feature check macros */
#define ALLOW_ABBREVIATED_KW()    (dialect_features->allow_abbreviated_keywords)
#define ALLOW_UNDERSCORES()       (dialect_features->allow_underscores)
#define ALLOW_PICTURE()           (dialect_features->allow_picture)
#define ALLOW_BASED()             (dialect_features->allow_based)
#define ALLOW_CONTROLLED()        (dialect_features->allow_controlled)
#define ALLOW_RECURSIVE()         (dialect_features->allow_recursive)
#define IS_PLM()                  (dialect_features->is_plm)
#define ALLOW_LITERALLY()         (dialect_features->allow_literally)
#define ALLOW_INTERRUPT()         (dialect_features->allow_interrupt)
#define IS_CASE_SENSITIVE()       (dialect_features->case_sensitive)

/* Predefined dialect feature sets */
extern dialect_features_t pli_features;
extern dialect_features_t pli_subset_features;
extern dialect_features_t pli_optimizing_features;
extern dialect_features_t plm_features;
extern dialect_features_t plm86_features;
extern dialect_features_t plm386_features;
extern dialect_features_t plc_features;

#endif /* DIALECT_H */
