/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Dialect support for PAL and ObjectPAL
 * Handles differences between Paradox versions and PAL variants
 */

#ifndef DIALECT_H
#define DIALECT_H

/* PAL dialect types */
typedef enum {
	DIALECT_PAL_1_0,        /* Paradox 1.0 PAL */
	DIALECT_PAL_3_0,        /* Paradox 3.0 PAL */
	DIALECT_PAL_3_5,        /* Paradox 3.5 PAL */
	DIALECT_PAL_4_0,        /* Paradox 4.0 PAL */
	DIALECT_PAL_4_5,        /* Paradox 4.5 PAL */
	DIALECT_OBJECTPAL_1_0,  /* ObjectPAL 1.0 (Paradox for Windows 1.0) */
	DIALECT_OBJECTPAL_5_0,  /* ObjectPAL for Paradox 5.0 */
	DIALECT_OBJECTPAL_7_0,  /* ObjectPAL for Paradox 7.0 */
	DIALECT_OBJECTPAL_8_0,  /* ObjectPAL for Paradox 8.0 */
	DIALECT_OBJECTPAL_9_0,  /* ObjectPAL for Paradox 9.0 (Corel) */
	DIALECT_OBJECTPAL_LATEST, /* Latest ObjectPAL version */
	DIALECT_AUTO            /* Auto-detect from source */
} pal_dialect_t;

/* Dialect feature flags */
typedef struct dialect_features {
	/* Language core features */
	int allow_procedures;           /* Procedures/methods */
	int allow_functions;            /* Functions with return values */
	int allow_local_vars;           /* Local variables in procedures */
	int allow_nested_procedures;    /* Nested procedure definitions */

	/* Object-oriented features (ObjectPAL) */
	int allow_objects;              /* Object types */
	int allow_methods;              /* Object methods */
	int allow_properties;           /* Object properties */
	int allow_inheritance;          /* Object inheritance */
	int allow_events;               /* Event handlers */
	int allow_private_members;      /* Private/protected visibility */
	int allow_constructors;         /* Constructor methods */
	int allow_destructors;          /* Destructor methods */

	/* Data types */
	int allow_variant_type;         /* Variant (any) type */
	int allow_currency_type;        /* Currency type */
	int allow_blob_types;           /* BLOB, Memo, Graphic types */
	int allow_datetime_types;       /* Date, Time, DateTime types */
	int allow_dynamic_arrays;       /* Dynamic arrays */
	int allow_strings;              /* String type */
	int allow_pointer_types;        /* Pointer/reference types */

	/* Control flow */
	int allow_while_loop;           /* while loop */
	int allow_for_loop;             /* for loop */
	int allow_foreach_loop;         /* foreach loop (ObjectPAL) */
	int allow_switch_statement;     /* switch/case statement */
	int allow_break_continue;       /* break/continue statements */
	int allow_return_statement;     /* return statement */
	int allow_try_except;           /* try-except exception handling */

	/* Database operations */
	int allow_sql_support;          /* SQL query support */
	int allow_table_operations;     /* Table manipulation */
	int allow_query_by_example;     /* Query By Example (QBE) */
	int allow_paradox_tables;       /* Direct Paradox table access */
	int allow_odbc_support;         /* ODBC database access */

	/* User interface */
	int allow_form_events;          /* Form event handlers */
	int allow_ui_objects;           /* UI object manipulation */
	int allow_menu_support;         /* Menu creation/handling */
	int allow_report_objects;       /* Report objects */
	int allow_graphics;             /* Graphics operations */

	/* String handling */
	int allow_string_concat;        /* String concatenation with + */
	int allow_string_functions;     /* Built-in string functions */
	int allow_format_functions;     /* String formatting */

	/* Comments */
	int allow_semicolon_comments;   /* ; comments (PAL style) */
	int allow_slash_comments;       /* // comments (ObjectPAL style) */
	int allow_block_comments;       /* { } or (* *) comments */

	/* Miscellaneous */
	int allow_include_files;        /* Include directive */
	int allow_debug_statements;     /* Debug/assert statements */
	int allow_inline_code;          /* Inline assembly/code */
	int case_sensitive;             /* Case-sensitive identifiers */

	/* Standard library features */
	int stdlib_math;                /* Math functions */
	int stdlib_string;              /* String manipulation */
	int stdlib_date;                /* Date/time functions */
	int stdlib_file;                /* File I/O */
	int stdlib_system;              /* System functions */

	/* Version-specific */
	int version_major;              /* Major version number */
	int version_minor;              /* Minor version number */
	const char *version_name;       /* Version name string */
} dialect_features_t;

/* Global current dialect */
extern pal_dialect_t current_dialect;
extern dialect_features_t *dialect_features;

/* Initialize dialect system */
void dialect_init(void);

/* Set dialect and configure features */
void set_dialect(pal_dialect_t dialect);

/* Get dialect name as string */
const char *get_dialect_name(pal_dialect_t dialect);

/* Parse dialect from command-line option */
pal_dialect_t parse_dialect(const char *name);

/* Check if a feature is enabled in current dialect */
int feature_enabled(int feature);

/* Feature check macros for common patterns */
#define ALLOW_OBJECTS()           (dialect_features->allow_objects)
#define ALLOW_METHODS()           (dialect_features->allow_methods)
#define ALLOW_INHERITANCE()       (dialect_features->allow_inheritance)
#define ALLOW_EVENTS()            (dialect_features->allow_events)
#define ALLOW_TRY_EXCEPT()        (dialect_features->allow_try_except)
#define ALLOW_FOREACH()           (dialect_features->allow_foreach_loop)
#define ALLOW_SQL()               (dialect_features->allow_sql_support)
#define IS_CASE_SENSITIVE()       (dialect_features->case_sensitive)
#define IS_OBJECTPAL()            (current_dialect >= DIALECT_OBJECTPAL_1_0)

/* Predefined dialect feature sets */
extern dialect_features_t pal_1_0_features;
extern dialect_features_t pal_3_0_features;
extern dialect_features_t pal_4_5_features;
extern dialect_features_t objectpal_1_0_features;
extern dialect_features_t objectpal_7_0_features;
extern dialect_features_t objectpal_latest_features;

#endif /* DIALECT_H */
