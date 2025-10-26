/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Pascal dialect support
 * Handles differences between ISO Pascal, Borland, Delphi, FreePascal, etc.
 */

#ifndef DIALECT_H
#define DIALECT_H

/* Wirth Language Family and Related Languages - Complete Taxonomy */
typedef enum {
	/* ===== PASCAL FAMILY ===== */

	/* Standard Pascal */
	DIALECT_ISO,            /* ISO 7185 Standard Pascal (1983) */
	DIALECT_ISO_EXTENDED,   /* ISO 10206 Extended Pascal (1990) */
	DIALECT_PASCAL_P,       /* Pascal-P (portable Pascal, Zurich) */
	DIALECT_PASCAL_S,       /* Pascal-S (subset for teaching) */

	/* UCSD Pascal variants */
	DIALECT_UCSD,           /* UCSD Pascal (P-System) */
	DIALECT_UCSD_II,        /* UCSD Pascal System II.0 */
	DIALECT_UCSD_IV,        /* UCSD Pascal System IV.0 */

	/* DEC/HP/VAX Pascal family */
	DIALECT_VAX_PASCAL,     /* VAX Pascal (DEC) */
	DIALECT_DEC_PASCAL,     /* DEC Pascal */
	DIALECT_VSI_PASCAL,     /* VSI Pascal (OpenVMS) */
	DIALECT_HP_PASCAL,      /* HP Pascal */

	/* Vendor-specific Pascal */
	DIALECT_MICROSOFT,      /* Microsoft Pascal 4.0 */
	DIALECT_IBM_PASCAL,     /* IBM Pascal/VS */
	DIALECT_SUN_PASCAL,     /* Sun Pascal */
	DIALECT_OREGON_PASCAL,  /* Oregon Pascal */

	/* Apple Pascal family */
	DIALECT_APPLE_PASCAL,   /* Apple Pascal (Apple II/III) */
	DIALECT_CLASCAL,        /* Clascal (Object Pascal for classic Mac) */
	DIALECT_MACPASCAL,      /* MacPascal / MPW Pascal */
	DIALECT_THINK,          /* Think Pascal (Symantec/Lightspeed) */

	/* PC Pascal compilers */
	DIALECT_TURBO_1,        /* Turbo Pascal 1.0 (CP/M) */
	DIALECT_TURBO_3,        /* Turbo Pascal 3.0 (DOS) */
	DIALECT_TURBO_5,        /* Turbo Pascal 5.5 (OOP added) */
	DIALECT_BORLAND,        /* Borland Pascal / Turbo Pascal 7.0 */
	DIALECT_BP_WIN,         /* Borland Pascal 7.0 for Windows */
	DIALECT_TMT_PASCAL,     /* TMT Pascal */
	DIALECT_VIRTUAL_PASCAL, /* Virtual Pascal (OS/2, Win32, Linux) */

	/* Object Pascal evolution */
	DIALECT_OBJECT_PASCAL,  /* Generic Object Pascal */
	DIALECT_DELPHI_1,       /* Delphi 1 (16-bit) */
	DIALECT_DELPHI_3,       /* Delphi 3 (first 32-bit) */
	DIALECT_DELPHI_7,       /* Delphi 7 (classic) */
	DIALECT_DELPHI,         /* Delphi Object Pascal (modern) */
	DIALECT_DELPHI_PRISM,   /* Delphi Prism (.NET) */
	DIALECT_OXYGENE,        /* Oxygene (RemObjects, .NET/Java/Cocoa) */

	/* Free Pascal modes */
	DIALECT_FREEPASCAL,     /* Free Pascal (FPC mode) */
	DIALECT_FPC_OBJFPC,     /* FPC ObjFPC mode */
	DIALECT_FPC_DELPHI,     /* FPC Delphi mode */
	DIALECT_FPC_TP,         /* FPC Turbo Pascal mode */
	DIALECT_FPC_MACPAS,     /* FPC Mac Pascal mode */
	DIALECT_FPC_ISO,        /* FPC ISO mode */

	/* Modern/Web Pascal */
	DIALECT_LAZARUS,        /* Lazarus IDE dialect */
	DIALECT_SMART_PASCAL,   /* Smart Mobile Studio (web) */
	DIALECT_DWSCRIPT,       /* Delphi Web Script */
	DIALECT_PASCALSCRIPT,   /* PascalScript (embedded) */
	DIALECT_PAS2JS,         /* Pas2JS (Pascal to JavaScript) */

	/* .NET Pascal */
	DIALECT_PASCAL_NET,     /* PascalABC.NET */
	DIALECT_CHROME,         /* Chrome (Elements compiler) */

	/* GNU Pascal */
	DIALECT_GNU_PASCAL,     /* GNU Pascal (GPC) */
	DIALECT_IP_PASCAL,      /* IP Pascal (ISO implementation) */

	/* Educational Pascal */
	DIALECT_CONCURRENT_PASCAL, /* Concurrent Pascal (Brinch Hansen) */
	DIALECT_PASCAL_PLUS,    /* Pascal Plus */
	DIALECT_PASCAL_FC,      /* Pascal-FC (for concurrency) */
	DIALECT_MODULA,         /* Modula (original, pre-Modula-2) */

	/* ===== MODULA-2 FAMILY ===== */

	/* PIM editions */
	DIALECT_MODULA2_PIM2,   /* Modula-2 PIM 2nd edition */
	DIALECT_MODULA2_PIM3,   /* Modula-2 PIM 3rd edition */
	DIALECT_MODULA2_PIM4,   /* Modula-2 PIM 4th edition */

	/* Standards */
	DIALECT_MODULA2_ISO,    /* ISO Modula-2 (ISO/IEC 10514) */

	/* Implementations */
	DIALECT_MODULA2_GNU,    /* GNU Modula-2 (gm2) */
	DIALECT_MODULA2_R10,    /* Modula-2 R10 (modern revision) */
	DIALECT_MODULA2_PLUS,   /* Modula-2+ (ETH extended) */
	DIALECT_MOCKA_M2,       /* Mocka Modula-2 */
	DIALECT_XDS_M2,         /* XDS Modula-2 */
	DIALECT_GP_M2,          /* Gardens Point Modula-2 */
	DIALECT_STONYBROOK_M2,  /* Stony Brook Modula-2 */
	DIALECT_ACK_M2,         /* ACK Modula-2 (Amsterdam Compiler Kit) */
	DIALECT_P1_M2,          /* p1 Modula-2 */
	DIALECT_LOGITECH_M2,    /* Logitech Modula-2 */

	/* ===== MODULA-3 FAMILY ===== */

	DIALECT_MODULA3,        /* Modula-3 (DEC SRC) */
	DIALECT_MODULA3_CM,     /* Critical Mass Modula-3 (CM3) */
	DIALECT_MODULA3_PM,     /* Polytechnique Modula-3 (PM3) */
	DIALECT_MODULA3_EZM3,   /* EzM3 (easy Modula-3) */
	DIALECT_MODULA3_SRC,    /* SRC Modula-3 (original DEC) */

	/* ===== OBERON FAMILY ===== */

	/* Oberon variants */
	DIALECT_OBERON,         /* Oberon (original 1987) */
	DIALECT_OBERON2,        /* Oberon-2 (1991) */
	DIALECT_OBERON07,       /* Oberon-07 (2007 revision) */
	DIALECT_OBERON_2013,    /* Project Oberon 2013 */

	/* Oberon implementations */
	DIALECT_ETH_OBERON,     /* ETH Oberon System */
	DIALECT_NATIVE_OBERON,  /* Native Oberon */
	DIALECT_OBERON_V4,      /* Oberon System V4 */

	/* Extended Oberon */
	DIALECT_COMPONENT_PASCAL, /* Component Pascal (BlackBox) */
	DIALECT_GPCP,           /* Gardens Point Component Pascal */
	DIALECT_ACTIVE_OBERON,  /* Active Oberon (multithreading) */
	DIALECT_OBERON_A2,      /* A2 Oberon / Bluebottle */

	/* Modern Oberon */
	DIALECT_ZONNON,         /* Zonnon (newest Wirth language, 2004) */
	DIALECT_OBERON_PLUS,    /* Oberon+ (modern extensions) */

	/* ===== ADA FAMILY ===== */

	DIALECT_ADA83,          /* Ada 83 (ANSI/MIL-STD-1815A) */
	DIALECT_ADA95,          /* Ada 95 (ISO/IEC 8652:1995) */
	DIALECT_ADA2005,        /* Ada 2005 (ISO/IEC 8652:1995/Amd 1:2007) */
	DIALECT_ADA2012,        /* Ada 2012 (ISO/IEC 8652:2012) */
	DIALECT_ADA2022,        /* Ada 2022 (ISO/IEC 8652:2023) */
	DIALECT_SPARK,          /* SPARK Ada (verification subset) */
	DIALECT_SPARK_2014,     /* SPARK 2014 */

	/* ===== XEROX PARC LANGUAGES ===== */

	DIALECT_MESA,           /* Mesa (Xerox PARC) */
	DIALECT_CEDAR,          /* Cedar (Mesa successor) */

	/* ===== EUCLID/TURING FAMILY ===== */

	DIALECT_EUCLID,         /* Euclid (verifiable Pascal subset) */
	DIALECT_TURING,         /* Turing (Euclid descendant) */
	DIALECT_TURING_PLUS,    /* Turing Plus */
	DIALECT_OBJECT_ORIENTED_TURING, /* Object-Oriented Turing (OOT) */

	/* ===== MODERN WIRTH-INFLUENCED ===== */

	DIALECT_NIM,            /* Nim (formerly Nimrod, Pascal-inspired) */
	DIALECT_SEED7,          /* Seed7 (extensible, Pascal-influenced) */

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

/* Predefined dialect feature sets - Complete Collection */

/* ===== PASCAL FAMILY ===== */

/* Standard Pascal */
extern dialect_features_t iso_features;
extern dialect_features_t iso_extended_features;
extern dialect_features_t pascal_p_features;
extern dialect_features_t pascal_s_features;

/* UCSD Pascal variants */
extern dialect_features_t ucsd_features;
extern dialect_features_t ucsd_ii_features;
extern dialect_features_t ucsd_iv_features;

/* DEC/HP/VAX Pascal */
extern dialect_features_t vax_pascal_features;
extern dialect_features_t dec_pascal_features;
extern dialect_features_t vsi_pascal_features;
extern dialect_features_t hp_pascal_features;

/* Vendor-specific Pascal */
extern dialect_features_t microsoft_features;
extern dialect_features_t ibm_pascal_features;
extern dialect_features_t sun_pascal_features;
extern dialect_features_t oregon_pascal_features;

/* Apple Pascal family */
extern dialect_features_t apple_pascal_features;
extern dialect_features_t clascal_features;
extern dialect_features_t macpascal_features;
extern dialect_features_t think_features;

/* PC Pascal compilers */
extern dialect_features_t turbo_1_features;
extern dialect_features_t turbo_3_features;
extern dialect_features_t turbo_5_features;
extern dialect_features_t borland_features;
extern dialect_features_t bp_win_features;
extern dialect_features_t tmt_pascal_features;
extern dialect_features_t virtual_pascal_features;

/* Object Pascal evolution */
extern dialect_features_t object_pascal_features;
extern dialect_features_t delphi_1_features;
extern dialect_features_t delphi_3_features;
extern dialect_features_t delphi_7_features;
extern dialect_features_t delphi_features;
extern dialect_features_t delphi_prism_features;
extern dialect_features_t oxygene_features;

/* Free Pascal modes */
extern dialect_features_t freepascal_features;
extern dialect_features_t fpc_objfpc_features;
extern dialect_features_t fpc_delphi_features;
extern dialect_features_t fpc_tp_features;
extern dialect_features_t fpc_macpas_features;
extern dialect_features_t fpc_iso_features;

/* Modern/Web Pascal */
extern dialect_features_t lazarus_features;
extern dialect_features_t smart_pascal_features;
extern dialect_features_t dwscript_features;
extern dialect_features_t pascalscript_features;
extern dialect_features_t pas2js_features;

/* .NET Pascal */
extern dialect_features_t pascal_net_features;
extern dialect_features_t chrome_features;

/* GNU Pascal */
extern dialect_features_t gnu_pascal_features;
extern dialect_features_t ip_pascal_features;

/* Educational Pascal */
extern dialect_features_t concurrent_pascal_features;
extern dialect_features_t pascal_plus_features;
extern dialect_features_t pascal_fc_features;
extern dialect_features_t modula_features;

/* ===== MODULA-2 FAMILY ===== */

/* PIM editions */
extern dialect_features_t modula2_pim2_features;
extern dialect_features_t modula2_pim3_features;
extern dialect_features_t modula2_pim4_features;

/* Standards */
extern dialect_features_t modula2_iso_features;

/* Implementations */
extern dialect_features_t modula2_gnu_features;
extern dialect_features_t modula2_r10_features;
extern dialect_features_t modula2_plus_features;
extern dialect_features_t mocka_m2_features;
extern dialect_features_t xds_m2_features;
extern dialect_features_t gp_m2_features;
extern dialect_features_t stonybrook_m2_features;
extern dialect_features_t ack_m2_features;
extern dialect_features_t p1_m2_features;
extern dialect_features_t logitech_m2_features;

/* ===== MODULA-3 FAMILY ===== */

extern dialect_features_t modula3_features;
extern dialect_features_t modula3_cm_features;
extern dialect_features_t modula3_pm_features;
extern dialect_features_t modula3_ezm3_features;
extern dialect_features_t modula3_src_features;

/* ===== OBERON FAMILY ===== */

/* Oberon variants */
extern dialect_features_t oberon_features;
extern dialect_features_t oberon2_features;
extern dialect_features_t oberon07_features;
extern dialect_features_t oberon_2013_features;

/* Oberon implementations */
extern dialect_features_t eth_oberon_features;
extern dialect_features_t native_oberon_features;
extern dialect_features_t oberon_v4_features;

/* Extended Oberon */
extern dialect_features_t component_pascal_features;
extern dialect_features_t gpcp_features;
extern dialect_features_t active_oberon_features;
extern dialect_features_t oberon_a2_features;

/* Modern Oberon */
extern dialect_features_t zonnon_features;
extern dialect_features_t oberon_plus_features;

/* ===== ADA FAMILY ===== */

extern dialect_features_t ada83_features;
extern dialect_features_t ada95_features;
extern dialect_features_t ada2005_features;
extern dialect_features_t ada2012_features;
extern dialect_features_t ada2022_features;
extern dialect_features_t spark_features;
extern dialect_features_t spark_2014_features;

/* ===== XEROX PARC LANGUAGES ===== */

extern dialect_features_t mesa_features;
extern dialect_features_t cedar_features;

/* ===== EUCLID/TURING FAMILY ===== */

extern dialect_features_t euclid_features;
extern dialect_features_t turing_features;
extern dialect_features_t turing_plus_features;
extern dialect_features_t oot_features;

/* ===== MODERN WIRTH-INFLUENCED ===== */

extern dialect_features_t nim_features;
extern dialect_features_t seed7_features;

#endif /* DIALECT_H */
