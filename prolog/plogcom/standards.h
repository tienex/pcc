/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Prolog Standards Support
 * From Edinburgh Prolog (1977) to Modern Extensions
 */

#ifndef STANDARDS_H
#define STANDARDS_H

/* Prolog Standard Versions */
typedef enum {
	PROLOG_EDINBURGH,      /* Edinburgh Prolog (1977-1980s) */
	PROLOG_DEC10,          /* DEC-10 Prolog (1970s) */
	PROLOG_CPROLOG,        /* C-Prolog (1980s) */
	PROLOG_QUINTUS,        /* Quintus Prolog (1980s-1990s) */
	PROLOG_ISO_1995,       /* ISO/IEC 13211-1:1995 */
	PROLOG_ISO_COR1_2007,  /* ISO Prolog Corrigendum 1 (2007) */
	PROLOG_ISO_COR2_2012,  /* ISO Prolog Corrigendum 2 (2012) */
	PROLOG_SICSTUS,        /* SICStus Prolog */
	PROLOG_SWI,            /* SWI-Prolog (modern) */
	PROLOG_YAP,            /* YAP Prolog */
	PROLOG_GNU,            /* GNU Prolog */
	PROLOG_TURBO,          /* Turbo Prolog */
	PROLOG_AUTO            /* Auto-detect */
} prolog_standard_t;

/* Standard feature flags */
typedef struct {
	/* Core features */
	int has_cuts;
	int has_negation;
	int has_if_then_else;

	/* Edinburgh Prolog features */
	int edinburgh_syntax;
	int dec10_io;
	int dec10_assert_retract;

	/* ISO Prolog 1995 features */
	int iso_syntax;
	int iso_operators;
	int iso_builtins;
	int iso_exceptions;
	int iso_char_conversion;
	int iso_double_quotes;  /* List vs string vs codes */
	int iso_modules;        /* ISO Part 2 */

	/* ISO Corrigenda */
	int iso_cor1_2007;      /* Fixes to 1995 standard */
	int iso_cor2_2012;      /* Additional fixes */

	/* Modern extensions */
	int has_constraints;    /* CLP(FD), CLP(R), etc. */
	int has_tabling;        /* SLG resolution */
	int has_threads;        /* Multi-threading */
	int has_dicts;          /* SWI-Prolog dicts */
	int has_strings;        /* Native strings */
	int has_rational;       /* Rational arithmetic */
	int has_unicode;        /* Unicode support */
	int has_cyclic_terms;   /* Cyclic term unification */

	/* Dialect-specific */
	int turbo_domains;      /* Turbo Prolog domains */
	int swi_extensions;     /* SWI-Prolog specific */
	int sicstus_extensions; /* SICStus specific */
	int yap_extensions;     /* YAP specific */
	int gnu_extensions;     /* GNU Prolog specific */
} standard_features_t;

/* Global standard configuration */
extern prolog_standard_t current_standard;
extern standard_features_t standard_features;

/* Function declarations */
void init_standard(prolog_standard_t std);
void set_standard(prolog_standard_t std);
prolog_standard_t detect_standard(void);
const char *get_standard_name(prolog_standard_t std);
int feature_enabled(const char *feature);

/* Standard-specific initialization */
void init_edinburgh_standard(void);
void init_dec10_standard(void);
void init_iso_1995_standard(void);
void init_iso_cor1_2007_standard(void);
void init_iso_cor2_2012_standard(void);
void init_swi_standard(void);
void init_sicstus_standard(void);
void init_yap_standard(void);
void init_gnu_standard(void);
void init_turbo_standard(void);

/* Feature checks */
#define FEATURE_CUTS()           (standard_features.has_cuts)
#define FEATURE_NEGATION()       (standard_features.has_negation)
#define FEATURE_IF_THEN_ELSE()   (standard_features.has_if_then_else)
#define FEATURE_ISO_SYNTAX()     (standard_features.iso_syntax)
#define FEATURE_ISO_OPERATORS()  (standard_features.iso_operators)
#define FEATURE_ISO_EXCEPTIONS() (standard_features.iso_exceptions)
#define FEATURE_CONSTRAINTS()    (standard_features.has_constraints)
#define FEATURE_TABLING()        (standard_features.has_tabling)
#define FEATURE_THREADS()        (standard_features.has_threads)
#define FEATURE_DICTS()          (standard_features.has_dicts)
#define FEATURE_STRINGS()        (standard_features.has_strings)
#define FEATURE_UNICODE()        (standard_features.has_unicode)
#define FEATURE_TURBO_DOMAINS()  (standard_features.turbo_domains)

/* Double-quote handling modes (ISO Prolog) */
typedef enum {
	DQ_CODES,   /* "abc" -> [97,98,99] (ISO default) */
	DQ_CHARS,   /* "abc" -> [a,b,c] */
	DQ_ATOM,    /* "abc" -> 'abc' */
	DQ_STRING   /* "abc" -> string object (SWI) */
} double_quote_mode_t;

extern double_quote_mode_t double_quote_mode;

#endif /* STANDARDS_H */
