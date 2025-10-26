/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Dialect support - Compatibility with all Xbase languages
 */

#ifndef DIALECT_H
#define DIALECT_H

/*
 * Xbase Language Dialects
 * Ordered from oldest to newest
 */
typedef enum {
	DIALECT_DBASE2,      /* dBase II (1981) - Original */
	DIALECT_DBASE3,      /* dBase III (1984) - Enhanced */
	DIALECT_DBASE3PLUS,  /* dBase III Plus (1985) */
	DIALECT_DBASE4,      /* dBase IV (1988) - Major update */
	DIALECT_CLIPPER,     /* CA-Clipper Summer '87/5.x (1985-1997) */
	DIALECT_FOXBASE,     /* FoxBase/FoxPro 1.x (1984) */
	DIALECT_FOXPRO,      /* FoxPro 2.x (1991) */
	DIALECT_VFP,         /* Visual FoxPro (1995-2007) */
	DIALECT_HARBOUR,     /* Harbour (1999-present) - Open source */
	DIALECT_XHARBOUR,    /* xHarbour (2001-present) - Extended Harbour */
	DIALECT_XBASEPP,     /* Xbase++ (1997-present) - Default, most modern */
	DIALECT_AUTO         /* Auto-detect from source */
} DIALECT;

/*
 * Dialect features - bitflags for feature detection
 */
#define FEAT_PROCEDURES         0x0001  /* PROCEDURE keyword */
#define FEAT_OOP                0x0002  /* CLASS/METHOD/ENDCLASS */
#define FEAT_LOCAL              0x0004  /* LOCAL variables */
#define FEAT_STATIC             0x0008  /* STATIC variables */
#define FEAT_PRIVATE            0x0010  /* PRIVATE variables */
#define FEAT_PUBLIC             0x0020  /* PUBLIC variables */
#define FEAT_MEMVAR             0x0040  /* MEMVAR keyword */
#define FEAT_FIELD              0x0080  /* FIELD keyword */
#define FEAT_CODEBLOCKS         0x0100  /* {|| code } */
#define FEAT_INLINE             0x0200  /* INLINE keyword */
#define FEAT_SEQUENCE           0x0400  /* BEGIN SEQUENCE */
#define FEAT_TRY                0x0800  /* TRY/CATCH/FINALLY */
#define FEAT_FOREACH            0x1000  /* FOR EACH loop */
#define FEAT_SWITCH             0x2000  /* SWITCH/CASE */
#define FEAT_CLASS_VAR          0x4000  /* CLASS VAR/METHOD */
#define FEAT_THREADS            0x8000  /* Threading support */
#define FEAT_WITH_OBJECT        0x10000 /* WITH OBJECT */
#define FEAT_ARRAY_ASSIGN       0x20000 /* a := {1,2,3} */
#define FEAT_HASH_ASSIGN        0x40000 /* h := {"key"=>val} */
#define FEAT_NAMESPACES         0x80000 /* Namespace support */

/*
 * Dialect feature matrix
 */
typedef struct {
	DIALECT dialect;
	const char *name;
	const char *description;
	unsigned int features;
	int year;  /* Introduction year */
} DIALECT_INFO;

/*
 * Global dialect settings
 */
extern DIALECT current_dialect;
extern int dialect_strict;  /* Strict mode - reject non-conforming syntax */

/*
 * Dialect information table
 */
extern const DIALECT_INFO dialect_info[];

/*
 * Dialect functions
 */
void dialect_init(void);
void dialect_set(DIALECT d);
DIALECT dialect_get(void);
const char *dialect_name(DIALECT d);
int dialect_has_feature(unsigned int feature);
DIALECT dialect_detect(const char *source);
void dialect_set_strict(int strict);

/*
 * Keyword compatibility - check if keyword is valid in current dialect
 */
int dialect_keyword_valid(const char *keyword);

#endif /* DIALECT_H */
