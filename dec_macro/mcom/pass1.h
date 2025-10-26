/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Pass 1 definitions - DEC MACRO Assembly Frontend
 * Properly integrated with PCC IR system
 */

#ifndef PASS1_H
#define PASS1_H

#include "../../mip/manifest.h"
#include "../../mip/node.h"

/* Use PCC's NODE structure (P1ND in pass1) */
#ifndef P1ND
#define P1ND NODE
#endif

/* Symbol table entry for labels and symbols */
typedef struct symtab {
	char *sname;            /* Symbol name */
	int sclass;             /* Symbol class */
	long svalue;            /* Symbol value (address/constant) */
	int sflags;             /* Miscellaneous flags */
	struct symtab *snext;   /* Next in hash chain */
	int lineno;             /* Definition line */
	int label_num;          /* PCC label number */
} SYMTAB;

/* Symbol classes */
#define SYM_LABEL      1    /* Label (address) */
#define SYM_MACRO      2    /* Macro definition */
#define SYM_CONSTANT   3    /* Constant (.EQU, =) */
#define SYM_REGISTER   4    /* Register symbol */
#define SYM_EXTERNAL   5    /* External reference (.EXTERN) */
#define SYM_GLOBAL     6    /* Global symbol (.GLOBL) */
#define SYM_LOCAL      7    /* Local symbol */

/* Symbol flags */
#define SF_DEFINED     (1<<0)   /* Symbol is defined */
#define SF_REFERENCED  (1<<1)   /* Symbol is referenced */
#define SF_EXPORTED    (1<<2)   /* Symbol is exported */
#define SF_ENTRY       (1<<3)   /* Entry point */

/* Macro definition */
typedef struct macro {
	char *mname;            /* Macro name */
	char **mparams;         /* Parameter names */
	int nparam;             /* Number of parameters */
	char *mbody;            /* Macro body text */
	struct macro *mnext;    /* Next macro */
} MACRO;

/* Instruction operand types */
#define OP_REGISTER    1    /* Register (R0-R15, AC0-AC3, etc.) */
#define OP_IMMEDIATE   2    /* Immediate value (#nn) */
#define OP_DIRECT      3    /* Direct address */
#define OP_INDIRECT    4    /* Indirect (@addr) */
#define OP_INDEXED     5    /* Indexed (offset(reg)) */
#define OP_AUTODEC     6    /* Autodecrement -(reg) */
#define OP_AUTOINC     7    /* Autoincrement (reg)+ */
#define OP_LITERAL     8    /* Literal value */
#define OP_SYMBOL      9    /* Symbol reference */

/* Operand structure */
typedef struct operand {
	int type;               /* Operand type */
	int reg;                /* Register number (if applicable) */
	long value;             /* Immediate/offset value */
	char *symbol;           /* Symbol name (if applicable) */
	int flags;              /* Modifier flags */
} OPERAND;

/* Operand flags */
#define OF_DEFERRED    (1<<0)   /* Deferred addressing @ */
#define OF_INDEXED     (1<<1)   /* Indexed addressing */
#define OF_AUTODEC     (1<<2)   /* Auto-decrement - */
#define OF_AUTOINC     (1<<3)   /* Auto-increment + */

/* Instruction structure */
typedef struct instruction {
	char *mnemonic;         /* Instruction mnemonic */
	int opcode;             /* Instruction opcode */
	int noperands;          /* Number of operands */
	OPERAND operands[4];    /* Operands (max 4) */
	int lineno;             /* Source line number */
} INSTRUCTION;

/* Directive types */
#define DIR_TITLE      1
#define DIR_IDENT      2
#define DIR_PSECT      3
#define DIR_ENTRY      4
#define DIR_END        5
#define DIR_GLOBL      6
#define DIR_EXTERN     7
#define DIR_BYTE       8
#define DIR_WORD       9
#define DIR_LONG       10
#define DIR_ASCII      11
#define DIR_ASCIZ      12
#define DIR_BLKB       13
#define DIR_BLKW       14
#define DIR_BLKL       15
#define DIR_ALIGN      16
#define DIR_EVEN       17
#define DIR_ODD        18
#define DIR_PAGE       19
#define DIR_SBTTL      20
#define DIR_MACRO      21
#define DIR_ENDM       22
#define DIR_MEXIT      23
#define DIR_IRP        24
#define DIR_IRPC       25
#define DIR_REPT       26
#define DIR_ENDR       27
#define DIR_IF         28
#define DIR_IFF        29
#define DIR_IFT        30
#define DIR_IFTF       31
#define DIR_IFB        32
#define DIR_IFNB       33
#define DIR_IFDEF      34
#define DIR_IFNDEF     35
#define DIR_ENDC       36
#define DIR_LIST       37
#define DIR_NLIST      38
#define DIR_ENABLE     39
#define DIR_DISABLE    40

/* Register definitions (PDP-11/VAX style) */
#define REG_R0         0
#define REG_R1         1
#define REG_R2         2
#define REG_R3         3
#define REG_R4         4
#define REG_R5         5
#define REG_R6         6
#define REG_R7         7
#define REG_R8         8
#define REG_R9         9
#define REG_R10        10
#define REG_R11        11
#define REG_R12        12
#define REG_R13        13
#define REG_R14        14
#define REG_R15        15

/* Global variables */
extern int lineno;          /* Current line number */
extern char *ftitle;        /* Current filename */
extern int nerrors;         /* Error count */
extern int nwarnings;       /* Warning count */
extern int current_psect;   /* Current program section */
extern long location_counter; /* Current address */
extern int blevel;          /* Block nesting level (for PCC) */

/* Symbol table functions */
void symtab_init(void);
SYMTAB *lookup(const char *name);
SYMTAB *install(const char *name, int class);
void define_symbol(SYMTAB *sym, long value);
void export_symbol(SYMTAB *sym);
void dump_symtab(void);

/* Macro functions */
void macro_init(void);
MACRO *define_macro(const char *name, char **params, int nparam, const char *body);
MACRO *lookup_macro(const char *name);
char *expand_macro(MACRO *m, char **args, int nargs);

/* Error handling */
void error_init(void);
void error(const char *fmt, ...);
void warning(const char *fmt, ...);
void error_at(int line, const char *fmt, ...);
void fatal(const char *fmt, ...);

/* Code generation - now using PCC IR */
void codegen_init(void);
void emit_instruction_ir(INSTRUCTION *inst);
void emit_directive_ir(int directive, ...);
void emit_label_ir(const char *label);
void emit_data_ir(int size, long value);
void emit_string_ir(const char *str, int null_term);

/* PCC IR node building */
P1ND *build_icon(long value);
P1ND *build_reg(int reg);
P1ND *build_oreg(int reg, long offset);
P1ND *build_assign(P1ND *left, P1ND *right);
P1ND *build_binop(int op, P1ND *left, P1ND *right);
P1ND *build_unop(int op, P1ND *child);

/* PCC backend interface */
void bjobcode(void);        /* Begin compilation unit */
void ejobcode(int);         /* End compilation unit */

/* Register name lookup */
int lookup_register(const char *name);
const char *register_name(int reg);

/* Label management */
int get_label(void);        /* Get new PCC label number */

/* Utility functions */
long eval_expression(const char *expr);
int is_local_label(const char *name);

#endif /* PASS1_H */
