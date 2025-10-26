/*	$Id$	*/

/*
 * STABS Debug Format Support - Enhanced Integration
 * Integrates with existing stabs.c implementation
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* STABS type numbers */
#define N_GSYM		0x20	/* Global symbol */
#define N_FNAME		0x22	/* Function name */
#define N_FUN		0x24	/* Function */
#define N_STSYM		0x26	/* Static symbol */
#define N_LCSYM		0x28	/* .lcomm symbol */
#define N_MAIN		0x2a	/* Main function */
#define N_ROSYM		0x2c	/* Read-only symbol */
#define N_PC		0x30	/* Global Pascal symbol */
#define N_NSYMS		0x32	/* Number of symbols */
#define N_NOMAP		0x34	/* No DST map */
#define N_OBJ		0x38	/* Object file */
#define N_OPT		0x3c	/* Debugger options */
#define N_RSYM		0x40	/* Register variable */
#define N_M2C		0x42	/* Modula-2 compilation unit */
#define N_SLINE		0x44	/* Line number in text segment */
#define N_DSLINE	0x46	/* Line number in data segment */
#define N_BSLINE	0x48	/* Line number in bss segment */
#define N_BROWS		0x48	/* Source code browser info */
#define N_DEFD		0x4a	/* Gnu Modula-2 definition module */
#define N_FLINE		0x4c	/* Function start/body/end line */
#define N_EHDECL	0x50	/* Gnu C++ exception variable */
#define N_MOD2		0x50	/* Modula-2 info */
#define N_CATCH		0x54	/* Gnu C++ catch clause */
#define N_SSYM		0x60	/* Structure/union element */
#define N_ENDM		0x62	/* Last stab for module */
#define N_SO		0x64	/* Source file name */
#define N_LSYM		0x80	/* Local variable */
#define N_BINCL		0x82	/* Include file beginning */
#define N_SOL		0x84	/* Include file name */
#define N_PSYM		0xa0	/* Parameter variable */
#define N_EINCL		0xa2	/* Include file end */
#define N_ENTRY		0xa4	/* Alternate entry point */
#define N_LBRAC		0xc0	/* Left bracket */
#define N_EXCL		0xc2	/* Deleted include file */
#define N_SCOPE		0xc4	/* Modula-2 scope */
#define N_RBRAC		0xe0	/* Right bracket */
#define N_BCOMM		0xe2	/* Begin common */
#define N_ECOMM		0xe4	/* End common */
#define N_ECOML		0xe8	/* End common (local) */
#define N_WITH		0xea	/* Pascal with statement */
#define N_NBTEXT	0xf0	/* Gould non-base registers */
#define N_NBDATA	0xf2
#define N_NBBSS		0xf4
#define N_NBSTS		0xf6
#define N_NBLCS		0xf8

static struct {
	int initialized;
} stabs_state;

void
debugsym_stabs_init(void)
{
	stabs_state.initialized = 1;
}

static int
get_stabs_type(debug_symbol_t *sym)
{
	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		return N_FUN;
	case DBGSYM_VARIABLE:
		if (sym->storage_class == AUTO)
			return N_LSYM;
		else if (sym->storage_class == STATIC)
			return N_STSYM;
		else
			return N_GSYM;
	case DBGSYM_PARAMETER:
		return sym->is_register ? N_RSYM : N_PSYM;
	case DBGSYM_TYPEDEF:
		return N_LSYM;
	default:
		return N_GSYM;
	}
}

static void
emit_stabs_type_string(debug_type_t *type, char *buf, int bufsize)
{
	if (type == NULL) {
		snprintf(buf, bufsize, "void:t1=1");
		return;
	}

	switch (type->encoding) {
	case DBGTYPE_INT32:
		snprintf(buf, bufsize, "int:t1=r1;-2147483648;2147483647;");
		break;
	case DBGTYPE_CHAR:
		snprintf(buf, bufsize, "char:t2=r2;0;127;");
		break;
	case DBGTYPE_POINTER:
		snprintf(buf, bufsize, "*1");
		break;
	default:
		snprintf(buf, bufsize, "1");
		break;
	}
}

void
debugsym_stabs_emit(debug_symbol_t *sym)
{
	int stabs_type;
	char type_str[256];

	if (sym == NULL)
		return;

	stabs_type = get_stabs_type(sym);
	emit_stabs_type_string(sym->type, type_str, sizeof(type_str));

	printf("\t.stabs \"%s:%s\",%d,0,%d,%ld\n",
	    sym->name ? sym->name : "",
	    type_str,
	    stabs_type,
	    sym->location.line,
	    sym->offset);
}

debug_symbol_t *
debugsym_stabs_parse(void *data, size_t len)
{
	return NULL;
}

void
debugsym_stabs_finish(void)
{
	/* Emit end marker */
	printf("\t.stabs \"\",N_SO,0,0,0\n");
}
