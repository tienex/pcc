/*	$Id$	*/

/*
 * Borland Debug Format Support (TD32/TDS)
 * Turbo Debugger 32-bit and .TDS symbol files
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Borland symbol types */
#define BOR_COMPILE	0x01
#define BOR_TYPES	0x02
#define BOR_SYMBOLS	0x03
#define BOR_ALIGNSYM	0x04
#define BOR_SRCMODULE	0x05
#define BOR_SRCLINES	0x06
#define BOR_NAMEMAP	0x07
#define BOR_CSECT	0x08

/* Borland type leaf codes */
#define LF_BOR_LABEL	0x00
#define LF_BOR_POINTER	0x01
#define LF_BOR_ARRAY	0x02
#define LF_BOR_STRUCT	0x03
#define LF_BOR_PROC	0x04

static struct {
	debug_format_t format;	/* TD32 or TDS */
	int module_index;
	int symbol_index;
} bor_state;

void
debugsym_borland_init(debug_format_t type)
{
	memset(&bor_state, 0, sizeof(bor_state));
	bor_state.format = type;
	bor_state.module_index = 0;
	bor_state.symbol_index = 0;

	printf("\t# Borland TD32/TDS debug info\n");
	printf("\t.section .debug$T\n");
}

void
debugsym_borland_emit(debug_symbol_t *sym)
{
	if (sym == NULL)
		return;

	printf("\t# Borland symbol: %s\n", sym->name ? sym->name : "(null)");
	printf("\t.byte 0x%x\n", BOR_SYMBOLS);  /* Symbol record type */
	printf("\t.short 0  # length\n");
	printf("\t.byte 0  # symbol type\n");

	/* Symbol name (length-prefixed) */
	if (sym->name) {
		int len = strlen(sym->name);
		if (len > 255)
			len = 255;
		printf("\t.byte %d\n", len);
		printf("\t.ascii \"%.*s\"\n", len, sym->name);
	} else {
		printf("\t.byte 0\n");
	}

	bor_state.symbol_index++;
}

debug_symbol_t *
debugsym_borland_parse(void *data, size_t len)
{
	return NULL;
}

void
debugsym_borland_finish(void)
{
	printf("\t# Borland symbols: %d\n", bor_state.symbol_index);
}
