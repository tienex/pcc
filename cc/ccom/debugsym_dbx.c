/*	$Id$	*/

/*
 * DBX Debug Format Support
 * Similar to STABS but with DBX-specific extensions
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct {
	int initialized;
	int type_count;
} dbx_state;

void
debugsym_dbx_init(void)
{
	memset(&dbx_state, 0, sizeof(dbx_state));
	dbx_state.initialized = 1;
	dbx_state.type_count = 1;

	printf("\t# DBX debug information\n");
}

void
debugsym_dbx_emit(debug_symbol_t *sym)
{
	if (sym == NULL)
		return;

	/* DBX uses similar format to STABS with some differences */
	printf("\t.stabs \"%s:", sym->name ? sym->name : "");

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		printf("F");
		break;
	case DBGSYM_VARIABLE:
		printf("G");
		break;
	case DBGSYM_PARAMETER:
		printf("p");
		break;
	case DBGSYM_TYPEDEF:
		printf("t");
		break;
	default:
		printf("G");
		break;
	}

	printf("%d\",%d,0,0,0\n", dbx_state.type_count, sym->location.line);
}

debug_symbol_t *
debugsym_dbx_parse(void *data, size_t len)
{
	return NULL;
}

void
debugsym_dbx_finish(void)
{
	printf("\t# End DBX debug\n");
}
