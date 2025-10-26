/*
 * COBOL dialect support (DEC, IBM, HP, Microsoft)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* IBM COBOL reserved words */
static const char *ibm_reserved[] = {
	"AMODE", "RMODE", "SQLIMS", "SQLCURS", NULL
};

/* DEC COBOL reserved words */
static const char *dec_reserved[] = {
	"TERMINAL", "SCREEN", "CDD", NULL
};

/* HP COBOL reserved words */
static const char *hp_reserved[] = {
	"INTRINSIC", "EXTERNAL-FORM", NULL
};

/* Microsoft COBOL reserved words */
static const char *ms_reserved[] = {
	"COM", "ACTIVEX", NULL
};

static const char **current_reserved = NULL;

void
set_dialect(int d)
{
	dialect = d;

	switch (d) {
	case DIALECT_IBM:
		current_reserved = ibm_reserved;
		break;
	case DIALECT_DEC:
		current_reserved = dec_reserved;
		break;
	case DIALECT_HP:
		current_reserved = hp_reserved;
		break;
	case DIALECT_MS:
		current_reserved = ms_reserved;
		break;
	default:
		current_reserved = NULL;
	}
}

int
is_reserved_word(const char *word)
{
	int i;

	if (!current_reserved)
		return 0;

	for (i = 0; current_reserved[i]; i++) {
		if (strcasecmp(word, current_reserved[i]) == 0)
			return 1;
	}

	return 0;
}

void
apply_dialect_semantics(void)
{
	/* Apply dialect-specific semantic rules */
	switch (dialect) {
	case DIALECT_IBM:
		/* IBM: COMP is binary, COMP-3 is packed decimal */
		break;

	case DIALECT_DEC:
		/* DEC: Different numeric formats */
		break;

	case DIALECT_HP:
		/* HP: Different file handling */
		break;

	case DIALECT_MS:
		/* Microsoft: Windows-specific features */
		break;
	}
}
