/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * Condition handling (ON/SIGNAL/REVERT)
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#define MAX_CONDITIONS 32

/* Condition handler table */
static pli_condition_t condition_table[MAX_CONDITIONS];
static int num_conditions = 0;

/* Condition state for queries */
static int last_oncode = 0;
static char last_onchar[256] = {0};
static char last_onsource[256] = {0};
static char last_onfile[256] = {0};
static char last_onkey[256] = {0};

/* Initialize condition handling */
static void init_conditions(void) {
	static int initialized = 0;

	if (initialized) return;

	/* Register standard conditions */
	pli_on("ERROR", pli_condition_error);
	pli_on("ENDFILE", pli_condition_endfile);
	pli_on("CONVERSION", pli_condition_conversion);
	pli_on("FIXEDOVERFLOW", pli_condition_fixedoverflow);
	pli_on("ZERODIVIDE", pli_condition_zerodivide);

	initialized = 1;
}

/* Find condition by name */
static pli_condition_t *find_condition(const char *name) {
	for (int i = 0; i < num_conditions; i++) {
		if (strcasecmp(condition_table[i].name, name) == 0) {
			return &condition_table[i];
		}
	}
	return NULL;
}

/* ON - Register condition handler */
void pli_on(const char *condition, void (*handler)(void)) {
	init_conditions();

	if (!condition || !handler) return;

	/* Check if already exists */
	pli_condition_t *cond = find_condition(condition);

	if (cond) {
		/* Update existing handler */
		cond->handler = handler;
		cond->enabled = 1;
	} else if (num_conditions < MAX_CONDITIONS) {
		/* Add new condition */
		condition_table[num_conditions].name = strdup(condition);
		condition_table[num_conditions].handler = handler;
		condition_table[num_conditions].enabled = 1;
		num_conditions++;
	}
}

/* SIGNAL - Raise a condition */
void pli_signal(const char *condition) {
	init_conditions();

	if (!condition) return;

	pli_condition_t *cond = find_condition(condition);

	if (cond && cond->enabled && cond->handler) {
		/* Set condition code */
		if (strcasecmp(condition, "ERROR") == 0) {
			last_oncode = 1;
		} else if (strcasecmp(condition, "ENDFILE") == 0) {
			last_oncode = 2;
		} else if (strcasecmp(condition, "CONVERSION") == 0) {
			last_oncode = 3;
		} else if (strcasecmp(condition, "FIXEDOVERFLOW") == 0) {
			last_oncode = 4;
		} else if (strcasecmp(condition, "ZERODIVIDE") == 0) {
			last_oncode = 5;
		} else {
			last_oncode = 99;  /* Unknown condition */
		}

		/* Call handler */
		cond->handler();
	} else {
		/* No handler - default action is to print error and exit */
		fprintf(stderr, "PL/I Condition raised: %s\n", condition);
		pli_exit(1);
	}
}

/* REVERT - Disable condition handler */
void pli_revert(const char *condition) {
	init_conditions();

	if (!condition) return;

	pli_condition_t *cond = find_condition(condition);

	if (cond) {
		cond->enabled = 0;
	}
}

/* Standard condition handlers */

void pli_condition_error(void) {
	fprintf(stderr, "ERROR condition raised\n");
	if (pli_error_msg[0]) {
		fprintf(stderr, "  Message: %s\n", pli_error_msg);
	}
	/* Don't exit - let program handle it */
}

void pli_condition_endfile(void) {
	/* ENDFILE is typically handled by the program */
	/* Just return to allow program to check */
}

void pli_condition_conversion(void) {
	fprintf(stderr, "CONVERSION condition raised\n");
	fprintf(stderr, "  Invalid data conversion\n");
}

void pli_condition_fixedoverflow(void) {
	fprintf(stderr, "FIXEDOVERFLOW condition raised\n");
	fprintf(stderr, "  Integer overflow detected\n");
}

void pli_condition_zerodivide(void) {
	fprintf(stderr, "ZERODIVIDE condition raised\n");
	fprintf(stderr, "  Division by zero\n");
	pli_exit(1);
}

/* Condition query functions */

pli_fixed_t pli_oncode(void) {
	return last_oncode;
}

void pli_onchar(char *dest) {
	if (!dest) return;
	strcpy(dest, last_onchar);
}

void pli_onsource(char *dest) {
	if (!dest) return;
	strcpy(dest, last_onsource);
}

void pli_onfile(char *dest) {
	if (!dest) return;
	strcpy(dest, last_onfile);
}

void pli_onkey(char *dest) {
	if (!dest) return;
	strcpy(dest, last_onkey);
}
