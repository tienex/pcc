/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * Startup and shutdown code
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <time.h>

/* Error state */
int pli_error_code = 0;
char pli_error_msg[256] = {0};

/* Initialize runtime */
void pli_init(void) {
	/* Initialize standard files */
	pli_sysin = malloc(sizeof(pli_file_t));
	pli_sysprint = malloc(sizeof(pli_file_t));

	if (pli_sysin) {
		pli_sysin->fp = stdin;
		pli_sysin->filename = "SYSIN";
		pli_sysin->mode = 1;  /* INPUT */
		pli_sysin->type = 0;  /* STREAM */
		pli_sysin->is_open = 1;
	}

	if (pli_sysprint) {
		pli_sysprint->fp = stdout;
		pli_sysprint->filename = "SYSPRINT";
		pli_sysprint->mode = 2;  /* OUTPUT */
		pli_sysprint->type = 0;  /* STREAM */
		pli_sysprint->is_open = 1;
	}

	/* Initialize random number generator */
	srand((unsigned int)time(NULL));

	/* Clear error state */
	pli_error_code = 0;
	pli_error_msg[0] = '\0';
}

/* Cleanup runtime */
void pli_finish(void) {
	/* Close standard files (but don't close stdin/stdout) */
	if (pli_sysin) {
		pli_sysin->is_open = 0;
		free(pli_sysin);
		pli_sysin = NULL;
	}

	if (pli_sysprint) {
		pli_sysprint->is_open = 0;
		free(pli_sysprint);
		pli_sysprint = NULL;
	}
}

/* STOP - Terminate program normally */
void pli_stop(void) {
	pli_finish();
	exit(0);
}

/* EXIT - Terminate with code */
void pli_exit(int code) {
	pli_finish();
	exit(code);
}

/* Time/date functions */
void pli_date(char *dest) {
	if (!dest) return;

	time_t now = time(NULL);
	struct tm *tm_info = localtime(&now);

	strftime(dest, 32, "%Y-%m-%d", tm_info);
}

void pli_time(char *dest) {
	if (!dest) return;

	time_t now = time(NULL);
	struct tm *tm_info = localtime(&now);

	strftime(dest, 32, "%H:%M:%S", tm_info);
}

void pli_datetime(char *dest) {
	if (!dest) return;

	time_t now = time(NULL);
	struct tm *tm_info = localtime(&now);

	strftime(dest, 64, "%Y-%m-%d %H:%M:%S", tm_info);
}
