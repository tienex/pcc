/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Unified I/O Operations
 * - Text I/O (read/write for all language variants)
 * - File operations
 * - Supports Pascal, Modula-2, Modula-3, Oberon, Ada I/O models
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "wirthrt.h"

/* Standard file handles */
WirthTextFile wirth_stdin = {
	.fp = NULL,  /* Will be set to stdin in init */
	.name = "stdin",
	.mode = 1,   /* read */
	.eof_flag = 0,
	.eoln_flag = 0,
	.line_pos = 0,
	.lang_variant = 0
};

WirthTextFile wirth_stdout = {
	.fp = NULL,  /* Will be set to stdout in init */
	.name = "stdout",
	.mode = 2,   /* write */
	.eof_flag = 0,
	.eoln_flag = 0,
	.line_pos = 0,
	.lang_variant = 0
};

WirthTextFile wirth_stderr = {
	.fp = NULL,  /* Will be set to stderr in init */
	.name = "stderr",
	.mode = 2,   /* write */
	.eof_flag = 0,
	.eoln_flag = 0,
	.line_pos = 0,
	.lang_variant = 0
};

/* Initialize standard handles (called by runtime init) */
static void init_stdio(void) __attribute__((constructor));
static void init_stdio(void) {
	wirth_stdin.fp = stdin;
	wirth_stdout.fp = stdout;
	wirth_stderr.fp = stderr;
}

/*
 * Text Output Operations
 */

void wirth_write_int(int value, int width) {
	if (width > 0) {
		printf("%*d", width, value);
	} else {
		printf("%d", value);
	}
}

void wirth_write_card(unsigned int value, int width) {
	if (width > 0) {
		printf("%*u", width, value);
	} else {
		printf("%u", value);
	}
}

void wirth_write_real(double value, int width, int precision) {
	if (width > 0 && precision >= 0) {
		printf("%*.*f", width, precision, value);
	} else if (width > 0) {
		printf("%*g", width, value);
	} else if (precision >= 0) {
		printf("%.*f", precision, value);
	} else {
		printf("%g", value);
	}
}

void wirth_write_char(char c) {
	putchar(c);
}

void wirth_write_string(const char *str) {
	if (str) {
		fputs(str, stdout);
	}
}

void wirth_write_bool(int value) {
	printf("%s", value ? "TRUE" : "FALSE");
}

void wirth_write_hex(unsigned int value, int width) {
	if (width > 0) {
		printf("%0*X", width, value);
	} else {
		printf("%X", value);
	}
}

void wirth_write_ln(void) {
	putchar('\n');
	fflush(stdout);
}

/*
 * Text Input Operations
 */

void wirth_read_int(int *value) {
	if (scanf("%d", value) != 1) {
		wirth_runtime_error(WIRTH_ERR_FILE_READ, "Failed to read integer");
	}
}

void wirth_read_card(unsigned int *value) {
	if (scanf("%u", value) != 1) {
		wirth_runtime_error(WIRTH_ERR_FILE_READ, "Failed to read cardinal");
	}
}

void wirth_read_real(double *value) {
	if (scanf("%lf", value) != 1) {
		wirth_runtime_error(WIRTH_ERR_FILE_READ, "Failed to read real");
	}
}

void wirth_read_char(char *c) {
	int ch = getchar();
	if (ch == EOF) {
		wirth_runtime_error(WIRTH_ERR_END_OF_FILE, "Unexpected end of file");
	}
	*c = (char)ch;
}

void wirth_read_string(char *str, size_t maxlen) {
	if (!str || maxlen == 0) {
		return;
	}

	/* Skip leading whitespace */
	int c;
	while ((c = getchar()) != EOF && isspace(c))
		;

	if (c == EOF) {
		str[0] = '\0';
		return;
	}

	/* Read until whitespace or maxlen */
	size_t i = 0;
	str[i++] = (char)c;

	while (i < maxlen - 1 && (c = getchar()) != EOF && !isspace(c)) {
		str[i++] = (char)c;
	}

	str[i] = '\0';

	/* Put back the terminating character if it wasn't EOF */
	if (c != EOF) {
		ungetc(c, stdin);
	}
}

void wirth_read_ln(void) {
	int c;
	while ((c = getchar()) != EOF && c != '\n')
		;
}

/*
 * File Operations
 */

WirthTextFile *wirth_file_open_read(const char *filename) {
	WirthTextFile *f = (WirthTextFile *)malloc(sizeof(WirthTextFile));
	if (!f) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate file structure");
		return NULL;
	}

	f->fp = fopen(filename, "r");
	if (!f->fp) {
		free(f);
		char msg[512];
		snprintf(msg, sizeof(msg), "Cannot open file '%s' for reading: %s",
		         filename, strerror(errno));
		wirth_runtime_error(WIRTH_ERR_FILE_NOT_FOUND, msg);
		return NULL;
	}

	f->name = strdup(filename);
	f->mode = 1;  /* read */
	f->eof_flag = 0;
	f->eoln_flag = 0;
	f->line_pos = 0;
	f->lang_variant = 0;
	f->line_buffer[0] = '\0';

	return f;
}

WirthTextFile *wirth_file_open_write(const char *filename) {
	WirthTextFile *f = (WirthTextFile *)malloc(sizeof(WirthTextFile));
	if (!f) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate file structure");
		return NULL;
	}

	f->fp = fopen(filename, "w");
	if (!f->fp) {
		free(f);
		char msg[512];
		snprintf(msg, sizeof(msg), "Cannot open file '%s' for writing: %s",
		         filename, strerror(errno));
		wirth_runtime_error(WIRTH_ERR_FILE_WRITE, msg);
		return NULL;
	}

	f->name = strdup(filename);
	f->mode = 2;  /* write */
	f->eof_flag = 0;
	f->eoln_flag = 0;
	f->line_pos = 0;
	f->lang_variant = 0;
	f->line_buffer[0] = '\0';

	return f;
}

WirthTextFile *wirth_file_open_append(const char *filename) {
	WirthTextFile *f = (WirthTextFile *)malloc(sizeof(WirthTextFile));
	if (!f) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate file structure");
		return NULL;
	}

	f->fp = fopen(filename, "a");
	if (!f->fp) {
		free(f);
		char msg[512];
		snprintf(msg, sizeof(msg), "Cannot open file '%s' for append: %s",
		         filename, strerror(errno));
		wirth_runtime_error(WIRTH_ERR_FILE_WRITE, msg);
		return NULL;
	}

	f->name = strdup(filename);
	f->mode = 3;  /* append */
	f->eof_flag = 0;
	f->eoln_flag = 0;
	f->line_pos = 0;
	f->lang_variant = 0;
	f->line_buffer[0] = '\0';

	return f;
}

void wirth_file_close(WirthTextFile *f) {
	if (!f) {
		return;
	}

	if (f->fp && f->fp != stdin && f->fp != stdout && f->fp != stderr) {
		fclose(f->fp);
	}

	if (f->name) {
		free(f->name);
	}

	f->fp = NULL;
	f->mode = 0;  /* closed */
	free(f);
}

int wirth_file_eof(WirthTextFile *f) {
	if (!f || !f->fp) {
		wirth_runtime_error(WIRTH_ERR_FILE_NOT_OPEN, "File not open");
		return 1;
	}

	return feof(f->fp) || f->eof_flag;
}
