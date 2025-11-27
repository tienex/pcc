/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * I/O functions implementation
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <string.h>

/* Global file handles */
pli_file_t *pli_sysin = NULL;
pli_file_t *pli_sysprint = NULL;

/* PUT operations */
void pli_put_skip(void) {
	if (!pli_sysprint) return;
	fprintf(pli_sysprint->fp, "\n");
}

void pli_put_page(void) {
	if (!pli_sysprint) return;
	fprintf(pli_sysprint->fp, "\f");
}

void pli_put_line(void) {
	if (!pli_sysprint) return;
	fprintf(pli_sysprint->fp, "\n");
}

void pli_put_string(const char *s) {
	if (!pli_sysprint || !s) return;
	fprintf(pli_sysprint->fp, "%s", s);
	fflush(pli_sysprint->fp);
}

void pli_put_fixed(pli_fixed_t n) {
	if (!pli_sysprint) return;
	fprintf(pli_sysprint->fp, "%d", n);
	fflush(pli_sysprint->fp);
}

void pli_put_float(pli_float_long_t f) {
	if (!pli_sysprint) return;
	fprintf(pli_sysprint->fp, "%g", f);
	fflush(pli_sysprint->fp);
}

void pli_put_char(char c) {
	if (!pli_sysprint) return;
	fputc(c, pli_sysprint->fp);
	fflush(pli_sysprint->fp);
}

/* GET operations */
void pli_get_skip(void) {
	if (!pli_sysin) return;
	int c;
	while ((c = fgetc(pli_sysin->fp)) != '\n' && c != EOF)
		;
}

void pli_get_page(void) {
	if (!pli_sysin) return;
	int c;
	while ((c = fgetc(pli_sysin->fp)) != '\f' && c != EOF)
		;
}

void pli_get_string(char *buf, size_t maxlen) {
	if (!pli_sysin || !buf) return;
	if (fgets(buf, maxlen, pli_sysin->fp)) {
		/* Remove trailing newline */
		size_t len = strlen(buf);
		if (len > 0 && buf[len-1] == '\n')
			buf[len-1] = '\0';
	}
}

pli_fixed_t pli_get_fixed(void) {
	if (!pli_sysin) return 0;
	pli_fixed_t n;
	if (fscanf(pli_sysin->fp, "%d", &n) == 1)
		return n;
	return 0;
}

pli_float_long_t pli_get_float(void) {
	if (!pli_sysin) return 0.0;
	pli_float_long_t f;
	if (fscanf(pli_sysin->fp, "%lf", &f) == 1)
		return f;
	return 0.0;
}

char pli_get_char(void) {
	if (!pli_sysin) return '\0';
	return fgetc(pli_sysin->fp);
}

/* File operations */
pli_file_t *pli_file_open(const char *filename, int mode, int type) {
	pli_file_t *f = malloc(sizeof(pli_file_t));
	if (!f) return NULL;

	const char *fmode;
	switch (mode) {
	case 1: /* INPUT */
		fmode = "r";
		break;
	case 2: /* OUTPUT */
		fmode = "w";
		break;
	case 3: /* UPDATE */
		fmode = "r+";
		break;
	default:
		free(f);
		return NULL;
	}

	f->fp = fopen(filename, fmode);
	if (!f->fp) {
		free(f);
		return NULL;
	}

	f->filename = strdup(filename);
	f->mode = mode;
	f->type = type;
	f->is_open = 1;

	return f;
}

void pli_file_close(pli_file_t *f) {
	if (!f) return;
	if (f->is_open && f->fp) {
		fclose(f->fp);
		f->fp = NULL;
		f->is_open = 0;
	}
	if (f->filename) {
		free(f->filename);
		f->filename = NULL;
	}
}

void pli_file_read(pli_file_t *f, void *buf, size_t len) {
	if (!f || !f->is_open || !buf) return;
	fread(buf, 1, len, f->fp);
}

void pli_file_write(pli_file_t *f, const void *buf, size_t len) {
	if (!f || !f->is_open || !buf) return;
	fwrite(buf, 1, len, f->fp);
	fflush(f->fp);
}
