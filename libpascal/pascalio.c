/*
 * Copyright (c) 2025 PCC Pascal Runtime Library
 *
 * Pascal I/O Operations
 *
 * Implements text and binary file I/O operations for Pascal programs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "pascalrt.h"

/* Standard input/output file handles */
PascalTextFile pascal_input;
PascalTextFile pascal_output;

/*
 * Initialize standard I/O handles
 */
static void
init_standard_files(void)
{
	static int initialized = 0;
	if (initialized)
		return;

	pascal_input.fp = stdin;
	pascal_input.name = strdup("input");
	pascal_input.mode = 1;  /* read mode */
	pascal_input.eof_flag = 0;
	pascal_input.eoln_flag = 0;
	pascal_input.line_pos = 0;

	pascal_output.fp = stdout;
	pascal_output.name = strdup("output");
	pascal_output.mode = 2;  /* write mode */
	pascal_output.eof_flag = 0;
	pascal_output.eoln_flag = 0;
	pascal_output.line_pos = 0;

	initialized = 1;
}

/*
 * Text File Operations
 */

void
pascal_assign(PascalTextFile *f, const char *filename)
{
	if (f->name != NULL && f->name != filename) {
		free(f->name);
	}
	f->name = strdup(filename);
	f->fp = NULL;
	f->mode = 0;
	f->eof_flag = 0;
	f->eoln_flag = 0;
	f->line_pos = 0;
}

void
pascal_reset_text(PascalTextFile *f)
{
	if (f->fp != NULL && f->fp != stdin && f->fp != stdout && f->fp != stderr) {
		fclose(f->fp);
	}

	f->fp = fopen(f->name, "r");
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_FOUND, f->name);
		return;
	}

	f->mode = 1;  /* read mode */
	f->eof_flag = 0;
	f->eoln_flag = 0;
	f->line_pos = 0;
}

void
pascal_rewrite_text(PascalTextFile *f)
{
	if (f->fp != NULL && f->fp != stdin && f->fp != stdout && f->fp != stderr) {
		fclose(f->fp);
	}

	f->fp = fopen(f->name, "w");
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_WRITE_ERROR, f->name);
		return;
	}

	f->mode = 2;  /* write mode */
	f->eof_flag = 0;
	f->eoln_flag = 0;
	f->line_pos = 0;
}

void
pascal_close_text(PascalTextFile *f)
{
	if (f->fp != NULL && f->fp != stdin && f->fp != stdout && f->fp != stderr) {
		fclose(f->fp);
	}
	f->fp = NULL;
	f->mode = 0;
}

void
pascal_flush_text(PascalTextFile *f)
{
	if (f->fp != NULL) {
		fflush(f->fp);
	}
}

int
pascal_eof_text(PascalTextFile *f)
{
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return 1;
	}
	return feof(f->fp) != 0;
}

int
pascal_eoln_text(PascalTextFile *f)
{
	int c;

	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return 1;
	}

	c = fgetc(f->fp);
	if (c == EOF) {
		return 1;
	}

	ungetc(c, f->fp);
	return (c == '\n' || c == '\r');
}

/*
 * Standard Output - Write Operations
 */

void
pascal_writeln(void)
{
	init_standard_files();
	printf("\n");
}

void
pascal_write_integer(int value)
{
	init_standard_files();
	printf("%d", value);
}

void
pascal_write_real(double value)
{
	init_standard_files();
	printf("%g", value);
}

void
pascal_write_char(char value)
{
	init_standard_files();
	putchar(value);
}

void
pascal_write_string(const char *str)
{
	init_standard_files();
	printf("%s", str);
}

void
pascal_write_boolean(int value)
{
	init_standard_files();
	printf("%s", value ? "TRUE" : "FALSE");
}

/*
 * Standard Input - Read Operations
 */

void
pascal_read_integer(int *value)
{
	init_standard_files();
	if (scanf("%d", value) != 1) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, "Failed to read integer");
	}
}

void
pascal_read_real(double *value)
{
	init_standard_files();
	if (scanf("%lf", value) != 1) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, "Failed to read real");
	}
}

void
pascal_read_char(char *value)
{
	int c;
	init_standard_files();
	c = getchar();
	if (c == EOF) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, "Failed to read char");
	}
	*value = (char)c;
}

void
pascal_read_string(char *str, size_t maxlen)
{
	init_standard_files();
	if (fgets(str, maxlen, stdin) == NULL) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, "Failed to read string");
	}
	/* Remove trailing newline if present */
	size_t len = strlen(str);
	if (len > 0 && str[len-1] == '\n') {
		str[len-1] = '\0';
	}
}

void
pascal_readln(void)
{
	int c;
	init_standard_files();
	/* Skip to end of line */
	while ((c = getchar()) != EOF && c != '\n')
		;
}

/*
 * File-specific Write Operations
 */

void
pascal_write_text_integer(PascalTextFile *f, int value)
{
	if (f->fp == NULL || f->mode != 2) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	fprintf(f->fp, "%d", value);
}

void
pascal_write_text_real(PascalTextFile *f, double value)
{
	if (f->fp == NULL || f->mode != 2) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	fprintf(f->fp, "%g", value);
}

void
pascal_write_text_char(PascalTextFile *f, char value)
{
	if (f->fp == NULL || f->mode != 2) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	fputc(value, f->fp);
}

void
pascal_write_text_string(PascalTextFile *f, const char *str)
{
	if (f->fp == NULL || f->mode != 2) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	fprintf(f->fp, "%s", str);
}

void
pascal_writeln_text(PascalTextFile *f)
{
	if (f->fp == NULL || f->mode != 2) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	fprintf(f->fp, "\n");
}

/*
 * File-specific Read Operations
 */

void
pascal_read_text_integer(PascalTextFile *f, int *value)
{
	if (f->fp == NULL || f->mode != 1) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	if (fscanf(f->fp, "%d", value) != 1) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, f->name);
	}
}

void
pascal_read_text_real(PascalTextFile *f, double *value)
{
	if (f->fp == NULL || f->mode != 1) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	if (fscanf(f->fp, "%lf", value) != 1) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, f->name);
	}
}

void
pascal_read_text_char(PascalTextFile *f, char *value)
{
	int c;
	if (f->fp == NULL || f->mode != 1) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	c = fgetc(f->fp);
	if (c == EOF) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, f->name);
	}
	*value = (char)c;
}

void
pascal_read_text_string(PascalTextFile *f, char *str, size_t maxlen)
{
	if (f->fp == NULL || f->mode != 1) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	if (fgets(str, maxlen, f->fp) == NULL) {
		pascal_runtime_error(PASCAL_ERR_READ_ERROR, f->name);
	}
	/* Remove trailing newline if present */
	size_t len = strlen(str);
	if (len > 0 && str[len-1] == '\n') {
		str[len-1] = '\0';
	}
}

void
pascal_readln_text(PascalTextFile *f)
{
	int c;
	if (f->fp == NULL || f->mode != 1) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	/* Skip to end of line */
	while ((c = fgetc(f->fp)) != EOF && c != '\n')
		;
}

/*
 * Binary File Operations
 */

void
pascal_reset_file(PascalFile *f, size_t element_size)
{
	if (f->fp != NULL) {
		fclose(f->fp);
	}

	f->fp = fopen(f->name, "rb");
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_FOUND, f->name);
		return;
	}

	f->mode = 1;  /* read mode */
	f->element_size = element_size;
}

void
pascal_rewrite_file(PascalFile *f, size_t element_size)
{
	if (f->fp != NULL) {
		fclose(f->fp);
	}

	f->fp = fopen(f->name, "wb");
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_WRITE_ERROR, f->name);
		return;
	}

	f->mode = 2;  /* write mode */
	f->element_size = element_size;
}

void
pascal_close_file(PascalFile *f)
{
	if (f->fp != NULL) {
		fclose(f->fp);
	}
	f->fp = NULL;
	f->mode = 0;
}

int
pascal_eof_file(PascalFile *f)
{
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return 1;
	}
	return feof(f->fp) != 0;
}

void
pascal_read_file(PascalFile *f, void *buffer)
{
	if (f->fp == NULL || f->mode != 1) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	if (fread(buffer, f->element_size, 1, f->fp) != 1) {
		if (feof(f->fp)) {
			pascal_runtime_error(PASCAL_ERR_READ_ERROR, "End of file reached");
		} else {
			pascal_runtime_error(PASCAL_ERR_READ_ERROR, f->name);
		}
	}
}

void
pascal_write_file(PascalFile *f, const void *buffer)
{
	if (f->fp == NULL || f->mode != 2) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	if (fwrite(buffer, f->element_size, 1, f->fp) != 1) {
		pascal_runtime_error(PASCAL_ERR_WRITE_ERROR, f->name);
	}
}

void
pascal_seek_file(PascalFile *f, long position)
{
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return;
	}
	if (fseek(f->fp, position * f->element_size, SEEK_SET) != 0) {
		pascal_runtime_error(PASCAL_ERR_INVALID_OPERATION, "Seek failed");
	}
}

long
pascal_filepos(PascalFile *f)
{
	long pos;
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return -1;
	}
	pos = ftell(f->fp);
	if (pos < 0) {
		return -1;
	}
	return pos / f->element_size;
}

long
pascal_filesize(PascalFile *f)
{
	long current, size;
	if (f->fp == NULL) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_OPEN, f->name);
		return -1;
	}

	current = ftell(f->fp);
	if (current < 0) {
		return -1;
	}

	if (fseek(f->fp, 0, SEEK_END) != 0) {
		return -1;
	}

	size = ftell(f->fp);
	fseek(f->fp, current, SEEK_SET);  /* Restore position */

	if (size < 0) {
		return -1;
	}

	return size / f->element_size;
}

/*
 * File Management Operations
 */

void
pascal_erase(const char *filename)
{
	if (remove(filename) != 0) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_FOUND, filename);
	}
}

void
pascal_rename_file(const char *oldname, const char *newname)
{
	if (rename(oldname, newname) != 0) {
		pascal_runtime_error(PASCAL_ERR_FILE_NOT_FOUND, oldname);
	}
}
