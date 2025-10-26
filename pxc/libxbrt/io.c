/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * I/O and miscellaneous functions
 */

#include "xbrt.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>

static int last_error = 0;

/* FOPEN() - Open file */
int xb_fopen(const char *filename, int mode) {
	int fd = open(filename, mode);
	if (fd < 0) last_error = errno;
	return fd;
}

/* FCREATE() - Create file */
int xb_fcreate(const char *filename, int attr) {
	int fd = creat(filename, attr ? attr : 0644);
	if (fd < 0) last_error = errno;
	return fd;
}

/* FCLOSE() - Close file */
void xb_fclose(int handle) {
	if (close(handle) < 0)
		last_error = errno;
}

/* FREAD() - Read from file */
int xb_fread(int handle, char *buffer, int bytes) {
	int result = read(handle, buffer, bytes);
	if (result < 0) last_error = errno;
	return result;
}

/* FWRITE() - Write to file */
int xb_fwrite(int handle, const char *buffer, int bytes) {
	int result = write(handle, buffer, bytes);
	if (result < 0) last_error = errno;
	return result;
}

/* FSEEK() - Seek in file */
long xb_fseek(int handle, long offset, int whence) {
	long result = lseek(handle, offset, whence);
	if (result < 0) last_error = errno;
	return result;
}

/* FERROR() - Get last error */
int xb_ferror(void) {
	return last_error;
}

/* FILE() - Check if file exists */
int xb_file(const char *filename) {
	return access(filename, F_OK) == 0;
}

/* FERASE() - Delete file */
int xb_ferase(const char *filename) {
	return unlink(filename);
}

/* FRENAME() - Rename file */
int xb_frename(const char *oldname, const char *newname) {
	return rename(oldname, newname);
}

/* MEMOREAD() - Read entire file */
char *xb_memoread(const char *filename) {
	FILE *fp = fopen(filename, "rb");
	if (!fp) return NULL;

	fseek(fp, 0, SEEK_END);
	long size = ftell(fp);
	fseek(fp, 0, SEEK_SET);

	char *content = malloc(size + 1);
	if (content) {
		fread(content, 1, size, fp);
		content[size] = '\0';
	}

	fclose(fp);
	return content;
}

/* MEMOWRIT() - Write entire file */
int xb_memowrit(const char *filename, const char *content) {
	FILE *fp = fopen(filename, "wb");
	if (!fp) return 0;

	size_t len = strlen(content);
	size_t written = fwrite(content, 1, len, fp);
	fclose(fp);

	return written == len;
}

/* Type checking */
char xb_valtype(const xb_value_t *val) {
	return val ? xb_value_type_name(val)[0] : 'U';
}

int xb_isnil(const xb_value_t *val) {
	return !val || val->type == XB_NIL || val->type == XB_NULL;
}

int xb_isnumber(const xb_value_t *val) {
	return val && val->type == XB_NUMERIC;
}

int xb_ischaracter(const xb_value_t *val) {
	return val && (val->type == XB_CHARACTER || val->type == XB_MEMO);
}

int xb_islogical(const xb_value_t *val) {
	return val && val->type == XB_LOGICAL;
}

int xb_isdate(const xb_value_t *val) {
	return val && val->type == XB_DATE;
}

int xb_isarray(const xb_value_t *val) {
	return val && val->type == XB_ARRAY;
}

int xb_isobject(const xb_value_t *val) {
	return val && val->type == XB_OBJECT;
}

int xb_isblock(const xb_value_t *val) {
	return val && val->type == XB_CODEBLOCK;
}

/* EMPTY() - Check if value is empty */
int xb_empty(const xb_value_t *val) {
	if (!val || val->type == XB_NIL || val->type == XB_NULL)
		return 1;

	switch (val->type) {
	case XB_NUMERIC:
		return val->data.numeric == 0.0;
	case XB_CHARACTER:
	case XB_MEMO:
		return !val->data.string || val->data.string[0] == '\0';
	case XB_LOGICAL:
		return !val->data.logical;
	case XB_ARRAY:
		return !val->data.array || val->data.array->length == 0;
	default:
		return 0;
	}
}

/* QOUT() - Quick output */
void xb_qout(const xb_value_t *val) {
	char *str = xb_value_to_string(val);
	if (str) {
		printf("%s\n", str);
		free(str);
	}
}
