/*
 * COBOL Runtime Library - File I/O Operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "cobolrt.h"

cobol_file_t *
__cobol_file_open(const char *filename, int mode, int org, int access)
{
	cobol_file_t *file;
	const char *fmode;

	file = (cobol_file_t *)__cobol_malloc(sizeof(cobol_file_t));
	memset(file, 0, sizeof(cobol_file_t));

	file->filename = strdup(filename);
	file->organization = org;
	file->access_mode = access;
	file->open_mode = mode;

	/* Determine file open mode */
	switch (mode) {
	case COB_OPEN_INPUT:
		fmode = "r";
		break;
	case COB_OPEN_OUTPUT:
		fmode = "w";
		break;
	case COB_OPEN_IO:
		fmode = "r+";
		break;
	case COB_OPEN_EXTEND:
		fmode = "a";
		break;
	default:
		free(file->filename);
		free(file);
		return NULL;
	}

	/* Open file */
	file->fp = fopen(filename, fmode);
	if (!file->fp) {
		file->status = COB_STATUS_FILE_OPEN;
		return file;
	}

	file->status = COB_STATUS_SUCCESS;
	file->current_record = 0;

	return file;
}

int
__cobol_file_close(cobol_file_t *file)
{
	if (!file)
		return COB_STATUS_FILE_CLOSED;

	if (file->fp) {
		fclose(file->fp);
		file->fp = NULL;
	}

	if (file->filename) {
		free(file->filename);
		file->filename = NULL;
	}

	free(file);
	return COB_STATUS_SUCCESS;
}

int
__cobol_file_read(cobol_file_t *file, cobol_field_t *record)
{
	size_t bytes_read;

	if (!file || !file->fp)
		return COB_STATUS_FILE_CLOSED;

	if (!record || !record->data)
		return COB_STATUS_NO_RECORD;

	/* Sequential read */
	if (file->organization == COB_ORG_SEQUENTIAL) {
		bytes_read = fread(record->data, 1, record->size, file->fp);

		if (bytes_read == 0) {
			if (feof(file->fp)) {
				file->status = COB_STATUS_EOF;
				return COB_STATUS_EOF;
			}
			file->status = COB_STATUS_NO_RECORD;
			return COB_STATUS_NO_RECORD;
		}

		/* Pad with spaces if needed */
		if (bytes_read < record->size)
			memset((char *)record->data + bytes_read, ' ', record->size - bytes_read);

		file->current_record++;
		file->status = COB_STATUS_SUCCESS;
		return COB_STATUS_SUCCESS;
	}

	/* TODO: Implement indexed and relative file access */
	file->status = COB_STATUS_NO_RECORD;
	return COB_STATUS_NO_RECORD;
}

int
__cobol_file_write(cobol_file_t *file, cobol_field_t *record)
{
	size_t bytes_written;

	if (!file || !file->fp)
		return COB_STATUS_FILE_CLOSED;

	if (!record || !record->data)
		return COB_STATUS_NO_RECORD;

	/* Write record */
	bytes_written = fwrite(record->data, 1, record->size, file->fp);

	if (bytes_written < record->size) {
		file->status = COB_STATUS_NO_RECORD;
		return COB_STATUS_NO_RECORD;
	}

	/* Add newline for sequential files */
	if (file->organization == COB_ORG_SEQUENTIAL)
		fputc('\n', file->fp);

	file->current_record++;
	file->status = COB_STATUS_SUCCESS;
	return COB_STATUS_SUCCESS;
}

int
__cobol_file_rewrite(cobol_file_t *file, cobol_field_t *record)
{
	long pos;

	if (!file || !file->fp)
		return COB_STATUS_FILE_CLOSED;

	if (!record || !record->data)
		return COB_STATUS_NO_RECORD;

	/* Save position */
	pos = ftell(file->fp);

	/* Seek back to start of record */
	if (fseek(file->fp, pos - record->size - 1, SEEK_SET) != 0) {
		file->status = COB_STATUS_NO_RECORD;
		return COB_STATUS_NO_RECORD;
	}

	/* Write record */
	if (fwrite(record->data, 1, record->size, file->fp) < record->size) {
		file->status = COB_STATUS_NO_RECORD;
		return COB_STATUS_NO_RECORD;
	}

	file->status = COB_STATUS_SUCCESS;
	return COB_STATUS_SUCCESS;
}

int
__cobol_file_delete(cobol_file_t *file)
{
	/* Not implemented - would require indexed file support */
	if (!file)
		return COB_STATUS_FILE_CLOSED;

	file->status = COB_STATUS_NO_RECORD;
	return COB_STATUS_NO_RECORD;
}

int
__cobol_file_start(cobol_file_t *file, cobol_field_t *key, int operator)
{
	/* Not implemented - requires indexed file support */
	if (!file)
		return COB_STATUS_FILE_CLOSED;

	file->status = COB_STATUS_NO_RECORD;
	return COB_STATUS_NO_RECORD;
}
