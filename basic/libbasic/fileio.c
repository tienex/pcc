/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * BASIC Runtime Library - File I/O and System Functions
 */

#include "basicrt.h"
#include <unistd.h>

/* File table */
basic_file_t basic_files[MAX_FILES];

/*
 * OPEN - Open a file
 */

void
basic_open(const char *filename, int file_num, const char *mode)
{
	const char *fmode;

	if (file_num < 1 || file_num >= MAX_FILES) {
		fprintf(stderr, "Error: Invalid file number %d\n", file_num);
		return;
	}

	/* Close if already open */
	if (basic_files[file_num].is_open) {
		basic_close(file_num);
	}

	/* Determine file mode */
	if (strcmp(mode, "INPUT") == 0 || strcmp(mode, "I") == 0) {
		fmode = "r";
		basic_files[file_num].mode = 0;
	} else if (strcmp(mode, "OUTPUT") == 0 || strcmp(mode, "O") == 0) {
		fmode = "w";
		basic_files[file_num].mode = 1;
	} else if (strcmp(mode, "APPEND") == 0 || strcmp(mode, "A") == 0) {
		fmode = "a";
		basic_files[file_num].mode = 2;
	} else if (strcmp(mode, "RANDOM") == 0 || strcmp(mode, "R") == 0) {
		fmode = "r+";
		basic_files[file_num].mode = 3;
	} else if (strcmp(mode, "BINARY") == 0 || strcmp(mode, "B") == 0) {
		fmode = "rb+";
		basic_files[file_num].mode = 4;
	} else {
		fmode = "r";
		basic_files[file_num].mode = 0;
	}

	basic_files[file_num].fp = fopen(filename, fmode);
	if (basic_files[file_num].fp == NULL) {
		fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
		return;
	}

	basic_files[file_num].file_num = file_num;
	basic_files[file_num].is_open = 1;
}

/*
 * CLOSE - Close a file
 */

void
basic_close(int file_num)
{
	if (file_num < 1 || file_num >= MAX_FILES)
		return;

	if (basic_files[file_num].is_open) {
		fclose(basic_files[file_num].fp);
		basic_files[file_num].is_open = 0;
		basic_files[file_num].fp = NULL;
	}
}

/*
 * PRINT # - Write to file
 */

void
basic_print_file(int file_num, const char *str)
{
	if (file_num < 1 || file_num >= MAX_FILES) {
		fprintf(stderr, "Error: Invalid file number %d\n", file_num);
		return;
	}

	if (!basic_files[file_num].is_open) {
		fprintf(stderr, "Error: File #%d not open\n", file_num);
		return;
	}

	fprintf(basic_files[file_num].fp, "%s", str);
}

/*
 * INPUT # - Read from file
 */

void
basic_input_file(int file_num, basic_string_t *str)
{
	char buffer[1024];

	if (file_num < 1 || file_num >= MAX_FILES) {
		fprintf(stderr, "Error: Invalid file number %d\n", file_num);
		return;
	}

	if (!basic_files[file_num].is_open) {
		fprintf(stderr, "Error: File #%d not open\n", file_num);
		return;
	}

	if (fgets(buffer, sizeof(buffer), basic_files[file_num].fp) != NULL) {
		size_t len = strlen(buffer);
		if (len > 0 && buffer[len-1] == '\n')
			buffer[len-1] = '\0';

		if (str->allocated < (int)strlen(buffer) + 1) {
			free(str->data);
			str->data = strdup(buffer);
			str->allocated = strlen(buffer) + 1;
		} else {
			strcpy(str->data, buffer);
		}
		str->length = strlen(buffer);
	}
}

/*
 * EOF - Check end of file
 */

int
basic_eof(int file_num)
{
	if (file_num < 1 || file_num >= MAX_FILES)
		return 1;

	if (!basic_files[file_num].is_open)
		return 1;

	return feof(basic_files[file_num].fp);
}

/*
 * LOF - Get file length
 */

int
basic_lof(int file_num)
{
	long pos, len;

	if (file_num < 1 || file_num >= MAX_FILES)
		return 0;

	if (!basic_files[file_num].is_open)
		return 0;

	pos = ftell(basic_files[file_num].fp);
	fseek(basic_files[file_num].fp, 0, SEEK_END);
	len = ftell(basic_files[file_num].fp);
	fseek(basic_files[file_num].fp, pos, SEEK_SET);

	return (int)len;
}

/*
 * LOC - Get current file position
 */

int
basic_loc(int file_num)
{
	if (file_num < 1 || file_num >= MAX_FILES)
		return 0;

	if (!basic_files[file_num].is_open)
		return 0;

	return (int)ftell(basic_files[file_num].fp);
}

/*
 * System functions
 */

void
basic_end(void)
{
	basic_runtime_cleanup();
	exit(0);
}

void
basic_stop(void)
{
	printf("STOP at line %d\n", 0);  /* Line number would be filled by compiler */
	basic_runtime_cleanup();
	exit(0);
}

void
basic_system(const char *cmd)
{
	system(cmd);
}

/*
 * TIMER - Seconds since midnight
 */

long
basic_timer(void)
{
	time_t now = time(NULL);
	struct tm *tm = localtime(&now);
	return (long)(tm->tm_hour * 3600 + tm->tm_min * 60 + tm->tm_sec);
}

/*
 * SLEEP - Sleep for n seconds
 */

void
basic_sleep(int seconds)
{
	sleep(seconds);
}

/*
 * BEEP - Make a beep sound
 */

void
basic_beep(void)
{
	printf("\a");
	fflush(stdout);
}

/*
 * Runtime initialization
 */

void
basic_runtime_init(void)
{
	int i;

	/* Initialize file table */
	for (i = 0; i < MAX_FILES; i++) {
		basic_files[i].fp = NULL;
		basic_files[i].file_num = i;
		basic_files[i].mode = 0;
		basic_files[i].is_open = 0;
	}

	/* Initialize random number generator */
	rnd_initialized = 0;
}

/*
 * Runtime cleanup
 */

void
basic_runtime_cleanup(void)
{
	int i;

	/* Close all open files */
	for (i = 0; i < MAX_FILES; i++) {
		if (basic_files[i].is_open) {
			basic_close(i);
		}
	}
}
