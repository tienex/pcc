/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * System functions (execute, file operations, etc.)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include "../include/palrt.h"

int32_t pal_execute(const char *command)
{
	if (!command)
		return -1;

	return (int32_t)system(command);
}

int32_t pal_shell(const char *command)
{
	if (!command)
		return -1;

	return (int32_t)system(command);
}

PAL_String *pal_getenv(const char *name)
{
	const char *value;

	if (!name)
		return pal_string_new("");

	value = getenv(name);
	if (!value)
		return pal_string_new("");

	return pal_string_new(value);
}

int pal_setenv(const char *name, const char *value)
{
	if (!name || !value)
		return -1;

#ifdef _WIN32
	char buf[1024];
	snprintf(buf, sizeof(buf), "%s=%s", name, value);
	return _putenv(buf);
#else
	return setenv(name, value, 1);
#endif
}

PAL_Logical pal_fileexists(const char *path)
{
	struct stat st;

	if (!path)
		return PAL_FALSE;

	if (stat(path, &st) == 0)
		return PAL_TRUE;

	return PAL_FALSE;
}

int pal_filecopy(const char *src, const char *dst)
{
	FILE *fsrc, *fdst;
	char buffer[4096];
	size_t bytes;

	if (!src || !dst)
		return -1;

	fsrc = fopen(src, "rb");
	if (!fsrc)
		return -1;

	fdst = fopen(dst, "wb");
	if (!fdst) {
		fclose(fsrc);
		return -1;
	}

	while ((bytes = fread(buffer, 1, sizeof(buffer), fsrc)) > 0) {
		if (fwrite(buffer, 1, bytes, fdst) != bytes) {
			fclose(fsrc);
			fclose(fdst);
			return -1;
		}
	}

	fclose(fsrc);
	fclose(fdst);
	return 0;
}

int pal_filedelete(const char *path)
{
	if (!path)
		return -1;

	return unlink(path);
}

int pal_filerename(const char *old_path, const char *new_path)
{
	if (!old_path || !new_path)
		return -1;

	return rename(old_path, new_path);
}
