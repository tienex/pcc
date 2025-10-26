/*
 * Copyright (c) 2025
 *
 * COBOL compiler driver for PCC
 * Supports OO COBOL with DEC, IBM, HP, and Microsoft dialects
 */

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../../cc/driver/strlist.h"
#include "../../common/compat.h"

#ifndef LIBEXECDIR
#define LIBEXECDIR "/usr/libexec"
#endif

/* Compiler dialect types */
#define DIALECT_DEFAULT 0
#define DIALECT_IBM     1
#define DIALECT_DEC     2
#define DIALECT_HP      3
#define DIALECT_MS      4

static char *progname;
static int debug = 0;
static int verbose = 0;
static int compile_only = 0;
static int assemble_only = 0;
static int dialect = DIALECT_DEFAULT;
static char *output_file = NULL;

/* Pass file locations */
static char *pass0 = LIBEXECDIR "/cbolcom";  /* COBOL compiler */
static char *passas = "/usr/bin/as";          /* Assembler */
static char *passld = "/usr/bin/ld";          /* Linker */

/* File lists */
static struct strlist source_files;
static struct strlist object_files;
static struct strlist compiler_flags;
static struct strlist assembler_flags;
static struct strlist linker_flags;

static void
error(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s: error: ", progname);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	exit(1);
}

static void
warning(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s: warning: ", progname);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

static char *
make_temp_file(const char *suffix)
{
	char template[] = "/tmp/cbolXXXXXX";
	int fd;
	char *name;

	fd = mkstemp(template);
	if (fd < 0)
		error("cannot create temporary file: %s", strerror(errno));
	close(fd);

	name = malloc(strlen(template) + strlen(suffix) + 1);
	sprintf(name, "%s%s", template, suffix);
	rename(template, name);

	return name;
}

static int
run_command(struct strlist *args)
{
	pid_t pid;
	int status;
	char **argv;

	argv = strlist_to_array(args);

	if (verbose) {
		int i;
		for (i = 0; argv[i]; i++)
			fprintf(stderr, "%s ", argv[i]);
		fprintf(stderr, "\n");
	}

	pid = fork();
	if (pid < 0)
		error("fork failed: %s", strerror(errno));

	if (pid == 0) {
		execv(argv[0], argv);
		error("exec failed for %s: %s", argv[0], strerror(errno));
	}

	waitpid(pid, &status, 0);
	free(argv);

	if (WIFEXITED(status))
		return WEXITSTATUS(status);

	return 1;
}

static int
compile_cobol(const char *input, const char *output)
{
	struct strlist args;
	int ret;

	strlist_init(&args);
	strlist_append(&args, pass0);

	/* Add dialect flag */
	switch (dialect) {
	case DIALECT_IBM:
		strlist_append(&args, "-Xibm");
		break;
	case DIALECT_DEC:
		strlist_append(&args, "-Xdec");
		break;
	case DIALECT_HP:
		strlist_append(&args, "-Xhp");
		break;
	case DIALECT_MS:
		strlist_append(&args, "-Xms");
		break;
	}

	/* Add compiler flags */
	strlist_append_list(&args, &compiler_flags);

	/* Add input and output */
	strlist_append(&args, (char *)input);
	strlist_append(&args, "-o");
	strlist_append(&args, (char *)output);

	ret = run_command(&args);
	strlist_free(&args);

	return ret;
}

static int
assemble(const char *input, const char *output)
{
	struct strlist args;
	int ret;

	strlist_init(&args);
	strlist_append(&args, passas);

	/* Add assembler flags */
	strlist_append_list(&args, &assembler_flags);

	strlist_append(&args, "-o");
	strlist_append(&args, (char *)output);
	strlist_append(&args, (char *)input);

	ret = run_command(&args);
	strlist_free(&args);

	return ret;
}

static int
link_objects(struct strlist *objects, const char *output)
{
	struct strlist args;
	int ret;

	strlist_init(&args);
	strlist_append(&args, passld);

	/* Add linker flags */
	strlist_append_list(&args, &linker_flags);

	strlist_append(&args, "-o");
	strlist_append(&args, (char *)output);

	/* Add object files */
	strlist_append_list(&args, objects);

	ret = run_command(&args);
	strlist_free(&args);

	return ret;
}

static void
process_file(const char *filename)
{
	char *base, *asm_file, *obj_file;
	int ret;

	/* Get base filename without extension */
	base = strdup(filename);
	char *dot = strrchr(base, '.');
	if (dot)
		*dot = '\0';

	/* Generate assembly file */
	if (assemble_only) {
		if (output_file)
			asm_file = strdup(output_file);
		else {
			asm_file = malloc(strlen(base) + 3);
			sprintf(asm_file, "%s.s", base);
		}
	} else {
		asm_file = make_temp_file(".s");
	}

	if (debug)
		fprintf(stderr, "Compiling %s to %s\n", filename, asm_file);

	ret = compile_cobol(filename, asm_file);
	if (ret != 0)
		error("compilation failed for %s", filename);

	if (assemble_only) {
		free(base);
		return;
	}

	/* Assemble */
	if (compile_only) {
		if (output_file)
			obj_file = strdup(output_file);
		else {
			obj_file = malloc(strlen(base) + 3);
			sprintf(obj_file, "%s.o", base);
		}
	} else {
		obj_file = make_temp_file(".o");
	}

	if (debug)
		fprintf(stderr, "Assembling %s to %s\n", asm_file, obj_file);

	ret = assemble(asm_file, obj_file);
	unlink(asm_file);
	free(asm_file);

	if (ret != 0)
		error("assembly failed for %s", filename);

	strlist_append(&object_files, obj_file);

	free(base);
}

static void
usage(void)
{
	fprintf(stderr, "Usage: %s [options] file...\n", progname);
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -c              Compile only, do not link\n");
	fprintf(stderr, "  -S              Compile to assembly only\n");
	fprintf(stderr, "  -o file         Output file name\n");
	fprintf(stderr, "  -g              Generate debug information\n");
	fprintf(stderr, "  -O              Enable optimization\n");
	fprintf(stderr, "  -v              Verbose output\n");
	fprintf(stderr, "  -d              Debug mode\n");
	fprintf(stderr, "  -std=<dialect>  Select COBOL dialect:\n");
	fprintf(stderr, "                    ibm - IBM COBOL\n");
	fprintf(stderr, "                    dec - DEC COBOL\n");
	fprintf(stderr, "                    hp  - HP COBOL\n");
	fprintf(stderr, "                    ms  - Microsoft COBOL\n");
	fprintf(stderr, "  -Wall           Enable all warnings\n");
	fprintf(stderr, "  -Werror         Treat warnings as errors\n");
	exit(1);
}

int
main(int argc, char **argv)
{
	int ch, i;

	progname = basename(argv[0]);

	/* Initialize string lists */
	strlist_init(&source_files);
	strlist_init(&object_files);
	strlist_init(&compiler_flags);
	strlist_init(&assembler_flags);
	strlist_init(&linker_flags);

	/* Parse command line */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] != '-') {
			/* Source file */
			strlist_append(&source_files, argv[i]);
			continue;
		}

		/* Handle options */
		if (strcmp(argv[i], "-c") == 0) {
			compile_only = 1;
		} else if (strcmp(argv[i], "-S") == 0) {
			assemble_only = 1;
		} else if (strcmp(argv[i], "-o") == 0) {
			if (++i >= argc)
				usage();
			output_file = argv[i];
		} else if (strcmp(argv[i], "-g") == 0) {
			strlist_append(&compiler_flags, "-g");
		} else if (strcmp(argv[i], "-O") == 0) {
			strlist_append(&compiler_flags, "-O");
		} else if (strcmp(argv[i], "-v") == 0) {
			verbose = 1;
		} else if (strcmp(argv[i], "-d") == 0) {
			debug = 1;
		} else if (strncmp(argv[i], "-std=", 5) == 0) {
			const char *std = argv[i] + 5;
			if (strcmp(std, "ibm") == 0)
				dialect = DIALECT_IBM;
			else if (strcmp(std, "dec") == 0)
				dialect = DIALECT_DEC;
			else if (strcmp(std, "hp") == 0)
				dialect = DIALECT_HP;
			else if (strcmp(std, "ms") == 0)
				dialect = DIALECT_MS;
			else
				error("unknown COBOL dialect: %s", std);
		} else if (strcmp(argv[i], "-Wall") == 0) {
			strlist_append(&compiler_flags, "-Wall");
		} else if (strcmp(argv[i], "-Werror") == 0) {
			strlist_append(&compiler_flags, "-Werror");
		} else {
			/* Pass unknown flags to compiler */
			strlist_append(&compiler_flags, argv[i]);
		}
	}

	/* Check for input files */
	if (strlist_count(&source_files) == 0)
		usage();

	/* Check output file restrictions */
	if (output_file && strlist_count(&source_files) > 1 &&
	    (compile_only || assemble_only)) {
		error("cannot specify -o with multiple files and -c or -S");
	}

	/* Process each source file */
	for (i = 0; i < strlist_count(&source_files); i++) {
		process_file(strlist_item(&source_files, i));
	}

	/* Link if necessary */
	if (!compile_only && !assemble_only) {
		const char *output = output_file ? output_file : "a.out";

		if (debug)
			fprintf(stderr, "Linking to %s\n", output);

		if (link_objects(&object_files, output) != 0)
			error("linking failed");

		/* Clean up temporary object files */
		for (i = 0; i < strlist_count(&object_files); i++) {
			unlink(strlist_item(&object_files, i));
		}
	}

	/* Cleanup */
	strlist_free(&source_files);
	strlist_free(&object_files);
	strlist_free(&compiler_flags);
	strlist_free(&assembler_flags);
	strlist_free(&linker_flags);

	return 0;
}
