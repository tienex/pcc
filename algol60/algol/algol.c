/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Driver program for ALGOL 60+ compiler
 * Orchestrates the compilation pipeline
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/wait.h>
#include <libgen.h>

/* File type detection */
#define TYPE_UNKNOWN        0
#define TYPE_ALGOL          1   /* .alg, .alg60, .algol */
#define TYPE_ASSEMBLY       2   /* .s */
#define TYPE_OBJECT         3   /* .o */

/* Program paths */
static char *acom = LIBEXECDIR "/acom";     /* ALGOL compiler */
static char *assembler = "as";              /* Assembler */
static char *linker = "ld";                 /* Linker */

/* Options */
static int verbose = 0;
static int compile_only = 0;   /* -c: compile to .o but don't link */
static int assemble_only = 0;  /* -S: compile to .s but don't assemble */
static char *output_file = NULL;

/* Temporary files */
#define MAX_TEMPS 100
static char *temp_files[MAX_TEMPS];
static int num_temps = 0;

/* Input files */
#define MAX_FILES 100
static char *input_files[MAX_FILES];
static int num_inputs = 0;

/*
 * Add temporary file for cleanup
 */
static void
add_temp(const char *file)
{
	if (num_temps < MAX_TEMPS)
		temp_files[num_temps++] = strdup(file);
}

/*
 * Cleanup temporary files
 */
static void
cleanup(void)
{
	int i;

	for (i = 0; i < num_temps; i++) {
		if (verbose)
			printf("rm %s\n", temp_files[i]);
		unlink(temp_files[i]);
		free(temp_files[i]);
	}
}

/*
 * Error message and exit
 */
static void
error(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "algol: error: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	cleanup();
	exit(1);
}

/*
 * Warning message
 */
static void
warning(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "algol: warning: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

/*
 * Detect file type from extension
 */
static int
file_type(const char *filename)
{
	const char *ext = strrchr(filename, '.');

	if (ext == NULL)
		return TYPE_UNKNOWN;

	if (strcmp(ext, ".alg") == 0 || strcmp(ext, ".alg60") == 0 ||
	    strcmp(ext, ".algol") == 0)
		return TYPE_ALGOL;

	if (strcmp(ext, ".s") == 0 || strcmp(ext, ".S") == 0)
		return TYPE_ASSEMBLY;

	if (strcmp(ext, ".o") == 0)
		return TYPE_OBJECT;

	return TYPE_UNKNOWN;
}

/*
 * Generate output filename
 */
static char *
gen_outfile(const char *infile, const char *suffix)
{
	char *out;
	char *base;
	char *dot;
	char tmp[1024];

	strcpy(tmp, infile);
	base = basename(tmp);

	/* Remove extension */
	dot = strrchr(base, '.');
	if (dot != NULL)
		*dot = '\0';

	/* Add new suffix */
	out = malloc(strlen(base) + strlen(suffix) + 2);
	sprintf(out, "%s%s", base, suffix);

	return out;
}

/*
 * Execute a command
 */
static int
execute(char **argv)
{
	pid_t pid;
	int status;

	if (verbose) {
		int i;
		for (i = 0; argv[i] != NULL; i++)
			printf("%s ", argv[i]);
		printf("\n");
	}

	pid = fork();
	if (pid < 0) {
		error("fork failed");
	} else if (pid == 0) {
		/* Child: execute command */
		execvp(argv[0], argv);
		perror(argv[0]);
		exit(1);
	} else {
		/* Parent: wait for child */
		if (waitpid(pid, &status, 0) < 0) {
			error("waitpid failed");
		}
		if (WIFEXITED(status)) {
			return WEXITSTATUS(status);
		} else {
			error("command terminated abnormally");
		}
	}

	return 0;
}

/*
 * Print usage
 */
static void
usage(void)
{
	fprintf(stderr,
	    "Usage: algol [options] file...\n"
	    "Options:\n"
	    "  -c            Compile to object file (.o)\n"
	    "  -S            Compile to assembly (.s)\n"
	    "  -o <file>     Output file name\n"
	    "  -v            Verbose output\n"
	    "  -h, --help    Show this help\n");
	exit(1);
}

/*
 * Main driver
 */
int
main(int argc, char **argv)
{
	int i, ret;
	char *asmfile, *objfile;

	/* Parse arguments */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			if (strcmp(argv[i], "-c") == 0) {
				compile_only = 1;
			} else if (strcmp(argv[i], "-S") == 0) {
				assemble_only = 1;
			} else if (strcmp(argv[i], "-o") == 0) {
				if (++i >= argc)
					error("-o requires argument");
				output_file = argv[i];
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "-h") == 0 ||
			           strcmp(argv[i], "--help") == 0) {
				usage();
			} else {
				error("unknown option '%s'", argv[i]);
			}
		} else {
			/* Input file */
			if (num_inputs >= MAX_FILES)
				error("too many input files");
			input_files[num_inputs++] = argv[i];
		}
	}

	/* Check we have input */
	if (num_inputs == 0) {
		error("no input files");
	}

	/* Process each input file */
	for (i = 0; i < num_inputs; i++) {
		char *infile = input_files[i];
		int type = file_type(infile);

		if (type == TYPE_ALGOL) {
			/* Compile ALGOL source */
			char *cmd[10];
			int idx = 0;

			/* Generate assembly filename */
			asmfile = assemble_only && output_file ?
			          output_file : gen_outfile(infile, ".s");

			/* Run acom (ALGOL compiler) */
			cmd[idx++] = acom;
			cmd[idx++] = "-o";
			cmd[idx++] = asmfile;
			if (verbose)
				cmd[idx++] = "-v";
			cmd[idx++] = infile;
			cmd[idx++] = NULL;

			ret = execute(cmd);
			if (ret != 0) {
				cleanup();
				exit(ret);
			}

			if (!assemble_only) {
				add_temp(asmfile);
			}

			if (assemble_only) {
				continue;  /* Stop here */
			}

			/* Assemble */
			objfile = compile_only && output_file ?
			          output_file : gen_outfile(infile, ".o");

			cmd[0] = assembler;
			cmd[1] = "-o";
			cmd[2] = objfile;
			cmd[3] = asmfile;
			cmd[4] = NULL;

			ret = execute(cmd);
			if (ret != 0) {
				cleanup();
				exit(ret);
			}

			if (!compile_only) {
				add_temp(objfile);
			}

		} else if (type == TYPE_ASSEMBLY) {
			/* Assemble only */
			char *cmd[10];
			objfile = compile_only && output_file ?
			          output_file : gen_outfile(infile, ".o");

			cmd[0] = assembler;
			cmd[1] = "-o";
			cmd[2] = objfile;
			cmd[3] = infile;
			cmd[4] = NULL;

			ret = execute(cmd);
			if (ret != 0) {
				cleanup();
				exit(ret);
			}

		} else if (type == TYPE_OBJECT) {
			/* Already compiled, will link later */
		} else {
			error("unknown file type: %s", infile);
		}
	}

	/* Link if needed */
	if (!compile_only && !assemble_only) {
		/* Simple linking - just link all object files */
		printf("Linking not yet implemented\n");
	}

	cleanup();
	return 0;
}
