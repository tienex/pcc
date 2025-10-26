/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Driver program for BLISS compiler
 * Orchestrates the compilation pipeline similar to cc/cc/cc.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/wait.h>
#include <libgen.h>

/* Compilation phases */
#define PHASE_COMPILE       1
#define PHASE_ASSEMBLE      2
#define PHASE_LINK          3

/* File type detection */
#define TYPE_UNKNOWN        0
#define TYPE_BLISS          1   /* .bli, .b32, .b36, .req */
#define TYPE_ASSEMBLY       2   /* .s */
#define TYPE_OBJECT         3   /* .o */

/* Program paths */
static char *bcom = LIBEXECDIR "/bcom";     /* BLISS compiler */
static char *assembler = "as";              /* Assembler */
static char *linker = "ld";                 /* Linker */

/* Options */
static int verbose = 0;
static int compile_only = 0;   /* -c: compile to .o but don't link */
static int assemble_only = 0;  /* -S: compile to .s but don't assemble */
static char *output_file = NULL;
static int optimization_level = 0;

/* Temporary files */
#define MAX_TEMPS 100
static char *temp_files[MAX_TEMPS];
static int num_temps = 0;

/* Input files */
#define MAX_FILES 100
static char *input_files[MAX_FILES];
static int num_inputs = 0;

/* Library and object files */
static char *libraries[MAX_FILES];
static int num_libraries = 0;
static char *library_paths[MAX_FILES];
static int num_library_paths = 0;

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
	fprintf(stderr, "bliss: error: ");
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
	fprintf(stderr, "bliss: warning: ");
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

	/* BLISS source files */
	if (strcmp(ext, ".bli") == 0 || strcmp(ext, ".bliss") == 0 ||
	    strcmp(ext, ".b32") == 0 || strcmp(ext, ".b36") == 0 ||
	    strcmp(ext, ".req") == 0)
		return TYPE_BLISS;

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
	dot = strrchr(base, '.');
	if (dot)
		*dot = '\0';

	out = malloc(strlen(base) + strlen(suffix) + 1);
	sprintf(out, "%s%s", base, suffix);

	return out;
}

/*
 * Execute external program
 */
static int
execute(char **argv)
{
	pid_t pid;
	int status;
	int i;

	if (verbose) {
		for (i = 0; argv[i]; i++)
			printf("%s ", argv[i]);
		printf("\n");
	}

	pid = fork();
	if (pid == -1) {
		perror("fork");
		return -1;
	}

	if (pid == 0) {
		/* Child process */
		execvp(argv[0], argv);
		perror(argv[0]);
		exit(1);
	}

	/* Parent process */
	if (waitpid(pid, &status, 0) == -1) {
		perror("waitpid");
		return -1;
	}

	if (WIFEXITED(status))
		return WEXITSTATUS(status);

	return -1;
}

/*
 * Compile BLISS source to assembly
 */
static int
compile_bliss(const char *infile, const char *outfile)
{
	char *argv[50];
	int argc = 0;

	argv[argc++] = bcom;
	argv[argc++] = "-o";
	argv[argc++] = (char *)outfile;
	if (verbose)
		argv[argc++] = "-v";
	argv[argc++] = (char *)infile;
	argv[argc] = NULL;

	return execute(argv);
}

/*
 * Assemble to object file
 */
static int
assemble(const char *infile, const char *outfile)
{
	char *argv[50];
	int argc = 0;

	argv[argc++] = assembler;
	argv[argc++] = "-o";
	argv[argc++] = (char *)outfile;
	argv[argc++] = (char *)infile;
	argv[argc] = NULL;

	return execute(argv);
}

/*
 * Link object files
 */
static int
link_files(char **objfiles, int n_objs, const char *outfile)
{
	char *argv[200];
	int argc = 0;
	int i;

	argv[argc++] = linker;
	argv[argc++] = "-o";
	argv[argc++] = (char *)outfile;

	/* Add object files */
	for (i = 0; i < n_objs; i++)
		argv[argc++] = objfiles[i];

	/* Add library paths */
	for (i = 0; i < num_library_paths; i++) {
		argv[argc++] = "-L";
		argv[argc++] = library_paths[i];
	}

	/* Add libraries */
	for (i = 0; i < num_libraries; i++) {
		argv[argc++] = "-l";
		argv[argc++] = libraries[i];
	}

	/* Add standard C library */
	argv[argc++] = "-lc";

	argv[argc] = NULL;

	return execute(argv);
}

/*
 * Process a single source file
 */
static char *
process_file(const char *file)
{
	int type = file_type(file);
	char *asmfile = NULL;
	char *objfile = NULL;
	int ret;

	switch (type) {
	case TYPE_BLISS:
		/* BLISS source -> assembly */
		if (assemble_only || compile_only) {
			asmfile = output_file ? output_file : gen_outfile(file, ".s");
		} else {
			asmfile = gen_outfile(file, ".s");
			add_temp(asmfile);
		}

		ret = compile_bliss(file, asmfile);
		if (ret != 0)
			error("compilation of '%s' failed", file);

		if (assemble_only)
			return NULL;

		/* Assembly -> object */
		if (compile_only) {
			objfile = output_file ? output_file : gen_outfile(file, ".o");
		} else {
			objfile = gen_outfile(file, ".o");
			add_temp(objfile);
		}

		ret = assemble(asmfile, objfile);
		if (ret != 0)
			error("assembly of '%s' failed", asmfile);

		return objfile;

	case TYPE_ASSEMBLY:
		/* Assembly -> object */
		if (compile_only) {
			objfile = output_file ? output_file : gen_outfile(file, ".o");
		} else {
			objfile = gen_outfile(file, ".o");
			add_temp(objfile);
		}

		ret = assemble(file, objfile);
		if (ret != 0)
			error("assembly of '%s' failed", file);

		return objfile;

	case TYPE_OBJECT:
		/* Already object file */
		return strdup(file);

	default:
		warning("unknown file type: %s", file);
		return NULL;
	}
}

/*
 * Print usage
 */
static void
usage(void)
{
	printf("Usage: bliss [options] file...\n");
	printf("Options:\n");
	printf("  -o <file>     Place output in <file>\n");
	printf("  -c            Compile to object files, don't link\n");
	printf("  -S            Compile to assembly, don't assemble\n");
	printf("  -L<dir>       Add directory to library search path\n");
	printf("  -l<library>   Link with library\n");
	printf("  -v            Verbose output\n");
	printf("  -h, --help    Show this help\n");
	printf("\nBLISS is a system programming language developed at CMU.\n");
	printf("File extensions: .bli, .bliss, .b32, .b36, .req\n");
	exit(0);
}

/*
 * Main driver
 */
int
main(int argc, char **argv)
{
	int i;
	char *objfiles[MAX_FILES];
	int n_objs = 0;
	char *final_output;

	atexit(cleanup);

	/* Parse arguments */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			if (strcmp(argv[i], "-o") == 0) {
				if (++i >= argc)
					error("-o requires argument");
				output_file = argv[i];
			} else if (strcmp(argv[i], "-c") == 0) {
				compile_only = 1;
			} else if (strcmp(argv[i], "-S") == 0) {
				assemble_only = 1;
			} else if (strncmp(argv[i], "-L", 2) == 0) {
				library_paths[num_library_paths++] = &argv[i][2];
			} else if (strncmp(argv[i], "-l", 2) == 0) {
				libraries[num_libraries++] = &argv[i][2];
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
				usage();
			} else {
				warning("unknown option: %s", argv[i]);
			}
		} else {
			/* Input file */
			input_files[num_inputs++] = argv[i];
		}
	}

	if (num_inputs == 0)
		error("no input files");

	if (num_inputs > 1 && output_file && (compile_only || assemble_only))
		error("cannot specify -o with multiple files and -c or -S");

	/* Process each input file */
	for (i = 0; i < num_inputs; i++) {
		char *obj = process_file(input_files[i]);
		if (obj != NULL)
			objfiles[n_objs++] = obj;
	}

	/* Link if needed */
	if (!compile_only && !assemble_only && n_objs > 0) {
		final_output = output_file ? output_file : "a.out";
		if (link_files(objfiles, n_objs, final_output) != 0)
			error("linking failed");
	}

	return 0;
}
