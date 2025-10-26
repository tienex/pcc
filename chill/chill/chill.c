/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Driver program for CCITT CHILL compiler
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
#define PHASE_PREPROCESS    1
#define PHASE_COMPILE       2
#define PHASE_ASSEMBLE      3
#define PHASE_LINK          4

/* File type detection */
#define TYPE_UNKNOWN        0
#define TYPE_CHILL          1   /* .ch */
#define TYPE_ASSEMBLY       2   /* .s */
#define TYPE_OBJECT         3   /* .o */

/* Program paths */
static char *chcom = LIBEXECDIR "/chcom";       /* CHILL compiler */
static char *assembler = "as";                  /* Assembler */
static char *linker = "ld";                     /* Linker */
static char *chill_lib = "-lchill";             /* CHILL runtime library */

/* Options */
static int verbose = 0;
static int compile_only = 0;   /* -c: compile to .o but don't link */
static int assemble_only = 0;  /* -S: compile to .s but don't assemble */
static int stop_after_chcom = 0;
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
	fprintf(stderr, "chill: error: ");
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
	fprintf(stderr, "chill: warning: ");
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

	if (strcmp(ext, ".ch") == 0 || strcmp(ext, ".chl") == 0)
		return TYPE_CHILL;

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
 * Compile CHILL source to assembly
 */
static int
compile_chill(const char *infile, const char *outfile)
{
	char *argv[50];
	int argc = 0;

	argv[argc++] = chcom;
	if (verbose)
		argv[argc++] = "-v";
	argv[argc++] = "-o";
	argv[argc++] = (char *)outfile;
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

	/* Add CHILL runtime library */
	argv[argc++] = chill_lib;

	/* Add additional libraries */
	for (i = 0; i < num_libraries; i++)
		argv[argc++] = libraries[i];

	/* Add standard libraries */
	argv[argc++] = "-lm";      /* Math library */
	argv[argc++] = "-lpthread"; /* Pthread library for process support */
	argv[argc++] = "-lc";      /* C library */

	argv[argc] = NULL;

	return execute(argv);
}

/*
 * Process single input file
 */
static char *
process_file(const char *infile)
{
	int ftype;
	char *asmfile = NULL;
	char *objfile = NULL;
	int ret;

	ftype = file_type(infile);

	switch (ftype) {
	case TYPE_CHILL:
		/* Compile CHILL to assembly */
		if (assemble_only || compile_only) {
			asmfile = output_file ? output_file : gen_outfile(infile, ".s");
		} else {
			asmfile = gen_outfile(infile, ".s");
			add_temp(asmfile);
		}

		ret = compile_chill(infile, asmfile);
		if (ret != 0)
			error("compilation of '%s' failed", infile);

		if (assemble_only)
			return NULL;

		/* Fall through to assemble */
		/* FALLTHROUGH */

	case TYPE_ASSEMBLY:
		if (ftype == TYPE_ASSEMBLY)
			asmfile = (char *)infile;

		/* Assemble to object file */
		if (compile_only) {
			objfile = output_file ? output_file : gen_outfile(infile, ".o");
		} else {
			objfile = gen_outfile(infile, ".o");
			add_temp(objfile);
		}

		ret = assemble(asmfile, objfile);
		if (ret != 0)
			error("assembly of '%s' failed", asmfile);

		return objfile;

	case TYPE_OBJECT:
		return (char *)infile;

	default:
		error("unknown file type: %s", infile);
		return NULL;
	}
}

/*
 * Print usage information
 */
static void
usage(void)
{
	fprintf(stderr, "Usage: chill [options] file...\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -c           Compile and assemble, but do not link\n");
	fprintf(stderr, "  -S           Compile only, do not assemble or link\n");
	fprintf(stderr, "  -o <file>    Place output in <file>\n");
	fprintf(stderr, "  -v           Verbose mode\n");
	fprintf(stderr, "  -O<level>    Set optimization level (0-3)\n");
	fprintf(stderr, "  -L<dir>      Add directory to library search path\n");
	fprintf(stderr, "  -l<lib>      Link with library\n");
	fprintf(stderr, "  -w           Suppress warnings\n");
	fprintf(stderr, "  --help       Display this information\n");
	fprintf(stderr, "  --version    Display compiler version\n");
	exit(1);
}

/*
 * Print version information
 */
static void
version(void)
{
	printf("PCC CHILL Compiler (chcom) version 1.0\n");
	printf("CCITT CHILL (Z.200) language implementation\n");
	exit(0);
}

/*
 * Main entry point
 */
int
main(int argc, char *argv[])
{
	int i;
	char *objfiles[MAX_FILES];
	int num_objs = 0;
	int ret;

	/* Parse command line arguments */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			if (strcmp(argv[i], "-c") == 0) {
				compile_only = 1;
			} else if (strcmp(argv[i], "-S") == 0) {
				assemble_only = 1;
			} else if (strcmp(argv[i], "-o") == 0) {
				if (++i >= argc)
					error("missing filename after '-o'");
				output_file = argv[i];
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "-w") == 0) {
				/* Suppress warnings - passed to chcom */
			} else if (strncmp(argv[i], "-O", 2) == 0) {
				optimization_level = atoi(&argv[i][2]);
			} else if (strncmp(argv[i], "-L", 2) == 0) {
				if (strlen(argv[i]) > 2) {
					library_paths[num_library_paths++] = &argv[i][2];
				} else if (++i < argc) {
					library_paths[num_library_paths++] = argv[i];
				}
			} else if (strncmp(argv[i], "-l", 2) == 0) {
				char *lib = malloc(strlen(argv[i]) + 1);
				sprintf(lib, "-l%s", &argv[i][2]);
				libraries[num_libraries++] = lib;
			} else if (strcmp(argv[i], "--help") == 0) {
				usage();
			} else if (strcmp(argv[i], "--version") == 0) {
				version();
			} else {
				warning("unknown option: %s", argv[i]);
			}
		} else {
			/* Input file */
			if (num_inputs >= MAX_FILES)
				error("too many input files");
			input_files[num_inputs++] = argv[i];
		}
	}

	/* Check for input files */
	if (num_inputs == 0) {
		fprintf(stderr, "chill: no input files\n");
		usage();
	}

	/* Check for conflicting options */
	if (compile_only && assemble_only)
		error("cannot specify both -c and -S");

	if ((compile_only || assemble_only) && num_inputs > 1 && output_file)
		error("cannot specify -o with -c or -S and multiple files");

	/* Process each input file */
	for (i = 0; i < num_inputs; i++) {
		char *objfile = process_file(input_files[i]);
		if (objfile != NULL)
			objfiles[num_objs++] = objfile;
	}

	/* Link if not compile-only or assemble-only */
	if (!compile_only && !assemble_only && num_objs > 0) {
		const char *outfile = output_file ? output_file : "a.out";
		ret = link_files(objfiles, num_objs, outfile);
		if (ret != 0)
			error("linking failed");
	}

	/* Cleanup temporary files */
	cleanup();

	return 0;
}
