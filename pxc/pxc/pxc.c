/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Driver program for pxc (Xbase++ compiler)
 * Orchestrates the compilation pipeline
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
#define TYPE_XBASEPP        1   /* .prg, .xbp */
#define TYPE_ASSEMBLY       2   /* .s */
#define TYPE_OBJECT         3   /* .o */
#define TYPE_C              4   /* .c (if we generate C code) */

/* Program paths */
static char *pxccom = LIBEXECDIR "/pxccom";  /* Xbase++ compiler */
static char *assembler = "as";                /* Assembler */
static char *linker = "ld";                   /* Linker */
static char *cc = "cc";                       /* C compiler (if needed) */

/* Options */
static int verbose = 0;
static int compile_only = 0;   /* -c: compile to .o but don't link */
static int assemble_only = 0;  /* -S: compile to .s but don't assemble */
static int emit_c = 0;          /* --emit-c: generate C code instead of assembly */
static char *output_file = NULL;
static int optimization_level = 0;
static int debug_mode = 0;

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
static char *object_files[MAX_FILES];
static int num_objects = 0;

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
	fprintf(stderr, "pxc: error: ");
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
	fprintf(stderr, "pxc: warning: ");
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

	if (strcmp(ext, ".prg") == 0 || strcmp(ext, ".xbp") == 0 ||
	    strcmp(ext, ".ch") == 0 || strcmp(ext, ".xbs") == 0)
		return TYPE_XBASEPP;

	if (strcmp(ext, ".s") == 0 || strcmp(ext, ".S") == 0)
		return TYPE_ASSEMBLY;

	if (strcmp(ext, ".o") == 0)
		return TYPE_OBJECT;

	if (strcmp(ext, ".c") == 0)
		return TYPE_C;

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

	strncpy(tmp, infile, sizeof(tmp) - 1);
	tmp[sizeof(tmp) - 1] = '\0';
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
 * Compile Xbase++ source to assembly or C
 */
static int
compile_xbasepp(const char *infile, const char *outfile)
{
	char *argv[50];
	int argc = 0;

	argv[argc++] = pxccom;

	if (verbose)
		argv[argc++] = "-v";

	if (debug_mode)
		argv[argc++] = "-d";

	if (optimization_level > 0)
		argv[argc++] = "-O";

	argv[argc++] = "-o";
	argv[argc++] = (char *)outfile;
	argv[argc++] = (char *)infile;
	argv[argc] = NULL;

	return execute(argv);
}

/*
 * Compile C source to assembly
 */
static int
compile_c(const char *infile, const char *outfile)
{
	char *argv[50];
	int argc = 0;

	argv[argc++] = cc;
	argv[argc++] = "-S";
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

	/* Add libraries */
	for (i = 0; i < num_libraries; i++) {
		argv[argc++] = "-l";
		argv[argc++] = libraries[i];
	}

	/* Add Xbase++ runtime library */
	argv[argc++] = "-lxbrt";

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
	char *cfile = NULL;
	char *objfile = NULL;
	int ret;

	switch (type) {
	case TYPE_XBASEPP:
		/* Xbase++ source -> assembly or C */
		if (emit_c) {
			/* Generate C code */
			if (compile_only) {
				cfile = output_file ? output_file : gen_outfile(file, ".c");
			} else {
				cfile = gen_outfile(file, ".c");
				add_temp(cfile);
			}

			ret = compile_xbasepp(file, cfile);
			if (ret != 0)
				error("compilation of %s failed", file);

			if (compile_only)
				return NULL;

			/* Compile C to assembly */
			if (assemble_only) {
				asmfile = output_file ? output_file : gen_outfile(file, ".s");
			} else {
				asmfile = gen_outfile(file, ".s");
				add_temp(asmfile);
			}

			ret = compile_c(cfile, asmfile);
			if (ret != 0)
				error("C compilation of %s failed", cfile);
		} else {
			/* Generate assembly directly */
			if (assemble_only) {
				asmfile = output_file ? output_file : gen_outfile(file, ".s");
			} else {
				asmfile = gen_outfile(file, ".s");
				add_temp(asmfile);
			}

			ret = compile_xbasepp(file, asmfile);
			if (ret != 0)
				error("compilation of %s failed", file);
		}

		if (assemble_only)
			return NULL;

		/* Assembly -> object */
		if (compile_only) {
			objfile = output_file ? output_file : gen_outfile(file, ".o");
		} else {
			objfile = gen_outfile(file, ".o");
			if (!compile_only)
				add_temp(objfile);
		}

		ret = assemble(asmfile, objfile);
		if (ret != 0)
			error("assembly of %s failed", asmfile);

		return objfile;

	case TYPE_C:
		/* C source -> assembly -> object */
		if (assemble_only) {
			asmfile = output_file ? output_file : gen_outfile(file, ".s");
		} else {
			asmfile = gen_outfile(file, ".s");
			add_temp(asmfile);
		}

		ret = compile_c(file, asmfile);
		if (ret != 0)
			error("C compilation of %s failed", file);

		if (assemble_only)
			return NULL;

		if (compile_only) {
			objfile = output_file ? output_file : gen_outfile(file, ".o");
		} else {
			objfile = gen_outfile(file, ".o");
			add_temp(objfile);
		}

		ret = assemble(asmfile, objfile);
		if (ret != 0)
			error("assembly of %s failed", asmfile);

		return objfile;

	case TYPE_ASSEMBLY:
		/* Assembly -> object */
		if (assemble_only)
			return NULL;

		if (compile_only) {
			objfile = output_file ? output_file : gen_outfile(file, ".o");
		} else {
			objfile = gen_outfile(file, ".o");
			add_temp(objfile);
		}

		ret = assemble(file, objfile);
		if (ret != 0)
			error("assembly of %s failed", file);

		return objfile;

	case TYPE_OBJECT:
		/* Already object file */
		return (char *)file;

	default:
		error("unknown file type: %s", file);
		return NULL;
	}
}

/*
 * Print usage
 */
static void
usage(void)
{
	fprintf(stderr, "Usage: pxc [options] file...\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -c           Compile to object files, do not link\n");
	fprintf(stderr, "  -S           Compile to assembly, do not assemble\n");
	fprintf(stderr, "  -o <file>    Specify output file\n");
	fprintf(stderr, "  -O           Enable optimizations\n");
	fprintf(stderr, "  -v           Verbose output\n");
	fprintf(stderr, "  -g           Generate debug information\n");
	fprintf(stderr, "  -L<dir>      Add library search path\n");
	fprintf(stderr, "  -l<lib>      Link with library\n");
	fprintf(stderr, "  --emit-c     Generate C code instead of assembly\n");
	fprintf(stderr, "  --help       Show this help message\n");
	exit(1);
}

/*
 * Main driver
 */
int
main(int argc, char **argv)
{
	int i;
	char *final_output;
	int ret;

	/* Parse command line */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			/* Option */
			if (strcmp(argv[i], "-c") == 0) {
				compile_only = 1;
			} else if (strcmp(argv[i], "-S") == 0) {
				assemble_only = 1;
			} else if (strcmp(argv[i], "-o") == 0) {
				if (++i >= argc)
					error("missing argument to -o");
				output_file = argv[i];
			} else if (strcmp(argv[i], "-O") == 0) {
				optimization_level = 1;
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "-g") == 0) {
				debug_mode = 1;
			} else if (strncmp(argv[i], "-L", 2) == 0) {
				if (argv[i][2])
					library_paths[num_library_paths++] = &argv[i][2];
				else if (++i < argc)
					library_paths[num_library_paths++] = argv[i];
			} else if (strncmp(argv[i], "-l", 2) == 0) {
				if (argv[i][2])
					libraries[num_libraries++] = &argv[i][2];
				else if (++i < argc)
					libraries[num_libraries++] = argv[i];
			} else if (strcmp(argv[i], "--emit-c") == 0) {
				emit_c = 1;
			} else if (strcmp(argv[i], "--help") == 0) {
				usage();
			} else {
				error("unknown option: %s", argv[i]);
			}
		} else {
			/* Input file */
			input_files[num_inputs++] = argv[i];
		}
	}

	/* Check for input files */
	if (num_inputs == 0)
		error("no input files");

	/* Process each input file */
	for (i = 0; i < num_inputs; i++) {
		char *objfile = process_file(input_files[i]);
		if (objfile)
			object_files[num_objects++] = objfile;
	}

	/* Link if needed */
	if (!compile_only && !assemble_only && num_objects > 0) {
		final_output = output_file ? output_file : "a.out";
		ret = link_files(object_files, num_objects, final_output);
		if (ret != 0)
			error("linking failed");
	}

	/* Cleanup */
	cleanup();

	return 0;
}
