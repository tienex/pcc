/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Driver program for PL/I compiler
 * Handles compilation pipeline for PL/I, PL/M, and related dialects
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
#define TYPE_PLI            1   /* .pli, .pl1, .plm */
#define TYPE_ASSEMBLY       2   /* .s */
#define TYPE_OBJECT         3   /* .o */

/* Program paths */
static char *plicom = LIBEXECDIR "/plicom";  /* PL/I compiler */
static char *assembler = "as";               /* Assembler */
static char *linker = "ld";                  /* Linker */

/* Options */
static int verbose = 0;
static int compile_only = 0;   /* -c: compile to .o but don't link */
static int assemble_only = 0;  /* -S: compile to .s but don't assemble */
static char *output_file = NULL;
static char *dialect = "pli";
static int optimization_level = 0;

/* Temporary files */
#define MAX_TEMPS 100
static char *temp_files[MAX_TEMPS];
static int num_temps = 0;

/* Input files */
#define MAX_FILES 100
static char *input_files[MAX_FILES];
static int num_inputs = 0;

/* Libraries */
static char *libraries[MAX_FILES];
static int num_libraries = 0;
static char *library_paths[MAX_FILES];
static int num_library_paths = 0;

/* Add temporary file for cleanup */
static void add_temp(const char *file) {
	if (num_temps < MAX_TEMPS)
		temp_files[num_temps++] = strdup(file);
}

/* Cleanup temporary files */
static void cleanup(void) {
	for (int i = 0; i < num_temps; i++) {
		if (verbose)
			printf("rm %s\n", temp_files[i]);
		unlink(temp_files[i]);
		free(temp_files[i]);
	}
}

/* Error message and exit */
static void error(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "pli: error: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	cleanup();
	exit(1);
}

/* Warning message */
static void warning(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "pli: warning: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

/* Detect file type from extension */
static int file_type(const char *filename) {
	const char *ext = strrchr(filename, '.');

	if (ext == NULL)
		return TYPE_UNKNOWN;

	if (strcmp(ext, ".pli") == 0 || strcmp(ext, ".pl1") == 0 ||
	    strcmp(ext, ".plm") == 0 || strcmp(ext, ".PLI") == 0 ||
	    strcmp(ext, ".PL1") == 0 || strcmp(ext, ".PLM") == 0)
		return TYPE_PLI;

	if (strcmp(ext, ".s") == 0 || strcmp(ext, ".S") == 0)
		return TYPE_ASSEMBLY;

	if (strcmp(ext, ".o") == 0)
		return TYPE_OBJECT;

	return TYPE_UNKNOWN;
}

/* Generate output filename */
static char *gen_outfile(const char *infile, const char *suffix) {
	char tmp[1024];
	char *out;
	char *base;
	char *dot;

	strcpy(tmp, infile);
	base = basename(tmp);
	dot = strrchr(base, '.');
	if (dot) *dot = '\0';

	out = malloc(strlen(base) + strlen(suffix) + 1);
	sprintf(out, "%s%s", base, suffix);
	return out;
}

/* Execute command */
static int execute(char **argv) {
	pid_t pid;
	int status;

	if (verbose) {
		for (int i = 0; argv[i]; i++)
			printf("%s ", argv[i]);
		printf("\n");
	}

	pid = fork();
	if (pid < 0) {
		error("fork failed");
	} else if (pid == 0) {
		/* Child process */
		execvp(argv[0], argv);
		error("exec failed for %s", argv[0]);
	}

	/* Parent process */
	waitpid(pid, &status, 0);
	return WIFEXITED(status) ? WEXITSTATUS(status) : 1;
}

/* Compile PL/I source to assembly */
static char *compile_pli(const char *infile) {
	char *outfile;
	char *argv[20];
	int argc = 0;

	if (assemble_only && output_file) {
		outfile = output_file;
	} else {
		outfile = gen_outfile(infile, ".s");
		add_temp(outfile);
	}

	argv[argc++] = plicom;
	argv[argc++] = "-d";
	argv[argc++] = dialect;
	argv[argc++] = "-o";
	argv[argc++] = outfile;
	if (verbose)
		argv[argc++] = "-v";
	argv[argc++] = (char *)infile;
	argv[argc] = NULL;

	if (execute(argv) != 0)
		error("compilation failed for %s", infile);

	return outfile;
}

/* Assemble to object file */
static char *assemble(const char *infile) {
	char *outfile;
	char *argv[20];
	int argc = 0;

	if (compile_only && output_file) {
		outfile = output_file;
	} else {
		outfile = gen_outfile(infile, ".o");
		if (!compile_only)
			add_temp(outfile);
	}

	argv[argc++] = assembler;
	argv[argc++] = "-o";
	argv[argc++] = outfile;
	argv[argc++] = (char *)infile;
	argv[argc] = NULL;

	if (execute(argv) != 0)
		error("assembly failed for %s", infile);

	return outfile;
}

/* Link object files */
static void link_files(char **objfiles, int num_objs) {
	char *outfile = output_file ? output_file : "a.out";
	char *argv[MAX_FILES + 20];
	int argc = 0;

	argv[argc++] = linker;
	argv[argc++] = "-o";
	argv[argc++] = outfile;

	for (int i = 0; i < num_objs; i++)
		argv[argc++] = objfiles[i];

	for (int i = 0; i < num_library_paths; i++) {
		argv[argc++] = "-L";
		argv[argc++] = library_paths[i];
	}

	for (int i = 0; i < num_libraries; i++) {
		argv[argc++] = "-l";
		argv[argc++] = libraries[i];
	}

	argv[argc] = NULL;

	if (execute(argv) != 0)
		error("linking failed");
}

/* Process one input file */
static char *process_file(const char *infile) {
	int type = file_type(infile);
	char *current = (char *)infile;

	switch (type) {
	case TYPE_PLI:
		current = compile_pli(current);
		if (assemble_only)
			return NULL;
		current = assemble(current);
		if (compile_only)
			return NULL;
		return current;

	case TYPE_ASSEMBLY:
		if (assemble_only)
			return NULL;
		current = assemble(current);
		if (compile_only)
			return NULL;
		return current;

	case TYPE_OBJECT:
		if (assemble_only || compile_only)
			return NULL;
		return current;

	default:
		error("unknown file type: %s", infile);
		return NULL;
	}
}

/* Usage */
static void usage(void) {
	printf("Usage: pli [options] file...\n");
	printf("Options:\n");
	printf("  -c           Compile and assemble, but do not link\n");
	printf("  -S           Compile only, do not assemble or link\n");
	printf("  -o <file>    Place output in <file>\n");
	printf("  -d <dialect> Set dialect (pli, plm, plm86, plm386, plc)\n");
	printf("  -v           Verbose mode\n");
	printf("  -l<library>  Link with library\n");
	printf("  -L<path>     Add library search path\n");
	printf("  -O<level>    Set optimization level\n");
	printf("  -h           Show this help\n");
	exit(0);
}

/* Main */
int main(int argc, char **argv) {
	char *objfiles[MAX_FILES];
	int num_objs = 0;
	int opt;

	/* Parse arguments */
	while ((opt = getopt(argc, argv, "cSo:d:vl:L:O:h")) != -1) {
		switch (opt) {
		case 'c':
			compile_only = 1;
			break;
		case 'S':
			assemble_only = 1;
			break;
		case 'o':
			output_file = optarg;
			break;
		case 'd':
			dialect = optarg;
			break;
		case 'v':
			verbose = 1;
			break;
		case 'l':
			libraries[num_libraries++] = optarg;
			break;
		case 'L':
			library_paths[num_library_paths++] = optarg;
			break;
		case 'O':
			optimization_level = atoi(optarg);
			break;
		case 'h':
			usage();
			break;
		default:
			usage();
		}
	}

	/* Get input files */
	for (int i = optind; i < argc; i++) {
		input_files[num_inputs++] = argv[i];
	}

	if (num_inputs == 0) {
		fprintf(stderr, "pli: no input files\n");
		usage();
	}

	/* Set up cleanup on exit */
	atexit(cleanup);

	/* Process each file */
	for (int i = 0; i < num_inputs; i++) {
		char *obj = process_file(input_files[i]);
		if (obj)
			objfiles[num_objs++] = obj;
	}

	/* Link if not stopped earlier */
	if (!compile_only && !assemble_only && num_objs > 0) {
		link_files(objfiles, num_objs);
	}

	return 0;
}
