/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Prolog compiler driver
 * Compatible with Turbo Prolog and GNU Prolog
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define VERSION "1.0.0"

/* Paths to compiler components */
#ifndef PLOGCOM
#define PLOGCOM "/usr/local/lib/pcc/plogcom"
#endif

#ifndef CC
#define CC "cc"
#endif

/* Print usage */
static void usage(void) {
	fprintf(stderr, "PCC Prolog Compiler v%s\n", VERSION);
	fprintf(stderr, "Usage: prolog [options] <file.pl>\n");
	fprintf(stderr, "\nOptions:\n");
	fprintf(stderr, "  -o <file>       Output file\n");
	fprintf(stderr, "  -c              Compile only (don't link)\n");
	fprintf(stderr, "  -t              Turbo Prolog mode\n");
	fprintf(stderr, "  -g              GNU Prolog mode (default)\n");
	fprintf(stderr, "  -O<n>           Optimization level\n");
	fprintf(stderr, "  -v              Verbose\n");
	fprintf(stderr, "  -h              Help\n");
	exit(1);
}

int main(int argc, char **argv) {
	char *input_file = NULL;
	char *output_file = NULL;
	char *c_file = NULL;
	int compile_only = 0;
	int turbo_mode = 0;
	int verbose = 0;
	int opt_level = 1;
	int i;

	/* Parse arguments */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 'o':
				if (i + 1 < argc)
					output_file = argv[++i];
				else {
					fprintf(stderr, "Error: -o requires argument\n");
					exit(1);
				}
				break;
			case 'c':
				compile_only = 1;
				break;
			case 't':
				turbo_mode = 1;
				break;
			case 'g':
				turbo_mode = 0;
				break;
			case 'O':
				opt_level = atoi(&argv[i][2]);
				break;
			case 'v':
				verbose = 1;
				break;
			case 'h':
				usage();
				break;
			default:
				fprintf(stderr, "Unknown option: %s\n", argv[i]);
				usage();
			}
		} else {
			input_file = argv[i];
		}
	}

	if (!input_file) {
		fprintf(stderr, "Error: No input file\n");
		usage();
	}

	/* Generate temporary C file name */
	c_file = malloc(strlen(input_file) + 10);
	sprintf(c_file, "%s.c", input_file);

	/* If no output file specified, use input file base name */
	if (!output_file) {
		if (compile_only) {
			output_file = malloc(strlen(input_file) + 10);
			sprintf(output_file, "%s.o", input_file);
		} else {
			output_file = "a.out";
		}
	}

	/* Build plogcom command */
	char cmd[4096];
	snprintf(cmd, sizeof(cmd), "%s %s %s -o %s %s",
	         PLOGCOM,
	         turbo_mode ? "-t" : "-g",
	         verbose ? "-v" : "",
	         c_file,
	         input_file);

	if (verbose)
		printf("Running: %s\n", cmd);

	/* Run plogcom */
	int status = system(cmd);
	if (status != 0) {
		fprintf(stderr, "Compilation failed\n");
		exit(1);
	}

	/* Compile C code */
	snprintf(cmd, sizeof(cmd), "%s -O%d %s %s -o %s",
	         CC,
	         opt_level,
	         compile_only ? "-c" : "",
	         c_file,
	         output_file);

	if (verbose)
		printf("Running: %s\n", cmd);

	status = system(cmd);
	if (status != 0) {
		fprintf(stderr, "C compilation failed\n");
		exit(1);
	}

	/* Clean up temporary C file if not compiling only */
	if (!compile_only && !verbose) {
		unlink(c_file);
	}

	free(c_file);

	return 0;
}
