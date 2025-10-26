/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Main entry point for Prolog compiler
 * Compatible with Turbo Prolog and GNU Prolog
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include "pass1.h"

/* Version information */
#define VERSION "1.0.0"
#define PROGNAME "plogcom"

/* Global variables */
int lineno = 1;
int current_column = 1;
struct compiler_options options;
struct predicate *predicate_table[256];
struct clause *clause_list = NULL;
struct module *current_module = NULL;

/* Input/output files */
static FILE *input_file = NULL;
static FILE *output_file = NULL;
static char *input_filename = NULL;
static char *output_filename = NULL;

/* External from lexer */
extern FILE *yyin;
extern int yyparse(void);

/* Print usage information */
static void usage(void) {
	fprintf(stderr, "PCC Prolog Compiler v%s\n", VERSION);
	fprintf(stderr, "Usage: %s [options] <input-file>\n", PROGNAME);
	fprintf(stderr, "\nOptions:\n");
	fprintf(stderr, "  -o <file>       Specify output file\n");
	fprintf(stderr, "  -t              Turbo Prolog compatibility mode\n");
	fprintf(stderr, "  -g              GNU Prolog compatibility mode\n");
	fprintf(stderr, "  -O<level>       Optimization level (0-3)\n");
	fprintf(stderr, "  -W<level>       Warning level (0-3)\n");
	fprintf(stderr, "  -d              Debug mode\n");
	fprintf(stderr, "  -f <format>     Output format:\n");
	fprintf(stderr, "                    c      - C code (default)\n");
	fprintf(stderr, "                    bc     - Bytecode\n");
	fprintf(stderr, "                    wam    - Warren Abstract Machine\n");
	fprintf(stderr, "  -v              Verbose output\n");
	fprintf(stderr, "  -h              Show this help\n");
	fprintf(stderr, "  --version       Show version information\n");
	exit(1);
}

/* Print version information */
static void version(void) {
	fprintf(stderr, "PCC Prolog Compiler v%s\n", VERSION);
	fprintf(stderr, "Compatible with Turbo Prolog and GNU Prolog\n");
	fprintf(stderr, "Copyright (c) 2025 PCC Project\n");
	exit(0);
}

/* Initialize compiler options */
static void init_options(void) {
	memset(&options, 0, sizeof(options));
	options.turbo_mode = 0;
	options.gnu_mode = 1;      /* Default to GNU Prolog mode */
	options.debug = 0;
	options.optimize = 1;
	options.warnings = 1;
	options.output_file = NULL;
	options.output_format = 0; /* Default to C code */
}

/* Parse command line arguments */
static void parse_args(int argc, char **argv) {
	int c;
	int option_index = 0;

	static struct option long_options[] = {
		{"version", no_argument, 0, 'V'},
		{"help", no_argument, 0, 'h'},
		{"turbo", no_argument, 0, 't'},
		{"gnu", no_argument, 0, 'g'},
		{"debug", no_argument, 0, 'd'},
		{"output", required_argument, 0, 'o'},
		{"format", required_argument, 0, 'f'},
		{"optimize", required_argument, 0, 'O'},
		{"warn", required_argument, 0, 'W'},
		{0, 0, 0, 0}
	};

	while ((c = getopt_long(argc, argv, "o:tgdO:W:f:vhV",
	                        long_options, &option_index)) != -1) {
		switch (c) {
		case 'o':
			output_filename = strdup(optarg);
			options.output_file = output_filename;
			break;
		case 't':
			options.turbo_mode = 1;
			options.gnu_mode = 0;
			break;
		case 'g':
			options.gnu_mode = 1;
			options.turbo_mode = 0;
			break;
		case 'd':
			options.debug = 1;
			break;
		case 'O':
			options.optimize = atoi(optarg);
			if (options.optimize < 0 || options.optimize > 3) {
				fprintf(stderr, "Invalid optimization level: %s\n", optarg);
				exit(1);
			}
			break;
		case 'W':
			options.warnings = atoi(optarg);
			if (options.warnings < 0 || options.warnings > 3) {
				fprintf(stderr, "Invalid warning level: %s\n", optarg);
				exit(1);
			}
			break;
		case 'f':
			if (strcmp(optarg, "c") == 0)
				options.output_format = 0;
			else if (strcmp(optarg, "bc") == 0)
				options.output_format = 1;
			else if (strcmp(optarg, "wam") == 0)
				options.output_format = 2;
			else {
				fprintf(stderr, "Invalid output format: %s\n", optarg);
				exit(1);
			}
			break;
		case 'v':
			/* Verbose mode - could add verbosity flag */
			break;
		case 'V':
			version();
			break;
		case 'h':
		default:
			usage();
		}
	}

	/* Get input file */
	if (optind >= argc) {
		fprintf(stderr, "Error: No input file specified\n");
		usage();
	}

	input_filename = argv[optind];
}

/* Open input and output files */
static int open_files(void) {
	/* Open input file */
	if (strcmp(input_filename, "-") == 0) {
		input_file = stdin;
		yyin = stdin;
	} else {
		input_file = fopen(input_filename, "r");
		if (!input_file) {
			fprintf(stderr, "Error: Cannot open input file: %s\n",
			        input_filename);
			return 1;
		}
		yyin = input_file;
	}

	/* Open output file */
	if (output_filename) {
		output_file = fopen(output_filename, "w");
		if (!output_file) {
			fprintf(stderr, "Error: Cannot open output file: %s\n",
			        output_filename);
			if (input_file != stdin)
				fclose(input_file);
			return 1;
		}
	} else {
		output_file = stdout;
	}

	return 0;
}

/* Close files */
static void close_files(void) {
	if (input_file && input_file != stdin)
		fclose(input_file);
	if (output_file && output_file != stdout)
		fclose(output_file);
}

/* Main entry point */
int main(int argc, char **argv) {
	int parse_result;

	/* Initialize options */
	init_options();

	/* Parse command line arguments */
	parse_args(argc, argv);

	/* Open files */
	if (open_files() != 0)
		return 1;

	/* Initialize symbol table */
	init_symtab();

	/* Initialize built-in predicates */
	init_builtins();

	/* Parse input */
	if (options.debug)
		fprintf(stderr, "Parsing %s...\n", input_filename);

	parse_result = yyparse();

	if (parse_result != 0) {
		fprintf(stderr, "Parse failed\n");
		close_files();
		return 1;
	}

	if (options.debug) {
		fprintf(stderr, "Parse successful\n");
		fprintf(stderr, "Dumping symbol table...\n");
		dump_symtab();
	}

	/* Generate code */
	if (options.debug)
		fprintf(stderr, "Generating code...\n");

	switch (options.output_format) {
	case 0: /* C code */
		generate_c_code(output_file);
		break;
	case 1: /* Bytecode */
		generate_bytecode(output_file);
		break;
	case 2: /* WAM */
		generate_wam(output_file);
		break;
	}

	if (options.debug)
		fprintf(stderr, "Code generation complete\n");

	/* Clean up */
	close_files();

	return 0;
}
