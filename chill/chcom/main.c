/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Main entry point for CHILL compiler (chcom)
 * CCITT High Level Language (Z.200) - Pass 1
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include "pass1.h"

/* External parser function */
extern int yyparse(void);
extern FILE *yyin;

/* Global variables */
int lineno = 1;
int current_column = 1;
char *current_file = "<stdin>";
FILE *outfile = NULL;
int blevel = 0;
int nerrors = 0;
int nwarnings = 0;
int verbose = 0;   /* Global verbose flag */
char *module_name = NULL; /* Current module name */

/* Command-line options */
static int dump_ast = 0;
static int dump_symtab = 0;
static int max_errors = 20;
static char *output_filename = NULL;

/* Forward declarations */
extern void init_modes(void);

/*
 * Print usage information
 */
static void
usage(void)
{
	fprintf(stderr,
	    "Usage: chcom [options] [file]\n"
	    "Options:\n"
	    "  -o file       Write output to file\n"
	    "  -W<warning>   Enable warning (all, unused, strict, etc.)\n"
	    "  -w            Suppress all warnings\n"
	    "  -ferror-limit=N  Set maximum errors before abort (default: 20)\n"
	    "  -v            Verbose output\n"
	    "  --dump-ast    Dump abstract syntax tree\n"
	    "  --dump-symtab Dump symbol table\n"
	    "  -h, --help    Show this help\n");
	exit(1);
}

/*
 * Parse command-line arguments
 */
static void
parse_args(int argc, char **argv)
{
	int i;

	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			/* Option */
			if (strcmp(argv[i], "-o") == 0) {
				/* Output file */
				if (++i >= argc) {
					fprintf(stderr, "error: -o requires argument\n");
					usage();
				}
				output_filename = argv[i];
				outfile = fopen(argv[i], "w");
				if (outfile == NULL) {
					perror(argv[i]);
					exit(1);
				}
			} else if (strcmp(argv[i], "-w") == 0) {
				/* Suppress all warnings */
				nwarnings = -1;  /* Disable warnings */
			} else if (strncmp(argv[i], "-ferror-limit=", 14) == 0) {
				max_errors = atoi(&argv[i][14]);
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "--dump-ast") == 0) {
				dump_ast = 1;
			} else if (strcmp(argv[i], "--dump-symtab") == 0) {
				dump_symtab = 1;
			} else if (strcmp(argv[i], "-h") == 0 ||
			           strcmp(argv[i], "--help") == 0) {
				usage();
			} else {
				fprintf(stderr, "error: unknown option '%s'\n", argv[i]);
				usage();
			}
		} else {
			/* Input file */
			current_file = argv[i];
			yyin = fopen(current_file, "r");
			if (yyin == NULL) {
				perror(current_file);
				exit(1);
			}
		}
	}

	/* Default output to stdout */
	if (outfile == NULL)
		outfile = stdout;
}

/*
 * Error reporting
 */
void
error(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d:%d: error: ", current_file, lineno, current_column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nerrors++;
	if (nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, aborting.\n");
		exit(1);
	}
}

/*
 * Warning reporting
 */
void
warning(const char *fmt, ...)
{
	va_list ap;

	if (nwarnings < 0)  /* Warnings disabled */
		return;

	fprintf(stderr, "%s:%d:%d: warning: ", current_file, lineno, current_column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nwarnings++;
}

/*
 * Parser error reporting
 */
void
yyerror(const char *s)
{
	error("%s", s);
}

/*
 * Main entry point
 */
int
main(int argc, char **argv)
{
	int ret;

	/* Initialize subsystems */
	symtab_init();
	init_modes();
	init_builtins();

	/* Parse arguments */
	parse_args(argc, argv);

	if (verbose) {
		fprintf(stderr, "PCC CHILL Compiler\n");
		fprintf(stderr, "CCITT High Level Language (Z.200)\n");
		fprintf(stderr, "Input: %s\n", current_file);
	}

	/* Parse input */
	ret = yyparse();

	/* Check for errors */
	if (nerrors > 0) {
		fprintf(stderr, "%d error%s generated.\n",
		    nerrors, nerrors == 1 ? "" : "s");
		return 1;
	}

	if (nwarnings > 0 && verbose) {
		fprintf(stderr, "%d warning%s generated.\n",
		    nwarnings, nwarnings == 1 ? "" : "s");
	}

	if (verbose) {
		fprintf(stderr, "Compilation successful.\n");
	}

	/* Close input file */
	if (yyin != stdin)
		fclose(yyin);

	/* Close output file before code generation */
	if (outfile != stdout && outfile != NULL) {
		fclose(outfile);
		outfile = NULL;
	}

	/* Generate assembly code */
	if (module_name != NULL) {
		codegen_module(output_filename, module_name);
		free(module_name);
		module_name = NULL;
	}

	return ret;
}
