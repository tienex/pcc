/*
 * COBOL compiler main entry point
 * Supports OO COBOL with DEC, IBM, HP, and Microsoft dialects
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "pass1.h"

/* Globals */
int lineno = 1;
int nerrors = 0;
int nwarnings = 0;
int dialect = DIALECT_DEFAULT;
int current_division = 0;
FILE *infile;
FILE *outfile;

/* Debug flags */
static int debug_scan = 0;
static int debug_parse = 0;
static int debug_symtab = 0;
static int debug_codegen = 0;

/* External parser interface */
extern int yyparse(void);
extern FILE *yyin;

static void
usage(void)
{
	fprintf(stderr, "Usage: cbolcom [options] input [output]\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -Xibm       IBM COBOL dialect\n");
	fprintf(stderr, "  -Xdec       DEC COBOL dialect\n");
	fprintf(stderr, "  -Xhp        HP COBOL dialect\n");
	fprintf(stderr, "  -Xms        Microsoft COBOL dialect\n");
	fprintf(stderr, "  -g          Generate debug information\n");
	fprintf(stderr, "  -O          Enable optimization\n");
	fprintf(stderr, "  -Wall       Enable all warnings\n");
	fprintf(stderr, "  -Werror     Treat warnings as errors\n");
	fprintf(stderr, "  -Zscan      Debug scanner\n");
	fprintf(stderr, "  -Zparse     Debug parser\n");
	fprintf(stderr, "  -Zsymtab    Debug symbol table\n");
	fprintf(stderr, "  -Zcodegen   Debug code generation\n");
	exit(1);
}

static void
init_compiler(void)
{
	/* Initialize symbol table */
	enter_scope();  /* Global scope */

	/* Apply dialect-specific initializations */
	apply_dialect_semantics();
}

int
main(int argc, char **argv)
{
	int ch;
	char *input_file = NULL;
	char *output_file = NULL;

	infile = stdin;
	outfile = stdout;

	/* Parse command line */
	while ((ch = getopt(argc, argv, "X:Z:o:gOW:")) != -1) {
		switch (ch) {
		case 'X':
			/* Dialect selection */
			if (strcmp(optarg, "ibm") == 0)
				dialect = DIALECT_IBM;
			else if (strcmp(optarg, "dec") == 0)
				dialect = DIALECT_DEC;
			else if (strcmp(optarg, "hp") == 0)
				dialect = DIALECT_HP;
			else if (strcmp(optarg, "ms") == 0)
				dialect = DIALECT_MS;
			else {
				fprintf(stderr, "Unknown dialect: %s\n", optarg);
				usage();
			}
			break;

		case 'Z':
			/* Debug flags */
			if (strcmp(optarg, "scan") == 0)
				debug_scan = 1;
			else if (strcmp(optarg, "parse") == 0)
				debug_parse = 1;
			else if (strcmp(optarg, "symtab") == 0)
				debug_symtab = 1;
			else if (strcmp(optarg, "codegen") == 0)
				debug_codegen = 1;
			break;

		case 'o':
			output_file = optarg;
			break;

		case 'g':
			/* Generate debug info - handled by backend */
			break;

		case 'O':
			/* Optimization - handled by backend */
			break;

		case 'W':
			/* Warning flags */
			break;

		default:
			usage();
		}
	}

	argc -= optind;
	argv += optind;

	/* Get input file */
	if (argc > 0) {
		input_file = argv[0];
		infile = fopen(input_file, "r");
		if (!infile) {
			fprintf(stderr, "Cannot open input file: %s\n", input_file);
			exit(1);
		}
	}

	/* Get output file */
	if (argc > 1) {
		output_file = argv[1];
	}
	if (output_file) {
		outfile = fopen(output_file, "w");
		if (!outfile) {
			fprintf(stderr, "Cannot open output file: %s\n", output_file);
			exit(1);
		}
	}

	/* Initialize compiler */
	set_dialect(dialect);
	init_compiler();

	/* Set up parser input */
	yyin = infile;

	/* Parse the input */
	if (debug_parse)
		fprintf(stderr, "Starting parse...\n");

	gen_prologue();

	if (yyparse() != 0 || nerrors > 0) {
		fprintf(stderr, "Compilation failed with %d error(s)\n", nerrors);
		exit(1);
	}

	gen_epilogue();

	if (debug_parse)
		fprintf(stderr, "Parse complete. Errors: %d, Warnings: %d\n",
		        nerrors, nwarnings);

	/* Cleanup */
	if (infile != stdin)
		fclose(infile);
	if (outfile != stdout)
		fclose(outfile);

	exit(nerrors > 0 ? 1 : 0);
}
