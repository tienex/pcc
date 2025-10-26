/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart compiler main entry point
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern FILE *yyin;
extern int yyparse(void);
extern Node *ast_root;

char *input_file = NULL;
char *output_file = NULL;
int verbose = 0;
int dump_ast = 0;
int dump_symtab = 0;

static void
usage(void)
{
	fprintf(stderr, "Usage: dcom [options] file.dart\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -o <file>    Output file\n");
	fprintf(stderr, "  -v           Verbose output\n");
	fprintf(stderr, "  -ast         Dump AST\n");
	fprintf(stderr, "  -symtab      Dump symbol table\n");
	fprintf(stderr, "  -h           Show this help\n");
	exit(1);
}

static void
parse_args(int argc, char **argv)
{
	int opt;

	while ((opt = getopt(argc, argv, "o:vh")) != -1) {
		switch (opt) {
		case 'o':
			output_file = optarg;
			break;
		case 'v':
			verbose = 1;
			break;
		case 'h':
			usage();
			break;
		default:
			usage();
		}
	}

	/* Check for special flags */
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-ast") == 0) {
			dump_ast = 1;
		} else if (strcmp(argv[i], "-symtab") == 0) {
			dump_symtab = 1;
		}
	}

	if (optind >= argc) {
		fprintf(stderr, "Error: no input file\n");
		usage();
	}

	input_file = argv[optind];
}

int
main(int argc, char **argv)
{
	parse_args(argc, argv);

	if (verbose) {
		printf("Compiling %s\n", input_file);
	}

	/* Open input file */
	yyin = fopen(input_file, "r");
	if (!yyin) {
		fprintf(stderr, "Error: cannot open %s\n", input_file);
		return 1;
	}

	/* Initialize symbol table */
	symtab_init();

	/* Parse input */
	if (yyparse() != 0) {
		fprintf(stderr, "Parse failed\n");
		fclose(yyin);
		return 1;
	}

	fclose(yyin);

	/* Check for errors */
	if (dart_get_error_count() > 0) {
		fprintf(stderr, "%d error(s) found\n", dart_get_error_count());
		return 1;
	}

	if (verbose || dump_ast) {
		if (ast_root) {
			print_ast(ast_root);
		} else {
			printf("No AST generated\n");
		}
	}

	if (verbose || dump_symtab) {
		symtab_dump();
	}

	if (verbose) {
		printf("Compilation successful\n");
	}

	/* Generate code */
	if (ast_root) {
		FILE *out = stdout;
		if (output_file) {
			out = fopen(output_file, "w");
			if (!out) {
				fprintf(stderr, "Error: cannot open output file %s\n", output_file);
				return 1;
			}
		}
		set_output_file(out);
		generate_code(ast_root);
		if (output_file && out != stdout) {
			fclose(out);
		}
	}

	/* Clean up */
	if (ast_root) {
		free_node(ast_root);
	}

	return 0;
}
