/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Frontend
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 */

/*
 * JavaScript Compiler Frontend - Main Driver
 *
 * This is the main entry point for the JavaScript compiler frontend.
 * It compiles JavaScript/TypeScript/CoffeeScript/LiveScript to PCC IR,
 * which can then be compiled to native code via PCC's backends.
 *
 * Usage:
 *   jscom [options] input.js
 *   jscom --target=es2025 --typescript input.ts
 *   jscom --optimize=2 --output=output.c input.js
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include "jspass1.h"

/* Global compiler context */
js_compiler_ctx_t *js_compiler_ctx = NULL;

/* Command line options */
static struct option long_options[] = {
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, 'v'},
	{"output", required_argument, NULL, 'o'},
	{"target", required_argument, NULL, 't'},
	{"optimize", required_argument, NULL, 'O'},
	{"strict", no_argument, NULL, 's'},
	{"module", no_argument, NULL, 'm'},
	{"typescript", no_argument, NULL, 'T'},
	{"es3", no_argument, NULL, '3'},
	{"es5", no_argument, NULL, '5'},
	{"es6", no_argument, NULL, '6'},
	{"es2020", no_argument, NULL, '2'},
	{"es2025", no_argument, NULL, '9'},
	{"verbose", no_argument, NULL, 'V'},
	{"dump-ast", no_argument, NULL, 'D'},
	{"type-check", no_argument, NULL, 'C'},
	{NULL, 0, NULL, 0}
};

static void
usage(void)
{
	printf("JavaScript Compiler Frontend for PCC\n");
	printf("Usage: jscom [options] input.js\n\n");
	printf("Options:\n");
	printf("  -h, --help           Show this help message\n");
	printf("  -v, --version        Show version information\n");
	printf("  -o, --output FILE    Output file (default: stdout)\n");
	printf("  -t, --target TARGET  Target ES version (es3, es5, es6, es2020, es2025)\n");
	printf("  -O, --optimize LEVEL Optimization level (0-3, default: 0)\n");
	printf("  -s, --strict         Enable strict mode\n");
	printf("  -m, --module         Treat as ES6 module\n");
	printf("  -T, --typescript     Enable TypeScript features\n");
	printf("  -V, --verbose        Verbose output\n");
	printf("  -D, --dump-ast       Dump AST (for debugging)\n");
	printf("  -C, --type-check     Enable type checking\n");
	printf("\nES Version Shortcuts:\n");
	printf("  --es3                Target ECMAScript 3\n");
	printf("  --es5                Target ECMAScript 5\n");
	printf("  --es6                Target ECMAScript 2015 (ES6)\n");
	printf("  --es2020             Target ECMAScript 2020\n");
	printf("  --es2025             Target ECMAScript 2025\n");
	printf("\nExamples:\n");
	printf("  jscom input.js                        # Compile to PCC IR\n");
	printf("  jscom --typescript input.ts -o out.c  # TypeScript to C\n");
	printf("  jscom --es2020 --module input.mjs     # ES2020 module\n");
	printf("\nThis frontend compiles JavaScript to PCC IR, which can then target:\n");
	printf("  - x86/x86-64 native code\n");
	printf("  - ARM native code\n");
	printf("  - WebAssembly\n");
	printf("  - C code (via c90 backend)\n");
	printf("  - And 10+ other architectures\n");
}

static void
version(void)
{
	printf("JavaScript Compiler Frontend for PCC version 1.0.0\n");
	printf("Copyright (c) 2025\n");
	printf("\nSupported Features:\n");
	printf("  - ECMAScript 3/5/6/2015-2025\n");
	printf("  - TypeScript annotations and features\n");
	printf("  - ActionScript extensions\n");
	printf("  - CoffeeScript syntax\n");
	printf("  - LiveScript features\n");
	printf("\nCompiles JavaScript to native code via PCC backends.\n");
}

static int
parse_es_version(const char *target)
{
	if (strcmp(target, "es3") == 0)
		return JS_ES3;
	else if (strcmp(target, "es5") == 0)
		return JS_ES5;
	else if (strcmp(target, "es6") == 0 || strcmp(target, "es2015") == 0)
		return JS_ES6;
	else if (strcmp(target, "es2016") == 0)
		return JS_ES2016;
	else if (strcmp(target, "es2017") == 0)
		return JS_ES2017;
	else if (strcmp(target, "es2018") == 0)
		return JS_ES2018;
	else if (strcmp(target, "es2019") == 0)
		return JS_ES2019;
	else if (strcmp(target, "es2020") == 0)
		return JS_ES2020;
	else if (strcmp(target, "es2021") == 0)
		return JS_ES2021;
	else if (strcmp(target, "es2022") == 0)
		return JS_ES2022;
	else if (strcmp(target, "es2023") == 0)
		return JS_ES2023;
	else if (strcmp(target, "es2024") == 0)
		return JS_ES2024;
	else if (strcmp(target, "es2025") == 0)
		return JS_ES2025;
	else {
		fprintf(stderr, "Unknown ES version: %s\n", target);
		return JS_ES2025; /* Default to latest */
	}
}

static void
dump_ast(js_node_t *node, int indent)
{
	int i;

	if (!node)
		return;

	for (i = 0; i < indent; i++)
		printf("  ");

	printf("Node type: %d", node->type);
	if (node->name)
		printf(", name: %s", node->name);
	printf("\n");

	if (node->left)
		dump_ast(node->left, indent + 1);
	if (node->right)
		dump_ast(node->right, indent + 1);
	if (node->extra)
		dump_ast(node->extra, indent + 1);

	for (i = 0; i < node->n_children; i++)
		dump_ast(node->children[i], indent + 1);
}

int
main(int argc, char **argv)
{
	int c, option_index = 0;
	char *input_file = NULL;
	char *output_file = NULL;
	int lang_features = JS_ES2025; /* Default to latest */
	int optimize_level = 0;
	int strict_mode = 0;
	int is_module = 0;
	int enable_typescript = 0;
	int verbose = 0;
	int dump_ast_flag = 0;
	int type_check = 0;
	FILE *input = NULL;
	FILE *output = NULL;
	js_node_t *ast = NULL;

	/* Parse command line options */
	while ((c = getopt_long(argc, argv, "hvo:t:O:smTVDC356",
	                        long_options, &option_index)) != -1) {
		switch (c) {
		case 'h':
			usage();
			exit(0);
		case 'v':
			version();
			exit(0);
		case 'o':
			output_file = optarg;
			break;
		case 't':
			lang_features = parse_es_version(optarg);
			break;
		case 'O':
			optimize_level = atoi(optarg);
			if (optimize_level < 0 || optimize_level > 3) {
				fprintf(stderr, "Invalid optimization level: %d\n",
				        optimize_level);
				exit(1);
			}
			break;
		case 's':
			strict_mode = 1;
			break;
		case 'm':
			is_module = 1;
			lang_features |= JS_MODULES;
			break;
		case 'T':
			enable_typescript = 1;
			lang_features |= JS_TYPESCRIPT;
			break;
		case 'V':
			verbose = 1;
			break;
		case 'D':
			dump_ast_flag = 1;
			break;
		case 'C':
			type_check = 1;
			break;
		case '3':
			lang_features = JS_ES3;
			break;
		case '5':
			lang_features = JS_ES5;
			break;
		case '6':
			lang_features = JS_ES6;
			break;
		case '2':
			lang_features = JS_ES2020;
			break;
		case '9':
			lang_features = JS_ES2025;
			break;
		default:
			usage();
			exit(1);
		}
	}

	/* Get input file */
	if (optind < argc) {
		input_file = argv[optind];
	} else {
		fprintf(stderr, "Error: No input file specified\n");
		usage();
		exit(1);
	}

	if (verbose) {
		printf("JavaScript Compiler Frontend\n");
		printf("Input file: %s\n", input_file);
		if (output_file)
			printf("Output file: %s\n", output_file);
		printf("Language features: 0x%x\n", lang_features);
		printf("Optimization level: %d\n", optimize_level);
		printf("Strict mode: %s\n", strict_mode ? "yes" : "no");
		printf("Module: %s\n", is_module ? "yes" : "no");
	}

	/* Open input file */
	input = fopen(input_file, "r");
	if (!input) {
		perror(input_file);
		exit(1);
	}

	/* Open output file */
	if (output_file) {
		output = fopen(output_file, "w");
		if (!output) {
			perror(output_file);
			fclose(input);
			exit(1);
		}
	} else {
		output = stdout;
	}

	/* Initialize compiler */
	js_compiler_init();
	js_compiler_ctx->filename = input_file;
	js_compiler_ctx->input = input;
	js_compiler_ctx->output = output;
	js_compiler_ctx->lang_features = lang_features;
	js_compiler_ctx->strict_mode = strict_mode;
	js_compiler_ctx->is_module = is_module;
	js_compiler_ctx->enable_type_checking = type_check;
	js_compiler_ctx->optimize_level = optimize_level;

	if (verbose)
		printf("Parsing JavaScript source...\n");

	/* Parse the input file */
	ast = js_parse_file(input_file);

	if (!ast) {
		fprintf(stderr, "Error: Failed to parse %s\n", input_file);
		fclose(input);
		if (output != stdout)
			fclose(output);
		exit(1);
	}

	js_compiler_ctx->ast_root = ast;

	if (verbose)
		printf("Parsing complete. AST created.\n");

	/* Dump AST if requested */
	if (dump_ast_flag) {
		printf("\n=== Abstract Syntax Tree ===\n");
		dump_ast(ast, 0);
		printf("\n");
	}

	/* Type checking */
	if (type_check || enable_typescript) {
		if (verbose)
			printf("Running type checker...\n");
		js_type_check(ast, js_compiler_ctx->global_scope);
		if (js_compiler_ctx->n_type_errors > 0) {
			fprintf(stderr, "Type checking failed with %d errors\n",
			        js_compiler_ctx->n_type_errors);
			/* Continue anyway for now */
		}
	}

	/* Emit PCC IR */
	if (verbose)
		printf("Generating PCC IR...\n");

	js_emit_program(ast, output);

	if (verbose)
		printf("Code generation complete.\n");

	/* Cleanup */
	if (verbose)
		printf("Cleaning up...\n");

	fclose(input);
	if (output != stdout)
		fclose(output);

	js_compiler_cleanup();

	if (verbose)
		printf("Compilation successful!\n");

	return 0;
}
