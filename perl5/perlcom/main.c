/*	$Id$	*/
/*
 * Perl 5 Compiler - Main Implementation
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* External functions from parser and lexer */
extern int yyparse(void);
extern FILE *yyin;

/* Symbol table */
static struct perl_symtab *symtab[256];
int perl_scope_level = 0;

/* Options */
static int verbose = 0;
static int optimize = 0;
static char *output_file = NULL;
static FILE *output_fp = NULL;

/* Forward declarations */
static unsigned int hash(const char *str);
static void usage(void);

/*
 * Symbol table management
 */
static unsigned int hash(const char *str) {
	unsigned int h = 0;
	while (*str) {
		h = h * 31 + *str++;
	}
	return h % 256;
}

struct perl_symtab *perl_lookup(char *name, int vartype) {
	unsigned int h = hash(name);
	struct perl_symtab *sym;

	for (sym = symtab[h]; sym != NULL; sym = sym->next) {
		if (strcmp(sym->name, name) == 0 && sym->vartype == vartype) {
			return sym;
		}
	}
	return NULL;
}

struct perl_symtab *perl_install(char *name, int vartype, int storage) {
	unsigned int h = hash(name);
	struct perl_symtab *sym;

	sym = malloc(sizeof(struct perl_symtab));
	if (sym == NULL) {
		perl_fatal("Out of memory");
	}

	sym->name = strdup(name);
	sym->vartype = vartype;
	sym->storage = storage;
	sym->scope = perl_scope_level;
	sym->value = NULL;
	sym->next = symtab[h];
	symtab[h] = sym;

	return sym;
}

void perl_enter_scope(void) {
	perl_scope_level++;
}

void perl_leave_scope(void) {
	int i;
	struct perl_symtab *sym, *prev, *next;

	/* Remove symbols from current scope */
	for (i = 0; i < 256; i++) {
		prev = NULL;
		for (sym = symtab[i]; sym != NULL; sym = next) {
			next = sym->next;
			if (sym->scope == perl_scope_level) {
				if (prev == NULL) {
					symtab[i] = next;
				} else {
					prev->next = next;
				}
				free(sym->name);
				free(sym);
			} else {
				prev = sym;
			}
		}
	}

	perl_scope_level--;
}

/*
 * Tree building functions
 */
void *perl_scalar_node(char *name) {
	struct perl_symtab *sym;

	sym = perl_lookup(name, P_SCALAR);
	if (sym == NULL && verbose) {
		perl_warning("Undefined scalar variable: $%s", name);
		/* Auto-vivify in Perl style */
		sym = perl_install(name, P_SCALAR, P_AUTO);
	}

	/* Create node - would normally create an AST node here */
	/* For now, just return a placeholder */
	return (void *)sym;
}

void *perl_array_node(char *name) {
	struct perl_symtab *sym;

	sym = perl_lookup(name, P_ARRAY);
	if (sym == NULL && verbose) {
		perl_warning("Undefined array variable: @%s", name);
		sym = perl_install(name, P_ARRAY, P_AUTO);
	}

	return (void *)sym;
}

void *perl_hash_node(char *name) {
	struct perl_symtab *sym;

	sym = perl_lookup(name, P_HASH);
	if (sym == NULL && verbose) {
		perl_warning("Undefined hash variable: %%%s", name);
		sym = perl_install(name, P_HASH, P_AUTO);
	}

	return (void *)sym;
}

void *perl_binop_node(int op, void *left, void *right) {
	/* Create binary operation node */
	/* This would interface with the PCC tree building system */
	if (verbose) {
		fprintf(stderr, "Binary op: %c\n", op);
	}
	return NULL;
}

void *perl_unop_node(int op, void *operand) {
	/* Create unary operation node */
	if (verbose) {
		fprintf(stderr, "Unary op: %d\n", op);
	}
	return NULL;
}

void *perl_const_node(int type, void *value) {
	/* Create constant node */
	if (verbose) {
		fprintf(stderr, "Constant of type: %d\n", type);
	}
	return NULL;
}

/*
 * Code generation functions
 */
void perl_gen_assign(void *lval, void *rval) {
	/* Generate assignment code */
	if (output_fp) {
		fprintf(output_fp, "; Assignment\n");
	}
}

void perl_gen_sub_call(char *name, void *args) {
	/* Generate subroutine call */
	if (output_fp) {
		fprintf(output_fp, "; Call subroutine: %s\n", name);
	}
}

void perl_gen_if(void *cond, void *then_block, void *else_block) {
	/* Generate if statement */
	if (output_fp) {
		fprintf(output_fp, "; If statement\n");
	}
}

void perl_gen_while(void *cond, void *block) {
	/* Generate while loop */
	if (output_fp) {
		fprintf(output_fp, "; While loop\n");
	}
}

void perl_gen_for(void *init, void *cond, void *incr, void *block) {
	/* Generate for loop */
	if (output_fp) {
		fprintf(output_fp, "; For loop\n");
	}
}

void perl_gen_foreach(void *var, void *list, void *block) {
	/* Generate foreach loop */
	if (output_fp) {
		fprintf(output_fp, "; Foreach loop\n");
	}
}

/*
 * Built-in function handlers
 */
void perl_builtin_print(void *args) {
	if (output_fp) {
		fprintf(output_fp, "; Print statement\n");
	}
}

void perl_builtin_push(void *array, void *values) {
	if (output_fp) {
		fprintf(output_fp, "; Push to array\n");
	}
}

void perl_builtin_pop(void *array) {
	if (output_fp) {
		fprintf(output_fp, "; Pop from array\n");
	}
}

void perl_builtin_shift(void *array) {
	if (output_fp) {
		fprintf(output_fp, "; Shift from array\n");
	}
}

void perl_builtin_unshift(void *array, void *values) {
	if (output_fp) {
		fprintf(output_fp, "; Unshift to array\n");
	}
}

/*
 * Regular expression support
 */
void *perl_regex_compile(char *pattern, char *flags) {
	if (verbose) {
		fprintf(stderr, "Compiling regex: /%s/%s\n", pattern, flags);
	}
	return NULL;
}

void *perl_regex_match(void *string, void *regex) {
	if (output_fp) {
		fprintf(output_fp, "; Regex match\n");
	}
	return NULL;
}

void *perl_regex_subst(void *string, void *pattern, void *replacement, char *flags) {
	if (output_fp) {
		fprintf(output_fp, "; Regex substitution\n");
	}
	return NULL;
}

/*
 * Error handling
 */
void perl_error(char *fmt, ...) {
	va_list ap;

	fprintf(stderr, "Error at %s:%d: ", perl_filename, perl_lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	perl_nerrors++;
}

void perl_warning(char *fmt, ...) {
	va_list ap;

	fprintf(stderr, "Warning at %s:%d: ", perl_filename, perl_lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

void perl_fatal(char *fmt, ...) {
	va_list ap;

	fprintf(stderr, "Fatal error: ");
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	exit(1);
}

/*
 * Initialization and cleanup
 */
void perl_init(void) {
	int i;

	/* Initialize symbol table */
	for (i = 0; i < 256; i++) {
		symtab[i] = NULL;
	}

	perl_scope_level = 0;
	perl_nerrors = 0;

	/* Install built-in variables */
	perl_install("_", P_SCALAR, P_GLOBAL);
	perl_install("@", P_SCALAR, P_GLOBAL);
	perl_install("!", P_SCALAR, P_GLOBAL);
	perl_install("?", P_SCALAR, P_GLOBAL);
	perl_install("$", P_SCALAR, P_GLOBAL);
	perl_install("0", P_SCALAR, P_GLOBAL);

	/* Install ARGV and ENV */
	perl_install("ARGV", P_ARRAY, P_GLOBAL);
	perl_install("ENV", P_HASH, P_GLOBAL);
	perl_install("INC", P_ARRAY, P_GLOBAL);
}

void perl_finish(void) {
	int i;
	struct perl_symtab *sym, *next;

	/* Free symbol table */
	for (i = 0; i < 256; i++) {
		for (sym = symtab[i]; sym != NULL; sym = next) {
			next = sym->next;
			free(sym->name);
			free(sym);
		}
		symtab[i] = NULL;
	}
}

/*
 * Usage
 */
static void usage(void) {
	fprintf(stderr, "Usage: perlcom [options] file.pl\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -o file   Write output to file\n");
	fprintf(stderr, "  -v        Verbose mode\n");
	fprintf(stderr, "  -O        Enable optimizations\n");
	exit(1);
}

/*
 * Main entry point
 */
int main(int argc, char **argv) {
	int i;
	char *input_file = NULL;

	/* Parse command line */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 'o':
				if (i + 1 >= argc) {
					usage();
				}
				output_file = argv[++i];
				break;
			case 'v':
				verbose = 1;
				break;
			case 'O':
				optimize = 1;
				break;
			default:
				usage();
			}
		} else {
			if (input_file != NULL) {
				fprintf(stderr, "Error: Multiple input files specified\n");
				usage();
			}
			input_file = argv[i];
		}
	}

	if (input_file == NULL) {
		yyin = stdin;
		perl_filename = "<stdin>";
	} else {
		yyin = fopen(input_file, "r");
		if (yyin == NULL) {
			perror(input_file);
			exit(1);
		}
		perl_filename = input_file;
	}

	if (output_file != NULL) {
		output_fp = fopen(output_file, "w");
		if (output_fp == NULL) {
			perror(output_file);
			exit(1);
		}
	} else {
		output_fp = stdout;
	}

	/* Initialize compiler */
	perl_init();

	if (verbose) {
		fprintf(stderr, "Compiling %s...\n", perl_filename);
	}

	/* Parse input */
	yyparse();

	if (perl_nerrors > 0) {
		fprintf(stderr, "%d error%s found\n",
			perl_nerrors, perl_nerrors == 1 ? "" : "s");
		exit(1);
	}

	if (verbose) {
		fprintf(stderr, "Compilation successful\n");
	}

	/* Cleanup */
	perl_finish();

	if (yyin != stdin) {
		fclose(yyin);
	}
	if (output_fp != stdout) {
		fclose(output_fp);
	}

	return 0;
}
