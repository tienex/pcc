/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * ISO Prolog Built-in Predicates
 * ISO/IEC 13211-1:1995 and amendments
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "standards.h"

/* ISO Prolog built-in predicates organized by category */

struct iso_builtin {
	char *name;
	int arity;
	char *category;
	char *description;
	int iso_version;  /* 0=1995, 1=Cor1, 2=Cor2 */
};

static struct iso_builtin iso_builtins[] = {
	/* 8.2 Term unification */
	{"=", 2, "unification", "Unification", 0},
	{"\\=", 2, "unification", "Not unifiable", 0},

	/* 8.3 Type testing */
	{"var", 1, "type_test", "Test if variable", 0},
	{"atom", 1, "type_test", "Test if atom", 0},
	{"integer", 1, "type_test", "Test if integer", 0},
	{"float", 1, "type_test", "Test if float", 0},
	{"atomic", 1, "type_test", "Test if atomic", 0},
	{"compound", 1, "type_test", "Test if compound", 0},
	{"nonvar", 1, "type_test", "Test if not variable", 0},
	{"number", 1, "type_test", "Test if number", 0},

	/* 8.4 Term comparison */
	{"@<", 2, "term_compare", "Term less than", 0},
	{"@=<", 2, "term_compare", "Term less or equal", 0},
	{"@>", 2, "term_compare", "Term greater than", 0},
	{"@>=", 2, "term_compare", "Term greater or equal", 0},
	{"==", 2, "term_compare", "Structural equality", 0},
	{"\\==", 2, "term_compare", "Structural inequality", 0},
	{"compare", 3, "term_compare", "Three-way comparison", 0},

	/* 8.5 Term creation and decomposition */
	{"functor", 3, "term_decomp", "Get functor and arity", 0},
	{"arg", 3, "term_decomp", "Get argument", 0},
	{"=..", 2, "term_decomp", "Univ - term to list", 0},
	{"copy_term", 2, "term_decomp", "Copy term", 0},
	{"term_variables", 2, "term_decomp", "Get variables in term", 0},

	/* 8.6 Arithmetic evaluation */
	{"is", 2, "arithmetic", "Arithmetic evaluation", 0},

	/* 8.7 Arithmetic comparison */
	{"=:=", 2, "arith_compare", "Arithmetic equal", 0},
	{"=\\=", 2, "arith_compare", "Arithmetic not equal", 0},
	{"<", 2, "arith_compare", "Arithmetic less than", 0},
	{"=<", 2, "arith_compare", "Arithmetic less or equal", 0},
	{">", 2, "arith_compare", "Arithmetic greater than", 0},
	{">=", 2, "arith_compare", "Arithmetic greater or equal", 0},

	/* 8.8 Clause retrieval and information */
	{"clause", 2, "database", "Retrieve clause", 0},
	{"current_predicate", 1, "database", "Check predicate exists", 0},

	/* 8.9 Clause creation and destruction */
	{"asserta", 1, "database", "Assert at beginning", 0},
	{"assertz", 1, "database", "Assert at end", 0},
	{"retract", 1, "database", "Retract clause", 0},
	{"abolish", 1, "database", "Remove all clauses", 0},

	/* 8.10 All solutions */
	{"findall", 3, "meta", "Find all solutions", 0},
	{"bagof", 3, "meta", "Collect solutions with duplicates", 0},
	{"setof", 3, "meta", "Collect sorted unique solutions", 0},

	/* 8.11 Stream selection and control */
	{"current_input", 1, "io_stream", "Get current input stream", 0},
	{"current_output", 1, "io_stream", "Get current output stream", 0},
	{"set_input", 1, "io_stream", "Set current input stream", 0},
	{"set_output", 1, "io_stream", "Set current output stream", 0},
	{"open", 3, "io_stream", "Open file stream", 0},
	{"open", 4, "io_stream", "Open file stream with options", 0},
	{"close", 1, "io_stream", "Close stream", 0},
	{"close", 2, "io_stream", "Close stream with options", 0},
	{"flush_output", 0, "io_stream", "Flush output", 0},
	{"flush_output", 1, "io_stream", "Flush output stream", 0},
	{"stream_property", 2, "io_stream", "Stream properties", 0},
	{"at_end_of_stream", 0, "io_stream", "Test end of current stream", 0},
	{"at_end_of_stream", 1, "io_stream", "Test end of stream", 0},
	{"set_stream_position", 2, "io_stream", "Set stream position", 0},

	/* 8.12 Character input/output */
	{"get_char", 1, "io_char", "Read character", 0},
	{"get_char", 2, "io_char", "Read character from stream", 0},
	{"get_code", 1, "io_char", "Read character code", 0},
	{"get_code", 2, "io_char", "Read character code from stream", 0},
	{"peek_char", 1, "io_char", "Peek at character", 0},
	{"peek_char", 2, "io_char", "Peek at character in stream", 0},
	{"peek_code", 1, "io_char", "Peek at character code", 0},
	{"peek_code", 2, "io_char", "Peek at character code in stream", 0},
	{"put_char", 1, "io_char", "Write character", 0},
	{"put_char", 2, "io_char", "Write character to stream", 0},
	{"put_code", 1, "io_char", "Write character code", 0},
	{"put_code", 2, "io_char", "Write character code to stream", 0},
	{"nl", 0, "io_char", "Write newline", 0},
	{"nl", 1, "io_char", "Write newline to stream", 0},

	/* 8.13 Byte input/output */
	{"get_byte", 1, "io_byte", "Read byte", 0},
	{"get_byte", 2, "io_byte", "Read byte from stream", 0},
	{"peek_byte", 1, "io_byte", "Peek at byte", 0},
	{"peek_byte", 2, "io_byte", "Peek at byte in stream", 0},
	{"put_byte", 1, "io_byte", "Write byte", 0},
	{"put_byte", 2, "io_byte", "Write byte to stream", 0},

	/* 8.14 Term input/output */
	{"read", 1, "io_term", "Read term", 0},
	{"read", 2, "io_term", "Read term from stream", 0},
	{"read_term", 2, "io_term", "Read term with options", 0},
	{"read_term", 3, "io_term", "Read term from stream with options", 0},
	{"write", 1, "io_term", "Write term", 0},
	{"write", 2, "io_term", "Write term to stream", 0},
	{"writeq", 1, "io_term", "Write term quoted", 0},
	{"writeq", 2, "io_term", "Write term quoted to stream", 0},
	{"write_term", 2, "io_term", "Write term with options", 0},
	{"write_term", 3, "io_term", "Write term to stream with options", 0},
	{"write_canonical", 1, "io_term", "Write term in canonical form", 0},
	{"write_canonical", 2, "io_term", "Write term in canonical form to stream", 0},
	{"op", 3, "io_term", "Define operator", 0},
	{"current_op", 3, "io_term", "Query operator", 0},
	{"char_conversion", 2, "io_term", "Define character conversion", 0},
	{"current_char_conversion", 2, "io_term", "Query character conversion", 0},

	/* 8.15 Logic and control */
	{"\\+", 1, "control", "Negation by failure", 0},
	{"once", 1, "control", "Execute once", 0},
	{"repeat", 0, "control", "Infinite backtracking", 0},

	/* 8.16 Atomic term processing */
	{"atom_length", 2, "atom", "Get atom length", 0},
	{"atom_concat", 3, "atom", "Concatenate atoms", 0},
	{"sub_atom", 5, "atom", "Extract sub-atom", 0},
	{"atom_chars", 2, "atom", "Convert atom to character list", 0},
	{"atom_codes", 2, "atom", "Convert atom to code list", 0},
	{"char_code", 2, "atom", "Convert character to code", 0},
	{"number_chars", 2, "atom", "Convert number to character list", 0},
	{"number_codes", 2, "atom", "Convert number to code list", 0},

	/* 8.17 Implementation defined hooks */
	{"set_prolog_flag", 2, "flags", "Set Prolog flag", 0},
	{"current_prolog_flag", 2, "flags", "Query Prolog flag", 0},
	{"halt", 0, "control", "Exit program", 0},
	{"halt", 1, "control", "Exit program with code", 0},

	/* Arithmetic functions (9.1-9.4) */
	{"+", 1, "arith_func", "Unary plus", 0},
	{"-", 1, "arith_func", "Unary minus", 0},
	{"+", 2, "arith_func", "Addition", 0},
	{"-", 2, "arith_func", "Subtraction", 0},
	{"*", 2, "arith_func", "Multiplication", 0},
	{"//", 2, "arith_func", "Integer division", 0},
	{"/", 2, "arith_func", "Division", 0},
	{"rem", 2, "arith_func", "Remainder", 0},
	{"mod", 2, "arith_func", "Modulo", 0},
	{"abs", 1, "arith_func", "Absolute value", 0},
	{"sign", 1, "arith_func", "Sign", 0},
	{"float_integer_part", 1, "arith_func", "Integer part of float", 0},
	{"float_fractional_part", 1, "arith_func", "Fractional part", 0},
	{"float", 1, "arith_func", "Convert to float", 0},
	{"floor", 1, "arith_func", "Floor", 0},
	{"truncate", 1, "arith_func", "Truncate", 0},
	{"round", 1, "arith_func", "Round", 0},
	{"ceiling", 1, "arith_func", "Ceiling", 0},
	{"**", 2, "arith_func", "Power", 0},
	{"sin", 1, "arith_func", "Sine", 0},
	{"cos", 1, "arith_func", "Cosine", 0},
	{"atan", 1, "arith_func", "Arctangent", 0},
	{"exp", 1, "arith_func", "Exponential", 0},
	{"log", 1, "arith_func", "Natural logarithm", 0},
	{"sqrt", 1, "arith_func", "Square root", 0},

	/* Bitwise functions */
	{">>", 2, "arith_func", "Shift right", 0},
	{"<<", 2, "arith_func", "Shift left", 0},
	{"/\\", 2, "arith_func", "Bitwise and", 0},
	{"\\/", 2, "arith_func", "Bitwise or", 0},
	{"\\", 1, "arith_func", "Bitwise complement", 0},

	/* ISO Corrigendum 1 additions */
	{"subsumes_term", 2, "term_compare", "Subsumption test", 1},
	{"acyclic_term", 1, "type_test", "Test if acyclic", 1},

	/* ISO Corrigendum 2 additions */
	{"ground", 1, "type_test", "Test if ground (no variables)", 2},

	/* Terminator */
	{NULL, 0, NULL, NULL, 0}
};

/* Initialize ISO built-in predicates */
void init_iso_builtins(void) {
	int i;
	int current_iso_level = 0;

	/* Determine which ISO level to support */
	if (standard_features.iso_cor2_2012)
		current_iso_level = 2;
	else if (standard_features.iso_cor1_2007)
		current_iso_level = 1;
	else if (standard_features.iso_builtins)
		current_iso_level = 0;
	else
		return; /* No ISO support */

	/* Register all ISO built-ins up to current level */
	for (i = 0; iso_builtins[i].name != NULL; i++) {
		if (iso_builtins[i].iso_version <= current_iso_level) {
			struct predicate *pred = define_predicate(
				iso_builtins[i].name,
				iso_builtins[i].arity
			);
			pred->is_builtin = 1;
		}
	}
}

/* Print ISO built-in categories */
void print_iso_categories(FILE *fp) {
	fprintf(fp, "ISO Prolog Built-in Predicates by Category:\n\n");

	fprintf(fp, "8.2 Term Unification\n");
	fprintf(fp, "8.3 Type Testing\n");
	fprintf(fp, "8.4 Term Comparison\n");
	fprintf(fp, "8.5 Term Creation and Decomposition\n");
	fprintf(fp, "8.6 Arithmetic Evaluation\n");
	fprintf(fp, "8.7 Arithmetic Comparison\n");
	fprintf(fp, "8.8 Clause Retrieval and Information\n");
	fprintf(fp, "8.9 Clause Creation and Destruction\n");
	fprintf(fp, "8.10 All Solutions\n");
	fprintf(fp, "8.11 Stream Selection and Control\n");
	fprintf(fp, "8.12 Character Input/Output\n");
	fprintf(fp, "8.13 Byte Input/Output\n");
	fprintf(fp, "8.14 Term Input/Output\n");
	fprintf(fp, "8.15 Logic and Control\n");
	fprintf(fp, "8.16 Atomic Term Processing\n");
	fprintf(fp, "8.17 Implementation Defined Hooks\n");
	fprintf(fp, "9.1-9.4 Arithmetic Functions\n");
}
