/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Main entry point for DEC MACRO assembler (mcom)
 * Properly integrated with PCC IR system
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "pass1.h"

/* External parser function */
extern int yyparse(void);
extern FILE *yyin;

/* Global variables */
int lineno = 1;
char *ftitle = "<stdin>";

/* Output file (for standalone mode) */
static FILE *output_file = NULL;

/* Command-line options */
static int verbose = 0;
static int dump_symtab_flag = 0;
static int list_output = 0;

/*
 * Print a NODE tree as assembly (for standalone mode)
 * This is a simple printer that converts PCC IR nodes back to assembly
 */
static void
print_node_as_asm(NODE *p, FILE *fp)
{
	if (p == NULL)
		return;

	switch (p->n_op) {
	case ASSIGN:
		/* Assignment: left = right */
		fprintf(fp, "\t");
		/* Determine instruction based on right side */
		if (p->n_right->n_op == ICON) {
			/* MOV immediate */
			fprintf(fp, "MOV\t#%lld,", (long long)getlval(p->n_right));
		} else if (p->n_right->n_op == REG) {
			/* MOV register */
			fprintf(fp, "MOV\tr%d,", p->n_right->n_rval);
		} else if (p->n_right->n_op == NAME) {
			/* MOV symbol */
			fprintf(fp, "MOV\t%s,", p->n_right->n_name);
		} else if (p->n_right->n_op == OREG) {
			/* MOV from offset(reg) */
			if (getlval(p->n_right) != 0)
				fprintf(fp, "MOV\t%lld(r%d),",
				    (long long)getlval(p->n_right), p->n_right->n_rval);
			else
				fprintf(fp, "MOV\t(r%d),", p->n_right->n_rval);
		} else if (p->n_right->n_op == PLUS) {
			/* ADD operation */
			fprintf(fp, "ADD\t");
			if (p->n_right->n_right->n_op == ICON)
				fprintf(fp, "#%lld,", (long long)getlval(p->n_right->n_right));
			else if (p->n_right->n_right->n_op == REG)
				fprintf(fp, "r%d,", p->n_right->n_right->n_rval);
			else
				fprintf(fp, "?,");
		} else if (p->n_right->n_op == MINUS) {
			/* SUB operation */
			fprintf(fp, "SUB\t");
			if (p->n_right->n_right->n_op == ICON)
				fprintf(fp, "#%lld,", (long long)getlval(p->n_right->n_right));
			else if (p->n_right->n_right->n_op == REG)
				fprintf(fp, "r%d,", p->n_right->n_right->n_rval);
			else
				fprintf(fp, "?,");
		} else {
			fprintf(fp, "MOV\t<complex>,");
		}

		/* Print destination */
		if (p->n_left->n_op == REG) {
			fprintf(fp, "r%d\n", p->n_left->n_rval);
		} else if (p->n_left->n_op == NAME) {
			fprintf(fp, "%s\n", p->n_left->n_name);
		} else if (p->n_left->n_op == OREG) {
			if (getlval(p->n_left) != 0)
				fprintf(fp, "%lld(r%d)\n",
				    (long long)getlval(p->n_left), p->n_left->n_rval);
			else
				fprintf(fp, "(r%d)\n", p->n_left->n_rval);
		} else {
			fprintf(fp, "<dest>\n");
		}
		break;

	case EQ:
		/* Comparison - print as TST or CMP */
		fprintf(fp, "\tTST\t");
		if (p->n_left->n_op == REG)
			fprintf(fp, "r%d\n", p->n_left->n_rval);
		else if (p->n_left->n_op == NAME)
			fprintf(fp, "%s\n", p->n_left->n_name);
		else
			fprintf(fp, "<operand>\n");
		break;

	default:
		fprintf(fp, "\t; <unhandled NODE op=%d>\n", p->n_op);
		break;
	}
}

/*
 * Simple pass2_compile implementation for standalone assembler
 * This receives interpass structures from send_passt() and
 * emits the assembly code
 */
void
pass2_compile(struct interpass *ip)
{
	if (output_file == NULL)
		output_file = stdout;

	switch (ip->type) {
	case IP_NODE:
		/* Convert NODE IR to assembly for standalone mode */
		/* In a full PCC integration, this would go to the backend */
		print_node_as_asm(ip->ip_node, output_file);
		break;

	case IP_DEFLAB:
		/* Define a label */
		fprintf(output_file, "L%d:\n", ip->ip_lbl);
		break;

	case IP_ASM:
		/* Inline assembly - just output it */
		fprintf(output_file, "%s", ip->ip_asm);
		break;

	case IP_PROLOG:
	case IP_EPILOG:
		/* Function prolog/epilog - not used in assembly mode */
		break;

	default:
		fprintf(stderr, "warning: unknown interpass type %d\n", ip->type);
		break;
	}

	free(ip);
}

/*
 * Implement send_passt for standalone assembler
 * This creates interpass structures and sends them to pass2_compile
 */
void
send_passt(int type, ...)
{
	struct interpass *ip;
	struct interpass_prolog *ipp;
	va_list ap;
	int sz;

	va_start(ap, type);

	/* Allocate interpass structure */
	if (type == IP_PROLOG || type == IP_EPILOG)
		sz = sizeof(struct interpass_prolog);
	else
		sz = sizeof(struct interpass);

	ip = (struct interpass *)malloc(sz);
	if (ip == NULL)
		fatal("out of memory");

	memset(ip, 0, sz);
	ip->type = type;
	ip->lineno = lineno;

	/* Fill in type-specific data */
	switch (type) {
	case IP_NODE:
		ip->ip_node = va_arg(ap, NODE *);
		break;

	case IP_DEFLAB:
		ip->ip_lbl = va_arg(ap, int);
		break;

	case IP_ASM:
		ip->ip_asm = va_arg(ap, char *);
		break;

	case IP_PROLOG:
	case IP_EPILOG:
		ipp = (struct interpass_prolog *)ip;
		/* We don't use these for assembly, but set up basic fields */
		ipp->ipp_autos = 0;
		ipp->ipp_name = "";
		ipp->ipp_type = 0;
		ipp->ipp_vis = 0;
		break;

	default:
		fprintf(stderr, "unknown interpass type %d\n", type);
		free(ip);
		va_end(ap);
		return;
	}

	va_end(ap);

	/* Send to pass2 */
	pass2_compile(ip);
}

/*
 * Print usage information
 */
static void
usage(void)
{
	fprintf(stderr,
	    "Usage: mcom [options] [file]\n"
	    "DEC MACRO Assembly Language Compiler (PCC Frontend)\n"
	    "\n"
	    "Options:\n"
	    "  -o file       Write output to file\n"
	    "  -l            Generate listing output\n"
	    "  -v            Verbose output\n"
	    "  --dump-symtab Dump symbol table\n"
	    "  -h, --help    Show this help\n"
	    "\n"
	    "Supported DEC architectures:\n"
	    "  - PDP-10 (36-bit)\n"
	    "  - PDP-11 (16-bit)\n"
	    "  - VAX (32-bit)\n"
	    "\n"
	    "This is a PCC-integrated assembler that uses PCC's IR system.\n"
	    "\n"
	    "Examples:\n"
	    "  mcom prog.mac           # Assemble prog.mac to stdout\n"
	    "  mcom -o prog.s prog.mac # Assemble to prog.s\n"
	    "  mcom -v prog.mac        # Verbose assembly\n");
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
				output_file = fopen(argv[i], "w");
				if (output_file == NULL) {
					perror(argv[i]);
					exit(1);
				}
			} else if (strcmp(argv[i], "-l") == 0) {
				/* Listing output */
				list_output = 1;
			} else if (strcmp(argv[i], "-v") == 0) {
				/* Verbose */
				verbose = 1;
			} else if (strcmp(argv[i], "--dump-symtab") == 0) {
				/* Dump symbol table */
				dump_symtab_flag = 1;
			} else if (strcmp(argv[i], "-h") == 0 ||
			           strcmp(argv[i], "--help") == 0) {
				usage();
			} else {
				fprintf(stderr, "error: unknown option '%s'\n", argv[i]);
				usage();
			}
		} else {
			/* Input file */
			ftitle = argv[i];
			yyin = fopen(ftitle, "r");
			if (yyin == NULL) {
				perror(ftitle);
				exit(1);
			}
		}
	}

	/* Default output to stdout */
	if (output_file == NULL)
		output_file = stdout;
}

/*
 * Main entry point
 */
int
main(int argc, char **argv)
{
	int ret;

	/* Initialize subsystems */
	error_init();
	symtab_init();
	macro_init();

	/* Parse arguments */
	parse_args(argc, argv);

	if (verbose) {
		fprintf(stderr, "PCC DEC MACRO Compiler\n");
		fprintf(stderr, "Input: %s\n", ftitle);
		fprintf(stderr, "Using PCC IR system\n");
	}

	/* Begin compilation unit (PCC interface) */
	bjobcode();

	/* Emit header comment */
	fprintf(output_file, "; Generated by PCC DEC MACRO Compiler\n");
	fprintf(output_file, "; Source: %s\n", ftitle);
	fprintf(output_file, "\n");

	/* Parse input */
	ret = yyparse();

	/* End compilation unit (PCC interface) */
	ejobcode(0);

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

	/* Dump symbol table if requested */
	if (dump_symtab_flag) {
		dump_symtab();
	}

	if (verbose) {
		fprintf(stderr, "Assembly successful.\n");
		fprintf(stderr, "Output uses PCC interpass mechanism (IP_ASM, IP_DEFLAB)\n");
	}

	/* Close files */
	if (yyin != stdin)
		fclose(yyin);
	if (output_file != stdout)
		fclose(output_file);

	return ret;
}
