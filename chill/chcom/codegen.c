/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Code generation for CHILL compiler
 * Generates assembly output from parsed CHILL programs
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static FILE *codegen_outfile = NULL;
static char *codegen_module_name = NULL;

/*
 * Initialize code generator
 */
void
codegen_init(const char *output_filename, const char *modname)
{
	if (output_filename == NULL) {
		codegen_outfile = stdout;
	} else if (strcmp(output_filename, "-") == 0) {
		codegen_outfile = stdout;
	} else {
		codegen_outfile = fopen(output_filename, "w");
		if (codegen_outfile == NULL) {
			error("cannot open output file '%s'", output_filename);
			return;
		}
	}

	codegen_module_name = strdup(modname);
}

/*
 * Emit assembly prologue
 */
void
codegen_prologue(void)
{
	if (codegen_outfile == NULL)
		return;

	fprintf(codegen_outfile, "\t.file\t\"%s.ch\"\n", codegen_module_name);
	fprintf(codegen_outfile, "\t.text\n");
}

/*
 * Emit data section
 */
void
codegen_data_section(void)
{
	if (codegen_outfile == NULL)
		return;

	fprintf(codegen_outfile, "\n\t.data\n");
	fprintf(codegen_outfile, "\t# Global variables would go here\n");
}

/*
 * Emit procedure code
 */
void
codegen_procedure(const char *name)
{
	if (codegen_outfile == NULL)
		return;

	fprintf(codegen_outfile, "\n\t.text\n");
	fprintf(codegen_outfile, "\t.globl\t%s\n", name);
	fprintf(codegen_outfile, "\t.type\t%s, @function\n", name);
	fprintf(codegen_outfile, "%s:\n", name);
	fprintf(codegen_outfile, "\tpushq\t%%rbp\n");
	fprintf(codegen_outfile, "\tmovq\t%%rsp, %%rbp\n");
	fprintf(codegen_outfile, "\t# procedure body would go here\n");
	fprintf(codegen_outfile, "\tpopq\t%%rbp\n");
	fprintf(codegen_outfile, "\tret\n");
	fprintf(codegen_outfile, "\t.size\t%s, .-%s\n", name, name);
}

/*
 * Emit module initialization
 */
void
codegen_module_init(void)
{
	if (codegen_outfile == NULL)
		return;

	fprintf(codegen_outfile, "\n\t.text\n");
	fprintf(codegen_outfile, "\t.globl\t_chill_module_init_%s\n", codegen_module_name);
	fprintf(codegen_outfile, "\t.type\t_chill_module_init_%s, @function\n", codegen_module_name);
	fprintf(codegen_outfile, "_chill_module_init_%s:\n", codegen_module_name);
	fprintf(codegen_outfile, "\tpushq\t%%rbp\n");
	fprintf(codegen_outfile, "\tmovq\t%%rsp, %%rbp\n");
	fprintf(codegen_outfile, "\t# module initialization\n");
	fprintf(codegen_outfile, "\tcall\tchill_runtime_init\n");
	fprintf(codegen_outfile, "\tpopq\t%%rbp\n");
	fprintf(codegen_outfile, "\tret\n");
	fprintf(codegen_outfile, "\t.size\t_chill_module_init_%s, .-_chill_module_init_%s\n",
	        codegen_module_name, codegen_module_name);
}

/*
 * Emit main function (if module is a program)
 */
void
codegen_main(void)
{
	if (codegen_outfile == NULL)
		return;

	fprintf(codegen_outfile, "\n\t.text\n");
	fprintf(codegen_outfile, "\t.globl\tmain\n");
	fprintf(codegen_outfile, "\t.type\tmain, @function\n");
	fprintf(codegen_outfile, "main:\n");
	fprintf(codegen_outfile, "\tpushq\t%%rbp\n");
	fprintf(codegen_outfile, "\tmovq\t%%rsp, %%rbp\n");
	fprintf(codegen_outfile, "\t# call module initialization\n");
	fprintf(codegen_outfile, "\tcall\t_chill_module_init_%s\n", codegen_module_name);
	fprintf(codegen_outfile, "\t# main program logic would go here\n");
	fprintf(codegen_outfile, "\txorl\t%%eax, %%eax\n");
	fprintf(codegen_outfile, "\tpopq\t%%rbp\n");
	fprintf(codegen_outfile, "\tret\n");
	fprintf(codegen_outfile, "\t.size\tmain, .-main\n");
}

/*
 * Emit assembly epilogue
 */
void
codegen_epilogue(void)
{
	if (codegen_outfile == NULL)
		return;

	fprintf(codegen_outfile, "\n\t.ident\t\"PCC CHILL Compiler\"\n");
	fprintf(codegen_outfile, "\t.section\t.note.GNU-stack,\"\",@progbits\n");
}

/*
 * Finalize code generation
 */
void
codegen_finish(void)
{
	if (codegen_outfile != NULL && codegen_outfile != stdout) {
		fclose(codegen_outfile);
	}

	if (codegen_module_name != NULL) {
		free(codegen_module_name);
		codegen_module_name = NULL;
	}
}

/*
 * Generate code for complete module
 */
void
codegen_module(const char *output_filename, const char *modname)
{
	codegen_init(output_filename, modname);
	codegen_prologue();
	codegen_data_section();
	codegen_module_init();
	codegen_main();
	codegen_epilogue();
	codegen_finish();
}
