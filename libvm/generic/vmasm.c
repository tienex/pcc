/*
 * Copyright (c) 2025 PCC Project
 *
 * VM Assembler Tool
 */

#include "vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(const char *prog) {
	fprintf(stderr, "Usage: %s [-o output] input.asm\n", prog);
	fprintf(stderr, "Assemble VM text assembly to binary bytecode\n");
	exit(1);
}

int main(int argc, char **argv) {
	const char *input_file = NULL;
	const char *output_file = NULL;

	/* Parse arguments */
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-o") == 0) {
			if (i + 1 >= argc) usage(argv[0]);
			output_file = argv[++i];
		} else if (argv[i][0] == '-') {
			usage(argv[0]);
		} else {
			input_file = argv[i];
		}
	}

	if (!input_file) usage(argv[0]);
	if (!output_file) {
		/* Generate output filename */
		static char outbuf[256];
		snprintf(outbuf, sizeof(outbuf), "%s.vmo", input_file);
		output_file = outbuf;
	}

	/* Assemble */
	if (vm_assemble_file(input_file, output_file) < 0) {
		fprintf(stderr, "Error: Failed to assemble %s\n", input_file);
		return 1;
	}

	printf("Assembled %s -> %s\n", input_file, output_file);
	return 0;
}
