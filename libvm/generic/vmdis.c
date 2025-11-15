/*
 * Copyright (c) 2025 PCC Project
 *
 * VM Disassembler Tool
 */

#include "vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(const char *prog) {
	fprintf(stderr, "Usage: %s [-o output] input.vmo\n", prog);
	fprintf(stderr, "Disassemble VM binary bytecode to text assembly\n");
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

	if (output_file) {
		/* Disassemble to file */
		if (vm_disassemble_file(input_file, output_file) < 0) {
			fprintf(stderr, "Error: Failed to disassemble %s\n", input_file);
			return 1;
		}
		printf("Disassembled %s -> %s\n", input_file, output_file);
	} else {
		/* Disassemble to stdout */
		FILE *in = fopen(input_file, "rb");
		if (!in) {
			perror(input_file);
			return 1;
		}

		fseek(in, 0, SEEK_END);
		long size = ftell(in);
		fseek(in, 0, SEEK_SET);

		uint8_t *binary = malloc(size);
		if (!binary) {
			fclose(in);
			return 1;
		}

		fread(binary, 1, size, in);
		fclose(in);

		char *text = vm_disassemble(binary, size);
		free(binary);

		if (!text) {
			fprintf(stderr, "Error: Failed to disassemble\n");
			return 1;
		}

		printf("%s", text);
		free(text);
	}

	return 0;
}
