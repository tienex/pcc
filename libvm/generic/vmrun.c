/*
 * Copyright (c) 2025 PCC Project
 *
 * VM Runtime Executor
 */

#include "vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(const char *prog) {
	fprintf(stderr, "Usage: %s [options] program.vmo\n", prog);
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -t          Enable trace mode\n");
	fprintf(stderr, "  -d          Enable debug mode\n");
	fprintf(stderr, "  -s          Print statistics\n");
	fprintf(stderr, "  -m SIZE     Set memory size (default: 1MB)\n");
	fprintf(stderr, "  -a          Input is text assembly (not binary)\n");
	exit(1);
}

int main(int argc, char **argv) {
	const char *input_file = NULL;
	int trace = 0;
	int debug = 0;
	int stats = 0;
	size_t mem_size = VM_DEFAULT_MEM_SIZE;
	vm_format_t format = VM_FORMAT_BINARY;

	/* Parse arguments */
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-t") == 0) {
			trace = 1;
		} else if (strcmp(argv[i], "-d") == 0) {
			debug = 1;
		} else if (strcmp(argv[i], "-s") == 0) {
			stats = 1;
		} else if (strcmp(argv[i], "-a") == 0) {
			format = VM_FORMAT_TEXT;
		} else if (strcmp(argv[i], "-m") == 0) {
			if (i + 1 >= argc) usage(argv[0]);
			mem_size = atoi(argv[++i]);
			if (mem_size < 1024) mem_size = 1024;
		} else if (argv[i][0] == '-') {
			usage(argv[0]);
		} else {
			input_file = argv[i];
		}
	}

	if (!input_file) usage(argv[0]);

	/* Create VM */
	vm_state_t *vm = vm_create_with_memory(mem_size);
	if (!vm) {
		fprintf(stderr, "Error: Failed to create VM\n");
		return 1;
	}

	/* Enable debug/trace */
	if (debug) vm_enable_debug(vm, stderr);
	if (trace) vm_trace(vm, 1);

	/* Load program */
	if (vm_load_file(vm, input_file, format) < 0) {
		fprintf(stderr, "Error: Failed to load %s\n", input_file);
		vm_destroy(vm);
		return 1;
	}

	/* Run */
	int result = vm_run(vm);

	/* Print statistics */
	if (stats) {
		fprintf(stderr, "\n");
		vm_print_stats(vm, stderr);
	}

	/* Check error */
	if (result < 0) {
		vm_error_t error = vm_get_error(vm);
		if (error != VM_ERROR_HALT) {
			fprintf(stderr, "Error: %s\n", vm_error_message(error));
			fprintf(stderr, "\n");
			vm_print_state(vm, stderr);
			vm_destroy(vm);
			return 1;
		}
	}

	/* Get exit code */
	int exit_code = vm_get_register(vm, VM_R0);

	vm_destroy(vm);
	return exit_code;
}
