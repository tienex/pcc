/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Driver program for Go compiler
 * Orchestrates: gocom -> assembler -> linker
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#define MAXARGS 1024

/* Paths (will be set by configure) */
#ifndef LIBEXECDIR
#define LIBEXECDIR "/usr/local/libexec/pcc"
#endif

#ifndef ASSEMBLER
#define ASSEMBLER "as"
#endif

#ifndef LINKER
#define LINKER "ld"
#endif

/* File types */
#define FILETYPE_GO     1   /* .go source */
#define FILETYPE_ASM    2   /* .s assembly */
#define FILETYPE_OBJ    3   /* .o object */

/* Global options */
static int verbose = 0;
static int compile_only = 0;  /* -c */
static int asm_only = 0;      /* -S */
static char *output_file = NULL;
static char *temp_asm = NULL;
static char *temp_obj = NULL;

/* File list */
static char *input_files[MAXARGS];
static int num_inputs = 0;
static char *obj_files[MAXARGS];
static int num_objs = 0;

/*
 * Print usage
 */
static void
usage(void)
{
	fprintf(stderr, "Usage: go [options] file.go...\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -o file   Write output to file\n");
	fprintf(stderr, "  -c        Compile and assemble, but do not link\n");
	fprintf(stderr, "  -S        Compile only; do not assemble or link\n");
	fprintf(stderr, "  -v        Verbose output (show commands)\n");
	fprintf(stderr, "  -h        Show this help\n");
	exit(1);
}

/*
 * Execute external program
 */
static int
run_cmd(char **argv)
{
	pid_t pid;
	int status;

	if (verbose) {
		int i;
		fprintf(stderr, "+");
		for (i = 0; argv[i] != NULL; i++)
			fprintf(stderr, " %s", argv[i]);
		fprintf(stderr, "\n");
	}

	pid = fork();
	if (pid == -1) {
		perror("fork");
		return -1;
	}

	if (pid == 0) {
		/* Child process */
		execvp(argv[0], argv);
		perror(argv[0]);
		exit(1);
	}

	/* Parent process */
	if (waitpid(pid, &status, 0) == -1) {
		perror("waitpid");
		return -1;
	}

	if (WIFEXITED(status))
		return WEXITSTATUS(status);

	return -1;
}

/*
 * Get file extension
 */
static const char *
get_extension(const char *filename)
{
	const char *dot = strrchr(filename, '.');
	if (dot == NULL)
		return "";
	return dot + 1;
}

/*
 * Change file extension
 */
static char *
change_extension(const char *filename, const char *new_ext)
{
	const char *dot = strrchr(filename, '.');
	size_t base_len;
	char *result;

	if (dot == NULL)
		base_len = strlen(filename);
	else
		base_len = dot - filename;

	result = malloc(base_len + strlen(new_ext) + 2);
	if (result == NULL) {
		perror("malloc");
		exit(1);
	}

	strncpy(result, filename, base_len);
	result[base_len] = '.';
	strcpy(result + base_len + 1, new_ext);

	return result;
}

/*
 * Compile Go source to assembly
 */
static int
compile_go(const char *input, const char *output)
{
	char *argv[MAXARGS];
	int argc = 0;
	char gocom_path[1024];

	snprintf(gocom_path, sizeof(gocom_path), "%s/gocom", LIBEXECDIR);

	argv[argc++] = gocom_path;
	argv[argc++] = "-o";
	argv[argc++] = (char *)output;
	argv[argc++] = (char *)input;
	argv[argc++] = NULL;

	return run_cmd(argv);
}

/*
 * Assemble .s to .o
 */
static int
assemble(const char *input, const char *output)
{
	char *argv[MAXARGS];
	int argc = 0;

	argv[argc++] = ASSEMBLER;
	argv[argc++] = "-o";
	argv[argc++] = (char *)output;
	argv[argc++] = (char *)input;
	argv[argc++] = NULL;

	return run_cmd(argv);
}

/*
 * Link object files
 */
static int
link_objs(void)
{
	char *argv[MAXARGS];
	int argc = 0;
	int i;

	argv[argc++] = LINKER;
	argv[argc++] = "-o";
	argv[argc++] = output_file ? output_file : "a.out";

	for (i = 0; i < num_objs; i++)
		argv[argc++] = obj_files[i];

	argv[argc++] = NULL;

	return run_cmd(argv);
}

/*
 * Process a single input file
 */
static int
process_file(const char *filename)
{
	const char *ext = get_extension(filename);
	char *asm_file = NULL;
	char *obj_file = NULL;
	int ret = 0;

	if (strcmp(ext, "go") == 0) {
		/* Go source file */

		/* Compile to assembly */
		if (output_file && asm_only) {
			asm_file = output_file;
		} else {
			asm_file = change_extension(filename, "s");
		}

		ret = compile_go(filename, asm_file);
		if (ret != 0) {
			fprintf(stderr, "compilation failed\n");
			goto cleanup;
		}

		if (asm_only) {
			/* Done - keep assembly file */
			return 0;
		}

		/* Assemble to object */
		if (output_file && compile_only) {
			obj_file = output_file;
		} else {
			obj_file = change_extension(filename, "o");
		}

		ret = assemble(asm_file, obj_file);
		if (ret != 0) {
			fprintf(stderr, "assembly failed\n");
			goto cleanup;
		}

		/* Clean up temporary assembly file */
		if (!asm_only && asm_file != output_file)
			unlink(asm_file);

		if (compile_only) {
			/* Done - keep object file */
			return 0;
		}

		/* Add to link list */
		obj_files[num_objs++] = strdup(obj_file);

	} else if (strcmp(ext, "s") == 0) {
		/* Assembly file */

		if (output_file && (asm_only || compile_only)) {
			obj_file = output_file;
		} else {
			obj_file = change_extension(filename, "o");
		}

		if (!asm_only) {
			ret = assemble(filename, obj_file);
			if (ret != 0) {
				fprintf(stderr, "assembly failed\n");
				goto cleanup;
			}
		}

		if (!compile_only)
			obj_files[num_objs++] = strdup(obj_file);

	} else if (strcmp(ext, "o") == 0) {
		/* Object file */
		obj_files[num_objs++] = strdup(filename);

	} else {
		fprintf(stderr, "error: unknown file type: %s\n", filename);
		return 1;
	}

cleanup:
	if (asm_file && asm_file != output_file && !asm_only)
		free(asm_file);
	if (obj_file && obj_file != output_file && !compile_only)
		free(obj_file);

	return ret;
}

/*
 * Main entry point
 */
int
main(int argc, char **argv)
{
	int i;
	int ret = 0;

	/* Parse arguments */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			if (strcmp(argv[i], "-o") == 0) {
				if (++i >= argc) {
					fprintf(stderr, "error: -o requires argument\n");
					usage();
				}
				output_file = argv[i];
			} else if (strcmp(argv[i], "-c") == 0) {
				compile_only = 1;
			} else if (strcmp(argv[i], "-S") == 0) {
				asm_only = 1;
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "-h") == 0) {
				usage();
			} else {
				fprintf(stderr, "error: unknown option '%s'\n", argv[i]);
				usage();
			}
		} else {
			/* Input file */
			input_files[num_inputs++] = argv[i];
		}
	}

	/* Check for input files */
	if (num_inputs == 0) {
		fprintf(stderr, "error: no input files\n");
		usage();
	}

	/* Process each input file */
	for (i = 0; i < num_inputs; i++) {
		ret = process_file(input_files[i]);
		if (ret != 0)
			return ret;
	}

	/* Link if not -c or -S */
	if (!compile_only && !asm_only && num_objs > 0) {
		ret = link_objs();
		if (ret != 0) {
			fprintf(stderr, "link failed\n");
			return ret;
		}

		/* Clean up temporary object files */
		for (i = 0; i < num_objs; i++) {
			/* Only remove if not an original input */
			int j;
			int is_input = 0;
			for (j = 0; j < num_inputs; j++) {
				if (strcmp(obj_files[i], input_files[j]) == 0) {
					is_input = 1;
					break;
				}
			}
			if (!is_input)
				unlink(obj_files[i]);
		}
	}

	return ret;
}
