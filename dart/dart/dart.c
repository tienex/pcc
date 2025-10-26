/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart compiler driver
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define DCOM_PATH "/usr/local/libexec/dcom"
#define AS_PATH "/usr/bin/as"
#define LD_PATH "/usr/bin/ld"

static char *input_file = NULL;
static char *output_file = "a.out";
static int compile_only = 0;
static int assembly_only = 0;
static int verbose = 0;
static int keep_temps = 0;

static void
usage(void)
{
	fprintf(stderr, "Usage: dart [options] file.dart\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -o <file>    Output file\n");
	fprintf(stderr, "  -c           Compile only (don't link)\n");
	fprintf(stderr, "  -S           Generate assembly only\n");
	fprintf(stderr, "  -v           Verbose output\n");
	fprintf(stderr, "  -k           Keep temporary files\n");
	fprintf(stderr, "  -h           Show this help\n");
	exit(1);
}

static void
parse_args(int argc, char **argv)
{
	int opt;

	while ((opt = getopt(argc, argv, "o:cSvkh")) != -1) {
		switch (opt) {
		case 'o':
			output_file = optarg;
			break;
		case 'c':
			compile_only = 1;
			break;
		case 'S':
			assembly_only = 1;
			break;
		case 'v':
			verbose = 1;
			break;
		case 'k':
			keep_temps = 1;
			break;
		case 'h':
			usage();
			break;
		default:
			usage();
		}
	}

	if (optind >= argc) {
		fprintf(stderr, "Error: no input file\n");
		usage();
	}

	input_file = argv[optind];
}

static int
run_command(char **argv)
{
	pid_t pid;
	int status;

	if (verbose) {
		printf("Running:");
		for (int i = 0; argv[i]; i++) {
			printf(" %s", argv[i]);
		}
		printf("\n");
	}

	pid = fork();
	if (pid == 0) {
		/* Child process */
		execvp(argv[0], argv);
		fprintf(stderr, "Error: failed to execute %s\n", argv[0]);
		exit(1);
	} else if (pid < 0) {
		fprintf(stderr, "Error: fork failed\n");
		return 1;
	}

	/* Parent process */
	waitpid(pid, &status, 0);
	return WIFEXITED(status) ? WEXITSTATUS(status) : 1;
}

static char *
get_temp_file(const char *suffix)
{
	static char buf[256];
	snprintf(buf, sizeof(buf), "/tmp/dart_XXXXXX%s", suffix);
	int fd = mkstemp(buf);
	if (fd < 0) {
		fprintf(stderr, "Error: cannot create temporary file\n");
		exit(1);
	}
	close(fd);
	return buf;
}

int
main(int argc, char **argv)
{
	char *asm_file;
	char *obj_file;
	int ret;

	parse_args(argc, argv);

	/* Determine output file names */
	if (assembly_only) {
		if (strcmp(output_file, "a.out") == 0) {
			/* Default output for -S */
			asm_file = strdup("output.s");
		} else {
			asm_file = strdup(output_file);
		}
	} else if (compile_only) {
		if (strcmp(output_file, "a.out") == 0) {
			/* Default output for -c */
			obj_file = strdup("output.o");
		} else {
			obj_file = strdup(output_file);
		}
		asm_file = get_temp_file(".s");
	} else {
		asm_file = get_temp_file(".s");
		obj_file = get_temp_file(".o");
	}

	/* Step 1: Compile Dart to assembly */
	char *compile_args[] = {
		DCOM_PATH,
		"-o", asm_file,
		input_file,
		NULL
	};

	ret = run_command(compile_args);
	if (ret != 0) {
		fprintf(stderr, "Compilation failed\n");
		return ret;
	}

	if (assembly_only) {
		if (!keep_temps && strcmp(asm_file, output_file) != 0) {
			unlink(asm_file);
		}
		return 0;
	}

	/* Step 2: Assemble */
	char *as_args[] = {
		AS_PATH,
		"-o", obj_file,
		asm_file,
		NULL
	};

	ret = run_command(as_args);
	if (!keep_temps) {
		unlink(asm_file);
	}
	if (ret != 0) {
		fprintf(stderr, "Assembly failed\n");
		return ret;
	}

	if (compile_only) {
		return 0;
	}

	/* Step 3: Link */
	char *ld_args[] = {
		LD_PATH,
		"-o", output_file,
		obj_file,
		NULL
	};

	ret = run_command(ld_args);
	if (!keep_temps) {
		unlink(obj_file);
	}
	if (ret != 0) {
		fprintf(stderr, "Linking failed\n");
		return ret;
	}

	if (verbose) {
		printf("Successfully created %s\n", output_file);
	}

	return 0;
}
