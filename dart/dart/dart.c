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
#define CC_PATH "/usr/bin/gcc"
#define LIBDART_PATH "/usr/local/lib"
#define LIBDART_INCLUDE "/usr/local/include"

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
	char *c_file;
	int ret;

	parse_args(argc, argv);

	/* Determine output file name */
	if (compile_only) {
		if (strcmp(output_file, "a.out") == 0) {
			c_file = strdup("output.c");
		} else {
			c_file = strdup(output_file);
		}
	} else {
		c_file = get_temp_file(".c");
	}

	/* Step 1: Compile Dart to C */
	char *compile_args[] = {
		DCOM_PATH,
		"-o", c_file,
		input_file,
		NULL
	};

	ret = run_command(compile_args);
	if (ret != 0) {
		fprintf(stderr, "Compilation failed\n");
		return ret;
	}

	if (compile_only) {
		return 0;
	}

	/* Step 2: Compile C to executable using gcc */
	char libdart_lib[256];
	char libdart_inc[256];
	snprintf(libdart_lib, sizeof(libdart_lib), "-L%s", LIBDART_PATH);
	snprintf(libdart_inc, sizeof(libdart_inc), "-I%s", LIBDART_INCLUDE);

	char *cc_args[] = {
		CC_PATH,
		libdart_inc,
		"-o", output_file,
		c_file,
		libdart_lib,
		"-ldart",
		NULL
	};

	ret = run_command(cc_args);
	if (!keep_temps) {
		unlink(c_file);
	}
	if (ret != 0) {
		fprintf(stderr, "C compilation failed\n");
		return ret;
	}

	if (verbose) {
		printf("Successfully created %s\n", output_file);
	}

	return 0;
}
