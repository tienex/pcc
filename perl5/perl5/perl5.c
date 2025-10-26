/*	$Id$	*/
/*
 * Perl 5 Compiler Driver
 *
 * This is the main driver program that orchestrates the compilation
 * pipeline for Perl 5 programs.
 */

#include "config.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#ifndef LIBEXECDIR
#define LIBEXECDIR "/usr/local/libexec/"
#endif

#define MAXARGS 1000
#define MAXFILES 100

/* Compilation phases */
#define PHASE_PREPROCESS	0x01
#define PHASE_COMPILE		0x02
#define PHASE_ASSEMBLE		0x04
#define PHASE_LINK		0x08

static char *perlcom = LIBEXECDIR "perlcom";
static char *assembler = "as";
static char *linker = "ld";

static char *input_files[MAXFILES];
static int num_input_files = 0;
static char *output_file = NULL;

static int verbose = 0;
static int save_temps = 0;
static int optimization_level = 0;
static int phases = PHASE_PREPROCESS | PHASE_COMPILE | PHASE_ASSEMBLE | PHASE_LINK;

/* Temporary files */
static char *temp_s_file = NULL;
static char *temp_o_file = NULL;

static void cleanup(void);
static void usage(void);
static int run_command(char **argv);
static char *temp_filename(const char *suffix);

/*
 * Cleanup temporary files
 */
static void cleanup(void) {
	if (!save_temps) {
		if (temp_s_file) {
			unlink(temp_s_file);
			free(temp_s_file);
			temp_s_file = NULL;
		}
		if (temp_o_file) {
			unlink(temp_o_file);
			free(temp_o_file);
			temp_o_file = NULL;
		}
	}
}

static void signal_handler(int sig) {
	cleanup();
	exit(1);
}

/*
 * Generate temporary filename
 */
static char *temp_filename(const char *suffix) {
	static int counter = 0;
	char *name;
	int pid = getpid();

	name = malloc(256);
	if (name == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}

	snprintf(name, 256, "/tmp/perl5_%d_%d%s", pid, counter++, suffix);
	return name;
}

/*
 * Run external command
 */
static int run_command(char **argv) {
	pid_t pid;
	int status;
	int i;

	if (verbose) {
		fprintf(stderr, "Executing:");
		for (i = 0; argv[i] != NULL; i++) {
			fprintf(stderr, " %s", argv[i]);
		}
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

	if (WIFEXITED(status)) {
		return WEXITSTATUS(status);
	}

	return -1;
}

/*
 * Compile Perl source to assembly
 */
static int compile_to_asm(const char *input, const char *output) {
	char *argv[MAXARGS];
	int argc = 0;

	argv[argc++] = perlcom;
	argv[argc++] = "-o";
	argv[argc++] = (char *)output;

	if (verbose) {
		argv[argc++] = "-v";
	}

	if (optimization_level > 0) {
		argv[argc++] = "-O";
	}

	argv[argc++] = (char *)input;
	argv[argc++] = NULL;

	return run_command(argv);
}

/*
 * Assemble to object file
 */
static int assemble(const char *input, const char *output) {
	char *argv[MAXARGS];
	int argc = 0;

	argv[argc++] = assembler;
	argv[argc++] = "-o";
	argv[argc++] = (char *)output;
	argv[argc++] = (char *)input;
	argv[argc++] = NULL;

	return run_command(argv);
}

/*
 * Link object files
 */
static int link_objects(char **inputs, int num_inputs, const char *output) {
	char *argv[MAXARGS];
	int argc = 0;
	int i;

	argv[argc++] = linker;
	argv[argc++] = "-o";
	argv[argc++] = (char *)output;

	for (i = 0; i < num_inputs; i++) {
		argv[argc++] = inputs[i];
	}

	argv[argc++] = NULL;

	return run_command(argv);
}

/*
 * Process a single file
 */
static int process_file(const char *filename) {
	const char *ext;
	int result;

	/* Determine file type by extension */
	ext = strrchr(filename, '.');
	if (ext == NULL) {
		fprintf(stderr, "Unknown file type: %s\n", filename);
		return -1;
	}

	if (strcmp(ext, ".pl") == 0 || strcmp(ext, ".pm") == 0) {
		/* Perl source file */
		if (phases & PHASE_COMPILE) {
			temp_s_file = temp_filename(".s");
			result = compile_to_asm(filename, temp_s_file);
			if (result != 0) {
				fprintf(stderr, "Compilation failed for %s\n", filename);
				return result;
			}
		}

		if (phases & PHASE_ASSEMBLE) {
			if (output_file && !(phases & PHASE_LINK)) {
				temp_o_file = (char *)output_file;
			} else {
				temp_o_file = temp_filename(".o");
			}
			result = assemble(temp_s_file, temp_o_file);
			if (result != 0) {
				fprintf(stderr, "Assembly failed for %s\n", filename);
				return result;
			}
		}

		return 0;
	} else if (strcmp(ext, ".s") == 0 || strcmp(ext, ".S") == 0) {
		/* Assembly file */
		if (phases & PHASE_ASSEMBLE) {
			if (output_file && !(phases & PHASE_LINK)) {
				temp_o_file = (char *)output_file;
			} else {
				temp_o_file = temp_filename(".o");
			}
			result = assemble(filename, temp_o_file);
			if (result != 0) {
				fprintf(stderr, "Assembly failed for %s\n", filename);
				return result;
			}
		}
		return 0;
	} else if (strcmp(ext, ".o") == 0) {
		/* Object file - just use it directly */
		temp_o_file = (char *)filename;
		return 0;
	}

	fprintf(stderr, "Unknown file type: %s\n", filename);
	return -1;
}

/*
 * Usage
 */
static void usage(void) {
	fprintf(stderr, "Usage: perl5 [options] file...\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -o file       Write output to file\n");
	fprintf(stderr, "  -c            Compile and assemble, but do not link\n");
	fprintf(stderr, "  -S            Compile only, do not assemble or link\n");
	fprintf(stderr, "  -E            Preprocess only (not yet supported)\n");
	fprintf(stderr, "  -v            Verbose mode\n");
	fprintf(stderr, "  -O            Enable optimizations\n");
	fprintf(stderr, "  -save-temps   Keep temporary files\n");
	exit(1);
}

/*
 * Main entry point
 */
int main(int argc, char **argv) {
	int i;
	int result;
	char **object_files;
	int num_object_files = 0;

	/* Set up signal handlers */
	signal(SIGINT, signal_handler);
	signal(SIGTERM, signal_handler);

	/* Parse command line */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			if (strcmp(argv[i], "-o") == 0) {
				if (i + 1 >= argc) {
					usage();
				}
				output_file = argv[++i];
			} else if (strcmp(argv[i], "-c") == 0) {
				phases &= ~PHASE_LINK;
			} else if (strcmp(argv[i], "-S") == 0) {
				phases &= ~(PHASE_ASSEMBLE | PHASE_LINK);
			} else if (strcmp(argv[i], "-E") == 0) {
				phases = PHASE_PREPROCESS;
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "-O") == 0) {
				optimization_level = 1;
			} else if (strcmp(argv[i], "-save-temps") == 0) {
				save_temps = 1;
			} else {
				fprintf(stderr, "Unknown option: %s\n", argv[i]);
				usage();
			}
		} else {
			if (num_input_files >= MAXFILES) {
				fprintf(stderr, "Too many input files\n");
				exit(1);
			}
			input_files[num_input_files++] = argv[i];
		}
	}

	if (num_input_files == 0) {
		fprintf(stderr, "No input files\n");
		usage();
	}

	/* Default output file */
	if (output_file == NULL) {
		if (phases & PHASE_LINK) {
			output_file = "a.out";
		}
	}

	/* Process each input file */
	object_files = malloc(num_input_files * sizeof(char *));
	if (object_files == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}

	for (i = 0; i < num_input_files; i++) {
		result = process_file(input_files[i]);
		if (result != 0) {
			cleanup();
			exit(1);
		}
		if (temp_o_file) {
			object_files[num_object_files++] = temp_o_file;
		}
	}

	/* Link if requested */
	if (phases & PHASE_LINK) {
		result = link_objects(object_files, num_object_files, output_file);
		if (result != 0) {
			fprintf(stderr, "Linking failed\n");
			cleanup();
			exit(1);
		}
	}

	cleanup();
	free(object_files);

	return 0;
}
