/*
 * PCSC - Portable C# Compiler Driver
 * Wrapper that invokes cscom and handles compilation pipeline
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define LIBEXECDIR "/usr/local/libexec"
#define CSCOM_PATH LIBEXECDIR "/cscom"

static void print_usage(const char *progname) {
	printf("Usage: %s [options] <file.cs>\n", progname);
	printf("Options are passed to cscom\n");
}

int main(int argc, char **argv) {
	if (argc < 2) {
		print_usage(argv[0]);
		return 1;
	}

	/* Build argument list for cscom */
	char **cscom_argv = malloc((argc + 1) * sizeof(char *));
	cscom_argv[0] = CSCOM_PATH;
	for (int i = 1; i < argc; i++) {
		cscom_argv[i] = argv[i];
	}
	cscom_argv[argc] = NULL;

	/* Execute cscom */
	pid_t pid = fork();
	if (pid == 0) {
		/* Child process */
		execv(CSCOM_PATH, cscom_argv);
		/* If execv returns, an error occurred */
		fprintf(stderr, "Error: Cannot execute %s\n", CSCOM_PATH);
		perror("execv");
		exit(1);
	} else if (pid > 0) {
		/* Parent process */
		int status;
		waitpid(pid, &status, 0);
		free(cscom_argv);

		if (WIFEXITED(status)) {
			return WEXITSTATUS(status);
		} else {
			return 1;
		}
	} else {
		/* Fork failed */
		perror("fork");
		free(cscom_argv);
		return 1;
	}
}
