/*
 * Copyright (c) 2025 PCC Pascal Runtime Library
 *
 * Pascal Core Runtime Support
 *
 * Implements initialization, cleanup, and error handling for Pascal programs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "pascalrt.h"

/* Global runtime state */
static struct {
	int initialized;
	int argc;
	char **argv;
	void (*error_handler)(PascalError, const char *);
} runtime_state = {0, 0, NULL, NULL};

/* Error messages */
static const char *error_messages[] = {
	[PASCAL_ERR_NONE] = "No error",
	[PASCAL_ERR_RANGE_CHECK] = "Range check error",
	[PASCAL_ERR_STACK_OVERFLOW] = "Stack overflow",
	[PASCAL_ERR_HEAP_OVERFLOW] = "Heap overflow",
	[PASCAL_ERR_INTEGER_OVERFLOW] = "Integer overflow",
	[PASCAL_ERR_DIVIDE_BY_ZERO] = "Division by zero",
	[PASCAL_ERR_INVALID_OPERATION] = "Invalid operation",
	[PASCAL_ERR_FILE_NOT_FOUND] = "File not found",
	[PASCAL_ERR_FILE_NOT_OPEN] = "File not open",
	[PASCAL_ERR_DISK_FULL] = "Disk full",
	[PASCAL_ERR_READ_ERROR] = "Read error",
	[PASCAL_ERR_WRITE_ERROR] = "Write error",
	[PASCAL_ERR_NIL_POINTER] = "Nil pointer dereference",
	[PASCAL_ERR_INVALID_POINTER] = "Invalid pointer operation",
};

/*
 * Default error handler
 */
static void
default_error_handler(PascalError error, const char *message)
{
	const char *error_msg;

	if (error >= 0 && error < sizeof(error_messages)/sizeof(error_messages[0])) {
		error_msg = error_messages[error];
	} else {
		error_msg = "Unknown error";
	}

	fprintf(stderr, "\nRuntime error %d: %s\n", error, error_msg);
	if (message != NULL && *message != '\0') {
		fprintf(stderr, "  %s\n", message);
	}

	exit(error);
}

/*
 * Signal handlers for runtime errors
 */
static void
signal_handler(int sig)
{
	switch (sig) {
	case SIGFPE:
		pascal_runtime_error(PASCAL_ERR_DIVIDE_BY_ZERO,
		                     "Floating point exception");
		break;
	case SIGSEGV:
		pascal_runtime_error(PASCAL_ERR_NIL_POINTER,
		                     "Segmentation fault (nil pointer dereference?)");
		break;
	case SIGABRT:
		pascal_runtime_error(PASCAL_ERR_INVALID_OPERATION,
		                     "Abnormal termination");
		break;
	default:
		pascal_runtime_error(PASCAL_ERR_INVALID_OPERATION,
		                     "Unexpected signal");
		break;
	}
}

/*
 * Initialize Pascal runtime
 */
void
pascal_init_runtime(int argc, char **argv)
{
	if (runtime_state.initialized) {
		return;
	}

	runtime_state.argc = argc;
	runtime_state.argv = argv;
	runtime_state.error_handler = default_error_handler;
	runtime_state.initialized = 1;

	/* Install signal handlers */
	signal(SIGFPE, signal_handler);
	signal(SIGSEGV, signal_handler);
	signal(SIGABRT, signal_handler);

	/* Initialize standard I/O handles */
	/* This is done automatically in pascalio.c when first used */
}

/*
 * Finalize Pascal runtime
 */
void
pascal_fini_runtime(void)
{
	if (!runtime_state.initialized) {
		return;
	}

	/* Close standard file handles if needed */
	/* Cleanup is mostly automatic with modern C runtimes */

	runtime_state.initialized = 0;
}

/*
 * Runtime error handling
 */
void
pascal_runtime_error(PascalError error, const char *message)
{
	if (runtime_state.error_handler != NULL) {
		runtime_state.error_handler(error, message);
	} else {
		default_error_handler(error, message);
	}
}

/*
 * Set custom error handler
 */
void
pascal_set_error_handler(void (*handler)(PascalError, const char *))
{
	runtime_state.error_handler = handler;
}

/*
 * Runtime initialization function (called automatically)
 */
static void __attribute__((constructor))
pascal_runtime_constructor(void)
{
	/* Perform minimal initialization */
	/* Full initialization happens in pascal_init_runtime */
}

/*
 * Runtime cleanup function (called automatically)
 */
static void __attribute__((destructor))
pascal_runtime_destructor(void)
{
	pascal_fini_runtime();
}
