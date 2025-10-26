/*
 * Copyright (c) 2025 PCC ALGOL 60+ Runtime Library
 *
 * Runtime initialization, cleanup, and error handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include "algol60.h"

/* Runtime state */
static int algol_initialized = 0;
static int algol_argc = 0;
static char **algol_argv = NULL;

/* Error handling */
static algol_exception_handler_t exception_handler = NULL;

/*
 * Default exception handler
 */
static void
default_exception_handler(int code, const char *message)
{
	fprintf(stderr, "ALGOL 60+ Runtime Error [%d]: %s\n", code, message);
	exit(code);
}

/*
 * Runtime initialization
 */
void
algol_init(int argc, char **argv)
{
	if (algol_initialized) {
		return;  /* Already initialized */
	}

	/* Store command-line arguments */
	algol_argc = argc;
	algol_argv = argv;

	/* Initialize random number generator */
	srand((unsigned int)time(NULL));

	/* Set default exception handler */
	exception_handler = default_exception_handler;

	algol_initialized = 1;
}

/*
 * Runtime cleanup
 */
void
algol_fini(void)
{
	if (!algol_initialized) {
		return;
	}

	/* Cleanup would go here if needed */
	/* For now, just mark as not initialized */
	algol_initialized = 0;
}

/*
 * Error handling
 */

void
algol_error(int code, const char *message, ...)
{
	char buffer[1024];
	va_list ap;

	va_start(ap, message);
	vsnprintf(buffer, sizeof(buffer), message, ap);
	va_end(ap);

	/* Print error message to stderr */
	fprintf(stderr, "ALGOL 60+ Error [%d]: %s\n", code, buffer);

	/* Errors are non-fatal by default, just continue */
}

void
algol_fatal_error(int code, const char *message, ...)
{
	char buffer[1024];
	va_list ap;

	va_start(ap, message);
	vsnprintf(buffer, sizeof(buffer), message, ap);
	va_end(ap);

	/* Call exception handler (which typically exits) */
	if (exception_handler != NULL) {
		exception_handler(code, buffer);
	} else {
		default_exception_handler(code, buffer);
	}

	/* Should not reach here, but just in case */
	exit(code);
}

/*
 * Exception handler management
 */

void
algol_set_exception_handler(algol_exception_handler_t handler)
{
	exception_handler = handler;
}

algol_exception_handler_t
algol_get_exception_handler(void)
{
	return exception_handler;
}

/*
 * Thunk evaluation (for call-by-name)
 */

void *
algol_thunk_eval(algol_thunk_t *thunk)
{
	if (thunk == NULL) {
		algol_error(ALGOL_ERR_NULL_PTR, "thunk_eval: null thunk");
		return NULL;
	}

	if (thunk->evaluate == NULL) {
		algol_error(ALGOL_ERR_NULL_PTR, "thunk_eval: null evaluation function");
		return NULL;
	}

	return thunk->evaluate(thunk->context);
}
