/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL runtime initialization and cleanup
 */

#include "chill.h"
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

/*
 * Signal handler for runtime errors
 */
static void
signal_handler(int sig)
{
	const char *signame;

	switch (sig) {
	case SIGFPE:
		signame = "Floating point exception";
		break;
	case SIGSEGV:
		signame = "Segmentation fault";
		break;
	case SIGILL:
		signame = "Illegal instruction";
		break;
	default:
		signame = "Unknown signal";
		break;
	}

	fprintf(stderr, "CHILL Runtime Error: %s (signal %d)\n", signame, sig);
	exit(1);
}

/*
 * Initialize CHILL runtime system
 */
void
chill_runtime_init(void)
{
	/* Install signal handlers for runtime errors */
	signal(SIGFPE, signal_handler);
	signal(SIGSEGV, signal_handler);
	signal(SIGILL, signal_handler);

	/* Initialize I/O system */
	/* (Standard C library handles this for us) */

	/* Initialize thread system */
	/* (pthreads initialization is automatic) */
}

/*
 * Cleanup CHILL runtime system
 */
void
chill_runtime_cleanup(void)
{
	/* Flush all output streams */
	fflush(stdout);
	fflush(stderr);

	/* Free any global resources */
	/* (In this simple implementation, there are none) */
}
