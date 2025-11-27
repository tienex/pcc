/*
 * Copyright (c) 2025 PCC BLISS Runtime Library
 *
 * BLISS Program Startup Code
 * Provides C runtime initialization and calls BLISS main routine
 */

#include "blissrt.h"
#include <stdlib.h>

/*
 * BLISS main routine - to be provided by user program
 * BLISS routines use C calling convention in PCC
 */
extern long bliss_main(void);

/*
 * Alternate name - some BLISS programs use "main" directly
 */
extern long main(void) __attribute__((weak, alias("_bliss_startup_main")));

/*
 * C runtime entry point
 * This is the actual main() that gets called by the C runtime
 */
int _bliss_startup_main(int argc, char **argv)
{
	long result;

	/* Initialize BLISS runtime library */
	bliss_runtime_init();

	/* Call BLISS main routine */
	result = bliss_main();

	/* Cleanup BLISS runtime */
	bliss_runtime_cleanup();

	/* Return result to OS */
	return (int)result;
}

/*
 * Alternative entry point for programs that define their own main
 */
void bliss_startup(void)
{
	bliss_runtime_init();
	/* Note: User must call bliss_runtime_cleanup() before exit */
}

/*
 * Emergency exit handler
 */
void bliss_exit(int code)
{
	bliss_runtime_cleanup();
	exit(code);
}
