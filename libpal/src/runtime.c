/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * Runtime initialization and cleanup
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "../include/palrt.h"

/* Global runtime state */
static int runtime_initialized = 0;

void pal_runtime_init(void)
{
	if (runtime_initialized)
		return;

	/* Initialize random number generator */
	srand((unsigned int)time(NULL));

	/* Clear exception state */
	pal_clear_exception();

	runtime_initialized = 1;
}

void pal_runtime_cleanup(void)
{
	if (!runtime_initialized)
		return;

	/* Cleanup resources if needed */
	runtime_initialized = 0;
}
