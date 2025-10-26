/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Runtime Initialization
 */

#include "dart.h"
#include <stdio.h>
#include <stdlib.h>

static bool dart_runtime_initialized = false;

void
dart_runtime_init(void)
{
	if (dart_runtime_initialized) {
		return;
	}

	/* Initialize any global state */
	dart_runtime_initialized = true;
}

void
dart_runtime_cleanup(void)
{
	if (!dart_runtime_initialized) {
		return;
	}

	/* Clean up any global state */
	dart_runtime_initialized = false;
}
