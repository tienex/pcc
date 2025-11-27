/*
 * C# Runtime Library - Main Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "csruntime.h"

/* Runtime state */
static struct {
	int initialized;
	struct cs_runtime_config config;
} runtime_state = {
	.initialized = 0,
};

/* Default configuration */
static struct cs_runtime_config default_config = {
	.initial_heap_size = 1024 * 1024,      /* 1 MB */
	.max_heap_size = 1024 * 1024 * 1024,   /* 1 GB */
	.enable_gc = 1,
	.enable_arc = 1,
	.gc_threshold = 1024 * 1024,           /* 1 MB */
	.debug_mode = 0,
};

/* Initialize C# runtime */
int CSRuntime_Init(struct cs_runtime_config *config) {
	if (runtime_state.initialized) {
		fprintf(stderr, "Warning: Runtime already initialized\n");
		return 0;
	}

	/* Use provided config or default */
	if (config) {
		runtime_state.config = *config;
	} else {
		runtime_state.config = default_config;
	}

	printf("Initializing C# Runtime Library v%d.%d.%d\n",
	       CSRUNTIME_VERSION_MAJOR,
	       CSRUNTIME_VERSION_MINOR,
	       CSRUNTIME_VERSION_PATCH);

	/* Initialize ARC subsystem */
	if (runtime_state.config.enable_arc) {
		printf("  ARC: Enabled\n");
		CS_ARC_EnableDebug(runtime_state.config.debug_mode);
	}

	/* Initialize autorelease pool */
	CS_AutoreleasePoolPush();

	runtime_state.initialized = 1;

	printf("Runtime initialized successfully\n");
	return 1;
}

/* Shutdown runtime */
void CSRuntime_Shutdown(void) {
	if (!runtime_state.initialized) {
		return;
	}

	printf("Shutting down C# Runtime...\n");

	/* Pop autorelease pool */
	CS_AutoreleasePoolPop();

	/* Print statistics if in debug mode */
	if (runtime_state.config.debug_mode) {
		CS_ARC_PrintStats();
	}

	runtime_state.initialized = 0;
	printf("Runtime shutdown complete\n");
}

/* Get runtime version */
const char *CSRuntime_GetVersion(void) {
	static char version[32];
	snprintf(version, sizeof(version), "%d.%d.%d",
	         CSRUNTIME_VERSION_MAJOR,
	         CSRUNTIME_VERSION_MINOR,
	         CSRUNTIME_VERSION_PATCH);
	return version;
}

/* Print runtime statistics */
void CSRuntime_PrintStats(void) {
	printf("\n========== C# Runtime Statistics ==========\n");
	printf("Version: %s\n", CSRuntime_GetVersion());
	printf("Initialized: %s\n", runtime_state.initialized ? "Yes" : "No");
	printf("\nConfiguration:\n");
	printf("  Initial heap: %zu bytes\n",
	       runtime_state.config.initial_heap_size);
	printf("  Max heap: %zu bytes\n", runtime_state.config.max_heap_size);
	printf("  GC enabled: %s\n", runtime_state.config.enable_gc ? "Yes" : "No");
	printf("  ARC enabled: %s\n", runtime_state.config.enable_arc ? "Yes" : "No");
	printf("  GC threshold: %d bytes\n", runtime_state.config.gc_threshold);
	printf("  Debug mode: %s\n", runtime_state.config.debug_mode ? "Yes" : "No");

	if (runtime_state.config.enable_arc) {
		printf("\n");
		CS_ARC_PrintStats();
	}

	printf("==========================================\n\n");
}
