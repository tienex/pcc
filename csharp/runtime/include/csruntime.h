/*
 * C# Runtime Library - Main Header
 * Provides runtime support for compiled C# programs
 */

#ifndef _CSRUNTIME_H_
#define _CSRUNTIME_H_

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Version information */
#define CSRUNTIME_VERSION_MAJOR 2
#define CSRUNTIME_VERSION_MINOR 0
#define CSRUNTIME_VERSION_PATCH 0

/* Runtime configuration */
struct cs_runtime_config {
	size_t initial_heap_size;
	size_t max_heap_size;
	int enable_gc;
	int enable_arc;
	int gc_threshold;
	int debug_mode;
};

/* Initialize runtime */
int CSRuntime_Init(struct cs_runtime_config *config);
void CSRuntime_Shutdown(void);

/* Runtime information */
const char *CSRuntime_GetVersion(void);
void CSRuntime_PrintStats(void);

/* Include sub-modules */
#include "csruntime_arc.h"
#include "csruntime_types.h"
#include "csruntime_string.h"
#include "csruntime_collections.h"
#include "csruntime_linq.h"
#include "csruntime_async.h"
#include "csruntime_memory.h"

#endif /* _CSRUNTIME_H_ */
