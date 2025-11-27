/*
 * C# Runtime - Memory Management
 */

#ifndef _CSRUNTIME_MEMORY_H_
#define _CSRUNTIME_MEMORY_H_

#include <stdint.h>
#include <stddef.h>

/* Memory allocation */
void *CS_Malloc(size_t size);
void *CS_Calloc(size_t count, size_t size);
void *CS_Realloc(void *ptr, size_t size);
void CS_Free(void *ptr);

/* Garbage collection */
void CS_GC_Collect(void);
void CS_GC_CollectGeneration(int32_t generation);
void CS_GC_WaitForPendingFinalizers(void);
int64_t CS_GC_GetTotalMemory(CSBool forceFullCollection);
int32_t CS_GC_GetGeneration(CSObject *obj);
void CS_GC_SuppressFinalize(CSObject *obj);
void CS_GC_ReRegisterForFinalize(CSObject *obj);

/* GC configuration */
void CS_GC_SetMaxGeneration(int32_t max_gen);
void CS_GC_SetGenerationThreshold(int32_t gen, size_t threshold);

/* Memory statistics */
typedef struct {
	uint64_t total_allocated;
	uint64_t total_freed;
	uint64_t current_usage;
	uint64_t peak_usage;
	uint64_t gc_collections_gen0;
	uint64_t gc_collections_gen1;
	uint64_t gc_collections_gen2;
} CSMemoryStats;

void CS_Memory_GetStats(CSMemoryStats *stats);
void CS_Memory_PrintStats(void);

/* Pinning */
typedef struct CSGCHandle {
	void *target;
	uint32_t handle_type;
} CSGCHandle;

CSGCHandle CS_GCHandle_Alloc(CSObject *obj, uint32_t type);
void CS_GCHandle_Free(CSGCHandle handle);
CSObject *CS_GCHandle_GetTarget(CSGCHandle handle);

/* Unsafe memory operations */
void *CS_Unsafe_AsPointer(void *obj);
void CS_Unsafe_CopyBlock(void *dest, void *src, uint32_t byteCount);
void CS_Unsafe_InitBlock(void *startAddress, uint8_t value, uint32_t byteCount);

#endif /* _CSRUNTIME_MEMORY_H_ */
