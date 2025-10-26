/*
 * C# Runtime - Memory Management Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include "../include/csruntime.h"

/* Generation thresholds */
#define GC_GEN0_THRESHOLD  (1024 * 1024)     /* 1 MB */
#define GC_GEN1_THRESHOLD  (4 * 1024 * 1024) /* 4 MB */
#define GC_GEN2_THRESHOLD  (8 * 1024 * 1024) /* 8 MB */
#define GC_MAX_GENERATIONS 3

/* Memory statistics */
static CSMemoryStats memory_stats = {0};
static pthread_mutex_t memory_mutex = PTHREAD_MUTEX_INITIALIZER;

/* GC configuration */
static struct {
	int32_t max_generation;
	size_t generation_thresholds[GC_MAX_GENERATIONS];
	size_t generation_sizes[GC_MAX_GENERATIONS];
	uint64_t gc_collections[GC_MAX_GENERATIONS];
	int gc_enabled;
} gc_config = {
	.max_generation = 2,
	.generation_thresholds = {GC_GEN0_THRESHOLD, GC_GEN1_THRESHOLD, GC_GEN2_THRESHOLD},
	.generation_sizes = {0, 0, 0},
	.gc_collections = {0, 0, 0},
	.gc_enabled = 1,
};

/* GC handle management */
#define MAX_GC_HANDLES 1024
static struct {
	CSGCHandle handles[MAX_GC_HANDLES];
	int handle_count;
	pthread_mutex_t handle_mutex;
} gc_handle_table = {
	.handle_count = 0,
	.handle_mutex = PTHREAD_MUTEX_INITIALIZER,
};

/* ========== Memory Allocation ========== */

void *CS_Malloc(size_t size) {
	if (size == 0) return NULL;

	void *ptr = malloc(size);
	if (!ptr) {
		fprintf(stderr, "CS_Malloc: Out of memory (requested %zu bytes)\n", size);
		return NULL;
	}

	pthread_mutex_lock(&memory_mutex);
	memory_stats.total_allocated += size;
	memory_stats.current_usage += size;
	if (memory_stats.current_usage > memory_stats.peak_usage) {
		memory_stats.peak_usage = memory_stats.current_usage;
	}
	pthread_mutex_unlock(&memory_mutex);

	return ptr;
}

void *CS_Calloc(size_t count, size_t size) {
	if (count == 0 || size == 0) return NULL;

	void *ptr = calloc(count, size);
	if (!ptr) {
		fprintf(stderr, "CS_Calloc: Out of memory (requested %zu bytes)\n", count * size);
		return NULL;
	}

	size_t total_size = count * size;
	pthread_mutex_lock(&memory_mutex);
	memory_stats.total_allocated += total_size;
	memory_stats.current_usage += total_size;
	if (memory_stats.current_usage > memory_stats.peak_usage) {
		memory_stats.peak_usage = memory_stats.current_usage;
	}
	pthread_mutex_unlock(&memory_mutex);

	return ptr;
}

void *CS_Realloc(void *ptr, size_t size) {
	if (size == 0) {
		CS_Free(ptr);
		return NULL;
	}

	if (!ptr) {
		return CS_Malloc(size);
	}

	void *new_ptr = realloc(ptr, size);
	if (!new_ptr) {
		fprintf(stderr, "CS_Realloc: Out of memory (requested %zu bytes)\n", size);
		return NULL;
	}

	/* Note: We don't track the old size, so we can't update stats accurately */
	pthread_mutex_lock(&memory_mutex);
	memory_stats.total_allocated += size;
	pthread_mutex_unlock(&memory_mutex);

	return new_ptr;
}

void CS_Free(void *ptr) {
	if (!ptr) return;

	free(ptr);

	pthread_mutex_lock(&memory_mutex);
	memory_stats.total_freed++;
	/* Note: We don't track individual allocation sizes for free */
	pthread_mutex_unlock(&memory_mutex);
}

/* ========== Garbage Collection ========== */

static void gc_mark_phase(int32_t generation) {
	/* Mark all reachable objects in the specified generation */
	/* This is a simplified implementation - a real GC would walk the stack,
	 * registers, and static roots to mark all reachable objects */
}

static void gc_sweep_phase(int32_t generation) {
	/* Sweep and collect all unmarked objects in the specified generation */
	/* This would deallocate objects that were not marked as reachable */
}

void CS_GC_Collect(void) {
	CS_GC_CollectGeneration(gc_config.max_generation);
}

void CS_GC_CollectGeneration(int32_t generation) {
	if (!gc_config.gc_enabled) {
		return;
	}

	if (generation < 0 || generation >= GC_MAX_GENERATIONS) {
		fprintf(stderr, "CS_GC_CollectGeneration: Invalid generation %d\n", generation);
		return;
	}

	pthread_mutex_lock(&memory_mutex);

	/* Perform collection on specified generation and all lower generations */
	for (int32_t gen = 0; gen <= generation; gen++) {
		gc_mark_phase(gen);
		gc_sweep_phase(gen);
		gc_config.gc_collections[gen]++;
		gc_config.generation_sizes[gen] = 0;
	}

	memory_stats.gc_collections_gen0 = gc_config.gc_collections[0];
	memory_stats.gc_collections_gen1 = gc_config.gc_collections[1];
	memory_stats.gc_collections_gen2 = gc_config.gc_collections[2];

	pthread_mutex_unlock(&memory_mutex);
}

void CS_GC_WaitForPendingFinalizers(void) {
	/* Wait for all pending finalizers to complete */
	/* In a real implementation, this would block until the finalizer thread
	 * has processed all objects in the finalization queue */
}

int64_t CS_GC_GetTotalMemory(CSBool forceFullCollection) {
	if (forceFullCollection) {
		CS_GC_Collect();
	}

	pthread_mutex_lock(&memory_mutex);
	int64_t total = (int64_t)memory_stats.current_usage;
	pthread_mutex_unlock(&memory_mutex);

	return total;
}

int32_t CS_GC_GetGeneration(CSObject *obj) {
	if (!obj) return -1;

	/* In a real implementation, objects would have generation metadata */
	/* For now, return a placeholder value */
	return 0;
}

void CS_GC_SuppressFinalize(CSObject *obj) {
	if (!obj) return;

	/* Clear the finalizer to prevent it from being called */
	obj->header.finalizer = NULL;
}

void CS_GC_ReRegisterForFinalize(CSObject *obj) {
	if (!obj) return;

	/* Re-register the object for finalization */
	/* In a real implementation, this would add the object back to the
	 * finalization queue if it has a finalizer */
}

/* ========== GC Configuration ========== */

void CS_GC_SetMaxGeneration(int32_t max_gen) {
	if (max_gen < 0 || max_gen >= GC_MAX_GENERATIONS) {
		fprintf(stderr, "CS_GC_SetMaxGeneration: Invalid generation %d\n", max_gen);
		return;
	}

	gc_config.max_generation = max_gen;
}

void CS_GC_SetGenerationThreshold(int32_t gen, size_t threshold) {
	if (gen < 0 || gen >= GC_MAX_GENERATIONS) {
		fprintf(stderr, "CS_GC_SetGenerationThreshold: Invalid generation %d\n", gen);
		return;
	}

	gc_config.generation_thresholds[gen] = threshold;
}

/* ========== Memory Statistics ========== */

void CS_Memory_GetStats(CSMemoryStats *stats) {
	if (!stats) return;

	pthread_mutex_lock(&memory_mutex);
	*stats = memory_stats;
	pthread_mutex_unlock(&memory_mutex);
}

void CS_Memory_PrintStats(void) {
	pthread_mutex_lock(&memory_mutex);

	printf("========== Memory Statistics ==========\n");
	printf("Total allocated: %llu bytes\n",
	       (unsigned long long)memory_stats.total_allocated);
	printf("Total freed: %llu allocations\n",
	       (unsigned long long)memory_stats.total_freed);
	printf("Current usage: %llu bytes\n",
	       (unsigned long long)memory_stats.current_usage);
	printf("Peak usage: %llu bytes\n",
	       (unsigned long long)memory_stats.peak_usage);
	printf("\nGC Collections:\n");
	printf("  Gen 0: %llu\n", (unsigned long long)memory_stats.gc_collections_gen0);
	printf("  Gen 1: %llu\n", (unsigned long long)memory_stats.gc_collections_gen1);
	printf("  Gen 2: %llu\n", (unsigned long long)memory_stats.gc_collections_gen2);
	printf("=====================================\n");

	pthread_mutex_unlock(&memory_mutex);
}

/* ========== GC Handles ========== */

CSGCHandle CS_GCHandle_Alloc(CSObject *obj, uint32_t type) {
	CSGCHandle handle = {0};

	if (!obj) {
		fprintf(stderr, "CS_GCHandle_Alloc: Cannot allocate handle for NULL object\n");
		return handle;
	}

	pthread_mutex_lock(&gc_handle_table.handle_mutex);

	if (gc_handle_table.handle_count >= MAX_GC_HANDLES) {
		fprintf(stderr, "CS_GCHandle_Alloc: Handle table full\n");
		pthread_mutex_unlock(&gc_handle_table.handle_mutex);
		return handle;
	}

	int index = gc_handle_table.handle_count++;
	gc_handle_table.handles[index].target = obj;
	gc_handle_table.handles[index].handle_type = type;

	handle = gc_handle_table.handles[index];

	/* Pinned handles prevent GC from moving the object */
	if (type == 1) { /* GCHandleType.Pinned */
		/* Mark object as pinned */
		obj->header.flags |= 0x01; /* PINNED flag */
	}

	pthread_mutex_unlock(&gc_handle_table.handle_mutex);

	return handle;
}

void CS_GCHandle_Free(CSGCHandle handle) {
	if (!handle.target) return;

	pthread_mutex_lock(&gc_handle_table.handle_mutex);

	/* Find and remove the handle */
	for (int i = 0; i < gc_handle_table.handle_count; i++) {
		if (gc_handle_table.handles[i].target == handle.target &&
		    gc_handle_table.handles[i].handle_type == handle.handle_type) {

			/* Unpin if necessary */
			if (handle.handle_type == 1) { /* GCHandleType.Pinned */
				CSObject *obj = (CSObject *)handle.target;
				obj->header.flags &= ~0x01; /* Clear PINNED flag */
			}

			/* Remove handle by shifting remaining handles */
			for (int j = i; j < gc_handle_table.handle_count - 1; j++) {
				gc_handle_table.handles[j] = gc_handle_table.handles[j + 1];
			}
			gc_handle_table.handle_count--;
			break;
		}
	}

	pthread_mutex_unlock(&gc_handle_table.handle_mutex);
}

CSObject *CS_GCHandle_GetTarget(CSGCHandle handle) {
	return (CSObject *)handle.target;
}

/* ========== Unsafe Memory Operations ========== */

void *CS_Unsafe_AsPointer(void *obj) {
	/* Return the raw pointer to the object */
	return obj;
}

void CS_Unsafe_CopyBlock(void *dest, void *src, uint32_t byteCount) {
	if (!dest || !src || byteCount == 0) return;

	memcpy(dest, src, byteCount);
}

void CS_Unsafe_InitBlock(void *startAddress, uint8_t value, uint32_t byteCount) {
	if (!startAddress || byteCount == 0) return;

	memset(startAddress, value, byteCount);
}
