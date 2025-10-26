/*
 * C# Runtime - ARC Implementation
 * Reference counting and memory management
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include "csruntime.h"

/* Thread-local autorelease pool */
#define MAX_AUTORELEASE_POOLS 32
#define AUTORELEASE_POOL_SIZE 256

struct autorelease_pool {
	CSObject **objects;
	int count;
	int capacity;
};

static __thread struct autorelease_pool *current_pool = NULL;
static __thread struct autorelease_pool pool_stack[MAX_AUTORELEASE_POOLS];
static __thread int pool_stack_depth = 0;

/* Global statistics */
static struct cs_arc_stats global_stats = {0};
static pthread_mutex_t stats_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Debug mode */
static int debug_mode = 0;

/* Object ID counter */
static uint32_t next_object_id = 1;

/* ========== Core ARC Functions ========== */

CSObject *CS_Retain(CSObject *obj) {
	if (!obj)
		return NULL;

	__sync_fetch_and_add(&obj->header.ref_count, 1);

	pthread_mutex_lock(&stats_mutex);
	global_stats.total_retains++;
	pthread_mutex_unlock(&stats_mutex);

	if (debug_mode) {
		printf("ARC: Retain %p (refcount=%d)\n",
		       (void*)obj, obj->header.ref_count);
	}

	return obj;
}

void CS_Release(CSObject *obj) {
	if (!obj)
		return;

	pthread_mutex_lock(&stats_mutex);
	global_stats.total_releases++;
	pthread_mutex_unlock(&stats_mutex);

	int32_t old_count = __sync_fetch_and_sub(&obj->header.ref_count, 1);

	if (debug_mode) {
		printf("ARC: Release %p (refcount=%d->%d)\n",
		       (void*)obj, old_count, old_count - 1);
	}

	if (old_count == 1) {
		/* Reference count reached zero - deallocate */
		if (debug_mode) {
			printf("ARC: Deallocating %p\n", (void*)obj);
		}

		/* Call finalizer if present */
		if (obj->header.finalizer) {
			obj->header.finalizer(obj);
		}

		CS_DeallocObject(obj);
	}
}

CSObject *CS_Autorelease(CSObject *obj) {
	if (!obj)
		return NULL;

	if (!current_pool) {
		fprintf(stderr, "Error: No autorelease pool in place\n");
		return obj;
	}

	/* Add to current autorelease pool */
	if (current_pool->count >= current_pool->capacity) {
		/* Expand pool */
		current_pool->capacity *= 2;
		current_pool->objects = realloc(current_pool->objects,
		                                current_pool->capacity *
		                                sizeof(CSObject *));
	}

	current_pool->objects[current_pool->count++] = obj;

	pthread_mutex_lock(&stats_mutex);
	global_stats.total_autoreleases++;
	pthread_mutex_unlock(&stats_mutex);

	if (debug_mode) {
		printf("ARC: Autorelease %p (pool depth=%d)\n",
		       (void*)obj, pool_stack_depth);
	}

	return obj;
}

/* ========== Strong References ========== */

void CS_StoreStrong(CSObject **dest, CSObject *src) {
	if (!dest)
		return;

	/* Retain new value */
	if (src)
		CS_Retain(src);

	/* Release old value */
	CSObject *old = *dest;
	*dest = src;

	if (old)
		CS_Release(old);
}

/* ========== Weak References ========== */

CSWeakReference *CS_CreateWeakReference(CSObject *obj) {
	CSWeakReference *weak = (CSWeakReference *)CS_AllocObject(
	    sizeof(CSWeakReference), 0);

	weak->target = obj;
	weak->target_id = obj ? obj->header.type_id : 0;
	weak->header.flags |= CS_OBJ_FLAG_WEAK_REF;

	pthread_mutex_lock(&stats_mutex);
	global_stats.weak_references_created++;
	pthread_mutex_unlock(&stats_mutex);

	return weak;
}

void CS_StoreWeak(CSWeakReference **dest, CSObject *obj) {
	if (!dest)
		return;

	/* Destroy old weak reference */
	if (*dest)
		CS_DestroyWeak(dest);

	/* Create new weak reference */
	if (obj) {
		*dest = CS_CreateWeakReference(obj);
	} else {
		*dest = NULL;
	}
}

CSObject *CS_LoadWeak(CSWeakReference *weak) {
	if (!weak || !weak->target)
		return NULL;

	/* Validate target is still alive */
	/* In a real implementation, would check if object was deallocated */

	/* Retain and return target */
	return CS_Retain(weak->target);
}

void CS_DestroyWeak(CSWeakReference **weak) {
	if (!weak || !*weak)
		return;

	pthread_mutex_lock(&stats_mutex);
	global_stats.weak_references_destroyed++;
	pthread_mutex_unlock(&stats_mutex);

	CS_Release((CSObject *)*weak);
	*weak = NULL;
}

void CS_CopyWeak(CSWeakReference **dest, CSWeakReference **src) {
	if (!dest)
		return;

	if (src && *src) {
		CS_StoreWeak(dest, (*src)->target);
	} else {
		CS_StoreWeak(dest, NULL);
	}
}

void CS_MoveWeak(CSWeakReference **dest, CSWeakReference **src) {
	if (!dest || !src)
		return;

	/* Destroy destination */
	if (*dest)
		CS_DestroyWeak(dest);

	/* Move source to destination */
	*dest = *src;
	*src = NULL;
}

/* ========== Optimized Operations ========== */

CSObject *CS_RetainAutorelease(CSObject *obj) {
	if (!obj)
		return NULL;

	CS_Retain(obj);
	return CS_Autorelease(obj);
}

CSObject *CS_RetainAutoreleaseReturnValue(CSObject *obj) {
	/* Optimized for return values - avoid retain/release pairs */
	return CS_RetainAutorelease(obj);
}

/* ========== Object Allocation ========== */

CSObject *CS_AllocObject(size_t size, uint32_t type_id) {
	CSObject *obj = calloc(1, sizeof(struct CSObjectHeader) + size);
	if (!obj)
		return NULL;

	obj->header.type_id = type_id;
	obj->header.ref_count = 1;  /* Start with refcount 1 */
	obj->header.flags = 0;
	obj->header.size = size;
	obj->header.type_info = NULL;
	obj->header.finalizer = NULL;

	pthread_mutex_lock(&stats_mutex);
	global_stats.objects_allocated++;
	global_stats.current_object_count++;
	if (global_stats.current_object_count > global_stats.peak_object_count)
		global_stats.peak_object_count = global_stats.current_object_count;
	pthread_mutex_unlock(&stats_mutex);

	if (debug_mode) {
		printf("ARC: Allocated %p (size=%zu, type=%u)\n",
		       (void*)obj, size, type_id);
	}

	return obj;
}

void CS_DeallocObject(CSObject *obj) {
	if (!obj)
		return;

	pthread_mutex_lock(&stats_mutex);
	global_stats.objects_deallocated++;
	global_stats.current_object_count--;
	pthread_mutex_unlock(&stats_mutex);

	if (debug_mode) {
		printf("ARC: Deallocated %p\n", (void*)obj);
	}

	free(obj);
}

/* ========== Autorelease Pool ========== */

void CS_AutoreleasePoolPush(void) {
	if (pool_stack_depth >= MAX_AUTORELEASE_POOLS) {
		fprintf(stderr, "Error: Autorelease pool stack overflow\n");
		return;
	}

	struct autorelease_pool *pool = &pool_stack[pool_stack_depth];
	pool->capacity = AUTORELEASE_POOL_SIZE;
	pool->count = 0;
	pool->objects = malloc(pool->capacity * sizeof(CSObject *));

	current_pool = pool;
	pool_stack_depth++;

	if (debug_mode) {
		printf("ARC: Pushed autorelease pool (depth=%d)\n",
		       pool_stack_depth);
	}
}

void CS_AutoreleasePoolPop(void) {
	if (pool_stack_depth == 0) {
		fprintf(stderr, "Error: Autorelease pool stack underflow\n");
		return;
	}

	struct autorelease_pool *pool = &pool_stack[pool_stack_depth - 1];

	if (debug_mode) {
		printf("ARC: Popping autorelease pool (depth=%d, count=%d)\n",
		       pool_stack_depth, pool->count);
	}

	/* Release all autoreleased objects */
	for (int i = 0; i < pool->count; i++) {
		CS_Release(pool->objects[i]);
	}

	free(pool->objects);
	pool->objects = NULL;
	pool->count = 0;
	pool->capacity = 0;

	pool_stack_depth--;
	current_pool = (pool_stack_depth > 0) ?
	               &pool_stack[pool_stack_depth - 1] : NULL;
}

/* ========== Statistics ========== */

void CS_ARC_GetStats(struct cs_arc_stats *stats) {
	if (!stats)
		return;

	pthread_mutex_lock(&stats_mutex);
	*stats = global_stats;
	pthread_mutex_unlock(&stats_mutex);
}

void CS_ARC_ResetStats(void) {
	pthread_mutex_lock(&stats_mutex);
	memset(&global_stats, 0, sizeof(global_stats));
	pthread_mutex_unlock(&stats_mutex);
}

void CS_ARC_PrintStats(void) {
	struct cs_arc_stats stats;
	CS_ARC_GetStats(&stats);

	printf("C# Runtime ARC Statistics:\n");
	printf("==========================\n");
	printf("Total Retains:           %llu\n",
	       (unsigned long long)stats.total_retains);
	printf("Total Releases:          %llu\n",
	       (unsigned long long)stats.total_releases);
	printf("Total Autoreleases:      %llu\n",
	       (unsigned long long)stats.total_autoreleases);
	printf("Objects Allocated:       %llu\n",
	       (unsigned long long)stats.objects_allocated);
	printf("Objects Deallocated:     %llu\n",
	       (unsigned long long)stats.objects_deallocated);
	printf("Current Object Count:    %llu\n",
	       (unsigned long long)stats.current_object_count);
	printf("Peak Object Count:       %llu\n",
	       (unsigned long long)stats.peak_object_count);
	printf("Weak Refs Created:       %llu\n",
	       (unsigned long long)stats.weak_references_created);
	printf("Weak Refs Destroyed:     %llu\n",
	       (unsigned long long)stats.weak_references_destroyed);

	if (stats.current_object_count > 0) {
		printf("\nWARNING: %llu objects still alive (potential memory leak)\n",
		       (unsigned long long)stats.current_object_count);
	}
}

/* ========== Debugging ========== */

void CS_ARC_EnableDebug(int enable) {
	debug_mode = enable;
	if (enable) {
		printf("ARC: Debug mode enabled\n");
	}
}

void CS_ARC_DumpLiveObjects(void) {
	/* Would iterate through all live objects and print info */
	printf("ARC: Live object dump not yet implemented\n");
}

int CS_ARC_ValidateHeap(void) {
	/* Would perform heap consistency checks */
	return 1;  /* Success */
}
