/*
 * C# Runtime - ARC (Automatic Reference Counting) Support
 * Implements reference counting for C# reference types
 */

#ifndef _CSRUNTIME_ARC_H_
#define _CSRUNTIME_ARC_H_

#include <stdint.h>
#include <stddef.h>

/* Forward declarations */
typedef struct CSObject CSObject;
typedef struct CSWeakReference CSWeakReference;

/* Object header (prepended to all reference types) */
struct CSObjectHeader {
	uint32_t type_id;         /* Type identifier */
	int32_t ref_count;        /* Reference count */
	uint32_t flags;           /* Object flags */
	uint32_t size;            /* Object size */
	void *type_info;          /* Pointer to type information */
	void (*finalizer)(void*); /* Finalizer function */
};

/* Base object structure */
struct CSObject {
	struct CSObjectHeader header;
	/* User data follows */
};

/* Weak reference structure */
struct CSWeakReference {
	struct CSObjectHeader header;
	CSObject *target;         /* Target object (or NULL if collected) */
	uint32_t target_id;       /* Original object ID for validation */
};

/* Object flags */
#define CS_OBJ_FLAG_MARKED      (1 << 0)  /* GC mark bit */
#define CS_OBJ_FLAG_FINALIZABLE (1 << 1)  /* Has finalizer */
#define CS_OBJ_FLAG_WEAK_REF    (1 << 2)  /* Is weak reference */
#define CS_OBJ_FLAG_PINNED      (1 << 3)  /* Pinned (can't be moved) */

/* ARC Functions - Core */
CSObject *CS_Retain(CSObject *obj);
void CS_Release(CSObject *obj);
CSObject *CS_Autorelease(CSObject *obj);

/* ARC Functions - Strong References */
void CS_StoreStrong(CSObject **dest, CSObject *src);

/* ARC Functions - Weak References */
CSWeakReference *CS_CreateWeakReference(CSObject *obj);
void CS_StoreWeak(CSWeakReference **dest, CSObject *obj);
CSObject *CS_LoadWeak(CSWeakReference *weak);
void CS_DestroyWeak(CSWeakReference **weak);
void CS_CopyWeak(CSWeakReference **dest, CSWeakReference **src);
void CS_MoveWeak(CSWeakReference **dest, CSWeakReference **src);

/* ARC Functions - Optimized */
CSObject *CS_RetainAutorelease(CSObject *obj);
CSObject *CS_RetainAutoreleaseReturnValue(CSObject *obj);

/* Object allocation */
CSObject *CS_AllocObject(size_t size, uint32_t type_id);
void CS_DeallocObject(CSObject *obj);

/* Autorelease pool */
void CS_AutoreleasePoolPush(void);
void CS_AutoreleasePoolPop(void);

/* ARC statistics */
struct cs_arc_stats {
	uint64_t total_retains;
	uint64_t total_releases;
	uint64_t total_autoreleases;
	uint64_t objects_allocated;
	uint64_t objects_deallocated;
	uint64_t peak_object_count;
	uint64_t current_object_count;
	uint64_t weak_references_created;
	uint64_t weak_references_destroyed;
};

void CS_ARC_GetStats(struct cs_arc_stats *stats);
void CS_ARC_ResetStats(void);
void CS_ARC_PrintStats(void);

/* Memory debugging */
void CS_ARC_EnableDebug(int enable);
void CS_ARC_DumpLiveObjects(void);
int CS_ARC_ValidateHeap(void);

#endif /* _CSRUNTIME_ARC_H_ */
