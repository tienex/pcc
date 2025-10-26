/*
 * Test C# Runtime - Memory Management
 */

#include <stdio.h>
#include <assert.h>
#include "../include/csruntime.h"

void test_malloc_free() {
	printf("Test: Memory allocation and freeing...\n");

	void *ptr1 = CS_Malloc(1024);
	assert(ptr1 != NULL);

	void *ptr2 = CS_Calloc(10, sizeof(int32_t));
	assert(ptr2 != NULL);

	void *ptr3 = CS_Realloc(ptr1, 2048);
	assert(ptr3 != NULL);

	CS_Free(ptr2);
	CS_Free(ptr3);

	printf("  PASSED\n");
}

void test_gc_operations() {
	printf("Test: Garbage collection operations...\n");

	/* Get initial memory */
	int64_t initial = CS_GC_GetTotalMemory(0);
	printf("  Initial memory: %lld bytes\n", (long long)initial);

	/* Allocate some objects */
	for (int i = 0; i < 100; i++) {
		CSObject *obj = CS_AllocObject(128, 1);
		/* Immediately release - should be collected */
		CS_Release(obj);
	}

	/* Force collection */
	CS_GC_Collect();

	/* Get memory after collection */
	int64_t after = CS_GC_GetTotalMemory(1);
	printf("  After GC: %lld bytes\n", (long long)after);

	printf("  PASSED\n");
}

void test_gc_generations() {
	printf("Test: Generational garbage collection...\n");

	/* Collect generation 0 */
	CS_GC_CollectGeneration(0);

	/* Collect generation 1 */
	CS_GC_CollectGeneration(1);

	/* Collect all generations */
	CS_GC_Collect();

	printf("  PASSED\n");
}

void test_gc_handles() {
	printf("Test: GC handles...\n");

	CSObject *obj = CS_AllocObject(64, 1);
	assert(obj != NULL);

	/* Allocate a normal handle */
	CSGCHandle handle = CS_GCHandle_Alloc(obj, 0);
	assert(handle.target != NULL);

	/* Get target */
	CSObject *target = CS_GCHandle_GetTarget(handle);
	assert(target == obj);

	/* Free handle */
	CS_GCHandle_Free(handle);

	/* Object should still be valid (we have a direct reference) */
	CS_Release(obj);

	printf("  PASSED\n");
}

void test_unsafe_operations() {
	printf("Test: Unsafe memory operations...\n");

	uint8_t buffer1[64];
	uint8_t buffer2[64];

	/* Initialize block */
	CS_Unsafe_InitBlock(buffer1, 0xAB, 64);

	/* Verify */
	for (int i = 0; i < 64; i++) {
		assert(buffer1[i] == 0xAB);
	}

	/* Copy block */
	CS_Unsafe_CopyBlock(buffer2, buffer1, 64);

	/* Verify copy */
	for (int i = 0; i < 64; i++) {
		assert(buffer2[i] == 0xAB);
	}

	/* Get pointer */
	void *ptr = CS_Unsafe_AsPointer(buffer1);
	assert(ptr == buffer1);

	printf("  PASSED\n");
}

void test_memory_stats() {
	printf("Test: Memory statistics...\n");

	CSMemoryStats stats;
	CS_Memory_GetStats(&stats);

	printf("  Total allocated: %llu bytes\n",
	       (unsigned long long)stats.total_allocated);
	printf("  Current usage: %llu bytes\n",
	       (unsigned long long)stats.current_usage);
	printf("  Peak usage: %llu bytes\n",
	       (unsigned long long)stats.peak_usage);

	/* Print detailed stats */
	CS_Memory_PrintStats();

	printf("  PASSED\n");
}

int main() {
	printf("\n========== C# Runtime Memory Tests ==========\n\n");

	/* Initialize runtime */
	struct cs_runtime_config config = {
		.initial_heap_size = 1024 * 1024,
		.max_heap_size = 1024 * 1024 * 1024,
		.enable_gc = 1,
		.enable_arc = 1,
		.gc_threshold = 512 * 1024,
		.debug_mode = 0,
	};

	CSRuntime_Init(&config);

	/* Run tests */
	test_malloc_free();
	test_gc_operations();
	test_gc_generations();
	test_gc_handles();
	test_unsafe_operations();
	test_memory_stats();

	/* Shutdown */
	CSRuntime_Shutdown();

	printf("\n========== All Memory Tests Passed ==========\n\n");
	return 0;
}
