/*
 * Test C# Runtime - ARC Functionality
 */

#include <stdio.h>
#include <assert.h>
#include "../include/csruntime.h"

void test_basic_retain_release() {
	printf("Test: Basic retain/release...\n");

	CSObject *obj = CS_AllocObject(64, 1);
	assert(obj != NULL);
	assert(obj->header.ref_count == 1);

	CS_Retain(obj);
	assert(obj->header.ref_count == 2);

	CS_Release(obj);
	assert(obj->header.ref_count == 1);

	CS_Release(obj);
	/* Object should be deallocated now */

	printf("  PASSED\n");
}

void test_autorelease() {
	printf("Test: Autorelease...\n");

	CS_AutoreleasePoolPush();

	CSObject *obj = CS_AllocObject(64, 1);
	CS_Autorelease(obj);

	/* Object still alive in autorelease pool */
	assert(obj->header.ref_count == 1);

	CS_AutoreleasePoolPop();

	/* Object should be released and deallocated */

	printf("  PASSED\n");
}

void test_strong_store() {
	printf("Test: Strong store...\n");

	CSObject *obj1 = CS_AllocObject(64, 1);
	CSObject *obj2 = CS_AllocObject(64, 1);
	CSObject *ptr = NULL;

	CS_StoreStrong(&ptr, obj1);
	assert(ptr == obj1);
	assert(obj1->header.ref_count == 2);

	CS_StoreStrong(&ptr, obj2);
	assert(ptr == obj2);
	assert(obj2->header.ref_count == 2);

	CS_StoreStrong(&ptr, NULL);
	assert(ptr == NULL);

	CS_Release(obj1);
	CS_Release(obj2);

	printf("  PASSED\n");
}

void test_weak_reference() {
	printf("Test: Weak reference...\n");

	CSObject *obj = CS_AllocObject(64, 1);
	CSWeakReference *weak = NULL;

	CS_StoreWeak(&weak, obj);
	assert(weak != NULL);
	assert(weak->target == obj);

	CSObject *loaded = CS_LoadWeak(weak);
	assert(loaded == obj);
	assert(loaded->header.ref_count == 2);

	CS_Release(loaded);
	CS_DestroyWeak(&weak);
	CS_Release(obj);

	printf("  PASSED\n");
}

void test_statistics() {
	printf("Test: Statistics...\n");

	CS_ARC_ResetStats();

	CSObject *obj1 = CS_AllocObject(64, 1);
	CSObject *obj2 = CS_AllocObject(64, 1);

	CS_Retain(obj1);
	CS_Retain(obj2);

	CS_Release(obj1);
	CS_Release(obj2);
	CS_Release(obj1);
	CS_Release(obj2);

	struct cs_arc_stats stats;
	CS_ARC_GetStats(&stats);

	assert(stats.total_retains == 2);
	assert(stats.total_releases == 4);
	assert(stats.objects_allocated == 2);
	assert(stats.objects_deallocated == 2);
	assert(stats.current_object_count == 0);

	printf("  PASSED\n");
}

int main() {
	printf("\n========== C# Runtime ARC Tests ==========\n\n");

	/* Initialize runtime */
	struct cs_runtime_config config = {
		.initial_heap_size = 1024 * 1024,
		.max_heap_size = 1024 * 1024 * 1024,
		.enable_gc = 0,
		.enable_arc = 1,
		.gc_threshold = 0,
		.debug_mode = 0,
	};

	CSRuntime_Init(&config);

	/* Run tests */
	test_basic_retain_release();
	test_autorelease();
	test_strong_store();
	test_weak_reference();
	test_statistics();

	/* Print statistics */
	printf("\n");
	CS_ARC_PrintStats();

	/* Shutdown */
	CSRuntime_Shutdown();

	printf("\n========== All Tests Passed ==========\n\n");
	return 0;
}
