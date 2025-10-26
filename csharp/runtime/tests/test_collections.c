/*
 * Test C# Runtime - Collections Functionality
 */

#include <stdio.h>
#include <assert.h>
#include "../include/csruntime.h"

/* Simple type info for testing */
static CSTypeInfo int_type = {
	.kind = CS_TYPE_INT32,
	.size = sizeof(int32_t),
	.name = "Int32",
};

void test_array() {
	printf("Test: Array operations...\n");

	CSArray *arr = CS_Array_Create(&int_type, 5);
	assert(arr != NULL);
	assert(CS_Array_Length(arr) == 5);

	/* Set elements */
	for (int32_t i = 0; i < 5; i++) {
		int32_t value = i * 10;
		CS_Array_SetElement(arr, i, &value);
	}

	/* Get elements */
	for (int32_t i = 0; i < 5; i++) {
		int32_t *value = (int32_t *)CS_Array_GetElement(arr, i);
		assert(value != NULL);
		assert(*value == i * 10);
	}

	/* Reverse */
	CS_Array_Reverse(arr);
	int32_t *first = (int32_t *)CS_Array_GetElement(arr, 0);
	assert(*first == 40);

	/* Copy */
	CSArray *copy = CS_Array_Copy(arr);
	assert(copy != NULL);
	assert(CS_Array_Length(copy) == 5);

	CS_Release((CSObject *)arr);
	CS_Release((CSObject *)copy);

	printf("  PASSED\n");
}

void test_list() {
	printf("Test: List<T> operations...\n");

	CSList *list = CS_List_Create(&int_type);
	assert(list != NULL);
	assert(CS_List_Count(list) == 0);

	/* Add elements */
	for (int32_t i = 0; i < 10; i++) {
		int32_t *value = (int32_t *)CS_Malloc(sizeof(int32_t));
		*value = i * 5;
		CS_List_Add(list, value);
	}

	assert(CS_List_Count(list) == 10);

	/* Get elements */
	for (int32_t i = 0; i < 10; i++) {
		int32_t *value = (int32_t *)CS_List_Get(list, i);
		assert(value != NULL);
		assert(*value == i * 5);
	}

	/* Contains */
	int32_t search = 25;
	int32_t idx = CS_List_IndexOf(list, CS_List_Get(list, 5));
	assert(idx == 5);

	/* Remove */
	CS_List_RemoveAt(list, 5);
	assert(CS_List_Count(list) == 9);

	/* Clear */
	CS_List_Clear(list);
	assert(CS_List_Count(list) == 0);

	CS_Release((CSObject *)list);

	printf("  PASSED\n");
}

void test_dictionary() {
	printf("Test: Dictionary<K,V> operations...\n");

	CSDictionary *dict = CS_Dictionary_Create(&int_type, &int_type);
	assert(dict != NULL);
	assert(CS_Dictionary_Count(dict) == 0);

	/* Add elements */
	for (int32_t i = 0; i < 5; i++) {
		int32_t *key = (int32_t *)CS_Malloc(sizeof(int32_t));
		int32_t *value = (int32_t *)CS_Malloc(sizeof(int32_t));
		*key = i;
		*value = i * 100;
		CS_Dictionary_Add(dict, key, value);
	}

	assert(CS_Dictionary_Count(dict) == 5);

	/* Get values */
	for (int32_t i = 0; i < 5; i++) {
		int32_t *key = (int32_t *)CS_Malloc(sizeof(int32_t));
		*key = i;

		void *value = NULL;
		CSBool found = CS_Dictionary_TryGetValue(dict, key, &value);
		assert(found == 1);
		assert(value != NULL);
		assert(*(int32_t *)value == i * 100);

		CS_Free(key);
	}

	/* Contains key */
	int32_t *test_key = (int32_t *)CS_Malloc(sizeof(int32_t));
	*test_key = 3;
	assert(CS_Dictionary_ContainsKey(dict, test_key) == 1);
	CS_Free(test_key);

	/* Clear */
	CS_Dictionary_Clear(dict);
	assert(CS_Dictionary_Count(dict) == 0);

	CS_Release((CSObject *)dict);

	printf("  PASSED\n");
}

void test_stack() {
	printf("Test: Stack<T> operations...\n");

	CSStack *stack = CS_Stack_Create(&int_type);
	assert(stack != NULL);
	assert(CS_Stack_Count(stack) == 0);

	/* Push elements */
	for (int32_t i = 0; i < 5; i++) {
		int32_t *value = (int32_t *)CS_Malloc(sizeof(int32_t));
		*value = i;
		CS_Stack_Push(stack, value);
	}

	assert(CS_Stack_Count(stack) == 5);

	/* Peek */
	int32_t *top = (int32_t *)CS_Stack_Peek(stack);
	assert(top != NULL);
	assert(*top == 4);
	assert(CS_Stack_Count(stack) == 5);

	/* Pop */
	for (int32_t i = 4; i >= 0; i--) {
		int32_t *value = (int32_t *)CS_Stack_Pop(stack);
		assert(value != NULL);
		assert(*value == i);
		CS_Free(value);
	}

	assert(CS_Stack_Count(stack) == 0);

	CS_Release((CSObject *)stack);

	printf("  PASSED\n");
}

void test_queue() {
	printf("Test: Queue<T> operations...\n");

	CSQueue *queue = CS_Queue_Create(&int_type);
	assert(queue != NULL);
	assert(CS_Queue_Count(queue) == 0);

	/* Enqueue elements */
	for (int32_t i = 0; i < 5; i++) {
		int32_t *value = (int32_t *)CS_Malloc(sizeof(int32_t));
		*value = i * 2;
		CS_Queue_Enqueue(queue, value);
	}

	assert(CS_Queue_Count(queue) == 5);

	/* Peek */
	int32_t *front = (int32_t *)CS_Queue_Peek(queue);
	assert(front != NULL);
	assert(*front == 0);
	assert(CS_Queue_Count(queue) == 5);

	/* Dequeue */
	for (int32_t i = 0; i < 5; i++) {
		int32_t *value = (int32_t *)CS_Queue_Dequeue(queue);
		assert(value != NULL);
		assert(*value == i * 2);
		CS_Free(value);
	}

	assert(CS_Queue_Count(queue) == 0);

	CS_Release((CSObject *)queue);

	printf("  PASSED\n");
}

int main() {
	printf("\n========== C# Runtime Collections Tests ==========\n\n");

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
	test_array();
	test_list();
	test_dictionary();
	test_stack();
	test_queue();

	/* Shutdown */
	CSRuntime_Shutdown();

	printf("\n========== All Collections Tests Passed ==========\n\n");
	return 0;
}
