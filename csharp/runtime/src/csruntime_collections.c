/*
 * C# Runtime - Collections Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/csruntime.h"

/* Default capacities */
#define DEFAULT_LIST_CAPACITY 4
#define DEFAULT_STACK_CAPACITY 4
#define DEFAULT_QUEUE_CAPACITY 4
#define DEFAULT_DICT_BUCKETS 16

/* ========== Arrays ========== */

CSArray *CS_Array_Create(CSTypeInfo *element_type, int32_t length) {
	if (!element_type || length < 0) return NULL;

	CSArray *arr = (CSArray *)CS_AllocObject(sizeof(CSArray), 1);
	if (!arr) return NULL;

	size_t element_size = element_type->size;
	arr->elements = CS_Calloc(length, element_size);
	if (!arr->elements && length > 0) {
		CS_Release((CSObject *)arr);
		return NULL;
	}

	arr->length = length;
	arr->element_type = element_type;

	return arr;
}

CSArray *CS_Array_CreateMultiDim(CSTypeInfo *element_type, int32_t *lengths,
                                  int32_t rank) {
	if (!element_type || !lengths || rank <= 0) return NULL;

	/* Calculate total length */
	int32_t total_length = 1;
	for (int32_t i = 0; i < rank; i++) {
		total_length *= lengths[i];
	}

	/* For now, just create a flat array (full multi-dim would need more metadata) */
	return CS_Array_Create(element_type, total_length);
}

int32_t CS_Array_Length(CSArray *arr) {
	return arr ? arr->length : 0;
}

int32_t CS_Array_Rank(CSArray *arr) {
	return arr ? 1 : 0; /* Simplified - always return 1 for now */
}

void *CS_Array_GetElement(CSArray *arr, int32_t index) {
	if (!arr || index < 0 || index >= arr->length) return NULL;

	size_t element_size = arr->element_type->size;
	return (char *)arr->elements + (index * element_size);
}

void CS_Array_SetElement(CSArray *arr, int32_t index, void *value) {
	if (!arr || !value || index < 0 || index >= arr->length) return;

	size_t element_size = arr->element_type->size;
	void *dest = (char *)arr->elements + (index * element_size);
	memcpy(dest, value, element_size);
}

CSArray *CS_Array_Copy(CSArray *arr) {
	if (!arr) return NULL;

	CSArray *copy = CS_Array_Create(arr->element_type, arr->length);
	if (!copy) return NULL;

	size_t total_size = arr->length * arr->element_type->size;
	memcpy(copy->elements, arr->elements, total_size);

	return copy;
}

void CS_Array_Clear(CSArray *arr) {
	if (!arr) return;

	size_t total_size = arr->length * arr->element_type->size;
	memset(arr->elements, 0, total_size);
}

void CS_Array_Sort(CSArray *arr) {
	if (!arr || arr->length <= 1) return;

	/* Simplified - would need comparison function for real implementation */
	/* qsort(arr->elements, arr->length, arr->element_type->size, compare_func); */
}

void CS_Array_Reverse(CSArray *arr) {
	if (!arr || arr->length <= 1) return;

	size_t element_size = arr->element_type->size;
	void *temp = CS_Malloc(element_size);
	if (!temp) return;

	for (int32_t i = 0; i < arr->length / 2; i++) {
		void *left = (char *)arr->elements + (i * element_size);
		void *right = (char *)arr->elements + ((arr->length - 1 - i) * element_size);

		memcpy(temp, left, element_size);
		memcpy(left, right, element_size);
		memcpy(right, temp, element_size);
	}

	CS_Free(temp);
}

int32_t CS_Array_IndexOf(CSArray *arr, void *value) {
	if (!arr || !value) return -1;

	size_t element_size = arr->element_type->size;
	for (int32_t i = 0; i < arr->length; i++) {
		void *element = (char *)arr->elements + (i * element_size);
		if (memcmp(element, value, element_size) == 0) {
			return i;
		}
	}

	return -1;
}

/* ========== List<T> ========== */

CSList *CS_List_Create(CSTypeInfo *element_type) {
	if (!element_type) return NULL;

	CSList *list = (CSList *)CS_AllocObject(sizeof(CSList), 1);
	if (!list) return NULL;

	list->element_type = element_type;
	list->items = (void **)CS_Calloc(DEFAULT_LIST_CAPACITY, sizeof(void *));
	if (!list->items) {
		CS_Release((CSObject *)list);
		return NULL;
	}

	list->count = 0;
	list->capacity = DEFAULT_LIST_CAPACITY;

	return list;
}

static void list_ensure_capacity(CSList *list, int32_t min_capacity) {
	if (list->capacity >= min_capacity) return;

	int32_t new_capacity = list->capacity * 2;
	if (new_capacity < min_capacity) new_capacity = min_capacity;

	void **new_items = (void **)CS_Realloc(list->items, new_capacity * sizeof(void *));
	if (!new_items) return;

	list->items = new_items;
	list->capacity = new_capacity;
}

void CS_List_Add(CSList *list, void *item) {
	if (!list) return;

	list_ensure_capacity(list, list->count + 1);
	list->items[list->count++] = item;

	/* Retain the item if it's a reference type */
	if (item) CS_Retain((CSObject *)item);
}

void CS_List_Insert(CSList *list, int32_t index, void *item) {
	if (!list || index < 0 || index > list->count) return;

	list_ensure_capacity(list, list->count + 1);

	/* Shift items */
	for (int32_t i = list->count; i > index; i--) {
		list->items[i] = list->items[i - 1];
	}

	list->items[index] = item;
	list->count++;

	if (item) CS_Retain((CSObject *)item);
}

void CS_List_Remove(CSList *list, void *item) {
	if (!list) return;

	int32_t index = CS_List_IndexOf(list, item);
	if (index >= 0) {
		CS_List_RemoveAt(list, index);
	}
}

void CS_List_RemoveAt(CSList *list, int32_t index) {
	if (!list || index < 0 || index >= list->count) return;

	void *item = list->items[index];

	/* Shift items */
	for (int32_t i = index; i < list->count - 1; i++) {
		list->items[i] = list->items[i + 1];
	}

	list->count--;
	list->items[list->count] = NULL;

	/* Release the removed item */
	if (item) CS_Release((CSObject *)item);
}

void *CS_List_Get(CSList *list, int32_t index) {
	if (!list || index < 0 || index >= list->count) return NULL;
	return list->items[index];
}

void CS_List_Set(CSList *list, int32_t index, void *value) {
	if (!list || index < 0 || index >= list->count) return;

	void *old_value = list->items[index];
	list->items[index] = value;

	if (value) CS_Retain((CSObject *)value);
	if (old_value) CS_Release((CSObject *)old_value);
}

int32_t CS_List_Count(CSList *list) {
	return list ? list->count : 0;
}

void CS_List_Clear(CSList *list) {
	if (!list) return;

	/* Release all items */
	for (int32_t i = 0; i < list->count; i++) {
		if (list->items[i]) {
			CS_Release((CSObject *)list->items[i]);
			list->items[i] = NULL;
		}
	}

	list->count = 0;
}

CSBool CS_List_Contains(CSList *list, void *item) {
	return CS_List_IndexOf(list, item) >= 0;
}

int32_t CS_List_IndexOf(CSList *list, void *item) {
	if (!list) return -1;

	for (int32_t i = 0; i < list->count; i++) {
		if (list->items[i] == item) {
			return i;
		}
	}

	return -1;
}

/* ========== Dictionary<TKey, TValue> ========== */

static uint32_t dict_hash(void *key) {
	/* Simple hash function - in real implementation would use key type's GetHashCode */
	uintptr_t ptr = (uintptr_t)key;
	return (uint32_t)(ptr ^ (ptr >> 32));
}

CSDictionary *CS_Dictionary_Create(CSTypeInfo *key_type,
                                    CSTypeInfo *value_type) {
	if (!key_type || !value_type) return NULL;

	CSDictionary *dict = (CSDictionary *)CS_AllocObject(sizeof(CSDictionary), 1);
	if (!dict) return NULL;

	dict->key_type = key_type;
	dict->value_type = value_type;
	dict->buckets = (CSDictEntry **)CS_Calloc(DEFAULT_DICT_BUCKETS,
	                                           sizeof(CSDictEntry *));
	if (!dict->buckets) {
		CS_Release((CSObject *)dict);
		return NULL;
	}

	dict->bucket_count = DEFAULT_DICT_BUCKETS;
	dict->count = 0;

	return dict;
}

void CS_Dictionary_Add(CSDictionary *dict, void *key, void *value) {
	if (!dict || !key) return;

	uint32_t hash = dict_hash(key);
	int32_t bucket = hash % dict->bucket_count;

	/* Check if key already exists */
	CSDictEntry *entry = dict->buckets[bucket];
	while (entry) {
		if (entry->key == key) {
			/* Update existing value */
			if (entry->value) CS_Release((CSObject *)entry->value);
			entry->value = value;
			if (value) CS_Retain((CSObject *)value);
			return;
		}
		entry = entry->next;
	}

	/* Add new entry */
	CSDictEntry *new_entry = (CSDictEntry *)CS_Malloc(sizeof(CSDictEntry));
	if (!new_entry) return;

	new_entry->hash = hash;
	new_entry->key = key;
	new_entry->value = value;
	new_entry->next = dict->buckets[bucket];

	if (key) CS_Retain((CSObject *)key);
	if (value) CS_Retain((CSObject *)value);

	dict->buckets[bucket] = new_entry;
	dict->count++;
}

CSBool CS_Dictionary_TryGetValue(CSDictionary *dict, void *key, void **value) {
	if (!dict || !key) return 0;

	uint32_t hash = dict_hash(key);
	int32_t bucket = hash % dict->bucket_count;

	CSDictEntry *entry = dict->buckets[bucket];
	while (entry) {
		if (entry->key == key) {
			if (value) *value = entry->value;
			return 1;
		}
		entry = entry->next;
	}

	return 0;
}

void CS_Dictionary_Remove(CSDictionary *dict, void *key) {
	if (!dict || !key) return;

	uint32_t hash = dict_hash(key);
	int32_t bucket = hash % dict->bucket_count;

	CSDictEntry *entry = dict->buckets[bucket];
	CSDictEntry *prev = NULL;

	while (entry) {
		if (entry->key == key) {
			if (prev) {
				prev->next = entry->next;
			} else {
				dict->buckets[bucket] = entry->next;
			}

			if (entry->key) CS_Release((CSObject *)entry->key);
			if (entry->value) CS_Release((CSObject *)entry->value);
			CS_Free(entry);

			dict->count--;
			return;
		}
		prev = entry;
		entry = entry->next;
	}
}

CSBool CS_Dictionary_ContainsKey(CSDictionary *dict, void *key) {
	return CS_Dictionary_TryGetValue(dict, key, NULL);
}

int32_t CS_Dictionary_Count(CSDictionary *dict) {
	return dict ? dict->count : 0;
}

void CS_Dictionary_Clear(CSDictionary *dict) {
	if (!dict) return;

	for (int32_t i = 0; i < dict->bucket_count; i++) {
		CSDictEntry *entry = dict->buckets[i];
		while (entry) {
			CSDictEntry *next = entry->next;
			if (entry->key) CS_Release((CSObject *)entry->key);
			if (entry->value) CS_Release((CSObject *)entry->value);
			CS_Free(entry);
			entry = next;
		}
		dict->buckets[i] = NULL;
	}

	dict->count = 0;
}

/* ========== IEnumerable/IEnumerator ========== */

CSEnumerator *CS_Enumerator_Create(void *collection) {
	if (!collection) return NULL;

	CSEnumerator *enumerator = (CSEnumerator *)CS_AllocObject(sizeof(CSEnumerator), 1);
	if (!enumerator) return NULL;

	enumerator->state = collection;
	/* vtable would be set by specific collection type */

	return enumerator;
}

void *CS_Enumerator_Current(CSEnumerator *enumerator) {
	if (!enumerator || !enumerator->vtable || !enumerator->vtable->GetCurrent) {
		return NULL;
	}

	return enumerator->vtable->GetCurrent(enumerator);
}

CSBool CS_Enumerator_MoveNext(CSEnumerator *enumerator) {
	if (!enumerator || !enumerator->vtable || !enumerator->vtable->MoveNext) {
		return 0;
	}

	return enumerator->vtable->MoveNext(enumerator);
}

void CS_Enumerator_Reset(CSEnumerator *enumerator) {
	if (!enumerator || !enumerator->vtable || !enumerator->vtable->Reset) {
		return;
	}

	enumerator->vtable->Reset(enumerator);
}

void CS_Enumerator_Dispose(CSEnumerator *enumerator) {
	if (!enumerator || !enumerator->vtable || !enumerator->vtable->Dispose) {
		return;
	}

	enumerator->vtable->Dispose(enumerator);
}

/* ========== Stack<T> ========== */

CSStack *CS_Stack_Create(CSTypeInfo *element_type) {
	if (!element_type) return NULL;

	CSStack *stack = (CSStack *)CS_AllocObject(sizeof(CSStack), 1);
	if (!stack) return NULL;

	stack->element_type = element_type;
	stack->items = (void **)CS_Calloc(DEFAULT_STACK_CAPACITY, sizeof(void *));
	if (!stack->items) {
		CS_Release((CSObject *)stack);
		return NULL;
	}

	stack->count = 0;
	stack->capacity = DEFAULT_STACK_CAPACITY;

	return stack;
}

static void stack_ensure_capacity(CSStack *stack, int32_t min_capacity) {
	if (stack->capacity >= min_capacity) return;

	int32_t new_capacity = stack->capacity * 2;
	if (new_capacity < min_capacity) new_capacity = min_capacity;

	void **new_items = (void **)CS_Realloc(stack->items, new_capacity * sizeof(void *));
	if (!new_items) return;

	stack->items = new_items;
	stack->capacity = new_capacity;
}

void CS_Stack_Push(CSStack *stack, void *item) {
	if (!stack) return;

	stack_ensure_capacity(stack, stack->count + 1);
	stack->items[stack->count++] = item;

	if (item) CS_Retain((CSObject *)item);
}

void *CS_Stack_Pop(CSStack *stack) {
	if (!stack || stack->count == 0) return NULL;

	void *item = stack->items[--stack->count];
	stack->items[stack->count] = NULL;

	/* Note: Caller is responsible for releasing if needed */
	return item;
}

void *CS_Stack_Peek(CSStack *stack) {
	if (!stack || stack->count == 0) return NULL;
	return stack->items[stack->count - 1];
}

int32_t CS_Stack_Count(CSStack *stack) {
	return stack ? stack->count : 0;
}

void CS_Stack_Clear(CSStack *stack) {
	if (!stack) return;

	for (int32_t i = 0; i < stack->count; i++) {
		if (stack->items[i]) {
			CS_Release((CSObject *)stack->items[i]);
			stack->items[i] = NULL;
		}
	}

	stack->count = 0;
}

/* ========== Queue<T> ========== */

CSQueue *CS_Queue_Create(CSTypeInfo *element_type) {
	if (!element_type) return NULL;

	CSQueue *queue = (CSQueue *)CS_AllocObject(sizeof(CSQueue), 1);
	if (!queue) return NULL;

	queue->element_type = element_type;
	queue->items = (void **)CS_Calloc(DEFAULT_QUEUE_CAPACITY, sizeof(void *));
	if (!queue->items) {
		CS_Release((CSObject *)queue);
		return NULL;
	}

	queue->head = 0;
	queue->tail = 0;
	queue->count = 0;
	queue->capacity = DEFAULT_QUEUE_CAPACITY;

	return queue;
}

static void queue_ensure_capacity(CSQueue *queue, int32_t min_capacity) {
	if (queue->capacity >= min_capacity) return;

	int32_t new_capacity = queue->capacity * 2;
	if (new_capacity < min_capacity) new_capacity = min_capacity;

	void **new_items = (void **)CS_Calloc(new_capacity, sizeof(void *));
	if (!new_items) return;

	/* Copy items in order */
	for (int32_t i = 0; i < queue->count; i++) {
		int32_t index = (queue->head + i) % queue->capacity;
		new_items[i] = queue->items[index];
	}

	CS_Free(queue->items);
	queue->items = new_items;
	queue->head = 0;
	queue->tail = queue->count;
	queue->capacity = new_capacity;
}

void CS_Queue_Enqueue(CSQueue *queue, void *item) {
	if (!queue) return;

	queue_ensure_capacity(queue, queue->count + 1);

	queue->items[queue->tail] = item;
	queue->tail = (queue->tail + 1) % queue->capacity;
	queue->count++;

	if (item) CS_Retain((CSObject *)item);
}

void *CS_Queue_Dequeue(CSQueue *queue) {
	if (!queue || queue->count == 0) return NULL;

	void *item = queue->items[queue->head];
	queue->items[queue->head] = NULL;
	queue->head = (queue->head + 1) % queue->capacity;
	queue->count--;

	/* Note: Caller is responsible for releasing if needed */
	return item;
}

void *CS_Queue_Peek(CSQueue *queue) {
	if (!queue || queue->count == 0) return NULL;
	return queue->items[queue->head];
}

int32_t CS_Queue_Count(CSQueue *queue) {
	return queue ? queue->count : 0;
}

void CS_Queue_Clear(CSQueue *queue) {
	if (!queue) return;

	while (queue->count > 0) {
		void *item = CS_Queue_Dequeue(queue);
		if (item) CS_Release((CSObject *)item);
	}
}
