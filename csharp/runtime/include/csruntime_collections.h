/*
 * C# Runtime - Collection Support
 */

#ifndef _CSRUNTIME_COLLECTIONS_H_
#define _CSRUNTIME_COLLECTIONS_H_

#include "csruntime_types.h"

/* ========== Arrays ========== */

typedef struct {
	CSObject base;
	int32_t length;
	CSTypeInfo *element_type;
	void *elements;  /* Actual array data */
} CSArray;

/* Array creation */
CSArray *CS_Array_Create(CSTypeInfo *element_type, int32_t length);
CSArray *CS_Array_CreateMultiDim(CSTypeInfo *element_type, int32_t *lengths,
                                  int32_t rank);

/* Array operations */
int32_t CS_Array_Length(CSArray *arr);
int32_t CS_Array_Rank(CSArray *arr);
void *CS_Array_GetElement(CSArray *arr, int32_t index);
void CS_Array_SetElement(CSArray *arr, int32_t index, void *value);
CSArray *CS_Array_Copy(CSArray *arr);
void CS_Array_Clear(CSArray *arr);
void CS_Array_Sort(CSArray *arr);
void CS_Array_Reverse(CSArray *arr);
int32_t CS_Array_IndexOf(CSArray *arr, void *value);

/* ========== List<T> ========== */

typedef struct {
	CSObject base;
	CSTypeInfo *element_type;
	void **items;
	int32_t count;
	int32_t capacity;
} CSList;

/* List operations */
CSList *CS_List_Create(CSTypeInfo *element_type);
void CS_List_Add(CSList *list, void *item);
void CS_List_Insert(CSList *list, int32_t index, void *item);
void CS_List_Remove(CSList *list, void *item);
void CS_List_RemoveAt(CSList *list, int32_t index);
void *CS_List_Get(CSList *list, int32_t index);
void CS_List_Set(CSList *list, int32_t index, void *value);
int32_t CS_List_Count(CSList *list);
void CS_List_Clear(CSList *list);
CSBool CS_List_Contains(CSList *list, void *item);
int32_t CS_List_IndexOf(CSList *list, void *item);

/* ========== Dictionary<TKey, TValue> ========== */

typedef struct CSDictEntry {
	uint32_t hash;
	void *key;
	void *value;
	struct CSDictEntry *next;
} CSDictEntry;

typedef struct {
	CSObject base;
	CSTypeInfo *key_type;
	CSTypeInfo *value_type;
	CSDictEntry **buckets;
	int32_t bucket_count;
	int32_t count;
} CSDictionary;

/* Dictionary operations */
CSDictionary *CS_Dictionary_Create(CSTypeInfo *key_type,
                                    CSTypeInfo *value_type);
void CS_Dictionary_Add(CSDictionary *dict, void *key, void *value);
CSBool CS_Dictionary_TryGetValue(CSDictionary *dict, void *key, void **value);
void CS_Dictionary_Remove(CSDictionary *dict, void *key);
CSBool CS_Dictionary_ContainsKey(CSDictionary *dict, void *key);
int32_t CS_Dictionary_Count(CSDictionary *dict);
void CS_Dictionary_Clear(CSDictionary *dict);

/* ========== IEnumerable/IEnumerator ========== */

typedef struct {
	void *(*GetCurrent)(void *enumerator);
	CSBool (*MoveNext)(void *enumerator);
	void (*Reset)(void *enumerator);
	void (*Dispose)(void *enumerator);
} CSEnumeratorVTable;

typedef struct {
	CSObject base;
	CSEnumeratorVTable *vtable;
	void *state;
} CSEnumerator;

/* Enumerator operations */
CSEnumerator *CS_Enumerator_Create(void *collection);
void *CS_Enumerator_Current(CSEnumerator *enumerator);
CSBool CS_Enumerator_MoveNext(CSEnumerator *enumerator);
void CS_Enumerator_Reset(CSEnumerator *enumerator);
void CS_Enumerator_Dispose(CSEnumerator *enumerator);

/* ========== Stack<T> ========== */

typedef struct {
	CSObject base;
	CSTypeInfo *element_type;
	void **items;
	int32_t count;
	int32_t capacity;
} CSStack;

CSStack *CS_Stack_Create(CSTypeInfo *element_type);
void CS_Stack_Push(CSStack *stack, void *item);
void *CS_Stack_Pop(CSStack *stack);
void *CS_Stack_Peek(CSStack *stack);
int32_t CS_Stack_Count(CSStack *stack);
void CS_Stack_Clear(CSStack *stack);

/* ========== Queue<T> ========== */

typedef struct {
	CSObject base;
	CSTypeInfo *element_type;
	void **items;
	int32_t head;
	int32_t tail;
	int32_t count;
	int32_t capacity;
} CSQueue;

CSQueue *CS_Queue_Create(CSTypeInfo *element_type);
void CS_Queue_Enqueue(CSQueue *queue, void *item);
void *CS_Queue_Dequeue(CSQueue *queue);
void *CS_Queue_Peek(CSQueue *queue);
int32_t CS_Queue_Count(CSQueue *queue);
void CS_Queue_Clear(CSQueue *queue);

#endif /* _CSRUNTIME_COLLECTIONS_H_ */
