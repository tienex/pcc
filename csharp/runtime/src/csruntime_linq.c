/*
 * C# Runtime - LINQ Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/csruntime.h"

/* Internal enumerator state for LINQ operations */
typedef struct {
	CSEnumerator *source;
	CSPredicateFunc predicate;
	CSSelectorFunc selector;
	int32_t index;
	void *current;
} LINQEnumeratorState;

/* ========== Filtering ========== */

static CSBool where_move_next(void *enumerator) {
	CSEnumerator *e = (CSEnumerator *)enumerator;
	LINQEnumeratorState *state = (LINQEnumeratorState *)e->state;

	while (CS_Enumerator_MoveNext(state->source)) {
		void *item = CS_Enumerator_Current(state->source);
		if (state->predicate(item)) {
			state->current = item;
			return 1;
		}
	}

	return 0;
}

static void *where_get_current(void *enumerator) {
	CSEnumerator *e = (CSEnumerator *)enumerator;
	LINQEnumeratorState *state = (LINQEnumeratorState *)e->state;
	return state->current;
}

static void where_reset(void *enumerator) {
	CSEnumerator *e = (CSEnumerator *)enumerator;
	LINQEnumeratorState *state = (LINQEnumeratorState *)e->state;
	CS_Enumerator_Reset(state->source);
	state->current = NULL;
}

static void where_dispose(void *enumerator) {
	CSEnumerator *e = (CSEnumerator *)enumerator;
	LINQEnumeratorState *state = (LINQEnumeratorState *)e->state;
	if (state->source) CS_Enumerator_Dispose(state->source);
	CS_Free(state);
}

CSEnumerator *CS_LINQ_Where(CSEnumerator *source, CSPredicateFunc predicate) {
	if (!source || !predicate) return NULL;

	CSEnumerator *result = CS_Enumerator_Create(source);
	if (!result) return NULL;

	LINQEnumeratorState *state = (LINQEnumeratorState *)CS_Malloc(sizeof(LINQEnumeratorState));
	if (!state) {
		CS_Release((CSObject *)result);
		return NULL;
	}

	state->source = source;
	state->predicate = predicate;
	state->selector = NULL;
	state->index = 0;
	state->current = NULL;

	CSEnumeratorVTable *vtable = (CSEnumeratorVTable *)CS_Malloc(sizeof(CSEnumeratorVTable));
	vtable->MoveNext = where_move_next;
	vtable->GetCurrent = where_get_current;
	vtable->Reset = where_reset;
	vtable->Dispose = where_dispose;

	result->vtable = vtable;
	result->state = state;

	return result;
}

CSEnumerator *CS_LINQ_OfType(CSEnumerator *source, CSTypeInfo *type) {
	if (!source || !type) return NULL;

	/* Simplified - would need runtime type checking */
	return source;
}

/* ========== Projection ========== */

static CSBool select_move_next(void *enumerator) {
	CSEnumerator *e = (CSEnumerator *)enumerator;
	LINQEnumeratorState *state = (LINQEnumeratorState *)e->state;

	if (CS_Enumerator_MoveNext(state->source)) {
		void *item = CS_Enumerator_Current(state->source);
		state->current = state->selector(item);
		return 1;
	}

	return 0;
}

CSEnumerator *CS_LINQ_Select(CSEnumerator *source, CSSelectorFunc selector) {
	if (!source || !selector) return NULL;

	CSEnumerator *result = CS_Enumerator_Create(source);
	if (!result) return NULL;

	LINQEnumeratorState *state = (LINQEnumeratorState *)CS_Malloc(sizeof(LINQEnumeratorState));
	if (!state) {
		CS_Release((CSObject *)result);
		return NULL;
	}

	state->source = source;
	state->predicate = NULL;
	state->selector = selector;
	state->index = 0;
	state->current = NULL;

	CSEnumeratorVTable *vtable = (CSEnumeratorVTable *)CS_Malloc(sizeof(CSEnumeratorVTable));
	vtable->MoveNext = select_move_next;
	vtable->GetCurrent = where_get_current;
	vtable->Reset = where_reset;
	vtable->Dispose = where_dispose;

	result->vtable = vtable;
	result->state = state;

	return result;
}

CSEnumerator *CS_LINQ_SelectMany(CSEnumerator *source, CSSelectorFunc selector) {
	/* Simplified - would need to flatten nested enumerables */
	return CS_LINQ_Select(source, selector);
}

/* ========== Ordering ========== */

CSEnumerator *CS_LINQ_OrderBy(CSEnumerator *source, CSSelectorFunc keySelector) {
	if (!source || !keySelector) return NULL;

	/* Simplified - real implementation would materialize and sort */
	return source;
}

CSEnumerator *CS_LINQ_OrderByDescending(CSEnumerator *source,
                                         CSSelectorFunc keySelector) {
	if (!source || !keySelector) return NULL;

	/* Simplified - real implementation would materialize and sort descending */
	return source;
}

CSEnumerator *CS_LINQ_ThenBy(CSEnumerator *source, CSSelectorFunc keySelector) {
	return CS_LINQ_OrderBy(source, keySelector);
}

CSEnumerator *CS_LINQ_ThenByDescending(CSEnumerator *source,
                                        CSSelectorFunc keySelector) {
	return CS_LINQ_OrderByDescending(source, keySelector);
}

/* ========== Quantifiers ========== */

CSBool CS_LINQ_All(CSEnumerator *source, CSPredicateFunc predicate) {
	if (!source || !predicate) return 0;

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		if (!predicate(item)) {
			return 0;
		}
	}

	return 1;
}

CSBool CS_LINQ_Any(CSEnumerator *source, CSPredicateFunc predicate) {
	if (!source) return 0;

	if (!predicate) {
		/* Any() without predicate - check if source has any elements */
		return CS_Enumerator_MoveNext(source);
	}

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		if (predicate(item)) {
			return 1;
		}
	}

	return 0;
}

CSBool CS_LINQ_Contains(CSEnumerator *source, void *value) {
	if (!source) return 0;

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		if (item == value) {
			return 1;
		}
	}

	return 0;
}

/* ========== Aggregation ========== */

int32_t CS_LINQ_Count(CSEnumerator *source) {
	if (!source) return 0;

	int32_t count = 0;
	while (CS_Enumerator_MoveNext(source)) {
		count++;
	}

	return count;
}

int32_t CS_LINQ_CountPredicate(CSEnumerator *source, CSPredicateFunc predicate) {
	if (!source || !predicate) return 0;

	int32_t count = 0;
	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		if (predicate(item)) {
			count++;
		}
	}

	return count;
}

void *CS_LINQ_Sum(CSEnumerator *source) {
	/* Simplified - would need type-specific sum implementation */
	if (!source) return NULL;

	int64_t sum = 0;
	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		/* Assuming int64_t values */
		sum += *(int64_t *)item;
	}

	int64_t *result = (int64_t *)CS_Malloc(sizeof(int64_t));
	*result = sum;
	return result;
}

void *CS_LINQ_Average(CSEnumerator *source) {
	if (!source) return NULL;

	int64_t sum = 0;
	int32_t count = 0;

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		sum += *(int64_t *)item;
		count++;
	}

	if (count == 0) return NULL;

	double *result = (double *)CS_Malloc(sizeof(double));
	*result = (double)sum / count;
	return result;
}

void *CS_LINQ_Min(CSEnumerator *source) {
	if (!source) return NULL;

	int64_t min = INT64_MAX;
	CSBool found = 0;

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		int64_t value = *(int64_t *)item;
		if (!found || value < min) {
			min = value;
			found = 1;
		}
	}

	if (!found) return NULL;

	int64_t *result = (int64_t *)CS_Malloc(sizeof(int64_t));
	*result = min;
	return result;
}

void *CS_LINQ_Max(CSEnumerator *source) {
	if (!source) return NULL;

	int64_t max = INT64_MIN;
	CSBool found = 0;

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		int64_t value = *(int64_t *)item;
		if (!found || value > max) {
			max = value;
			found = 1;
		}
	}

	if (!found) return NULL;

	int64_t *result = (int64_t *)CS_Malloc(sizeof(int64_t));
	*result = max;
	return result;
}

/* ========== Element Operations ========== */

void *CS_LINQ_First(CSEnumerator *source) {
	if (!source || !CS_Enumerator_MoveNext(source)) {
		return NULL;
	}

	return CS_Enumerator_Current(source);
}

void *CS_LINQ_FirstOrDefault(CSEnumerator *source) {
	return CS_LINQ_First(source);
}

void *CS_LINQ_Last(CSEnumerator *source) {
	if (!source) return NULL;

	void *last = NULL;
	while (CS_Enumerator_MoveNext(source)) {
		last = CS_Enumerator_Current(source);
	}

	return last;
}

void *CS_LINQ_LastOrDefault(CSEnumerator *source) {
	return CS_LINQ_Last(source);
}

void *CS_LINQ_Single(CSEnumerator *source) {
	if (!source || !CS_Enumerator_MoveNext(source)) {
		return NULL;
	}

	void *result = CS_Enumerator_Current(source);

	/* Ensure there's only one element */
	if (CS_Enumerator_MoveNext(source)) {
		return NULL; /* More than one element */
	}

	return result;
}

void *CS_LINQ_SingleOrDefault(CSEnumerator *source) {
	return CS_LINQ_Single(source);
}

void *CS_LINQ_ElementAt(CSEnumerator *source, int32_t index) {
	if (!source || index < 0) return NULL;

	int32_t current_index = 0;
	while (CS_Enumerator_MoveNext(source)) {
		if (current_index == index) {
			return CS_Enumerator_Current(source);
		}
		current_index++;
	}

	return NULL;
}

/* ========== Set Operations ========== */

CSEnumerator *CS_LINQ_Distinct(CSEnumerator *source) {
	/* Simplified - would need hash set to track seen values */
	return source;
}

CSEnumerator *CS_LINQ_Union(CSEnumerator *first, CSEnumerator *second) {
	/* Simplified - would need to concatenate and deduplicate */
	return first;
}

CSEnumerator *CS_LINQ_Intersect(CSEnumerator *first, CSEnumerator *second) {
	/* Simplified - would need hash set intersection */
	return first;
}

CSEnumerator *CS_LINQ_Except(CSEnumerator *first, CSEnumerator *second) {
	/* Simplified - would need hash set difference */
	return first;
}

/* ========== Partitioning ========== */

static CSBool take_move_next(void *enumerator) {
	CSEnumerator *e = (CSEnumerator *)enumerator;
	LINQEnumeratorState *state = (LINQEnumeratorState *)e->state;

	if (state->index >= *(int32_t *)&state->predicate) {
		return 0;
	}

	if (CS_Enumerator_MoveNext(state->source)) {
		state->current = CS_Enumerator_Current(state->source);
		state->index++;
		return 1;
	}

	return 0;
}

CSEnumerator *CS_LINQ_Take(CSEnumerator *source, int32_t count) {
	if (!source || count < 0) return NULL;

	CSEnumerator *result = CS_Enumerator_Create(source);
	if (!result) return NULL;

	LINQEnumeratorState *state = (LINQEnumeratorState *)CS_Malloc(sizeof(LINQEnumeratorState));
	if (!state) {
		CS_Release((CSObject *)result);
		return NULL;
	}

	state->source = source;
	*(int32_t *)&state->predicate = count; /* Store count in predicate field */
	state->selector = NULL;
	state->index = 0;
	state->current = NULL;

	CSEnumeratorVTable *vtable = (CSEnumeratorVTable *)CS_Malloc(sizeof(CSEnumeratorVTable));
	vtable->MoveNext = take_move_next;
	vtable->GetCurrent = where_get_current;
	vtable->Reset = where_reset;
	vtable->Dispose = where_dispose;

	result->vtable = vtable;
	result->state = state;

	return result;
}

CSEnumerator *CS_LINQ_Skip(CSEnumerator *source, int32_t count) {
	if (!source || count < 0) return NULL;

	/* Skip the first 'count' elements */
	for (int32_t i = 0; i < count && CS_Enumerator_MoveNext(source); i++) {
		/* Skip */
	}

	return source;
}

CSEnumerator *CS_LINQ_TakeWhile(CSEnumerator *source, CSPredicateFunc predicate) {
	/* Simplified - would need special enumerator that stops when predicate fails */
	return CS_LINQ_Where(source, predicate);
}

CSEnumerator *CS_LINQ_SkipWhile(CSEnumerator *source, CSPredicateFunc predicate) {
	if (!source || !predicate) return NULL;

	/* Skip elements while predicate is true */
	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		if (!predicate(item)) {
			/* Reset to allow this item to be read again */
			break;
		}
	}

	return source;
}

/* ========== Joining ========== */

CSEnumerator *CS_LINQ_Join(CSEnumerator *outer, CSEnumerator *inner,
                           CSSelectorFunc outerKeySelector,
                           CSSelectorFunc innerKeySelector,
                           CSSelectorFunc resultSelector) {
	/* Simplified - real implementation would build hash lookup */
	return outer;
}

CSEnumerator *CS_LINQ_GroupJoin(CSEnumerator *outer, CSEnumerator *inner,
                                CSSelectorFunc outerKeySelector,
                                CSSelectorFunc innerKeySelector,
                                CSSelectorFunc resultSelector) {
	/* Simplified - real implementation would group inner elements */
	return outer;
}

/* ========== Grouping ========== */

CSEnumerator *CS_LINQ_GroupBy(CSEnumerator *source,
                              CSSelectorFunc keySelector) {
	/* Simplified - real implementation would create groupings */
	return source;
}

/* ========== Conversion ========== */

CSArray *CS_LINQ_ToArray(CSEnumerator *source, CSTypeInfo *element_type) {
	if (!source || !element_type) return NULL;

	/* Count elements first */
	int32_t count = 0;
	while (CS_Enumerator_MoveNext(source)) {
		count++;
	}

	/* Create array */
	CSArray *array = CS_Array_Create(element_type, count);
	if (!array) return NULL;

	/* Reset and fill array */
	CS_Enumerator_Reset(source);
	int32_t index = 0;
	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		CS_Array_SetElement(array, index++, item);
	}

	return array;
}

CSList *CS_LINQ_ToList(CSEnumerator *source, CSTypeInfo *element_type) {
	if (!source || !element_type) return NULL;

	CSList *list = CS_List_Create(element_type);
	if (!list) return NULL;

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		CS_List_Add(list, item);
	}

	return list;
}

CSDictionary *CS_LINQ_ToDictionary(CSEnumerator *source,
                                    CSSelectorFunc keySelector,
                                    CSSelectorFunc valueSelector) {
	if (!source || !keySelector || !valueSelector) return NULL;

	/* Simplified - would need type info */
	CSDictionary *dict = CS_Dictionary_Create(NULL, NULL);
	if (!dict) return NULL;

	while (CS_Enumerator_MoveNext(source)) {
		void *item = CS_Enumerator_Current(source);
		void *key = keySelector(item);
		void *value = valueSelector(item);
		CS_Dictionary_Add(dict, key, value);
	}

	return dict;
}

/* ========== Concatenation ========== */

CSEnumerator *CS_LINQ_Concat(CSEnumerator *first, CSEnumerator *second) {
	/* Simplified - would need composite enumerator */
	return first;
}

/* ========== Generation ========== */

CSEnumerator *CS_LINQ_Range(int32_t start, int32_t count) {
	/* Simplified - would need range enumerator */
	return NULL;
}

CSEnumerator *CS_LINQ_Repeat(void *element, int32_t count) {
	/* Simplified - would need repeat enumerator */
	return NULL;
}

CSEnumerator *CS_LINQ_Empty(CSTypeInfo *element_type) {
	/* Simplified - would need empty enumerator */
	return NULL;
}

/* ========== Expression Trees ========== */

CSExpression *CS_Expression_Constant(void *value, CSTypeInfo *type) {
	CSExpression *expr = (CSExpression *)CS_Malloc(sizeof(CSExpression));
	if (!expr) return NULL;

	expr->type = CS_EXPR_CONSTANT;
	expr->result_type = type;
	expr->data = value;

	return expr;
}

CSExpression *CS_Expression_Parameter(const char *name, CSTypeInfo *type) {
	CSExpression *expr = (CSExpression *)CS_Malloc(sizeof(CSExpression));
	if (!expr) return NULL;

	expr->type = CS_EXPR_PARAMETER;
	expr->result_type = type;
	expr->data = (void *)name;

	return expr;
}

CSExpression *CS_Expression_MemberAccess(CSExpression *obj, const char *member) {
	CSExpression *expr = (CSExpression *)CS_Malloc(sizeof(CSExpression));
	if (!expr) return NULL;

	expr->type = CS_EXPR_MEMBER_ACCESS;
	expr->result_type = NULL; /* Would need member type info */
	expr->data = (void *)member;

	return expr;
}

CSExpression *CS_Expression_Call(CSExpression *obj, const char *method,
                                 CSExpression **args, int32_t arg_count) {
	CSExpression *expr = (CSExpression *)CS_Malloc(sizeof(CSExpression));
	if (!expr) return NULL;

	expr->type = CS_EXPR_METHOD_CALL;
	expr->result_type = NULL; /* Would need method return type */
	expr->data = (void *)method;

	return expr;
}

CSExpression *CS_Expression_Lambda(CSExpression *body,
                                   CSExpression **parameters,
                                   int32_t param_count) {
	CSExpression *expr = (CSExpression *)CS_Malloc(sizeof(CSExpression));
	if (!expr) return NULL;

	expr->type = CS_EXPR_LAMBDA;
	expr->result_type = NULL; /* Would need delegate type */
	expr->data = body;

	return expr;
}

void *CS_Expression_Compile(CSExpression *expr) {
	/* Simplified - real implementation would compile expression tree to function */
	return NULL;
}
