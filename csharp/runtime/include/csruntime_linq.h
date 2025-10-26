/*
 * C# Runtime - LINQ Support
 */

#ifndef _CSRUNTIME_LINQ_H_
#define _CSRUNTIME_LINQ_H_

#include "csruntime_types.h"
#include "csruntime_collections.h"

/* Delegate types for LINQ */
typedef CSBool (*CSPredicateFunc)(void *item);
typedef void *(*CSSelectorFunc)(void *item);
typedef int32_t (*CSComparerFunc)(void *a, void *b);

/* ========== LINQ Query Methods ========== */

/* Filtering */
CSEnumerator *CS_LINQ_Where(CSEnumerator *source, CSPredicateFunc predicate);
CSEnumerator *CS_LINQ_OfType(CSEnumerator *source, CSTypeInfo *type);

/* Projection */
CSEnumerator *CS_LINQ_Select(CSEnumerator *source, CSSelectorFunc selector);
CSEnumerator *CS_LINQ_SelectMany(CSEnumerator *source, CSSelectorFunc selector);

/* Ordering */
CSEnumerator *CS_LINQ_OrderBy(CSEnumerator *source, CSSelectorFunc keySelector);
CSEnumerator *CS_LINQ_OrderByDescending(CSEnumerator *source,
                                         CSSelectorFunc keySelector);
CSEnumerator *CS_LINQ_ThenBy(CSEnumerator *source, CSSelectorFunc keySelector);
CSEnumerator *CS_LINQ_ThenByDescending(CSEnumerator *source,
                                        CSSelectorFunc keySelector);

/* Quantifiers */
CSBool CS_LINQ_All(CSEnumerator *source, CSPredicateFunc predicate);
CSBool CS_LINQ_Any(CSEnumerator *source, CSPredicateFunc predicate);
CSBool CS_LINQ_Contains(CSEnumerator *source, void *value);

/* Aggregation */
int32_t CS_LINQ_Count(CSEnumerator *source);
int32_t CS_LINQ_CountPredicate(CSEnumerator *source, CSPredicateFunc predicate);
void *CS_LINQ_Sum(CSEnumerator *source);
void *CS_LINQ_Average(CSEnumerator *source);
void *CS_LINQ_Min(CSEnumerator *source);
void *CS_LINQ_Max(CSEnumerator *source);

/* Element operations */
void *CS_LINQ_First(CSEnumerator *source);
void *CS_LINQ_FirstOrDefault(CSEnumerator *source);
void *CS_LINQ_Last(CSEnumerator *source);
void *CS_LINQ_LastOrDefault(CSEnumerator *source);
void *CS_LINQ_Single(CSEnumerator *source);
void *CS_LINQ_SingleOrDefault(CSEnumerator *source);
void *CS_LINQ_ElementAt(CSEnumerator *source, int32_t index);

/* Set operations */
CSEnumerator *CS_LINQ_Distinct(CSEnumerator *source);
CSEnumerator *CS_LINQ_Union(CSEnumerator *first, CSEnumerator *second);
CSEnumerator *CS_LINQ_Intersect(CSEnumerator *first, CSEnumerator *second);
CSEnumerator *CS_LINQ_Except(CSEnumerator *first, CSEnumerator *second);

/* Partitioning */
CSEnumerator *CS_LINQ_Take(CSEnumerator *source, int32_t count);
CSEnumerator *CS_LINQ_Skip(CSEnumerator *source, int32_t count);
CSEnumerator *CS_LINQ_TakeWhile(CSEnumerator *source, CSPredicateFunc predicate);
CSEnumerator *CS_LINQ_SkipWhile(CSEnumerator *source, CSPredicateFunc predicate);

/* Joining */
CSEnumerator *CS_LINQ_Join(CSEnumerator *outer, CSEnumerator *inner,
                           CSSelectorFunc outerKeySelector,
                           CSSelectorFunc innerKeySelector,
                           CSSelectorFunc resultSelector);
CSEnumerator *CS_LINQ_GroupJoin(CSEnumerator *outer, CSEnumerator *inner,
                                CSSelectorFunc outerKeySelector,
                                CSSelectorFunc innerKeySelector,
                                CSSelectorFunc resultSelector);

/* Grouping */
CSEnumerator *CS_LINQ_GroupBy(CSEnumerator *source,
                              CSSelectorFunc keySelector);

/* Conversion */
CSArray *CS_LINQ_ToArray(CSEnumerator *source, CSTypeInfo *element_type);
CSList *CS_LINQ_ToList(CSEnumerator *source, CSTypeInfo *element_type);
CSDictionary *CS_LINQ_ToDictionary(CSEnumerator *source,
                                    CSSelectorFunc keySelector,
                                    CSSelectorFunc valueSelector);

/* Concatenation */
CSEnumerator *CS_LINQ_Concat(CSEnumerator *first, CSEnumerator *second);

/* Generation */
CSEnumerator *CS_LINQ_Range(int32_t start, int32_t count);
CSEnumerator *CS_LINQ_Repeat(void *element, int32_t count);
CSEnumerator *CS_LINQ_Empty(CSTypeInfo *element_type);

/* ========== LINQ Expression Trees ========== */

typedef enum {
	CS_EXPR_CONSTANT,
	CS_EXPR_PARAMETER,
	CS_EXPR_MEMBER_ACCESS,
	CS_EXPR_METHOD_CALL,
	CS_EXPR_LAMBDA,
	CS_EXPR_BINARY,
	CS_EXPR_UNARY,
} CSExpressionType;

typedef struct CSExpression {
	CSExpressionType type;
	CSTypeInfo *result_type;
	void *data;
} CSExpression;

/* Expression tree creation */
CSExpression *CS_Expression_Constant(void *value, CSTypeInfo *type);
CSExpression *CS_Expression_Parameter(const char *name, CSTypeInfo *type);
CSExpression *CS_Expression_MemberAccess(CSExpression *obj, const char *member);
CSExpression *CS_Expression_Call(CSExpression *obj, const char *method,
                                 CSExpression **args, int32_t arg_count);
CSExpression *CS_Expression_Lambda(CSExpression *body,
                                   CSExpression **parameters,
                                   int32_t param_count);

/* Expression tree compilation */
void *CS_Expression_Compile(CSExpression *expr);

#endif /* _CSRUNTIME_LINQ_H_ */
