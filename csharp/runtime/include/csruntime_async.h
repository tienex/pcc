/*
 * C# Runtime - Async/Await Support
 */

#ifndef _CSRUNTIME_ASYNC_H_
#define _CSRUNTIME_ASYNC_H_

#include "csruntime_types.h"

/* Task state */
typedef enum {
	CS_TASK_CREATED,
	CS_TASK_WAITING_FOR_ACTIVATION,
	CS_TASK_WAITING_TO_RUN,
	CS_TASK_RUNNING,
	CS_TASK_WAITING_FOR_CHILDREN,
	CS_TASK_RAN_TO_COMPLETION,
	CS_TASK_CANCELED,
	CS_TASK_FAULTED,
} CSTaskStatus;

/* Task structure */
typedef struct CSTask {
	CSObject base;
	CSTaskStatus status;
	void *result;
	CSTypeInfo *result_type;
	void *exception;
	CSBool is_completed;
	CSBool is_canceled;
	CSBool is_faulted;
	void (*continuation)(struct CSTask *);
	struct CSTask *continuation_task;
} CSTask;

/* Task<T> operations */
CSTask *CS_Task_Create(void (*action)(void *), void *state);
CSTask *CS_Task_CreateWithResult(void *(*func)(void *), void *state,
                                  CSTypeInfo *result_type);
void CS_Task_Start(CSTask *task);
void CS_Task_Wait(CSTask *task);
CSBool CS_Task_WaitTimeout(CSTask *task, int32_t milliseconds);
void *CS_Task_GetResult(CSTask *task);

/* Async state machine */
typedef struct {
	int32_t state;
	void *builder;
	void *local_vars;
	CSTask *awaiter;
} CSAsyncStateMachine;

/* State machine operations */
CSAsyncStateMachine *CS_AsyncStateMachine_Create(void);
void CS_AsyncStateMachine_MoveNext(CSAsyncStateMachine *sm);
void CS_AsyncStateMachine_SetResult(CSAsyncStateMachine *sm, void *result);
void CS_AsyncStateMachine_SetException(CSAsyncStateMachine *sm, void *exception);

/* Awaiter interface */
typedef struct {
	CSBool (*IsCompleted)(void *awaiter);
	void *(*GetResult)(void *awaiter);
	void (*OnCompleted)(void *awaiter, void (*continuation)(void));
	void (*UnsafeOnCompleted)(void *awaiter, void (*continuation)(void));
} CSAwaiterVTable;

typedef struct {
	CSObject base;
	CSAwaiterVTable *vtable;
	void *state;
} CSAwaiter;

/* Awaiter operations */
CSBool CS_Awaiter_IsCompleted(CSAwaiter *awaiter);
void *CS_Awaiter_GetResult(CSAwaiter *awaiter);
void CS_Awaiter_OnCompleted(CSAwaiter *awaiter, void (*continuation)(void));

/* Task completion source */
typedef struct {
	CSObject base;
	CSTask *task;
} CSTaskCompletionSource;

CSTaskCompletionSource *CS_TaskCompletionSource_Create(CSTypeInfo *result_type);
void CS_TaskCompletionSource_SetResult(CSTaskCompletionSource *tcs,
                                        void *result);
void CS_TaskCompletionSource_SetException(CSTaskCompletionSource *tcs,
                                          void *exception);
void CS_TaskCompletionSource_SetCanceled(CSTaskCompletionSource *tcs);
CSTask *CS_TaskCompletionSource_GetTask(CSTaskCompletionSource *tcs);

/* Task combinators */
CSTask *CS_Task_WhenAll(CSTask **tasks, int32_t count);
CSTask *CS_Task_WhenAny(CSTask **tasks, int32_t count);
CSTask *CS_Task_Delay(int32_t milliseconds);
CSTask *CS_Task_FromResult(void *result, CSTypeInfo *result_type);
CSTask *CS_Task_FromException(void *exception);
CSTask *CS_Task_CompletedTask(void);

/* Async method builder */
typedef struct {
	CSTask *task;
	CSAsyncStateMachine *state_machine;
} CSAsyncTaskMethodBuilder;

CSAsyncTaskMethodBuilder *CS_AsyncTaskMethodBuilder_Create(void);
void CS_AsyncTaskMethodBuilder_Start(CSAsyncTaskMethodBuilder *builder,
                                     CSAsyncStateMachine *sm);
void CS_AsyncTaskMethodBuilder_SetResult(CSAsyncTaskMethodBuilder *builder,
                                         void *result);
void CS_AsyncTaskMethodBuilder_SetException(CSAsyncTaskMethodBuilder *builder,
                                            void *exception);
void CS_AsyncTaskMethodBuilder_AwaitUnsafeOnCompleted(
    CSAsyncTaskMethodBuilder *builder,
    CSAwaiter *awaiter,
    CSAsyncStateMachine *sm);
CSTask *CS_AsyncTaskMethodBuilder_GetTask(CSAsyncTaskMethodBuilder *builder);

/* ValueTask support (C# 7.0+) */
typedef struct {
	CSObject base;
	CSBool has_value;
	void *value;
	CSTask *task;
} CSValueTask;

CSValueTask *CS_ValueTask_Create(void *value);
CSValueTask *CS_ValueTask_FromTask(CSTask *task);
CSBool CS_ValueTask_IsCompleted(CSValueTask *vt);
void *CS_ValueTask_GetResult(CSValueTask *vt);

#endif /* _CSRUNTIME_ASYNC_H_ */
