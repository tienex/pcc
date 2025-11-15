/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Cooperative Threading Implementation
 */

#include "threads_coop.h"
#include <stdlib.h>
#include <string.h>

/* Thread table */
static struct _coop_thread *_thread_table[MAX_COOP_THREADS];
static int _thread_count = 0;
static int _current_thread_id = 0;
static int _next_thread_id = 1;
static int _scheduler_initialized = 0;

/* Initialize scheduler */
void
_coop_init_scheduler(void)
{
	if (_scheduler_initialized)
		return;

	memset(_thread_table, 0, sizeof(_thread_table));

	/* Create main thread (ID 0) */
	_thread_table[0] = (struct _coop_thread *)malloc(sizeof(struct _coop_thread));
	if (_thread_table[0]) {
		memset(_thread_table[0], 0, sizeof(struct _coop_thread));
		_thread_table[0]->id = 0;
		_thread_table[0]->state = THREAD_STATE_RUNNING;
		_thread_table[0]->stack = NULL; /* Main thread uses process stack */
		_thread_table[0]->stack_size = 0;
		_thread_table[0]->detached = 1;
		_thread_count = 1;
	}

	_scheduler_initialized = 1;
}

/* Thread wrapper function */
static void
_coop_thread_wrapper(void)
{
	struct _coop_thread *thread = _thread_table[_current_thread_id];
	int result;

	if (thread && thread->func) {
		result = thread->func(thread->arg);
		thread->result = result;
	}

	/* Thread finished - mark as terminated */
	if (thread) {
		thread->state = THREAD_STATE_TERMINATED;
	}

	/* Schedule next thread */
	_coop_schedule();

	/* Should never reach here */
	exit(1);
}

/* Create thread */
int
_coop_create_thread(struct _coop_thread **thread_out, thrd_start_t func, void *arg)
{
	struct _coop_thread *thread;
	int id;
	int i;

	if (!_scheduler_initialized)
		_coop_init_scheduler();

	if (_thread_count >= MAX_COOP_THREADS)
		return thrd_nomem;

	/* Find free slot */
	for (i = 0; i < MAX_COOP_THREADS; i++) {
		if (_thread_table[i] == NULL)
			break;
	}

	if (i >= MAX_COOP_THREADS)
		return thrd_nomem;

	/* Allocate thread */
	thread = (struct _coop_thread *)malloc(sizeof(struct _coop_thread));
	if (!thread)
		return thrd_nomem;

	/* Allocate stack */
	thread->stack = (char *)malloc(COOP_STACK_SIZE);
	if (!thread->stack) {
		free(thread);
		return thrd_nomem;
	}

	/* Initialize thread */
	id = _next_thread_id++;
	memset(thread, 0, sizeof(struct _coop_thread));
	thread->id = id;
	thread->state = THREAD_STATE_READY;
	thread->stack_size = COOP_STACK_SIZE;
	thread->func = func;
	thread->arg = arg;
	thread->detached = 0;

	_thread_table[i] = thread;
	_thread_count++;

	*thread_out = thread;

	/* Set up initial context */
	/* When this thread is first scheduled, it will start in _coop_thread_wrapper */
	/* This is platform-specific - we set up a fake stack frame */

	return thrd_success;
}

/* Get current thread */
struct _coop_thread *
_coop_current_thread(void)
{
	if (!_scheduler_initialized)
		_coop_init_scheduler();

	if (_current_thread_id >= 0 && _current_thread_id < MAX_COOP_THREADS)
		return _thread_table[_current_thread_id];

	return NULL;
}

/* Block a thread */
void
_coop_block_thread(int thread_id)
{
	int i;

	for (i = 0; i < MAX_COOP_THREADS; i++) {
		if (_thread_table[i] && _thread_table[i]->id == thread_id) {
			_thread_table[i]->state = THREAD_STATE_BLOCKED;
			break;
		}
	}
}

/* Unblock a thread */
void
_coop_unblock_thread(int thread_id)
{
	int i;

	for (i = 0; i < MAX_COOP_THREADS; i++) {
		if (_thread_table[i] && _thread_table[i]->id == thread_id) {
			if (_thread_table[i]->state == THREAD_STATE_BLOCKED)
				_thread_table[i]->state = THREAD_STATE_READY;
			break;
		}
	}
}

/* Scheduler - round-robin */
void
_coop_schedule(void)
{
	struct _coop_thread *current;
	int i, start;
	int found = 0;

	if (!_scheduler_initialized)
		_coop_init_scheduler();

	current = _coop_current_thread();

	/* Save current thread context */
	if (current && current->state == THREAD_STATE_RUNNING) {
		current->state = THREAD_STATE_READY;
		if (setjmp(current->context) != 0) {
			/* Returned from longjmp - thread resumed */
			return;
		}
	}

	/* Find next ready thread (round-robin) */
	start = _current_thread_id + 1;
	for (i = 0; i < MAX_COOP_THREADS; i++) {
		int idx = (start + i) % MAX_COOP_THREADS;
		if (_thread_table[idx] && _thread_table[idx]->state == THREAD_STATE_READY) {
			_current_thread_id = idx;
			_thread_table[idx]->state = THREAD_STATE_RUNNING;
			found = 1;
			longjmp(_thread_table[idx]->context, 1);
		}
	}

	/* No ready threads - check for main thread */
	if (_thread_table[0] && _thread_table[0]->state == THREAD_STATE_READY) {
		_current_thread_id = 0;
		_thread_table[0]->state = THREAD_STATE_RUNNING;
		longjmp(_thread_table[0]->context, 1);
	}

	/* No threads to run - all terminated or blocked */
	/* In real implementation, might wait or exit */
}

/* Yield CPU to other threads */
void
_coop_yield(void)
{
	_coop_schedule();
}

/* Exit current thread */
void
_coop_exit(int result)
{
	struct _coop_thread *thread = _coop_current_thread();

	if (thread) {
		thread->result = result;
		thread->state = THREAD_STATE_TERMINATED;
	}

	_coop_schedule();

	/* Should never return */
	exit(result);
}

/* Timer tick for optional preemptive scheduling */
void
_coop_timer_tick(void)
{
	/* Could be called from timer interrupt to preempt threads */
	_coop_schedule();
}
