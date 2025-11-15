/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Cooperative Threading Implementation
 *
 * Provides real C11 threading on platforms without native thread support
 * using cooperative multitasking with setjmp/longjmp and a scheduler.
 */

#ifndef THREADS_COOP_H
#define THREADS_COOP_H

#include "../threads.h"
#include <setjmp.h>

/* Thread states */
#define THREAD_STATE_READY	0
#define THREAD_STATE_RUNNING	1
#define THREAD_STATE_BLOCKED	2
#define THREAD_STATE_TERMINATED	3

/* Maximum number of threads */
#ifndef MAX_COOP_THREADS
#define MAX_COOP_THREADS	32
#endif

/* Stack size per thread (8KB default) */
#ifndef COOP_STACK_SIZE
#define COOP_STACK_SIZE		8192
#endif

/* Thread control block */
struct _coop_thread {
	int id;
	int state;
	jmp_buf context;
	char *stack;
	size_t stack_size;
	thrd_start_t func;
	void *arg;
	int result;
	int detached;
};

/* Mutex control block */
struct _coop_mutex {
	int locked;
	int owner_id;
};

/* Condition variable control block */
struct _coop_cond {
	int waiting_count;
	int waiting_threads[MAX_COOP_THREADS];
};

/* Scheduler functions */
void _coop_init_scheduler(void);
int _coop_create_thread(struct _coop_thread **thread, thrd_start_t func, void *arg);
void _coop_yield(void);
void _coop_exit(int result);
void _coop_schedule(void);
struct _coop_thread *_coop_current_thread(void);
void _coop_block_thread(int thread_id);
void _coop_unblock_thread(int thread_id);

/* Timer tick for preemptive scheduling (optional) */
void _coop_timer_tick(void);

#endif /* THREADS_COOP_H */
