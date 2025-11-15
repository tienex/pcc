/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Coroutine and Process/Thread Support
 * - Modula-2 coroutines
 * - Modula-3 threads
 * - Ada tasks
 * - Synchronization primitives (mutex, condition, semaphore)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <pthread.h>
#include <time.h>
#include <errno.h>
#include "wirthrt.h"

/*
 * Process/Thread Structures
 */

struct WirthProcess {
	pthread_t thread;
	WirthProcessProc proc;
	void *arg;
	size_t stack_size;
	int priority;
	int running;
};

struct WirthMutex {
	pthread_mutex_t mutex;
};

struct WirthCondition {
	pthread_cond_t cond;
};

struct WirthSemaphore {
	pthread_mutex_t mutex;
	pthread_cond_t cond;
	int count;
};

/*
 * Coroutine Support (Modula-2)
 *
 * Note: This is a simplified implementation using setjmp/longjmp.
 * A full implementation would use platform-specific context switching.
 */

WirthCoroutine *wirth_coroutine_create(WirthProcessProc proc, void *arg, size_t stack_size) {
	WirthCoroutine *co = (WirthCoroutine *)malloc(sizeof(WirthCoroutine));
	if (!co) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate coroutine");
		return NULL;
	}

	co->stack = malloc(stack_size);
	if (!co->stack) {
		free(co);
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate coroutine stack");
		return NULL;
	}

	co->stack_size = stack_size;
	co->active = 0;

	/* Note: Actual context initialization would require platform-specific code */
	(void)proc;
	(void)arg;

	return co;
}

void wirth_coroutine_transfer(WirthCoroutine *from, WirthCoroutine *to) {
	if (!from || !to) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid coroutine transfer");
		return;
	}

	/* Save current context */
	if (setjmp(from->context) == 0) {
		from->active = 0;
		to->active = 1;
		/* Restore target context */
		longjmp(to->context, 1);
	}
}

void wirth_coroutine_destroy(WirthCoroutine *co) {
	if (co) {
		if (co->stack) {
			free(co->stack);
		}
		free(co);
	}
}

/*
 * Process/Thread Support
 */

static void *thread_wrapper(void *arg) {
	WirthProcess *p = (WirthProcess *)arg;
	if (p && p->proc) {
		p->running = 1;
		p->proc(p->arg);
		p->running = 0;
	}
	return NULL;
}

WirthProcess *wirth_process_create(WirthProcessProc proc, void *arg, size_t stack_size, int priority) {
	WirthProcess *p = (WirthProcess *)malloc(sizeof(WirthProcess));
	if (!p) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate process");
		return NULL;
	}

	p->proc = proc;
	p->arg = arg;
	p->stack_size = stack_size;
	p->priority = priority;
	p->running = 0;

	return p;
}

void wirth_process_start(WirthProcess *p) {
	if (!p) {
		return;
	}

	pthread_attr_t attr;
	pthread_attr_init(&attr);

	if (p->stack_size > 0) {
		pthread_attr_setstacksize(&attr, p->stack_size);
	}

	int result = pthread_create(&p->thread, &attr, thread_wrapper, p);
	pthread_attr_destroy(&attr);

	if (result != 0) {
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Cannot create thread");
	}
}

void wirth_process_suspend(WirthProcess *p) {
	/* Note: POSIX doesn't have direct thread suspension */
	/* This would require a condition variable and cooperative scheduling */
	(void)p;
	wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION,
	                    "Thread suspension not implemented");
}

void wirth_process_resume(WirthProcess *p) {
	/* Note: POSIX doesn't have direct thread resumption */
	(void)p;
	wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION,
	                    "Thread resumption not implemented");
}

void wirth_process_yield(void) {
	sched_yield();
}

void wirth_process_sleep(unsigned int milliseconds) {
	struct timespec ts;
	ts.tv_sec = milliseconds / 1000;
	ts.tv_nsec = (milliseconds % 1000) * 1000000;
	nanosleep(&ts, NULL);
}

/*
 * Mutex Operations
 */

WirthMutex *wirth_mutex_create(void) {
	WirthMutex *m = (WirthMutex *)malloc(sizeof(WirthMutex));
	if (!m) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate mutex");
		return NULL;
	}

	int result = pthread_mutex_init(&m->mutex, NULL);
	if (result != 0) {
		free(m);
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Cannot initialize mutex");
		return NULL;
	}

	return m;
}

void wirth_mutex_lock(WirthMutex *m) {
	if (!m) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid mutex");
		return;
	}

	int result = pthread_mutex_lock(&m->mutex);
	if (result != 0) {
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Mutex lock failed");
	}
}

void wirth_mutex_unlock(WirthMutex *m) {
	if (!m) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid mutex");
		return;
	}

	int result = pthread_mutex_unlock(&m->mutex);
	if (result != 0) {
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Mutex unlock failed");
	}
}

void wirth_mutex_destroy(WirthMutex *m) {
	if (m) {
		pthread_mutex_destroy(&m->mutex);
		free(m);
	}
}

/*
 * Condition Variable Operations
 */

WirthCondition *wirth_condition_create(void) {
	WirthCondition *c = (WirthCondition *)malloc(sizeof(WirthCondition));
	if (!c) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate condition variable");
		return NULL;
	}

	int result = pthread_cond_init(&c->cond, NULL);
	if (result != 0) {
		free(c);
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Cannot initialize condition variable");
		return NULL;
	}

	return c;
}

void wirth_condition_wait(WirthCondition *c, WirthMutex *m) {
	if (!c || !m) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid condition or mutex");
		return;
	}

	int result = pthread_cond_wait(&c->cond, &m->mutex);
	if (result != 0) {
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Condition wait failed");
	}
}

void wirth_condition_signal(WirthCondition *c) {
	if (!c) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid condition variable");
		return;
	}

	int result = pthread_cond_signal(&c->cond);
	if (result != 0) {
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Condition signal failed");
	}
}

void wirth_condition_broadcast(WirthCondition *c) {
	if (!c) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid condition variable");
		return;
	}

	int result = pthread_cond_broadcast(&c->cond);
	if (result != 0) {
		wirth_runtime_error(WIRTH_ERR_TASKING_ERROR, "Condition broadcast failed");
	}
}

void wirth_condition_destroy(WirthCondition *c) {
	if (c) {
		pthread_cond_destroy(&c->cond);
		free(c);
	}
}

/*
 * Semaphore Operations
 */

WirthSemaphore *wirth_semaphore_create(int initial) {
	WirthSemaphore *s = (WirthSemaphore *)malloc(sizeof(WirthSemaphore));
	if (!s) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate semaphore");
		return NULL;
	}

	pthread_mutex_init(&s->mutex, NULL);
	pthread_cond_init(&s->cond, NULL);
	s->count = initial;

	return s;
}

void wirth_semaphore_wait(WirthSemaphore *s) {
	if (!s) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid semaphore");
		return;
	}

	pthread_mutex_lock(&s->mutex);
	while (s->count <= 0) {
		pthread_cond_wait(&s->cond, &s->mutex);
	}
	s->count--;
	pthread_mutex_unlock(&s->mutex);
}

void wirth_semaphore_signal(WirthSemaphore *s) {
	if (!s) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "Invalid semaphore");
		return;
	}

	pthread_mutex_lock(&s->mutex);
	s->count++;
	pthread_cond_signal(&s->cond);
	pthread_mutex_unlock(&s->mutex);
}

void wirth_semaphore_destroy(WirthSemaphore *s) {
	if (s) {
		pthread_mutex_destroy(&s->mutex);
		pthread_cond_destroy(&s->cond);
		free(s);
	}
}
