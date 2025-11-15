/*
 * Copyright (c) 2025 PCC Project
 *
 * Green Threads and Native Threads Library
 *
 * Unified threading interface supporting:
 * - Green threads (user-space cooperative)
 * - Native OS threads (POSIX, Windows)
 * - Fiber-based scheduling
 * - Work-stealing scheduler
 */

#ifndef _PCC_GTHREAD_H_
#define _PCC_GTHREAD_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Thread types */
typedef enum {
	THREAD_GREEN = 0,      /* Green/user-space thread */
	THREAD_NATIVE          /* Native OS thread */
} thread_type_t;

/* Scheduling policies */
typedef enum {
	SCHED_COOPERATIVE = 0,  /* Cooperative scheduling (yield-based) */
	SCHED_PREEMPTIVE,       /* Preemptive scheduling (timer-based) */
	SCHED_PRIORITY,         /* Priority-based preemptive */
	SCHED_ROUND_ROBIN       /* Round-robin preemptive */
} sched_policy_t;

/* Thread handle */
typedef struct gthread gthread_t;

/* Thread function */
typedef void *(*gthread_func_t)(void *arg);

/* Thread attributes */
typedef struct {
	thread_type_t type;
	size_t stack_size;
	int priority;
	int detached;
} gthread_attr_t;

/* Mutex */
typedef struct gthread_mutex gthread_mutex_t;

/* Condition variable */
typedef struct gthread_cond gthread_cond_t;

/* Semaphore */
typedef struct gthread_sem gthread_sem_t;

/* Thread-local storage key */
typedef struct gthread_key gthread_key_t;

/*
 * Thread Management
 */
int gthread_create(gthread_t **thread, const gthread_attr_t *attr,
                   gthread_func_t func, void *arg);
int gthread_join(gthread_t *thread, void **retval);
int gthread_detach(gthread_t *thread);
void gthread_exit(void *retval);
gthread_t *gthread_self(void);
int gthread_equal(gthread_t *t1, gthread_t *t2);
void gthread_yield(void);

/*
 * Synchronization
 */
gthread_mutex_t *gthread_mutex_create(void);
int gthread_mutex_lock(gthread_mutex_t *mutex);
int gthread_mutex_trylock(gthread_mutex_t *mutex);
int gthread_mutex_unlock(gthread_mutex_t *mutex);
void gthread_mutex_destroy(gthread_mutex_t *mutex);

gthread_cond_t *gthread_cond_create(void);
int gthread_cond_wait(gthread_cond_t *cond, gthread_mutex_t *mutex);
int gthread_cond_timedwait(gthread_cond_t *cond, gthread_mutex_t *mutex, uint64_t timeout_ms);
int gthread_cond_signal(gthread_cond_t *cond);
int gthread_cond_broadcast(gthread_cond_t *cond);
void gthread_cond_destroy(gthread_cond_t *cond);

gthread_sem_t *gthread_sem_create(int initial);
int gthread_sem_wait(gthread_sem_t *sem);
int gthread_sem_post(gthread_sem_t *sem);
void gthread_sem_destroy(gthread_sem_t *sem);

/*
 * Thread-Local Storage
 */
int gthread_key_create(gthread_key_t **key, void (*destructor)(void *));
int gthread_setspecific(gthread_key_t *key, const void *value);
void *gthread_getspecific(gthread_key_t *key);
void gthread_key_delete(gthread_key_t *key);

/*
 * Scheduler
 */
void gthread_sched_init(int num_workers);
void gthread_sched_shutdown(void);
void gthread_sched_set_policy(sched_policy_t policy);
sched_policy_t gthread_sched_get_policy(void);
void gthread_sched_set_quantum(uint64_t quantum_us);  /* Time slice in microseconds */
uint64_t gthread_sched_get_quantum(void);
void gthread_set_priority(gthread_t *thread, int priority);
int gthread_get_priority(gthread_t *thread);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_GTHREAD_H_ */
