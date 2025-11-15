/*
 * Copyright (c) 2025 PCC Project
 * Green Threads Scheduler - Cooperative and Preemptive
 * Supports multiple scheduling policies across all platforms
 */

#include "gthread.h"
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#if defined(__unix__) || defined(__linux__) || defined(__APPLE__)
  #include <signal.h>
  #include <sys/time.h>
  #define HAS_TIMER_SIGNALS 1
#elif defined(_WIN32)
  #include <windows.h>
  #define HAS_TIMER_SIGNALS 1
#else
  #define HAS_TIMER_SIGNALS 0
#endif

#define DEFAULT_STACK_SIZE (64 * 1024)
#define DEFAULT_QUANTUM_US 10000  /* 10ms */
#define MAX_PRIORITY 10

/* Thread states */
typedef enum {
	THREAD_STATE_READY,
	THREAD_STATE_RUNNING,
	THREAD_STATE_BLOCKED,
	THREAD_STATE_TERMINATED
} thread_state_t;

/* Green thread control block */
struct gthread {
	int id;
	thread_state_t state;
	int priority;
	gthread_func_t func;
	void *arg;
	void *result;

	/* Context */
	jmp_buf context;
	char *stack;
	size_t stack_size;

	/* Scheduling */
	uint64_t time_slice_start;
	uint64_t time_slice_remaining;

	/* List linkage */
	struct gthread *next;
	struct gthread *prev;
};

/* Ready queue (per priority level) */
typedef struct {
	gthread_t *head;
	gthread_t *tail;
} ready_queue_t;

/* Global scheduler state */
static struct {
	int initialized;
	sched_policy_t policy;
	uint64_t quantum_us;

	/* Ready queues (per priority) */
	ready_queue_t ready_queues[MAX_PRIORITY];

	/* Current running thread */
	gthread_t *current;
	gthread_t *idle_thread;

	/* Thread ID counter */
	int next_id;

	/* Preemption timer */
#ifdef HAS_TIMER_SIGNALS
#if defined(__unix__) || defined(__linux__) || defined(__APPLE__)
	struct itimerval timer;
	struct sigaction old_sigaction;
#elif defined(_WIN32)
	HANDLE timer_handle;
	HANDLE timer_queue;
#endif
#endif

	int preemption_enabled;
} scheduler = {
	.initialized = 0,
	.policy = SCHED_COOPERATIVE,
	.quantum_us = DEFAULT_QUANTUM_US,
	.preemption_enabled = 0
};

/* Forward declarations */
static void schedule(void);
static void preemption_handler(int sig);

/*
 * Ready queue operations
 */

static void enqueue_thread(gthread_t *thread) {
	int prio = thread->priority;
	if (prio < 0) prio = 0;
	if (prio >= MAX_PRIORITY) prio = MAX_PRIORITY - 1;

	ready_queue_t *queue = &scheduler.ready_queues[prio];

	thread->next = NULL;
	thread->prev = queue->tail;

	if (queue->tail) {
		queue->tail->next = thread;
	} else {
		queue->head = thread;
	}
	queue->tail = thread;
	thread->state = THREAD_STATE_READY;
}

static gthread_t *dequeue_thread(void) {
	/* Find highest priority non-empty queue */
	for (int prio = MAX_PRIORITY - 1; prio >= 0; prio--) {
		ready_queue_t *queue = &scheduler.ready_queues[prio];
		if (!queue->head) continue;

		gthread_t *thread = queue->head;
		queue->head = thread->next;
		if (queue->head) {
			queue->head->prev = NULL;
		} else {
			queue->tail = NULL;
		}

		thread->next = thread->prev = NULL;
		return thread;
	}

	return NULL;
}

static void remove_from_queue(gthread_t *thread) {
	int prio = thread->priority;
	if (prio < 0) prio = 0;
	if (prio >= MAX_PRIORITY) prio = MAX_PRIORITY - 1;

	ready_queue_t *queue = &scheduler.ready_queues[prio];

	if (thread->prev) {
		thread->prev->next = thread->next;
	} else {
		queue->head = thread->next;
	}

	if (thread->next) {
		thread->next->prev = thread->prev;
	} else {
		queue->tail = thread->prev;
	}

	thread->next = thread->prev = NULL;
}

/*
 * Time utilities
 */

static uint64_t get_time_us(void) {
#if defined(__unix__) || defined(__linux__) || defined(__APPLE__)
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return (uint64_t)tv.tv_sec * 1000000 + tv.tv_usec;
#elif defined(_WIN32)
	LARGE_INTEGER freq, counter;
	QueryPerformanceFrequency(&freq);
	QueryPerformanceCounter(&counter);
	return (counter.QuadPart * 1000000) / freq.QuadPart;
#else
	return 0;
#endif
}

/*
 * Preemption timer
 */

#ifdef HAS_TIMER_SIGNALS

#if defined(__unix__) || defined(__linux__) || defined(__APPLE__)

static void preemption_handler(int sig) {
	(void)sig;
	if (scheduler.policy != SCHED_COOPERATIVE) {
		gthread_yield();
	}
}

static void setup_timer(void) {
	/* Install signal handler */
	struct sigaction sa;
	memset(&sa, 0, sizeof(sa));
	sa.sa_handler = preemption_handler;
	sa.sa_flags = SA_RESTART;
	sigemptyset(&sa.sa_mask);
	sigaction(SIGALRM, &sa, &scheduler.old_sigaction);

	/* Setup interval timer */
	scheduler.timer.it_interval.tv_sec = 0;
	scheduler.timer.it_interval.tv_usec = scheduler.quantum_us;
	scheduler.timer.it_value = scheduler.timer.it_interval;
	setitimer(ITIMER_REAL, &scheduler.timer, NULL);
}

static void stop_timer(void) {
	/* Stop timer */
	struct itimerval zero_timer = {0};
	setitimer(ITIMER_REAL, &zero_timer, NULL);

	/* Restore old signal handler */
	sigaction(SIGALRM, &scheduler.old_sigaction, NULL);
}

#elif defined(_WIN32)

static VOID CALLBACK timer_callback(PVOID param, BOOLEAN timer_or_wait) {
	(void)param; (void)timer_or_wait;
	if (scheduler.policy != SCHED_COOPERATIVE) {
		gthread_yield();
	}
}

static void setup_timer(void) {
	scheduler.timer_queue = CreateTimerQueue();
	CreateTimerQueueTimer(&scheduler.timer_handle, scheduler.timer_queue,
	                      timer_callback, NULL,
	                      scheduler.quantum_us / 1000,  /* Convert to ms */
	                      scheduler.quantum_us / 1000,
	                      0);
}

static void stop_timer(void) {
	if (scheduler.timer_handle) {
		DeleteTimerQueueTimer(scheduler.timer_queue, scheduler.timer_handle, NULL);
		scheduler.timer_handle = NULL;
	}
	if (scheduler.timer_queue) {
		DeleteTimerQueue(scheduler.timer_queue);
		scheduler.timer_queue = NULL;
	}
}

#endif

#else

static void setup_timer(void) {
	/* No timer support on this platform */
}

static void stop_timer(void) {
}

#endif

/*
 * Scheduler
 */

static void thread_wrapper(void) {
	gthread_t *thread = scheduler.current;
	if (thread && thread->func) {
		thread->result = thread->func(thread->arg);
	}

	/* Thread finished, mark as terminated */
	if (thread) {
		thread->state = THREAD_STATE_TERMINATED;
	}

	/* Schedule next thread */
	schedule();
}

static void schedule(void) {
	gthread_t *prev = scheduler.current;

	/* Get next thread to run */
	gthread_t *next = dequeue_thread();
	if (!next) {
		/* No threads ready, run idle thread */
		next = scheduler.idle_thread;
	}

	if (next == prev) {
		/* Same thread, just continue */
		return;
	}

	scheduler.current = next;

	if (prev && prev->state == THREAD_STATE_RUNNING) {
		/* Put previous thread back in ready queue if not terminated/blocked */
		prev->state = THREAD_STATE_READY;
		enqueue_thread(prev);
	}

	if (next) {
		next->state = THREAD_STATE_RUNNING;
		next->time_slice_start = get_time_us();

		/* Context switch */
		if (prev) {
			if (setjmp(prev->context) == 0) {
				longjmp(next->context, 1);
			}
		} else {
			longjmp(next->context, 1);
		}
	}
}

/*
 * Public API
 */

void gthread_sched_init(int num_workers) {
	(void)num_workers;  /* Not used for green threads */

	if (scheduler.initialized) return;

	memset(&scheduler, 0, sizeof(scheduler));
	scheduler.policy = SCHED_COOPERATIVE;
	scheduler.quantum_us = DEFAULT_QUANTUM_US;
	scheduler.next_id = 1;

	/* Create idle thread */
	scheduler.idle_thread = calloc(1, sizeof(gthread_t));
	scheduler.idle_thread->id = 0;
	scheduler.idle_thread->priority = 0;
	scheduler.idle_thread->state = THREAD_STATE_READY;

	scheduler.initialized = 1;
}

void gthread_sched_shutdown(void) {
	if (!scheduler.initialized) return;

	if (scheduler.preemption_enabled) {
		stop_timer();
	}

	/* Free all threads */
	for (int prio = 0; prio < MAX_PRIORITY; prio++) {
		gthread_t *t = scheduler.ready_queues[prio].head;
		while (t) {
			gthread_t *next = t->next;
			if (t->stack) free(t->stack);
			free(t);
			t = next;
		}
	}

	if (scheduler.idle_thread) {
		free(scheduler.idle_thread);
	}

	scheduler.initialized = 0;
}

void gthread_sched_set_policy(sched_policy_t policy) {
	scheduler.policy = policy;

	if (policy != SCHED_COOPERATIVE && !scheduler.preemption_enabled) {
		setup_timer();
		scheduler.preemption_enabled = 1;
	} else if (policy == SCHED_COOPERATIVE && scheduler.preemption_enabled) {
		stop_timer();
		scheduler.preemption_enabled = 0;
	}
}

sched_policy_t gthread_sched_get_policy(void) {
	return scheduler.policy;
}

void gthread_sched_set_quantum(uint64_t quantum_us) {
	scheduler.quantum_us = quantum_us;

	/* Restart timer with new quantum */
	if (scheduler.preemption_enabled) {
		stop_timer();
		setup_timer();
	}
}

uint64_t gthread_sched_get_quantum(void) {
	return scheduler.quantum_us;
}

void gthread_set_priority(gthread_t *thread, int priority) {
	if (!thread) return;

	if (thread->state == THREAD_STATE_READY) {
		/* Remove from current queue and re-enqueue with new priority */
		remove_from_queue(thread);
		thread->priority = priority;
		enqueue_thread(thread);
	} else {
		thread->priority = priority;
	}
}

int gthread_get_priority(gthread_t *thread) {
	return thread ? thread->priority : 0;
}

int gthread_create(gthread_t **thread, const gthread_attr_t *attr,
                   gthread_func_t func, void *arg) {
	if (!scheduler.initialized) {
		gthread_sched_init(1);
	}

	gthread_t *t = calloc(1, sizeof(gthread_t));
	if (!t) return -1;

	t->id = scheduler.next_id++;
	t->func = func;
	t->arg = arg;
	t->priority = attr && attr->priority > 0 ? attr->priority : 5;
	t->state = THREAD_STATE_READY;
	t->stack_size = attr && attr->stack_size > 0 ? attr->stack_size : DEFAULT_STACK_SIZE;

	/* Allocate stack */
	t->stack = malloc(t->stack_size);
	if (!t->stack) {
		free(t);
		return -1;
	}

	/* Setup initial context */
	if (setjmp(t->context) == 0) {
		/* Modify stack pointer to point to our stack */
		/* Platform-specific stack setup would go here */
		/* For now, we'll use a simplified approach */
	}

	/* Enqueue thread */
	enqueue_thread(t);

	*thread = t;
	return 0;
}

void gthread_yield(void) {
	if (!scheduler.initialized || !scheduler.current) return;
	schedule();
}

void gthread_exit(void *retval) {
	if (!scheduler.current) return;

	scheduler.current->result = retval;
	scheduler.current->state = THREAD_STATE_TERMINATED;
	schedule();
}

gthread_t *gthread_self(void) {
	return scheduler.current;
}

int gthread_equal(gthread_t *t1, gthread_t *t2) {
	return (t1 && t2 && t1->id == t2->id) ? 1 : 0;
}
