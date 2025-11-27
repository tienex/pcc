/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Goroutine support
 * Simplified implementation using pthreads
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "runtime.h"

/* Goroutine structure */
typedef struct goroutine {
	go_routine_id id;
	pthread_t thread;
	go_func fn;
	void *arg;
	bool started;
	bool finished;
	struct goroutine *next;
} goroutine;

/* Global scheduler state */
static struct {
	goroutine *routines;
	pthread_mutex_t lock;
	go_routine_id next_id;
	bool initialized;
} scheduler = { NULL, PTHREAD_MUTEX_INITIALIZER, 1, false };

/* Thread-local storage for current goroutine */
static __thread go_routine_id current_routine_id = 0;

/*
 * Goroutine entry point wrapper
 */
static void *
goroutine_entry(void *arg)
{
	goroutine *g = (goroutine *)arg;

	current_routine_id = g->id;

	/* Run the function */
	if (g->fn != NULL)
		g->fn();

	/* Mark as finished */
	pthread_mutex_lock(&scheduler.lock);
	g->finished = true;
	pthread_mutex_unlock(&scheduler.lock);

	return NULL;
}

/*
 * Initialize scheduler
 */
void
go_sched_init(void)
{
	if (scheduler.initialized)
		return;

	scheduler.routines = NULL;
	scheduler.next_id = 1;
	scheduler.initialized = true;
	current_routine_id = 0;  /* Main goroutine */
}

/*
 * Create new goroutine
 */
go_routine_id
go_routine_create(go_func fn, void *arg)
{
	goroutine *g;
	int ret;

	if (!scheduler.initialized)
		go_sched_init();

	/* Allocate goroutine structure */
	g = (goroutine *)go_malloc(sizeof(goroutine));
	g->id = __sync_fetch_and_add(&scheduler.next_id, 1);
	g->fn = fn;
	g->arg = arg;
	g->started = false;
	g->finished = false;

	/* Add to list */
	pthread_mutex_lock(&scheduler.lock);
	g->next = scheduler.routines;
	scheduler.routines = g;
	pthread_mutex_unlock(&scheduler.lock);

	/* Create thread */
	ret = pthread_create(&g->thread, NULL, goroutine_entry, g);
	if (ret != 0) {
		fprintf(stderr, "runtime: failed to create goroutine\n");
		go_free(g);
		return 0;
	}

	g->started = true;

	/* Detach thread so it cleans up automatically */
	pthread_detach(g->thread);

	return g->id;
}

/*
 * Yield to scheduler
 */
void
go_routine_yield(void)
{
	/* In this simple implementation, just yield the CPU */
	pthread_yield();
}

/*
 * Exit current goroutine
 */
void
go_routine_exit(void)
{
	pthread_exit(NULL);
}

/*
 * Get current goroutine ID
 */
go_routine_id
go_routine_self(void)
{
	return current_routine_id;
}

/*
 * Run scheduler (wait for all goroutines)
 */
void
go_sched_run(void)
{
	goroutine *g, *prev, *next;
	bool any_running;

	if (!scheduler.initialized)
		return;

	/* Wait for all goroutines to finish */
	do {
		any_running = false;

		pthread_mutex_lock(&scheduler.lock);

		prev = NULL;
		for (g = scheduler.routines; g != NULL; g = next) {
			next = g->next;

			if (!g->finished) {
				any_running = true;
				prev = g;
			} else {
				/* Remove finished goroutine */
				if (prev == NULL)
					scheduler.routines = next;
				else
					prev->next = next;

				go_free(g);
			}
		}

		pthread_mutex_unlock(&scheduler.lock);

		/* Sleep a bit to avoid busy waiting */
		if (any_running) {
			struct timespec ts = { 0, 10000000 };  /* 10ms */
			nanosleep(&ts, NULL);
		}

	} while (any_running);
}
