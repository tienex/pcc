/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL process and concurrency support (simplified)
 */

#include "chill.h"
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>

/*
 * Process structure (implemented using threads)
 */
struct chill_process {
	pthread_t thread;
	void (*func)(void *);
	void *arg;
	int running;
};

/*
 * Signal structure (implemented using condition variables)
 */
struct chill_signal {
	char *name;
	pthread_mutex_t mutex;
	pthread_cond_t cond;
	void *data;
	size_t data_size;
	int has_data;
};

/*
 * Thread wrapper function
 */
static void *
thread_wrapper(void *arg)
{
	chill_process_t *proc = (chill_process_t *)arg;

	if (proc->func != NULL) {
		proc->func(proc->arg);
	}

	proc->running = 0;
	return NULL;
}

/*
 * Process management functions
 */

chill_process_t *
chill_process_create(void (*func)(void *), void *arg)
{
	chill_process_t *proc;

	proc = (chill_process_t *)malloc(sizeof(chill_process_t));
	if (proc == NULL) {
		chill_raise(CHILL_EXC_OUTOFMEM, "Failed to create process");
		return NULL;
	}

	proc->func = func;
	proc->arg = arg;
	proc->running = 0;

	return proc;
}

void
chill_process_start(chill_process_t *proc)
{
	if (proc == NULL) {
		return;
	}

	if (proc->running) {
		chill_raise(CHILL_EXC_INVALIDOP, "Process already running");
		return;
	}

	proc->running = 1;
	if (pthread_create(&proc->thread, NULL, thread_wrapper, proc) != 0) {
		proc->running = 0;
		chill_raise(CHILL_EXC_INVALIDOP, "Failed to start process");
	}
}

void
chill_process_stop(chill_process_t *proc)
{
	if (proc == NULL) {
		return;
	}

	if (proc->running) {
		pthread_cancel(proc->thread);
		pthread_join(proc->thread, NULL);
		proc->running = 0;
	}

	free(proc);
}

void
chill_delay(chill_duration_t duration)
{
	/* Duration is in microseconds */
	if (duration > 0) {
		usleep((useconds_t)duration);
	}
}

/*
 * Signal/message passing functions
 */

chill_signal_t *
chill_signal_create(const char *name)
{
	chill_signal_t *sig;

	sig = (chill_signal_t *)malloc(sizeof(chill_signal_t));
	if (sig == NULL) {
		chill_raise(CHILL_EXC_OUTOFMEM, "Failed to create signal");
		return NULL;
	}

	sig->name = (name != NULL) ? strdup(name) : NULL;
	sig->data = NULL;
	sig->data_size = 0;
	sig->has_data = 0;

	pthread_mutex_init(&sig->mutex, NULL);
	pthread_cond_init(&sig->cond, NULL);

	return sig;
}

void
chill_send(chill_signal_t *sig, void *data, size_t size)
{
	if (sig == NULL) {
		return;
	}

	pthread_mutex_lock(&sig->mutex);

	/* Free old data if any */
	if (sig->data != NULL) {
		free(sig->data);
	}

	/* Copy new data */
	if (size > 0 && data != NULL) {
		sig->data = malloc(size);
		if (sig->data == NULL) {
			pthread_mutex_unlock(&sig->mutex);
			chill_raise(CHILL_EXC_OUTOFMEM, "Failed to allocate signal data");
			return;
		}
		memcpy(sig->data, data, size);
		sig->data_size = size;
		sig->has_data = 1;
	} else {
		sig->data = NULL;
		sig->data_size = 0;
		sig->has_data = 1;
	}

	/* Signal waiting threads */
	pthread_cond_signal(&sig->cond);
	pthread_mutex_unlock(&sig->mutex);
}

int
chill_receive(chill_signal_t *sig, void *buffer, size_t size, chill_duration_t timeout)
{
	int result = 0;

	if (sig == NULL) {
		return -1;
	}

	pthread_mutex_lock(&sig->mutex);

	/* Wait for data with timeout */
	if (!sig->has_data && timeout > 0) {
		struct timespec ts;
		clock_gettime(CLOCK_REALTIME, &ts);
		ts.tv_sec += timeout / 1000000;
		ts.tv_nsec += (timeout % 1000000) * 1000;
		if (ts.tv_nsec >= 1000000000) {
			ts.tv_sec++;
			ts.tv_nsec -= 1000000000;
		}

		int ret = pthread_cond_timedwait(&sig->cond, &sig->mutex, &ts);
		if (ret == ETIMEDOUT) {
			pthread_mutex_unlock(&sig->mutex);
			return -1;
		}
	} else if (!sig->has_data) {
		/* Wait indefinitely */
		pthread_cond_wait(&sig->cond, &sig->mutex);
	}

	/* Copy data to buffer */
	if (sig->has_data && sig->data != NULL && buffer != NULL) {
		size_t copy_size = (size < sig->data_size) ? size : sig->data_size;
		memcpy(buffer, sig->data, copy_size);
		result = (int)copy_size;
	}

	/* Clear signal data after receiving */
	if (sig->data != NULL) {
		free(sig->data);
		sig->data = NULL;
	}
	sig->data_size = 0;
	sig->has_data = 0;

	pthread_mutex_unlock(&sig->mutex);
	return result;
}
