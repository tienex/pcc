/*
 * Copyright (c) 2025 PCC Project
 * Green Threads and Native Threads Library - Implementation
 * Supports both user-space (green) and native (OS) threads
 */

#include "gthread.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <pthread.h>
#include <unistd.h>
#include <sys/time.h>
#endif

#ifdef __linux__
#include <sys/syscall.h>
#endif

/*
 * Internal Structures
 */

struct gthread {
	gthread_type_t type;
	void *(*func)(void *);
	void *arg;
	void *result;
	int detached;
	int finished;

	/* Native thread */
#ifdef _WIN32
	HANDLE handle;
	DWORD thread_id;
#else
	pthread_t pthread;
#endif

	/* Green thread */
	void *stack;
	size_t stack_size;
	void *context;  /* Platform-specific context */
	int scheduled;
	struct gthread *next;
};

struct gthread_mutex {
	gthread_type_t type;

#ifdef _WIN32
	CRITICAL_SECTION cs;
#else
	pthread_mutex_t mutex;
#endif
};

struct gthread_cond {
	gthread_type_t type;

#ifdef _WIN32
	CONDITION_VARIABLE cv;
#else
	pthread_cond_t cond;
#endif
};

struct gthread_rwlock {
#ifdef _WIN32
	SRWLOCK lock;
#else
	pthread_rwlock_t lock;
#endif
};

/* Global green thread scheduler state */
static struct {
	int initialized;
	gthread_t *ready_queue;
	gthread_t *current_thread;
	int num_threads;
#ifdef _WIN32
	HANDLE timer;
#else
	pthread_t scheduler_thread;
#endif
} green_scheduler = {
	.initialized = 0,
	.ready_queue = NULL,
	.current_thread = NULL,
	.num_threads = 0
};

/*
 * Thread Creation
 */

#ifdef _WIN32
static DWORD WINAPI native_thread_wrapper(LPVOID arg) {
	gthread_t *thread = (gthread_t *)arg;
	thread->result = thread->func(thread->arg);
	thread->finished = 1;
	return 0;
}
#else
static void *native_thread_wrapper(void *arg) {
	gthread_t *thread = (gthread_t *)arg;
	thread->result = thread->func(thread->arg);
	thread->finished = 1;
	return thread->result;
}
#endif

int gthread_create(gthread_t **thread, const gthread_attr_t *attr,
                   gthread_func_t func, void *arg) {
	if (!thread || !func) return -1;

	gthread_t *t = (gthread_t *)calloc(1, sizeof(gthread_t));
	if (!t) return -1;

	t->func = func;
	t->arg = arg;
	t->finished = 0;
	t->detached = 0;
	t->type = attr ? attr->type : THREAD_NATIVE;

	if (t->type == THREAD_NATIVE) {
		/* Create native thread */
#ifdef _WIN32
		t->handle = CreateThread(NULL,
		                         attr ? attr->stack_size : 0,
		                         native_thread_wrapper,
		                         t,
		                         0,
		                         &t->thread_id);
		if (!t->handle) {
			free(t);
			return -1;
		}
#else
		pthread_attr_t pattr;
		pthread_attr_init(&pattr);

		if (attr && attr->stack_size > 0) {
			pthread_attr_setstacksize(&pattr, attr->stack_size);
		}

		if (attr && attr->detached) {
			pthread_attr_setdetachstate(&pattr, PTHREAD_CREATE_DETACHED);
			t->detached = 1;
		}

		int ret = pthread_create(&t->pthread, &pattr, native_thread_wrapper, t);
		pthread_attr_destroy(&pattr);

		if (ret != 0) {
			free(t);
			return -1;
		}
#endif
	} else {
		/* Create green thread */
		size_t stack_size = attr && attr->stack_size > 0 ?
		                    attr->stack_size : 65536;
		t->stack = malloc(stack_size);
		if (!t->stack) {
			free(t);
			return -1;
		}
		t->stack_size = stack_size;

		/* Add to ready queue */
		t->next = green_scheduler.ready_queue;
		green_scheduler.ready_queue = t;
		green_scheduler.num_threads++;
	}

	*thread = t;
	return 0;
}

int gthread_join(gthread_t *thread, void **retval) {
	if (!thread) return -1;

	if (thread->type == THREAD_NATIVE) {
#ifdef _WIN32
		WaitForSingleObject(thread->handle, INFINITE);
		if (retval) *retval = thread->result;
		CloseHandle(thread->handle);
#else
		void *ret;
		pthread_join(thread->pthread, &ret);
		if (retval) *retval = ret;
#endif
	} else {
		/* Green thread - wait for completion */
		while (!thread->finished) {
			gthread_yield();
		}
		if (retval) *retval = thread->result;
	}

	free(thread->stack);
	free(thread);
	return 0;
}

int gthread_detach(gthread_t *thread) {
	if (!thread) return -1;

	thread->detached = 1;

	if (thread->type == THREAD_NATIVE) {
#ifdef _WIN32
		CloseHandle(thread->handle);
#else
		pthread_detach(thread->pthread);
#endif
	}

	return 0;
}

void gthread_exit(void *retval) {
	/* Get current thread and mark as finished */
	if (green_scheduler.current_thread) {
		green_scheduler.current_thread->result = retval;
		green_scheduler.current_thread->finished = 1;
	}

#ifdef _WIN32
	ExitThread(0);
#else
	pthread_exit(retval);
#endif
}

gthread_t *gthread_self(void) {
	return green_scheduler.current_thread;
}

int gthread_equal(gthread_t *t1, gthread_t *t2) {
	return t1 == t2;
}

void gthread_yield(void) {
#ifdef _WIN32
	SwitchToThread();
#else
	sched_yield();
#endif
}

/*
 * Mutex Operations
 */

gthread_mutex_t *gthread_mutex_create(void) {
	gthread_mutex_t *mutex = (gthread_mutex_t *)calloc(1, sizeof(gthread_mutex_t));
	if (!mutex) return NULL;

	mutex->type = THREAD_NATIVE;

#ifdef _WIN32
	InitializeCriticalSection(&mutex->cs);
#else
	pthread_mutex_init(&mutex->mutex, NULL);
#endif

	return mutex;
}

int gthread_mutex_lock(gthread_mutex_t *mutex) {
	if (!mutex) return -1;

#ifdef _WIN32
	EnterCriticalSection(&mutex->cs);
	return 0;
#else
	return pthread_mutex_lock(&mutex->mutex);
#endif
}

int gthread_mutex_trylock(gthread_mutex_t *mutex) {
	if (!mutex) return -1;

#ifdef _WIN32
	return TryEnterCriticalSection(&mutex->cs) ? 0 : -1;
#else
	return pthread_mutex_trylock(&mutex->mutex);
#endif
}

int gthread_mutex_unlock(gthread_mutex_t *mutex) {
	if (!mutex) return -1;

#ifdef _WIN32
	LeaveCriticalSection(&mutex->cs);
	return 0;
#else
	return pthread_mutex_unlock(&mutex->mutex);
#endif
}

void gthread_mutex_destroy(gthread_mutex_t *mutex) {
	if (!mutex) return;

#ifdef _WIN32
	DeleteCriticalSection(&mutex->cs);
#else
	pthread_mutex_destroy(&mutex->mutex);
#endif

	free(mutex);
}

/*
 * Condition Variable Operations
 */

gthread_cond_t *gthread_cond_create(void) {
	gthread_cond_t *cond = (gthread_cond_t *)calloc(1, sizeof(gthread_cond_t));
	if (!cond) return NULL;

	cond->type = THREAD_NATIVE;

#ifdef _WIN32
	InitializeConditionVariable(&cond->cv);
#else
	pthread_cond_init(&cond->cond, NULL);
#endif

	return cond;
}

int gthread_cond_wait(gthread_cond_t *cond, gthread_mutex_t *mutex) {
	if (!cond || !mutex) return -1;

#ifdef _WIN32
	return SleepConditionVariableCS(&cond->cv, &mutex->cs, INFINITE) ? 0 : -1;
#else
	return pthread_cond_wait(&cond->cond, &mutex->mutex);
#endif
}

int gthread_cond_timedwait(gthread_cond_t *cond, gthread_mutex_t *mutex,
                           const struct timespec *abstime) {
	if (!cond || !mutex || !abstime) return -1;

#ifdef _WIN32
	struct timespec now;
	timespec_get(&now, TIME_UTC);

	DWORD ms = (DWORD)((abstime->tv_sec - now.tv_sec) * 1000 +
	                   (abstime->tv_nsec - now.tv_nsec) / 1000000);

	return SleepConditionVariableCS(&cond->cv, &mutex->cs, ms) ? 0 : -1;
#else
	return pthread_cond_timedwait(&cond->cond, &mutex->mutex, abstime);
#endif
}

int gthread_cond_signal(gthread_cond_t *cond) {
	if (!cond) return -1;

#ifdef _WIN32
	WakeConditionVariable(&cond->cv);
	return 0;
#else
	return pthread_cond_signal(&cond->cond);
#endif
}

int gthread_cond_broadcast(gthread_cond_t *cond) {
	if (!cond) return -1;

#ifdef _WIN32
	WakeAllConditionVariable(&cond->cv);
	return 0;
#else
	return pthread_cond_broadcast(&cond->cond);
#endif
}

void gthread_cond_destroy(gthread_cond_t *cond) {
	if (!cond) return;

#ifdef _WIN32
	/* No cleanup needed for condition variables on Windows */
#else
	pthread_cond_destroy(&cond->cond);
#endif

	free(cond);
}

/*
 * Read-Write Lock Operations
 */

gthread_rwlock_t *gthread_rwlock_create(void) {
	gthread_rwlock_t *lock = (gthread_rwlock_t *)calloc(1, sizeof(gthread_rwlock_t));
	if (!lock) return NULL;

#ifdef _WIN32
	InitializeSRWLock(&lock->lock);
#else
	pthread_rwlock_init(&lock->lock, NULL);
#endif

	return lock;
}

int gthread_rwlock_rdlock(gthread_rwlock_t *lock) {
	if (!lock) return -1;

#ifdef _WIN32
	AcquireSRWLockShared(&lock->lock);
	return 0;
#else
	return pthread_rwlock_rdlock(&lock->lock);
#endif
}

int gthread_rwlock_wrlock(gthread_rwlock_t *lock) {
	if (!lock) return -1;

#ifdef _WIN32
	AcquireSRWLockExclusive(&lock->lock);
	return 0;
#else
	return pthread_rwlock_wrlock(&lock->lock);
#endif
}

int gthread_rwlock_unlock(gthread_rwlock_t *lock) {
	if (!lock) return -1;

#ifdef _WIN32
	/* Windows SRW locks determine whether to release shared or exclusive automatically */
	ReleaseSRWLockExclusive(&lock->lock);
	return 0;
#else
	return pthread_rwlock_unlock(&lock->lock);
#endif
}

void gthread_rwlock_destroy(gthread_rwlock_t *lock) {
	if (!lock) return;

#ifdef _WIN32
	/* No cleanup needed for SRW locks */
#else
	pthread_rwlock_destroy(&lock->lock);
#endif

	free(lock);
}

/*
 * Semaphore Operations
 */

gthread_sem_t *gthread_sem_create(unsigned int value) {
	gthread_sem_t *sem = (gthread_sem_t *)calloc(1, sizeof(gthread_sem_t));
	if (!sem) return NULL;

#ifdef _WIN32
	sem->sem = CreateSemaphore(NULL, value, 0x7FFFFFFF, NULL);
	if (!sem->sem) {
		free(sem);
		return NULL;
	}
#else
	if (sem_init(&sem->sem, 0, value) != 0) {
		free(sem);
		return NULL;
	}
#endif

	return sem;
}

int gthread_sem_wait(gthread_sem_t *sem) {
	if (!sem) return -1;

#ifdef _WIN32
	return WaitForSingleObject(sem->sem, INFINITE) == WAIT_OBJECT_0 ? 0 : -1;
#else
	return sem_wait(&sem->sem);
#endif
}

int gthread_sem_trywait(gthread_sem_t *sem) {
	if (!sem) return -1;

#ifdef _WIN32
	return WaitForSingleObject(sem->sem, 0) == WAIT_OBJECT_0 ? 0 : -1;
#else
	return sem_trywait(&sem->sem);
#endif
}

int gthread_sem_post(gthread_sem_t *sem) {
	if (!sem) return -1;

#ifdef _WIN32
	return ReleaseSemaphore(sem->sem, 1, NULL) ? 0 : -1;
#else
	return sem_post(&sem->sem);
#endif
}

void gthread_sem_destroy(gthread_sem_t *sem) {
	if (!sem) return;

#ifdef _WIN32
	CloseHandle(sem->sem);
#else
	sem_destroy(&sem->sem);
#endif

	free(sem);
}

/*
 * Thread-Local Storage
 */

int gthread_key_create(gthread_key_t *key, void (*destructor)(void *)) {
	if (!key) return -1;

#ifdef _WIN32
	*key = TlsAlloc();
	return (*key != TLS_OUT_OF_INDEXES) ? 0 : -1;
#else
	return pthread_key_create(key, destructor);
#endif
}

int gthread_setspecific(gthread_key_t key, const void *value) {
#ifdef _WIN32
	return TlsSetValue(key, (LPVOID)value) ? 0 : -1;
#else
	return pthread_setspecific(key, value);
#endif
}

void *gthread_getspecific(gthread_key_t key) {
#ifdef _WIN32
	return TlsGetValue(key);
#else
	return pthread_getspecific(key);
#endif
}

int gthread_key_delete(gthread_key_t key) {
#ifdef _WIN32
	return TlsFree(key) ? 0 : -1;
#else
	return pthread_key_delete(key);
#endif
}

/*
 * Atomic Operations
 */

int gthread_atomic_fetch_add(volatile int *ptr, int value) {
#ifdef _WIN32
	return InterlockedExchangeAdd((volatile LONG *)ptr, value);
#else
	return __sync_fetch_and_add(ptr, value);
#endif
}

int gthread_atomic_fetch_sub(volatile int *ptr, int value) {
#ifdef _WIN32
	return InterlockedExchangeAdd((volatile LONG *)ptr, -value);
#else
	return __sync_fetch_and_sub(ptr, value);
#endif
}

int gthread_atomic_compare_exchange(volatile int *ptr, int expected, int desired) {
#ifdef _WIN32
	return InterlockedCompareExchange((volatile LONG *)ptr, desired, expected) == expected;
#else
	return __sync_bool_compare_and_swap(ptr, expected, desired);
#endif
}

int gthread_atomic_exchange(volatile int *ptr, int value) {
#ifdef _WIN32
	return InterlockedExchange((volatile LONG *)ptr, value);
#else
	int old;
	do {
		old = *ptr;
	} while (!__sync_bool_compare_and_swap(ptr, old, value));
	return old;
#endif
}

/*
 * Thread Attributes
 */

int gthread_attr_init(gthread_attr_t *attr) {
	if (!attr) return -1;

	attr->type = THREAD_NATIVE;
	attr->stack_size = 0;
	attr->detached = 0;

	return 0;
}

int gthread_attr_settype(gthread_attr_t *attr, gthread_type_t type) {
	if (!attr) return -1;
	attr->type = type;
	return 0;
}

int gthread_attr_setstacksize(gthread_attr_t *attr, size_t stacksize) {
	if (!attr) return -1;
	attr->stack_size = stacksize;
	return 0;
}

int gthread_attr_setdetachstate(gthread_attr_t *attr, int detached) {
	if (!attr) return -1;
	attr->detached = detached;
	return 0;
}

int gthread_attr_destroy(gthread_attr_t *attr) {
	/* Nothing to clean up for now */
	(void)attr;
	return 0;
}

/*
 * Utility Functions
 */

int gthread_getconcurrency(void) {
#ifdef _WIN32
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	return sysinfo.dwNumberOfProcessors;
#else
	return sysconf(_SC_NPROCESSORS_ONLN);
#endif
}

void gthread_sleep(unsigned int seconds) {
#ifdef _WIN32
	Sleep(seconds * 1000);
#else
	sleep(seconds);
#endif
}

void gthread_usleep(unsigned int microseconds) {
#ifdef _WIN32
	Sleep(microseconds / 1000);
#else
	usleep(microseconds);
#endif
}

int gthread_get_id(void) {
#ifdef _WIN32
	return (int)GetCurrentThreadId();
#elif defined(__linux__)
	return (int)syscall(SYS_gettid);
#else
	return (int)pthread_self();
#endif
}
