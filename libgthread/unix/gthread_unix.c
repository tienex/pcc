/* Unix/POSIX Threading Implementation */
#include <pthread.h>
#include <unistd.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string.h>

struct gthread { pthread_t pthread; void *(*func)(void *); void *arg; void *result; int detached; };
struct gthread_mutex { pthread_mutex_t mutex; };
struct gthread_cond { pthread_cond_t cond; };
struct gthread_rwlock { pthread_rwlock_t lock; };

static void *native_thread_wrapper(void *arg) {
	gthread_t *t = arg;
	t->result = t->func(t->arg);
	return t->result;
}

int gthread_create(gthread_t **thread, const gthread_attr_t *attr, gthread_func_t func, void *arg) {
	gthread_t *t = calloc(1, sizeof(gthread_t));
	if (!t) return -1;
	t->func = func; t->arg = arg;
	
	pthread_attr_t pattr;
	pthread_attr_init(&pattr);
	if (attr && attr->stack_size > 0) pthread_attr_setstacksize(&pattr, attr->stack_size);
	
	if (pthread_create(&t->pthread, &pattr, native_thread_wrapper, t) != 0) {
		free(t); pthread_attr_destroy(&pattr); return -1;
	}
	pthread_attr_destroy(&pattr);
	*thread = t;
	return 0;
}

int gthread_join(gthread_t *thread, void **result) {
	if (!thread) return -1;
	int ret = pthread_join(thread->pthread, result);
	free(thread);
	return ret;
}

void gthread_exit(void *result) { pthread_exit(result); }
gthread_t *gthread_self(void) {
	gthread_t *t = calloc(1, sizeof(gthread_t));
	t->pthread = pthread_self();
	return t;
}

int gthread_mutex_init(gthread_mutex_t **mutex, const gthread_mutexattr_t *attr) {
	(void)attr;
	gthread_mutex_t *m = calloc(1, sizeof(gthread_mutex_t));
	if (!m) return -1;
	pthread_mutex_init(&m->mutex, NULL);
	*mutex = m;
	return 0;
}

void gthread_mutex_destroy(gthread_mutex_t *mutex) {
	if (!mutex) return;
	pthread_mutex_destroy(&mutex->mutex);
	free(mutex);
}

int gthread_mutex_lock(gthread_mutex_t *mutex) {
	return mutex ? pthread_mutex_lock(&mutex->mutex) : -1;
}

int gthread_mutex_unlock(gthread_mutex_t *mutex) {
	return mutex ? pthread_mutex_unlock(&mutex->mutex) : -1;
}

int gthread_cond_init(gthread_cond_t **cond, const gthread_condattr_t *attr) {
	(void)attr;
	gthread_cond_t *c = calloc(1, sizeof(gthread_cond_t));
	if (!c) return -1;
	pthread_cond_init(&c->cond, NULL);
	*cond = c;
	return 0;
}

void gthread_cond_destroy(gthread_cond_t *cond) {
	if (!cond) return;
	pthread_cond_destroy(&cond->cond);
	free(cond);
}

int gthread_cond_wait(gthread_cond_t *cond, gthread_mutex_t *mutex) {
	return (cond && mutex) ? pthread_cond_wait(&cond->cond, &mutex->mutex) : -1;
}

int gthread_cond_signal(gthread_cond_t *cond) {
	return cond ? pthread_cond_signal(&cond->cond) : -1;
}

int gthread_rwlock_init(gthread_rwlock_t **lock, const gthread_rwlockattr_t *attr) {
	(void)attr;
	gthread_rwlock_t *l = calloc(1, sizeof(gthread_rwlock_t));
	if (!l) return -1;
	pthread_rwlock_init(&l->lock, NULL);
	*lock = l;
	return 0;
}

void gthread_rwlock_destroy(gthread_rwlock_t *lock) {
	if (!lock) return;
	pthread_rwlock_destroy(&lock->lock);
	free(lock);
}

int gthread_rwlock_rdlock(gthread_rwlock_t *lock) {
	return lock ? pthread_rwlock_rdlock(&lock->lock) : -1;
}

int gthread_rwlock_wrlock(gthread_rwlock_t *lock) {
	return lock ? pthread_rwlock_wrlock(&lock->lock) : -1;
}

int gthread_rwlock_unlock(gthread_rwlock_t *lock) {
	return lock ? pthread_rwlock_unlock(&lock->lock) : -1;
}

void gthread_yield(void) { sched_yield(); }
void gthread_sleep(unsigned int milliseconds) { usleep(milliseconds * 1000); }

int gthread_atomic_fetch_add(volatile int *ptr, int value) {
	return __sync_fetch_and_add(ptr, value);
}

int gthread_atomic_compare_exchange(volatile int *ptr, int expected, int desired) {
	return __sync_bool_compare_and_swap(ptr, expected, desired) ? expected : *ptr;
}
