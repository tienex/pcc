/* os2_16 Threading Stub - No native threading support */
#include <stdlib.h>

struct gthread { void *(*func)(void *); void *arg; void *result; };
struct gthread_mutex { int lock; };
struct gthread_cond { int dummy; };
struct gthread_rwlock { int dummy; };

/* Minimal cooperative threading */
int gthread_create(gthread_t **thread, const gthread_attr_t *attr, gthread_func_t func, void *arg) {
	(void)attr;
	gthread_t *t = calloc(1, sizeof(gthread_t));
	if (!t) return -1;
	t->func = func; t->arg = arg;
	/* Immediately execute in current thread for non-threaded systems */
	t->result = func(arg);
	*thread = t;
	return 0;
}

int gthread_join(gthread_t *thread, void **result) {
	if (!thread) return -1;
	if (result) *result = thread->result;
	free(thread);
	return 0;
}

int gthread_mutex_init(gthread_mutex_t **mutex, const gthread_mutexattr_t *attr) {
	(void)attr;
	*mutex = calloc(1, sizeof(gthread_mutex_t));
	return *mutex ? 0 : -1;
}

void gthread_mutex_destroy(gthread_mutex_t *mutex) { free(mutex); }
int gthread_mutex_lock(gthread_mutex_t *mutex) { (void)mutex; return 0; }
int gthread_mutex_unlock(gthread_mutex_t *mutex) { (void)mutex; return 0; }
void gthread_yield(void) { /* no-op */ }
void gthread_sleep(unsigned int milliseconds) { (void)milliseconds; }
