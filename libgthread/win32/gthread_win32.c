/* Win32 Threading Implementation */
#ifdef _WIN32
#include <windows.h>
struct gthread { HANDLE handle; DWORD id; void *(*func)(void *); void *arg; void *result; };
struct gthread_mutex { CRITICAL_SECTION cs; };
struct gthread_cond { CONDITION_VARIABLE cv; };
struct gthread_rwlock { SRWLOCK lock; };

static DWORD WINAPI thread_proc(LPVOID arg) {
	gthread_t *t = arg;
	t->result = t->func(t->arg);
	return 0;
}

int gthread_create(gthread_t **thread, const gthread_attr_t *attr, gthread_func_t func, void *arg) {
	(void)attr;
	gthread_t *t = calloc(1, sizeof(gthread_t));
	if (!t) return -1;
	t->func = func; t->arg = arg;
	t->handle = CreateThread(NULL, 0, thread_proc, t, 0, &t->id);
	if (!t->handle) { free(t); return -1; }
	*thread = t;
	return 0;
}

int gthread_join(gthread_t *thread, void **result) {
	if (!thread) return -1;
	WaitForSingleObject(thread->handle, INFINITE);
	if (result) *result = thread->result;
	CloseHandle(thread->handle);
	free(thread);
	return 0;
}

void gthread_yield(void) { Sleep(0); }
void gthread_sleep(unsigned int milliseconds) { Sleep(milliseconds); }

int gthread_mutex_init(gthread_mutex_t **mutex, const gthread_mutexattr_t *attr) {
	(void)attr;
	gthread_mutex_t *m = calloc(1, sizeof(gthread_mutex_t));
	if (!m) return -1;
	InitializeCriticalSection(&m->cs);
	*mutex = m;
	return 0;
}

void gthread_mutex_destroy(gthread_mutex_t *mutex) {
	if (!mutex) return;
	DeleteCriticalSection(&mutex->cs);
	free(mutex);
}

int gthread_mutex_lock(gthread_mutex_t *mutex) {
	if (!mutex) return -1;
	EnterCriticalSection(&mutex->cs);
	return 0;
}

int gthread_mutex_unlock(gthread_mutex_t *mutex) {
	if (!mutex) return -1;
	LeaveCriticalSection(&mutex->cs);
	return 0;
}
#endif
