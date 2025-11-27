/*
 * C11 threads implementation
 * Portable threading library for systems lacking C11 thread support
 */

#include "threads.h"
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

#if defined(_WIN32) || defined(_WIN64)
/* ========== Windows Implementation ========== */

#include <process.h>

/* Thread wrapper for Windows */
static unsigned __stdcall thrd_wrapper(void *arg)
{
    thrd_t *thr = (thrd_t *)arg;
    int result = thr->func(thr->arg);
    return (unsigned)result;
}

int thrd_create(thrd_t *thr, thrd_start_t func, void *arg)
{
    thr->func = func;
    thr->arg = arg;
    thr->handle = (HANDLE)_beginthreadex(NULL, 0, thrd_wrapper, thr, 0, NULL);
    if (thr->handle == NULL)
        return thrd_nomem;
    return thrd_success;
}

int thrd_equal(thrd_t thr0, thrd_t thr1)
{
    return GetThreadId(thr0.handle) == GetThreadId(thr1.handle);
}

thrd_t thrd_current(void)
{
    thrd_t thr;
    HANDLE pseudo = GetCurrentThread();
    DuplicateHandle(GetCurrentProcess(), pseudo, GetCurrentProcess(),
                   &thr.handle, 0, FALSE, DUPLICATE_SAME_ACCESS);
    return thr;
}

int thrd_sleep(const struct timespec *duration, struct timespec *remaining)
{
    DWORD ms = (DWORD)(duration->tv_sec * 1000 + duration->tv_nsec / 1000000);
    Sleep(ms);
    if (remaining) {
        remaining->tv_sec = 0;
        remaining->tv_nsec = 0;
    }
    return 0;
}

void thrd_yield(void)
{
    SwitchToThread();
}

_Noreturn void thrd_exit(int res)
{
    _endthreadex((unsigned)res);
    /* unreachable, but some compilers may complain without this */
    for(;;);
}

int thrd_detach(thrd_t thr)
{
    CloseHandle(thr.handle);
    return thrd_success;
}

int thrd_join(thrd_t thr, int *res)
{
    DWORD ret;
    if (WaitForSingleObject(thr.handle, INFINITE) != WAIT_OBJECT_0)
        return thrd_error;
    if (res) {
        if (!GetExitCodeThread(thr.handle, &ret))
            return thrd_error;
        *res = (int)ret;
    }
    CloseHandle(thr.handle);
    return thrd_success;
}

/* Mutex functions */
int mtx_init(mtx_t *mtx, int type)
{
    InitializeCriticalSection(mtx);
    return thrd_success;
}

int mtx_lock(mtx_t *mtx)
{
    EnterCriticalSection(mtx);
    return thrd_success;
}

int mtx_timedlock(mtx_t *restrict mtx, const struct timespec *restrict ts)
{
    /* Windows critical sections don't support timed locks natively */
    /* This is a simplified implementation */
    if (TryEnterCriticalSection(mtx))
        return thrd_success;

    /* Busy wait with timeout (not ideal, but portable) */
    struct timespec now;
    timespec_get(&now, TIME_UTC);

    while (now.tv_sec < ts->tv_sec ||
           (now.tv_sec == ts->tv_sec && now.tv_nsec < ts->tv_nsec)) {
        if (TryEnterCriticalSection(mtx))
            return thrd_success;
        Sleep(1);
        timespec_get(&now, TIME_UTC);
    }
    return thrd_timedout;
}

int mtx_trylock(mtx_t *mtx)
{
    return TryEnterCriticalSection(mtx) ? thrd_success : thrd_busy;
}

int mtx_unlock(mtx_t *mtx)
{
    LeaveCriticalSection(mtx);
    return thrd_success;
}

void mtx_destroy(mtx_t *mtx)
{
    DeleteCriticalSection(mtx);
}

/* Condition variable functions */
int cnd_init(cnd_t *cond)
{
    InitializeConditionVariable(cond);
    return thrd_success;
}

int cnd_signal(cnd_t *cond)
{
    WakeConditionVariable(cond);
    return thrd_success;
}

int cnd_broadcast(cnd_t *cond)
{
    WakeAllConditionVariable(cond);
    return thrd_success;
}

int cnd_wait(cnd_t *cond, mtx_t *mtx)
{
    if (!SleepConditionVariableCS(cond, mtx, INFINITE))
        return thrd_error;
    return thrd_success;
}

int cnd_timedwait(cnd_t *restrict cond, mtx_t *restrict mtx,
                  const struct timespec *restrict ts)
{
    struct timespec now;
    DWORD timeout;

    timespec_get(&now, TIME_UTC);

    if (ts->tv_sec < now.tv_sec ||
        (ts->tv_sec == now.tv_sec && ts->tv_nsec <= now.tv_nsec)) {
        return thrd_timedout;
    }

    timeout = (DWORD)((ts->tv_sec - now.tv_sec) * 1000 +
                     (ts->tv_nsec - now.tv_nsec) / 1000000);

    if (!SleepConditionVariableCS(cond, mtx, timeout))
        return (GetLastError() == ERROR_TIMEOUT) ? thrd_timedout : thrd_error;

    return thrd_success;
}

void cnd_destroy(cnd_t *cond)
{
    /* No-op on Windows */
    (void)cond;
}

/* Thread-local storage */
int tss_create(tss_t *key, tss_dtor_t dtor)
{
    *key = TlsAlloc();
    if (*key == TLS_OUT_OF_INDEXES)
        return thrd_error;
    /* Note: Windows doesn't support destructors directly */
    /* Applications must manually clean up TLS data */
    return thrd_success;
}

void *tss_get(tss_t key)
{
    return TlsGetValue(key);
}

int tss_set(tss_t key, void *val)
{
    return TlsSetValue(key, val) ? thrd_success : thrd_error;
}

void tss_delete(tss_t key)
{
    TlsFree(key);
}

/* Call once */
static BOOL CALLBACK call_once_wrapper(PINIT_ONCE InitOnce, PVOID Parameter, PVOID *Context)
{
    void (*func)(void) = (void (*)(void))Parameter;
    func();
    return TRUE;
}

void call_once(once_flag *flag, void (*func)(void))
{
    InitOnceExecuteOnce(flag, call_once_wrapper, func, NULL);
}

#else
/* ========== POSIX Implementation ========== */

#include <pthread.h>
#include <sched.h>

/* Thread wrapper structure for POSIX */
struct thrd_wrapper_arg {
    thrd_start_t func;
    void *arg;
};

static void *thrd_wrapper(void *arg)
{
    struct thrd_wrapper_arg *wrapper = (struct thrd_wrapper_arg *)arg;
    thrd_start_t func = wrapper->func;
    void *func_arg = wrapper->arg;
    free(wrapper);

    int result = func(func_arg);
    return (void *)(intptr_t)result;
}

int thrd_create(thrd_t *thr, thrd_start_t func, void *arg)
{
    struct thrd_wrapper_arg *wrapper;
    int ret;

    wrapper = malloc(sizeof(*wrapper));
    if (!wrapper)
        return thrd_nomem;

    wrapper->func = func;
    wrapper->arg = arg;

    ret = pthread_create(thr, NULL, thrd_wrapper, wrapper);
    if (ret == 0)
        return thrd_success;

    free(wrapper);
    return (ret == ENOMEM) ? thrd_nomem : thrd_error;
}

int thrd_equal(thrd_t thr0, thrd_t thr1)
{
    return pthread_equal(thr0, thr1);
}

thrd_t thrd_current(void)
{
    return pthread_self();
}

int thrd_sleep(const struct timespec *duration, struct timespec *remaining)
{
    return nanosleep(duration, remaining);
}

void thrd_yield(void)
{
    sched_yield();
}

_Noreturn void thrd_exit(int res)
{
    pthread_exit((void *)(intptr_t)res);
}

int thrd_detach(thrd_t thr)
{
    return (pthread_detach(thr) == 0) ? thrd_success : thrd_error;
}

int thrd_join(thrd_t thr, int *res)
{
    void *ret_val;

    if (pthread_join(thr, &ret_val) != 0)
        return thrd_error;

    if (res)
        *res = (int)(intptr_t)ret_val;

    return thrd_success;
}

/* Mutex functions */
int mtx_init(mtx_t *mtx, int type)
{
    pthread_mutexattr_t attr;
    int ret;

    pthread_mutexattr_init(&attr);

    if (type & mtx_recursive)
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);

    ret = pthread_mutex_init(mtx, &attr);
    pthread_mutexattr_destroy(&attr);

    return (ret == 0) ? thrd_success : thrd_error;
}

int mtx_lock(mtx_t *mtx)
{
    return (pthread_mutex_lock(mtx) == 0) ? thrd_success : thrd_error;
}

int mtx_timedlock(mtx_t *restrict mtx, const struct timespec *restrict ts)
{
    int ret = pthread_mutex_timedlock(mtx, ts);
    if (ret == 0)
        return thrd_success;
    return (ret == ETIMEDOUT) ? thrd_timedout : thrd_error;
}

int mtx_trylock(mtx_t *mtx)
{
    int ret = pthread_mutex_trylock(mtx);
    if (ret == 0)
        return thrd_success;
    return (ret == EBUSY) ? thrd_busy : thrd_error;
}

int mtx_unlock(mtx_t *mtx)
{
    return (pthread_mutex_unlock(mtx) == 0) ? thrd_success : thrd_error;
}

void mtx_destroy(mtx_t *mtx)
{
    pthread_mutex_destroy(mtx);
}

/* Condition variable functions */
int cnd_init(cnd_t *cond)
{
    return (pthread_cond_init(cond, NULL) == 0) ? thrd_success : thrd_error;
}

int cnd_signal(cnd_t *cond)
{
    return (pthread_cond_signal(cond) == 0) ? thrd_success : thrd_error;
}

int cnd_broadcast(cnd_t *cond)
{
    return (pthread_cond_broadcast(cond) == 0) ? thrd_success : thrd_error;
}

int cnd_wait(cnd_t *cond, mtx_t *mtx)
{
    return (pthread_cond_wait(cond, mtx) == 0) ? thrd_success : thrd_error;
}

int cnd_timedwait(cnd_t *restrict cond, mtx_t *restrict mtx,
                  const struct timespec *restrict ts)
{
    int ret = pthread_cond_timedwait(cond, mtx, ts);
    if (ret == 0)
        return thrd_success;
    return (ret == ETIMEDOUT) ? thrd_timedout : thrd_error;
}

void cnd_destroy(cnd_t *cond)
{
    pthread_cond_destroy(cond);
}

/* Thread-local storage */
int tss_create(tss_t *key, tss_dtor_t dtor)
{
    return (pthread_key_create(key, dtor) == 0) ? thrd_success : thrd_error;
}

void *tss_get(tss_t key)
{
    return pthread_getspecific(key);
}

int tss_set(tss_t key, void *val)
{
    return (pthread_setspecific(key, val) == 0) ? thrd_success : thrd_error;
}

void tss_delete(tss_t key)
{
    pthread_key_delete(key);
}

/* Call once */
void call_once(once_flag *flag, void (*func)(void))
{
    pthread_once(flag, func);
}

#endif /* Platform-specific implementations */
