/*
 * C11 threads implementation
 * Portable threading library for systems lacking C11 thread support
 */

#include "threads.h"
#include <stdlib.h>
#include <stdint.h>

#include <process.h>

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

