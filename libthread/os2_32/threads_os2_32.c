/*
 * C11 threads implementation for OS/2 32-bit
 * Uses OS/2 native threading APIs
 */

#define INCL_DOSPROCESS
#define INCL_DOSSEMAPHORES
#define INCL_DOSERRORS
#include <os2.h>

#include "../threads.h"
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

/* Thread wrapper structure for OS/2 */
struct thrd_wrapper_arg {
    thrd_start_t func;
    void *arg;
};

static void thrd_wrapper(void *arg)
{
    struct thrd_wrapper_arg *wrapper = (struct thrd_wrapper_arg *)arg;
    thrd_start_t func = wrapper->func;
    void *func_arg = wrapper->arg;
    free(wrapper);

    int result = func(func_arg);
    DosExit(EXIT_THREAD, result);
}

int thrd_create(thrd_t *thr, thrd_start_t func, void *arg)
{
    struct thrd_wrapper_arg *wrapper;
    APIRET rc;

    wrapper = malloc(sizeof(*wrapper));
    if (!wrapper)
        return thrd_nomem;

    wrapper->func = func;
    wrapper->arg = arg;

    rc = DosCreateThread(&thr->tid, (PFNTHREAD)thrd_wrapper, (ULONG)wrapper, 0, 8192);
    if (rc == 0)
        return thrd_success;

    free(wrapper);
    return (rc == ERROR_NOT_ENOUGH_MEMORY) ? thrd_nomem : thrd_error;
}

int thrd_equal(thrd_t thr0, thrd_t thr1)
{
    return thr0.tid == thr1.tid;
}

thrd_t thrd_current(void)
{
    thrd_t thr;
    PTIB ptib;
    PPIB ppib;

    DosGetInfoBlocks(&ptib, &ppib);
    thr.tid = ptib->tib_ptib2->tib2_ultid;

    return thr;
}

int thrd_sleep(const struct timespec *duration, struct timespec *remaining)
{
    ULONG ms = (ULONG)(duration->tv_sec * 1000 + duration->tv_nsec / 1000000);
    DosSleep(ms);
    if (remaining) {
        remaining->tv_sec = 0;
        remaining->tv_nsec = 0;
    }
    return 0;
}

void thrd_yield(void)
{
    DosSleep(0);
}

_Noreturn void thrd_exit(int res)
{
    DosExit(EXIT_THREAD, res);
}

int thrd_detach(thrd_t thr)
{
    return thrd_success;
}

int thrd_join(thrd_t thr, int *res)
{
    APIRET rc = DosWaitThread(&thr.tid, DCWW_WAIT);
    if (rc != 0)
        return thrd_error;
    return thrd_success;
}

/* Mutex functions */
int mtx_init(mtx_t *mtx, int type)
{
    APIRET rc = DosCreateMutexSem(NULL, &mtx->hmtx, 0, FALSE);
    mtx->recursive = (type & mtx_recursive) ? 1 : 0;
    return (rc == 0) ? thrd_success : thrd_error;
}

int mtx_lock(mtx_t *mtx)
{
    APIRET rc = DosRequestMutexSem(mtx->hmtx, SEM_INDEFINITE_WAIT);
    return (rc == 0) ? thrd_success : thrd_error;
}

int mtx_timedlock(mtx_t *restrict mtx, const struct timespec *restrict ts)
{
    struct timespec now;
    ULONG timeout;
    APIRET rc;

    timespec_get(&now, TIME_UTC);

    if (ts->tv_sec < now.tv_sec ||
        (ts->tv_sec == now.tv_sec && ts->tv_nsec <= now.tv_nsec)) {
        return thrd_timedout;
    }

    timeout = (ULONG)((ts->tv_sec - now.tv_sec) * 1000 +
                     (ts->tv_nsec - now.tv_nsec) / 1000000);

    rc = DosRequestMutexSem(mtx->hmtx, timeout);
    if (rc == 0)
        return thrd_success;
    return (rc == ERROR_TIMEOUT) ? thrd_timedout : thrd_error;
}

int mtx_trylock(mtx_t *mtx)
{
    APIRET rc = DosRequestMutexSem(mtx->hmtx, SEM_IMMEDIATE_RETURN);
    if (rc == 0)
        return thrd_success;
    return (rc == ERROR_TIMEOUT) ? thrd_busy : thrd_error;
}

int mtx_unlock(mtx_t *mtx)
{
    APIRET rc = DosReleaseMutexSem(mtx->hmtx);
    return (rc == 0) ? thrd_success : thrd_error;
}

void mtx_destroy(mtx_t *mtx)
{
    DosCloseMutexSem(mtx->hmtx);
}

/* Condition variable functions */
int cnd_init(cnd_t *cond)
{
    APIRET rc = DosCreateEventSem(NULL, &cond->hev, 0, FALSE);
    return (rc == 0) ? thrd_success : thrd_error;
}

int cnd_signal(cnd_t *cond)
{
    APIRET rc = DosPostEventSem(cond->hev);
    return (rc == 0) ? thrd_success : thrd_error;
}

int cnd_broadcast(cnd_t *cond)
{
    /* OS/2 event semaphores are auto-reset, so this is same as signal */
    return cnd_signal(cond);
}

int cnd_wait(cnd_t *cond, mtx_t *mtx)
{
    APIRET rc;

    mtx_unlock(mtx);
    rc = DosWaitEventSem(cond->hev, SEM_INDEFINITE_WAIT);
    mtx_lock(mtx);

    return (rc == 0) ? thrd_success : thrd_error;
}

int cnd_timedwait(cnd_t *restrict cond, mtx_t *restrict mtx,
                  const struct timespec *restrict ts)
{
    struct timespec now;
    ULONG timeout;
    APIRET rc;

    timespec_get(&now, TIME_UTC);

    if (ts->tv_sec < now.tv_sec ||
        (ts->tv_sec == now.tv_sec && ts->tv_nsec <= now.tv_nsec)) {
        return thrd_timedout;
    }

    timeout = (ULONG)((ts->tv_sec - now.tv_sec) * 1000 +
                     (ts->tv_nsec - now.tv_nsec) / 1000000);

    mtx_unlock(mtx);
    rc = DosWaitEventSem(cond->hev, timeout);
    mtx_lock(mtx);

    if (rc == 0)
        return thrd_success;
    return (rc == ERROR_TIMEOUT) ? thrd_timedout : thrd_error;
}

void cnd_destroy(cnd_t *cond)
{
    DosCloseEventSem(cond->hev);
}

/* Thread-local storage */
static ULONG _tls_key_counter = 0;

int tss_create(tss_t *key, tss_dtor_t dtor)
{
    /* OS/2 TLS implementation using thread-local memory */
    if (_tls_key_counter >= 64)
        return thrd_error;
    *key = _tls_key_counter++;
    return thrd_success;
}

void *tss_get(tss_t key)
{
    /* Simplified - would need proper OS/2 TLS API */
    return NULL;
}

int tss_set(tss_t key, void *val)
{
    /* Simplified - would need proper OS/2 TLS API */
    return thrd_success;
}

void tss_delete(tss_t key)
{
    /* No-op in simplified implementation */
}

/* Call once */
void call_once(once_flag *flag, void (*func)(void))
{
    if (*flag == 0) {
        *flag = 1;
        func();
    }
}
