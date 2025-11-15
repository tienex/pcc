/*
 * C11 threads implementation for Mac OS Classic (pre-OSX)
 * Uses Thread Manager API (System 7.5+) or stubs for earlier versions
 */

#include "../threads.h"
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

/*
 * Mac OS Classic has Thread Manager for cooperative multitasking
 * However, for maximum compatibility, we provide stubs
 * Real implementation would use NewThread(), YieldToAnyThread(), etc.
 */

int thrd_create(thrd_t *thr, thrd_start_t func, void *arg)
{
    /* Would use NewThread() on Thread Manager systems */
    /* For now, return error */
    return thrd_error;
}

int thrd_equal(thrd_t thr0, thrd_t thr1)
{
    /* Only one thread in stub version */
    return 1;
}

thrd_t thrd_current(void)
{
    thrd_t thr = {0};
    /* Would use GetCurrentThread() */
    return thr;
}

int thrd_sleep(const struct timespec *duration, struct timespec *remaining)
{
    /* Could use Delay() from Toolbox */
    /* Delay is in ticks (1/60th second) */
    unsigned long ticks = (unsigned long)(duration->tv_sec * 60 +
                                          duration->tv_nsec / 16666667);
    /* Delay(ticks, &finalTicks); */
    if (remaining) {
        remaining->tv_sec = 0;
        remaining->tv_nsec = 0;
    }
    return 0;
}

void thrd_yield(void)
{
    /* Would use YieldToAnyThread() on Thread Manager */
}

_Noreturn void thrd_exit(int res)
{
    /* Would use DisposeThread() on Thread Manager */
    exit(res);
}

int thrd_detach(thrd_t thr)
{
    return thrd_error;
}

int thrd_join(thrd_t thr, int *res)
{
    return thrd_error;
}

/* Mutex functions - stubs */
int mtx_init(mtx_t *mtx, int type)
{
    /* No locking needed in cooperative multitasking */
    return thrd_success;
}

int mtx_lock(mtx_t *mtx)
{
    return thrd_success;
}

int mtx_timedlock(mtx_t *restrict mtx, const struct timespec *restrict ts)
{
    return thrd_success;
}

int mtx_trylock(mtx_t *mtx)
{
    return thrd_success;
}

int mtx_unlock(mtx_t *mtx)
{
    return thrd_success;
}

void mtx_destroy(mtx_t *mtx)
{
    /* No-op */
}

/* Condition variable functions - stubs */
int cnd_init(cnd_t *cond)
{
    return thrd_success;
}

int cnd_signal(cnd_t *cond)
{
    return thrd_success;
}

int cnd_broadcast(cnd_t *cond)
{
    return thrd_success;
}

int cnd_wait(cnd_t *cond, mtx_t *mtx)
{
    /* Would deadlock */
    return thrd_error;
}

int cnd_timedwait(cnd_t *restrict cond, mtx_t *restrict mtx,
                  const struct timespec *restrict ts)
{
    return thrd_timedout;
}

void cnd_destroy(cnd_t *cond)
{
    /* No-op */
}

/* Thread-local storage - stubs using global variables */
static void *_tss_storage[TSS_DTOR_ITERATIONS] = {0};

int tss_create(tss_t *key, tss_dtor_t dtor)
{
    static tss_t next_key = 0;
    if (next_key >= TSS_DTOR_ITERATIONS)
        return thrd_error;
    *key = next_key++;
    return thrd_success;
}

void *tss_get(tss_t key)
{
    if (key >= TSS_DTOR_ITERATIONS)
        return NULL;
    return _tss_storage[key];
}

int tss_set(tss_t key, void *val)
{
    if (key >= TSS_DTOR_ITERATIONS)
        return thrd_error;
    _tss_storage[key] = val;
    return thrd_success;
}

void tss_delete(tss_t key)
{
    if (key < TSS_DTOR_ITERATIONS)
        _tss_storage[key] = NULL;
}

/* Call once */
void call_once(once_flag *flag, void (*func)(void))
{
    if (*flag == 0) {
        *flag = 1;
        func();
    }
}
