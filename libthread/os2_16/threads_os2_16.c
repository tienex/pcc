/*
 * C11 threads implementation
 * DOS stub - no threading support on DOS
 */

#include "../threads.h"
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

/*
 * DOS doesn't support threading
 * All functions return error codes or provide minimal stubs
 */

int thrd_create(thrd_t *thr, thrd_start_t func, void *arg)
{
    /* Cannot create threads on DOS */
    return thrd_error;
}

int thrd_equal(thrd_t thr0, thrd_t thr1)
{
    /* Only one thread exists on DOS */
    return 1;
}

thrd_t thrd_current(void)
{
    thrd_t thr = {0};
    return thr;
}

int thrd_sleep(const struct timespec *duration, struct timespec *remaining)
{
    /* Simple busy wait - not ideal but DOS has limited sleep support */
    /* Would need to use DOS interrupt 0x15/0x86 for real delay */
    return 0;
}

void thrd_yield(void)
{
    /* No-op on DOS - no other threads to yield to */
}

_Noreturn void thrd_exit(int res)
{
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
    /* No locking needed on single-threaded DOS */
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
    /* Would deadlock on single-threaded system */
    return thrd_error;
}

int cnd_timedwait(cnd_t *restrict cond, mtx_t *restrict mtx,
                  const struct timespec *restrict ts)
{
    /* Would deadlock on single-threaded system */
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
    /* Simple index-based TLS for DOS */
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
