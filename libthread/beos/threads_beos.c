/*
 * C11 threads implementation
 * Portable threading library for systems lacking C11 thread support
 */

#include "threads.h"
#include <stdlib.h>
#include <stdint.h>

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
