/*
 * C11 threads.h - Portable threading library
 * Implements C11 thread support for systems that lack it
 */

#ifndef _THREADS_H_
#define _THREADS_H_

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Thread return values */
enum {
    thrd_success  = 0,
    thrd_nomem    = 1,
    thrd_timedout = 2,
    thrd_busy     = 3,
    thrd_error    = 4
};

/* Mutex types */
enum {
    mtx_plain     = 0x01,
    mtx_recursive = 0x02,
    mtx_timed     = 0x04
};

/* Thread-specific storage destructor */
typedef void (*tss_dtor_t)(void*);

/* Thread function */
typedef int (*thrd_start_t)(void*);

/* Platform-specific implementations */
#if defined(_WIN32) || defined(_WIN64)
/* Windows implementation */
#include <windows.h>

typedef struct {
    HANDLE handle;
    thrd_start_t func;
    void *arg;
} thrd_t;

typedef CRITICAL_SECTION mtx_t;
typedef CONDITION_VARIABLE cnd_t;
typedef DWORD tss_t;
typedef INIT_ONCE once_flag;

#define ONCE_FLAG_INIT INIT_ONCE_STATIC_INIT
#define TSS_DTOR_ITERATIONS 4

#elif defined(__unix__) || defined(__APPLE__) || defined(__linux__)
/* POSIX implementation */
#include <pthread.h>

typedef pthread_t thrd_t;
typedef pthread_mutex_t mtx_t;
typedef pthread_cond_t cnd_t;
typedef pthread_key_t tss_t;
typedef pthread_once_t once_flag;

#define ONCE_FLAG_INIT PTHREAD_ONCE_INIT
#define TSS_DTOR_ITERATIONS PTHREAD_DESTRUCTOR_ITERATIONS

#else
#error "Unsupported platform for C11 threads"
#endif

/* Thread management */
int thrd_create(thrd_t *thr, thrd_start_t func, void *arg);
int thrd_equal(thrd_t thr0, thrd_t thr1);
thrd_t thrd_current(void);
int thrd_sleep(const struct timespec *duration, struct timespec *remaining);
void thrd_yield(void);
_Noreturn void thrd_exit(int res);
int thrd_detach(thrd_t thr);
int thrd_join(thrd_t thr, int *res);

/* Mutex management */
int mtx_init(mtx_t *mtx, int type);
int mtx_lock(mtx_t *mtx);
int mtx_timedlock(mtx_t *restrict mtx, const struct timespec *restrict ts);
int mtx_trylock(mtx_t *mtx);
int mtx_unlock(mtx_t *mtx);
void mtx_destroy(mtx_t *mtx);

/* Condition variable management */
int cnd_init(cnd_t *cond);
int cnd_signal(cnd_t *cond);
int cnd_broadcast(cnd_t *cond);
int cnd_wait(cnd_t *cond, mtx_t *mtx);
int cnd_timedwait(cnd_t *restrict cond, mtx_t *restrict mtx,
                  const struct timespec *restrict ts);
void cnd_destroy(cnd_t *cond);

/* Thread-local storage */
int tss_create(tss_t *key, tss_dtor_t dtor);
void *tss_get(tss_t key);
int tss_set(tss_t key, void *val);
void tss_delete(tss_t key);

/* Call once */
void call_once(once_flag *flag, void (*func)(void));

#ifdef __cplusplus
}
#endif

#endif /* _THREADS_H_ */
