/*
 * Test program for C11 threading support (threads.h)
 * This tests the portable libthread implementation
 */

#include <stdio.h>
#include <threads.h>

#define NUM_THREADS 4
#define COUNT_PER_THREAD 10000

mtx_t mutex;
int shared_counter = 0;

int worker_thread(void *arg) {
    int thread_id = *(int *)arg;
    int local_count = 0;

    printf("Thread %d starting\n", thread_id);

    for (int i = 0; i < COUNT_PER_THREAD; i++) {
        mtx_lock(&mutex);
        shared_counter++;
        local_count++;
        mtx_unlock(&mutex);
    }

    printf("Thread %d finished: incremented counter %d times\n",
           thread_id, local_count);
    return thread_id;
}

int main(void) {
    thrd_t threads[NUM_THREADS];
    int thread_ids[NUM_THREADS];
    int result;

    printf("Testing C11 threading support (threads.h)\n");
    printf("Creating %d threads, each incrementing counter %d times\n\n",
           NUM_THREADS, COUNT_PER_THREAD);

    /* Initialize mutex */
    if (mtx_init(&mutex, mtx_plain) != thrd_success) {
        fprintf(stderr, "Failed to initialize mutex\n");
        return 1;
    }

    /* Create threads */
    for (int i = 0; i < NUM_THREADS; i++) {
        thread_ids[i] = i + 1;
        if (thrd_create(&threads[i], worker_thread, &thread_ids[i]) != thrd_success) {
            fprintf(stderr, "Failed to create thread %d\n", i + 1);
            return 1;
        }
    }

    /* Wait for all threads to complete */
    for (int i = 0; i < NUM_THREADS; i++) {
        if (thrd_join(threads[i], &result) != thrd_success) {
            fprintf(stderr, "Failed to join thread %d\n", i + 1);
            return 1;
        }
        printf("Joined thread %d (returned %d)\n", i + 1, result);
    }

    /* Verify the result */
    int expected = NUM_THREADS * COUNT_PER_THREAD;
    printf("\nExpected counter value: %d\n", expected);
    printf("Actual counter value:   %d\n", shared_counter);

    mtx_destroy(&mutex);

    if (shared_counter == expected) {
        printf("\nC11 threading test PASSED!\n");
        return 0;
    } else {
        printf("\nC11 threading test FAILED!\n");
        return 1;
    }
}
