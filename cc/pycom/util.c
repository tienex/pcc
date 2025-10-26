/*
 * Utility Functions
 */

#include "pycom.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

/* Global variables */
int error_count = 0;
int line_number = 1;

/* Error reporting */
void error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "Error: ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");

    va_end(args);

    error_count++;

    /* Exit after too many errors */
    if (error_count >= 10) {
        fprintf(stderr, "Too many errors, stopping compilation\n");
        exit(1);
    }
}

/* Memory allocation wrappers */
void *xmalloc(size_t size) {
    void *ptr = malloc(size);
    if (!ptr && size != 0) {
        error("Out of memory");
        exit(1);
    }
    return ptr;
}

void *xrealloc(void *ptr, size_t size) {
    void *new_ptr = realloc(ptr, size);
    if (!new_ptr && size != 0) {
        error("Out of memory");
        exit(1);
    }
    return new_ptr;
}

char *xstrdup(const char *str) {
    if (!str) return NULL;

    size_t len = strlen(str);
    char *copy = xmalloc(len + 1);
    strcpy(copy, str);
    return copy;
}
