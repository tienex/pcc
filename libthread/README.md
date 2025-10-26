# libthread - Portable C11 Threading Library

## Overview

This library provides a portable implementation of C11 threading primitives
(`threads.h`) for systems that lack native C11 thread support. It wraps
platform-specific threading APIs (POSIX threads on Unix-like systems,
Windows threads on Windows) to provide a standard C11 interface.

## Features

- **C11 Thread API**: Complete implementation of C11 `<threads.h>`
- **Multi-Platform**: Supports POSIX (Linux, BSD, macOS) and Windows
- **Standards Compliant**: Implements C11 ISO/IEC 9899:2011 threading standard
- **Automatic Linking**: Linked automatically if OS lacks C11 thread support

## API

The library implements the complete C11 threading API from `<threads.h>`:

### Thread Management

- `thrd_create()` - Create a new thread
- `thrd_current()` - Get the current thread identifier
- `thrd_detach()` - Detach a thread
- `thrd_equal()` - Compare thread identifiers
- `thrd_exit()` - Terminate the calling thread
- `thrd_join()` - Wait for a thread to terminate
- `thrd_sleep()` - Suspend execution for an interval
- `thrd_yield()` - Yield the processor

### Mutex Management

- `mtx_init()` - Create a mutex
- `mtx_destroy()` - Destroy a mutex
- `mtx_lock()` - Lock a mutex
- `mtx_timedlock()` - Lock a mutex with timeout
- `mtx_trylock()` - Try to lock a mutex
- `mtx_unlock()` - Unlock a mutex

### Condition Variables

- `cnd_init()` - Create a condition variable
- `cnd_destroy()` - Destroy a condition variable
- `cnd_broadcast()` - Unblock all threads waiting on a condition variable
- `cnd_signal()` - Unblock one thread waiting on a condition variable
- `cnd_timedwait()` - Wait on a condition variable with timeout
- `cnd_wait()` - Wait on a condition variable

### Thread-Local Storage

- `tss_create()` - Create thread-specific storage key
- `tss_delete()` - Delete thread-specific storage key
- `tss_get()` - Get thread-specific value
- `tss_set()` - Set thread-specific value

### Call Once

- `call_once()` - Call a function exactly once

## Usage

```c
#include <threads.h>
#include <stdio.h>

int thread_func(void *arg) {
    printf("Hello from thread!\n");
    return 0;
}

int main(void) {
    thrd_t thread;

    if (thrd_create(&thread, thread_func, NULL) != thrd_success) {
        fprintf(stderr, "Failed to create thread\n");
        return 1;
    }

    thrd_join(thread, NULL);
    return 0;
}
```

## Platform Support

### POSIX Systems (Linux, BSD, macOS, etc.)
- Wraps `pthread.h` API
- Automatically links with `-lpthread` if needed

### Windows
- Wraps Windows threading API
- Uses native Windows threads, mutexes, and condition variables

## Return Values

All functions return standardized error codes:

- `thrd_success` - Success
- `thrd_nomem` - Out of memory
- `thrd_timedout` - Timeout occurred
- `thrd_busy` - Resource busy
- `thrd_error` - General error

## Building

This library is automatically built by the PCC build system if your system
lacks native C11 threading support. The configure script will detect this and
include the library in the build.

On POSIX systems, the library will automatically link against the pthread
library if needed.

## Notes

- Thread-local storage destructors are fully supported on POSIX systems
- On Windows, TLS destructors must be handled manually by the application
- The library is thread-safe and re-entrant

## License

This library is part of the Portable C Compiler (PCC) project and is distributed
under the same BSD-style license as PCC.
