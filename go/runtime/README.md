# PCC Go Runtime Library

Runtime support library for Go programs compiled with the PCC Go compiler.

## Overview

This library provides the essential runtime infrastructure needed to execute Go programs, including memory management, goroutines, channels, strings, slices, maps, interfaces, and built-in functions.

## Architecture

The runtime is implemented in C and provides:

1. **Memory Management** - Allocation, deallocation, and GC interface
2. **String Operations** - Immutable strings with length tracking
3. **Slice Operations** - Dynamic arrays with capacity management
4. **Map Operations** - Hash table implementation
5. **Channel Operations** - Synchronous/buffered communication channels
6. **Goroutine Support** - Lightweight thread implementation using pthreads
7. **Panic/Recover** - Exception handling mechanism
8. **Interface Support** - Dynamic typing and type assertions
9. **Built-in Functions** - make, new, len, cap, append, copy, etc.
10. **Atomic Operations** - Thread-safe primitives

## Components

### Core Runtime (runtime.c/h)

Main runtime initialization and type system:
- `go_runtime_init()` - Initialize runtime
- `go_runtime_exit()` - Clean shutdown
- Type descriptors for all basic types
- Hash functions for built-in types

### Memory Management (runtime.c, gc.c)

Memory allocation wrappers and GC interface:
- `go_malloc()`, `go_calloc()`, `go_realloc()`, `go_free()` - Safe allocation
- `go_gc_init()`, `go_gc_run()` - Garbage collector interface (stub)
- `go_gc_register()`, `go_gc_unregister()` - Object tracking

**Note**: Current GC is a stub. For production, integrate a real collector like Boehm GC.

### String Operations (string.c)

Immutable string type:
```c
typedef struct {
    const char *data;
    go_int len;
} go_string;
```

Functions:
- `go_string_new()` - Create from C string
- `go_string_concat()` - Concatenate strings
- `go_string_equal()` - Compare for equality
- `go_string_slice()` - Sub-string extraction
- Type conversions (int, float, bool to string)

### Slice Operations (slice.c)

Dynamic arrays with capacity:
```c
typedef struct {
    void *data;
    go_int len;
    go_int cap;
} go_slice;
```

Functions:
- `go_slice_new()` - Create with capacity
- `go_slice_append()` - Add element (auto-grow)
- `go_slice_copy()` - Copy elements
- `go_slice_sub()` - Sub-slice
- Bounds checking with panic on overflow

### Map Operations (map.c)

Hash table with dynamic resizing:

Functions:
- `go_map_new()` - Create map
- `go_map_insert()` - Insert/update entry
- `go_map_lookup()` - Find entry
- `go_map_delete()` - Remove entry
- `go_map_len()` - Get size
- Iterator support for range loops

**Features**:
- FNV-1a hash function
- Load factor-based resizing
- Generic key/value storage

### Channel Operations (chan.c)

Synchronous communication using pthreads:

```c
go_channel *ch = go_chan_new(elem_size, buffer_size);
go_chan_send(ch, &value);    // Blocks if full
go_chan_recv(ch, &value);    // Blocks if empty
go_chan_close(ch);
```

**Features**:
- Buffered and unbuffered channels
- Blocking send/receive
- Non-blocking try_send/try_recv
- Close detection
- Thread-safe with mutexes/condition variables

### Goroutine Support (goroutine.c)

Lightweight threads using pthreads:

```c
void my_function(void) { /* ... */ }

go_routine_id id = go_routine_create(my_function, NULL);
```

**Features**:
- pthread-based implementation
- Automatic cleanup (detached threads)
- Goroutine IDs
- Thread-local storage for current goroutine

**Limitations**:
- Not true green threads (OS threads used)
- No work-stealing scheduler
- No cooperative scheduling

### Panic/Recover (panic.c)

Exception handling:

```c
go_panic(value);          // Unwind and run defers
go_defer(fn, arg);        // Register deferred function
go_interface val = go_recover();  // Catch panic
```

**Features**:
- LIFO defer execution
- Panic propagation
- Recovery in deferred functions
- Thread-local panic state

### Interface Support (iface.c)

Dynamic typing:

```c
typedef struct {
    void *type;    // Type descriptor
    void *data;    // Actual data
} go_interface;
```

Functions:
- `go_iface_new()` - Box value in interface
- `go_iface_type_assert()` - Check type
- `go_type_assert()` - Extract value with type check

### Built-in Functions (builtin.c)

Standard Go built-ins:

```c
// Memory allocation
void *ptr = go_new(&go_type_int);
go_slice s = go_make_slice(&go_type_int, 10, 20);
go_map m = go_make_map(&go_type_int, &go_type_string, 16);
go_channel *ch = go_make_chan(&go_type_int, 5);

// Length and capacity
go_int len = go_len_slice(s);
go_int cap = go_cap_slice(s);

// Slice operations
s = go_append(s, &value, 1, sizeof(int));
go_int n = go_copy_slice(dst, src, sizeof(int));

// Map operations
go_delete_map(m, &key);

// Channel operations
go_close_chan(ch);

// I/O
go_print_int(42);
go_print_string(str);
go_print_newline();
```

### Atomic Operations (atomic.c)

Thread-safe primitives using GCC/Clang built-ins:

```c
go_atomic_add_int32(&counter, 1);
bool swapped = go_atomic_cas_int64(&value, old, new);
int32_t val = go_atomic_load_int32(&var);
go_atomic_store_ptr(&ptr, new_ptr);
```

## Building

```bash
cd go/runtime
make

# Produces libgoruntime.a
```

## Linking

Link compiled Go programs with the runtime library:

```bash
gcc -o myprogram myprogram.o -L. -lgoruntime -lpthread
```

## Usage Example

```c
#include "runtime.h"

int main(int argc, char **argv) {
    // Initialize runtime
    go_runtime_init(argc, argv);

    // Create a string
    go_string s = go_string_new("Hello, World!");
    go_print_string(s);
    go_print_newline();

    // Create a slice
    go_slice slice = go_make_slice(&go_type_int, 0, 10);

    int value = 42;
    slice = go_slice_append(slice, &value, sizeof(int));

    go_print_int(go_len_slice(slice));  // Prints: 1
    go_print_newline();

    // Create a channel
    go_channel *ch = go_make_chan(&go_type_int, 5);
    go_chan_send(ch, &value);

    int received;
    go_chan_recv(ch, &received);

    go_chan_close(ch);

    return 0;
}
```

## Type System

All Go basic types have runtime descriptors:

| Go Type | Runtime Descriptor | Size (64-bit) |
|---------|-------------------|---------------|
| bool | go_type_bool | 1 byte |
| int8 | go_type_int8 | 1 byte |
| uint8 | go_type_uint8 | 1 byte |
| int16 | go_type_int16 | 2 bytes |
| uint16 | go_type_uint16 | 2 bytes |
| int32 | go_type_int32 | 4 bytes |
| uint32 | go_type_uint32 | 4 bytes |
| int64 | go_type_int64 | 8 bytes |
| uint64 | go_type_uint64 | 8 bytes |
| int | go_type_int | 8 bytes |
| uint | go_type_uint | 8 bytes |
| uintptr | go_type_uintptr | 8 bytes |
| float32 | go_type_float32 | 4 bytes |
| float64 | go_type_float64 | 8 bytes |
| string | go_type_string | 16 bytes |

## Thread Safety

- **Channels**: Thread-safe (mutex-protected)
- **Maps**: NOT thread-safe (requires external synchronization)
- **Slices**: NOT thread-safe (requires external synchronization)
- **Strings**: Immutable (inherently thread-safe)
- **Atomic ops**: Thread-safe by design

## Dependencies

- **pthreads**: Required for goroutines and channels
- **GCC/Clang built-ins**: Required for atomic operations

## Limitations

### Current Implementation

1. **Garbage Collection**: Stub only - no automatic memory reclamation
2. **Goroutines**: OS threads (not green threads), no scheduler
3. **Select**: Simplified implementation, not fully concurrent
4. **Reflection**: Limited type introspection
5. **Packages**: No package import/linking support

### For Production Use

Consider:
- Integrate Boehm GC or custom collector
- Implement M:N threading model
- Add full select() with fair selection
- Complete reflection support
- Package system implementation

## Performance Characteristics

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| String concat | O(n+m) | Allocates new string |
| Slice append | O(1) amortized | Doubles capacity when full |
| Slice index | O(1) | With bounds checking |
| Map insert | O(1) average | Resizes at load factor 0.75 |
| Map lookup | O(1) average | Hash table |
| Channel send | O(1) | May block |
| Channel recv | O(1) | May block |
| Goroutine create | O(1) | pthread_create overhead |

## Files

- **runtime.h** - Main header (370 lines)
- **runtime.c** - Core runtime (180 lines)
- **string.c** - String operations (170 lines)
- **slice.c** - Slice operations (140 lines)
- **map.c** - Hash map implementation (320 lines)
- **chan.c** - Channel operations (260 lines)
- **goroutine.c** - Goroutine support (180 lines)
- **panic.c** - Panic/recover/defer (140 lines)
- **iface.c** - Interface operations (90 lines)
- **builtin.c** - Built-in functions (160 lines)
- **gc.c** - GC stub (90 lines)
- **atomic.c** - Atomic operations (90 lines)

**Total: ~2,190 lines of runtime code**

## License

Same license as PCC (BSD-style).

## See Also

- [Go Runtime Specification](https://go.dev/ref/spec)
- [Go Runtime Source](https://github.com/golang/go/tree/master/src/runtime)
- [PCC Go Compiler](../README.md)
