# libgc - Generic Garbage Collector

Generic mark-and-sweep garbage collector for PCC language runtimes.

## Overview

This library provides a language-agnostic garbage collector that can be used by any PCC language runtime (OCaml, Pascal, Fortran, etc.). It implements:

- **Mark-and-Sweep GC**: Classic two-phase garbage collection
- **Configurable Heap**: Customizable heap size and thresholds
- **Callback-Based Marking**: Language runtimes provide mark callbacks
- **Root Registration**: Register global and local roots
- **Heap Compaction**: Optional compaction to reduce fragmentation
- **Statistics**: Detailed GC metrics and debugging

## Features

### Generic Design

The GC is completely language-agnostic:
- No assumptions about value representation
- Callback-based object traversal
- Flexible object headers
- User-defined flags for language-specific metadata

### Memory Management

- Automatic collection when threshold is reached
- Manual collection via `gc_collect()`
- Configurable heap growth
- Memory pooling for better performance

### Statistics and Debugging

- Track total allocated/freed bytes
- Monitor current heap usage
- Count GC collections
- Verbose mode for debugging

## API Usage

### Initialization

```c
#include "gc.h"

gc_config_t config = GC_DEFAULT_CONFIG;
config.heap_size = 32 * 1024 * 1024;  /* 32MB */
config.gc_threshold = 16 * 1024 * 1024; /* Trigger at 16MB */
config.verbose = 1;  /* Enable debug output */

gc_context_t *gc = gc_init(&config);
```

### Allocation

```c
/* Allocate 100 bytes */
void *obj = gc_alloc(gc, 100);

/* Allocate with alignment */
void *aligned = gc_alloc_aligned(gc, 256, 64);
```

### Root Registration

Roots are objects that should never be collected:

```c
void *my_global_object;
gc_register_root(gc, &my_global_object);

/* Later, unregister when no longer needed */
gc_unregister_root(gc, &my_global_object);
```

### Mark Callback

The language runtime must provide a callback to mark referenced objects:

```c
void my_mark_callback(gc_context_t *gc, void *obj, void *userdata)
{
    /* Example: mark children of this object */
    MyObject *o = (MyObject *)obj;

    if (o->child1)
        gc_mark(gc, o->child1);
    if (o->child2)
        gc_mark(gc, o->child2);
}

gc_set_mark_callback(gc, my_mark_callback, NULL);
```

### Garbage Collection

```c
/* Manual collection */
size_t freed = gc_collect(gc);
printf("Freed %zu objects\n", freed);

/* Automatic collection is triggered when usage > threshold */
gc_set_auto(gc, 1);  /* Enable (default) */
gc_set_auto(gc, 0);  /* Disable */
```

### Object Headers

Each allocated object has a header:

```c
typedef struct gc_object {
    size_t size;         /* Size in bytes */
    uint8_t marked;      /* Mark bit for GC */
    uint8_t flags;       /* User-defined flags */
    struct gc_object *next;
} gc_object_t;
```

Access headers:

```c
void *obj = gc_alloc(gc, 100);

gc_object_t *header = gc_get_header(obj);
printf("Object size: %zu\n", header->size);

/* Set user-defined flags */
gc_set_flags(obj, 0x42);
uint8_t flags = gc_get_flags(obj);
```

### Statistics

```c
gc_stats_t stats;
gc_get_stats(gc, &stats);

printf("Total allocated: %zu bytes\n", stats.total_allocated);
printf("Current usage: %zu bytes\n", stats.current_usage);
printf("Collections: %zu\n", stats.num_collections);

/* Or print all stats */
gc_print_stats(gc);
```

### Heap Compaction

```c
/* Enable compaction in config */
config.enable_compaction = 1;

/* Manually trigger compaction */
size_t moved = gc_compact(gc);
printf("Moved %zu objects\n", moved);
```

### Cleanup

```c
/* Destroy GC and free all memory */
gc_destroy(gc);
```

## Integration Example (OCaml)

```c
#include "gc.h"
#include "ocaml_runtime.h"

static gc_context_t *ocaml_gc;

void ocaml_mark_value(gc_context_t *gc, void *obj, void *userdata)
{
    ocaml_value_t val = (ocaml_value_t)obj;
    ocaml_header_t *hdr = HEADER(val);

    /* Mark children */
    for (size_t i = 0; i < hdr->size; i++) {
        ocaml_value_t field = FIELD(val, i);
        if (IS_BLOCK(field))
            gc_mark(gc, (void *)field);
    }
}

void ocaml_gc_init(size_t heap_size)
{
    gc_config_t config = GC_DEFAULT_CONFIG;
    config.heap_size = heap_size;

    ocaml_gc = gc_init(&config);
    gc_set_mark_callback(ocaml_gc, ocaml_mark_value, NULL);
}

ocaml_value_t ocaml_alloc(size_t size, uint8_t tag)
{
    /* Allocate OCaml header + data */
    size_t total = sizeof(ocaml_header_t) + size * sizeof(ocaml_value_t);
    ocaml_header_t *hdr = gc_alloc(ocaml_gc, total);

    hdr->size = size;
    hdr->tag = tag;

    return (ocaml_value_t)(hdr + 1);
}
```

## Configuration Options

```c
typedef struct gc_config {
    size_t heap_size;          /* Initial heap (default: 16MB) */
    size_t gc_threshold;       /* Trigger GC at this usage */
    float growth_factor;       /* Heap growth (default: 1.5) */
    int enable_compaction;     /* Enable compaction (default: 0) */
    int verbose;               /* Debug output (default: 0) */
} gc_config_t;
```

## Performance Tips

1. **Choose appropriate heap size**: Larger heaps reduce GC frequency
2. **Set proper threshold**: Balance between memory usage and GC overhead
3. **Register roots sparingly**: Too many roots slow down marking
4. **Use compaction judiciously**: Compaction has overhead
5. **Disable auto-GC for batch jobs**: Manual control can be more efficient

## Thread Safety

**Note**: This GC is **not thread-safe**. For multi-threaded applications:
- Use one GC per thread, or
- Protect GC calls with mutexes, or
- Use a thread-safe allocator for shared objects

## Limitations

- Simple mark-and-sweep (not generational)
- No incremental collection
- No concurrent collection
- Limited compaction (reorders but doesn't move)

## Future Enhancements

- Generational GC for better performance
- Incremental collection to reduce pause times
- Concurrent marking for multi-core systems
- Better compaction with object relocation
- Memory pools for common object sizes
- Finalizer support for resource cleanup

## Building

The library is built as part of the PCC build system:

```bash
./configure
make
make install
```

This installs:
- `libgc.a` to `$(libdir)`
- `gc.h` to `$(includedir)`

## Testing

See `test_gc.c` for comprehensive test examples.
