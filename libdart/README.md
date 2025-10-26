# Dart Runtime Library (libdart)

A comprehensive runtime library providing core types and functionality for Dart programs compiled with the PCC Dart compiler.

## Overview

The Dart runtime library implements all the fundamental types, collections, and runtime support needed to execute Dart programs. It provides a complete object system with reference counting, collection classes, string operations, I/O functions, and exception handling.

## Features

### Core Type System

- **Object Model**: Reference-counted object system with automatic memory management
- **Type System**: Support for null, bool, int, double, String, List, Map, Set
- **Type Checking**: Runtime type information and type checking
- **Equality**: Value and reference equality
- **Hashing**: Hash code generation for all object types

### Data Types

#### Primitive Types
- **Null**: Singleton null value
- **Boolean**: true/false values
- **Integer**: 64-bit signed integers
- **Double**: Double-precision floating point

#### String
- **Creation**: From C strings or with explicit length
- **Operations**: concat, substring, indexOf, replace
- **Case conversion**: toUpper, toLower
- **Trimming**: trim whitespace
- **Splitting**: split by separator
- **Comparison**: equals, compare

#### List (Dynamic Array)
- **Creation**: Empty or with initial capacity
- **Access**: get, set by index
- **Modification**: add, insert, removeAt, clear
- **Search**: contains, indexOf
- **Sorting**: Custom comparator support
- **Slicing**: sublist, reversed
- **Properties**: length, capacity

#### Map (Hash Map)
- **Creation**: Empty or with initial capacity
- **Access**: get, set by key
- **Modification**: remove, clear
- **Query**: containsKey, size
- **Iteration**: keys, values
- **Hash-based**: O(1) average access time
- **Auto-resizing**: Dynamic capacity growth

#### Set (Hash Set)
- **Creation**: Empty or with initial capacity
- **Modification**: add, remove, clear
- **Query**: contains, size
- **Conversion**: toList
- **Set Operations**: union, intersection, difference
- **Hash-based**: O(1) average operations

### I/O Functions

- **dart_print()**: Print any object to stdout
- **dart_print_string()**: Print C string
- **dart_read_line()**: Read line from stdin

### Exception Handling

- **Creation**: dart_exception_new()
- **Throwing**: dart_exception_throw()
- **Catching**: dart_exception_catch()
- **Stack traces**: Captured on throw
- **Unhandled exceptions**: Automatic termination

### Memory Management

- **Reference Counting**: Automatic memory management
- **dart_object_retain()**: Increment reference count
- **dart_object_release()**: Decrement and free when zero
- **Custom allocators**: dart_malloc, dart_calloc, dart_realloc, dart_free
- **Statistics**: Track allocation/deallocation

### Iterators

- **Creation**: dart_iterator_new()
- **Iteration**: has_next, next
- **Support**: Lists, Strings
- **Cleanup**: dart_iterator_free()

## API Reference

### Object Management

```c
DartObject *dart_object_new(DartType type, size_t size);
void dart_object_retain(DartObject *obj);
void dart_object_release(DartObject *obj);
bool dart_object_equals(DartObject *a, DartObject *b);
int dart_object_hash(DartObject *obj);
DartString *dart_object_to_string(DartObject *obj);
```

### Null

```c
DartObject *dart_null(void);
bool dart_is_null(DartObject *obj);
```

### Boolean

```c
DartObject *dart_bool_new(bool value);
bool dart_bool_value(DartObject *obj);
```

### Integer

```c
DartObject *dart_int_new(int64_t value);
int64_t dart_int_value(DartObject *obj);
```

### Double

```c
DartObject *dart_double_new(double value);
double dart_double_value(DartObject *obj);
```

### String

```c
DartString *dart_string_new(const char *str);
DartString *dart_string_empty(void);
const char *dart_string_cstr(DartString *str);
size_t dart_string_length(DartString *str);
DartString *dart_string_concat(DartString *a, DartString *b);
DartString *dart_string_substring(DartString *str, size_t start, size_t end);
bool dart_string_equals(DartString *a, DartString *b);
DartString *dart_string_to_upper(DartString *str);
DartString *dart_string_to_lower(DartString *str);
```

### List

```c
DartList *dart_list_new(void);
size_t dart_list_length(DartList *list);
DartObject *dart_list_get(DartList *list, size_t index);
void dart_list_set(DartList *list, size_t index, DartObject *value);
void dart_list_add(DartList *list, DartObject *value);
bool dart_list_contains(DartList *list, DartObject *value);
```

### Map

```c
DartMap *dart_map_new(void);
DartObject *dart_map_get(DartMap *map, DartObject *key);
void dart_map_set(DartMap *map, DartObject *key, DartObject *value);
bool dart_map_contains_key(DartMap *map, DartObject *key);
DartList *dart_map_keys(DartMap *map);
```

### Set

```c
DartSet *dart_set_new(void);
void dart_set_add(DartSet *set, DartObject *value);
bool dart_set_contains(DartSet *set, DartObject *value);
DartSet *dart_set_union(DartSet *a, DartSet *b);
```

## Building

```bash
# Build static and shared libraries
make

# Install
make install

# Clean
make clean
```

## Usage Example

```c
#include <dart.h>

int main(void) {
    /* Initialize runtime */
    dart_runtime_init();

    /* Create a list */
    DartList *list = dart_list_new();

    /* Add some integers */
    dart_list_add(list, dart_int_new(10));
    dart_list_add(list, dart_int_new(20));
    dart_list_add(list, dart_int_new(30));

    /* Print list length */
    printf("List length: %zu\n", dart_list_length(list));

    /* Access elements */
    DartObject *first = dart_list_get(list, 0);
    printf("First element: %lld\n", dart_int_value(first));

    /* Create a string */
    DartString *str = dart_string_new("Hello, Dart!");
    dart_print((DartObject *)str);

    /* String operations */
    DartString *upper = dart_string_to_upper(str);
    dart_print((DartObject *)upper);

    /* Create a map */
    DartMap *map = dart_map_new();
    dart_map_set(map, (DartObject *)dart_string_new("name"),
                      (DartObject *)dart_string_new("Alice"));

    DartObject *name = dart_map_get(map,
                         (DartObject *)dart_string_new("name"));
    dart_print(name);

    /* Clean up */
    dart_object_release((DartObject *)list);
    dart_object_release((DartObject *)str);
    dart_object_release((DartObject *)upper);
    dart_object_release((DartObject *)map);

    dart_runtime_cleanup();
    return 0;
}
```

Compile with:
```bash
gcc -o example example.c -ldart
```

## Memory Management

The library uses reference counting for automatic memory management:

1. **Retain**: Increment reference count when holding a reference
2. **Release**: Decrement reference count when done
3. **Automatic**: Objects freed when reference count reaches zero

### Rules

- Objects created with `_new()` functions have reference count 1
- Use `dart_object_retain()` when storing a reference
- Use `dart_object_release()` when done with a reference
- Collections automatically retain/release their elements

## Performance

### Time Complexity

- **List**: O(1) access, O(1) amortized append, O(n) insert/delete
- **Map**: O(1) average get/set/remove
- **Set**: O(1) average add/contains/remove
- **String**: O(n) most operations, O(1) length

### Space Complexity

- **List**: O(n) with dynamic growth (capacity >= length)
- **Map**: O(n) with load factor 0.75
- **Set**: O(n) with load factor 0.75
- **String**: O(n) with null terminator

## Thread Safety

The library is **not thread-safe** by default. For concurrent access:

- Use external synchronization (mutexes, locks)
- Clone objects for thread-local use
- Avoid sharing mutable objects across threads

## Limitations

- No garbage collection (reference counting only)
- No circular reference detection
- Single-threaded by default
- Limited Unicode support (UTF-8 strings)
- No async/await runtime support (planned)
- No reflection/mirrors (planned)

## Implementation Details

### Reference Counting

```c
struct DartObject {
    DartType type;
    int ref_count;      // -1 for immortal objects
    void *data;
    void (*destructor)(DartObject *);
};
```

### Hash Tables

Both Map and Set use hash tables with:
- Separate chaining for collision resolution
- Dynamic resizing at 75% load factor
- Custom hash functions per type

### String Internals

```c
struct DartString {
    DartObject base;
    size_t length;
    size_t capacity;
    char *data;         // Null-terminated
};
```

## Testing

A test suite is included in the examples directory:

```bash
cd examples
make test
```

## Contributing

Contributions welcome! Areas for improvement:

- Unicode/UTF-8 support
- Circular reference detection
- Thread safety
- Performance optimization
- Additional collection types
- Async/await support

## License

Part of the Portable C Compiler project.
See COPYING for license information.

## See Also

- [PCC Dart Compiler](../dart/README.md)
- [PCC Project](http://pcc.ludd.ltu.se/)
