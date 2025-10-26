# Dart Compiler Runtime Integration

This document describes how the Dart compiler integrates with the libdart runtime library.

## Overview

The Dart compiler uses a two-stage compilation process:

1. **Dart → C**: The Dart compiler (`dcom`) parses Dart source code and generates C code that uses the libdart runtime library
2. **C → Executable**: A C compiler (gcc) compiles the generated C code and links it with libdart

```
source.dart → [dcom] → output.c → [gcc + libdart] → executable
```

## Code Generation Strategy

### Basic Principles

1. **All Dart values are DartObject pointers**: Every Dart variable, literal, and expression result is represented as a `DartObject *` in generated code
2. **Reference counting**: Objects are retained/released automatically in the generated code
3. **Type conversion**: Dart types are mapped to libdart types at runtime
4. **Runtime initialization**: Every program includes `dart_runtime_init()` and `dart_runtime_cleanup()`

### Type Mapping

| Dart Type | C Type | Runtime Constructor |
|-----------|--------|---------------------|
| `null` | `DartObject *` | `dart_null()` |
| `bool` | `DartObject *` | `dart_bool_new(value)` |
| `int` | `DartObject *` | `dart_int_new(value)` |
| `double` | `DartObject *` | `dart_double_new(value)` |
| `String` | `DartString *` | `dart_string_new(str)` |
| `List<T>` | `DartList *` | `dart_list_new()` |
| `Map<K,V>` | `DartMap *` | `dart_map_new()` |
| `Set<T>` | `DartSet *` | `dart_set_new()` |

### Generated Code Structure

Every compiled Dart program follows this structure:

```c
#include <dart.h>
#include <stdio.h>
#include <stdlib.h>

/* Forward declarations */
// ... function prototypes

/* User-defined functions */
void _dart_main(void) {
    // ... generated code
}

/* Runtime wrapper */
int main(int argc, char **argv) {
    dart_runtime_init();
    _dart_main();
    dart_runtime_cleanup();
    return 0;
}
```

## Expression Translation

### Literals

```dart
// Dart
var x = 42;
var y = 3.14;
var s = "hello";
var b = true;
var n = null;
```

```c
// Generated C
DartObject *x = dart_int_new(42);
DartObject *y = dart_double_new(3.14);
DartObject *s = (DartObject *)dart_string_new("hello");
DartObject *b = dart_bool_new(true);
DartObject *n = dart_null();
```

### Binary Operations

```dart
// Dart
var result = x + y;
```

```c
// Generated C
DartObject *_t0 = NULL;
_t0 = dart_int_new(dart_int_value(x) + dart_int_value(y));
DartObject *result = _t0;
```

### Function Calls

```dart
// Dart
print("Hello, World!");
```

```c
// Generated C
DartObject *_t0 = (DartObject *)dart_string_new("Hello, World!");
dart_print(_t0);
```

### String Operations

```dart
// Dart
var msg = "Hello" + " World";
var upper = msg.toUpperCase();
```

```c
// Generated C
DartObject *_t0 = (DartObject *)dart_string_new("Hello");
DartObject *_t1 = (DartObject *)dart_string_new(" World");
DartObject *msg = (DartObject *)dart_string_concat((DartString *)_t0, (DartString *)_t1);
DartObject *upper = (DartObject *)dart_string_to_upper((DartString *)msg);
```

### Collections

```dart
// Dart
var list = [1, 2, 3];
list.add(4);
var first = list[0];
```

```c
// Generated C
DartList *list = dart_list_new();
dart_list_add(list, dart_int_new(1));
dart_list_add(list, dart_int_new(2));
dart_list_add(list, dart_int_new(3));
dart_list_add(list, dart_int_new(4));
DartObject *first = dart_list_get(list, 0);
```

## Statement Translation

### Variable Declarations

```dart
// Dart
var x;
var y = 10;
int z = 20;
```

```c
// Generated C
DartObject *x = dart_null();
DartObject *y = dart_int_new(10);
DartObject *z = dart_int_new(20);
```

### If Statements

```dart
// Dart
if (condition) {
    // then
} else {
    // else
}
```

```c
// Generated C
DartObject *_t0 = NULL;
// ... evaluate condition into _t0
if (!dart_bool_value(_t0)) goto _L0;
    // then clause
    goto _L1;
_L0:
    // else clause
_L1:
```

### While Loops

```dart
// Dart
while (condition) {
    // body
}
```

```c
// Generated C
_L0:
DartObject *_t0 = NULL;
// ... evaluate condition into _t0
if (!dart_bool_value(_t0)) goto _L1;
    // body
    goto _L0;
_L1:
```

### For Loops

```dart
// Dart
for (var i = 0; i < 10; i++) {
    // body
}
```

```c
// Generated C
DartObject *i = dart_int_new(0);
_L0:
DartObject *_t0 = NULL;
_t0 = dart_bool_new(dart_int_value(i) < 10);
if (!dart_bool_value(_t0)) goto _L1;
    // body
    i = dart_int_new(dart_int_value(i) + 1);
    goto _L0;
_L1:
```

## Built-in Functions

### print()

```dart
// Dart
print(value);
```

```c
// Generated C
dart_print(value);
```

### assert()

```dart
// Dart
assert(condition);
```

```c
// Generated C
if (!dart_bool_value(condition)) {
    DartException *ex = dart_exception_new("Assertion failed");
    dart_exception_throw(ex);
}
```

### identical()

```dart
// Dart
var same = identical(a, b);
```

```c
// Generated C
DartObject *same = dart_bool_new(a == b);
```

## Memory Management

### Reference Counting Rules

1. **Creation**: Objects returned from `_new()` functions have refcount = 1
2. **Assignment**: When storing in a variable, retain the object
3. **Function arguments**: Arguments are not retained (callee's responsibility)
4. **Return values**: Caller must release returned objects
5. **Cleanup**: Release all local variables before function exit

### Example with Proper Memory Management

```dart
// Dart
void example() {
    var x = 42;
    var y = x + 10;
    print(y);
}
```

```c
// Generated C
void _dart_example(void) {
    DartObject *x = dart_int_new(42);         // refcount = 1

    DartObject *_t0 = dart_int_new(10);       // refcount = 1
    DartObject *_t1 = dart_int_new(
        dart_int_value(x) + dart_int_value(_t0)
    );                                        // refcount = 1
    DartObject *y = _t1;

    dart_print(y);

    // Cleanup
    dart_object_release(x);                   // refcount = 0, freed
    dart_object_release(_t0);                 // refcount = 0, freed
    dart_object_release(y);                   // refcount = 0, freed
}
```

## Compiler Driver Integration

The `dart` compiler driver performs these steps:

1. **Compile Dart to C**:
   ```bash
   dcom -o output.c input.dart
   ```

2. **Compile C to executable**:
   ```bash
   gcc -I/usr/local/include -o program output.c -L/usr/local/lib -ldart
   ```

3. **Run**:
   ```bash
   LD_LIBRARY_PATH=/usr/local/lib ./program
   ```

### Command Line Options

```bash
# Compile Dart program
dart input.dart              # Creates a.out

# Specify output file
dart -o program input.dart   # Creates program

# Compile to C only (don't link)
dart -c input.dart           # Creates output.c

# Verbose output
dart -v input.dart

# Keep temporary files
dart -k input.dart
```

## Testing

### Runtime Test Programs

Three test programs demonstrate the integration:

1. **test_runtime.c**: Basic integer operations
2. **test_strings.c**: String operations
3. **test_collections.c**: List operations

### Running Tests

```bash
cd dart/tests
make test
```

### Expected Output

```
=== Testing Dart Runtime Integration ===

--- Test: Runtime Operations ---
52

--- Test: String Operations ---
Hello World!

--- Test: Collection Operations ---
List length: 3
  [0] = 10
  [1] = 20
  [2] = 30

All tests passed!
```

## Performance Considerations

### Optimization Opportunities

1. **Temporary variable elimination**: Reuse temporaries when possible
2. **Constant folding**: Evaluate constant expressions at compile time
3. **Dead code elimination**: Remove unused variables and expressions
4. **Inline small functions**: Reduce function call overhead
5. **String interning**: Deduplicate string literals

### Current Limitations

1. No garbage collection (reference counting only)
2. No tail call optimization
3. No inlining of runtime functions
4. All values boxed (no unboxed integers/doubles)
5. No escape analysis for stack allocation

## Future Enhancements

### Planned Features

1. **Async/await support**: Generate state machines for async functions
2. **Class support**: Virtual method tables and inheritance
3. **Generics**: Template instantiation or type erasure
4. **Null safety**: Compile-time null checks
5. **Optimization passes**: Constant folding, CSE, DCE

### Potential Optimizations

1. **Unboxed integers**: Use C `int64_t` for local variables
2. **String builder**: Optimize string concatenation chains
3. **Collection preallocation**: Reserve capacity for known sizes
4. **Inline caching**: Cache type information for polymorphic calls
5. **Specialized operators**: Fast path for common operations

## Debugging

### Compiler Debug Options

```bash
# Dump AST
dcom -ast input.dart

# Dump symbol table
dcom -symtab input.dart

# Verbose output
dcom -v input.dart
```

### Runtime Debug Options

```c
// In generated code, add:
#define DART_DEBUG 1

// This enables:
// - Reference counting traces
// - Memory allocation tracking
// - Type checking assertions
```

### Common Issues

1. **Segmentation fault**: Usually null pointer or use-after-free
   - Check reference counting
   - Verify object initialization

2. **Memory leaks**: Objects not released
   - Ensure all `_new()` calls have matching `_release()`
   - Check for circular references

3. **Link errors**: libdart not found
   - Set `LD_LIBRARY_PATH=/usr/local/lib`
   - Or install libdart system-wide

## API Reference

See [libdart/README.md](../libdart/README.md) for complete runtime library API documentation.

### Key Functions

- `dart_runtime_init()` - Initialize runtime (call once at startup)
- `dart_runtime_cleanup()` - Cleanup runtime (call once at exit)
- `dart_object_new()` - Create new object
- `dart_object_retain()` - Increment reference count
- `dart_object_release()` - Decrement reference count (may free)
- `dart_print()` - Print any object to stdout

### Type Constructors

- `dart_null()` - Null value
- `dart_bool_new(bool)` - Boolean value
- `dart_int_new(int64_t)` - Integer value
- `dart_double_new(double)` - Double value
- `dart_string_new(const char *)` - String from C string
- `dart_list_new()` - Empty list
- `dart_map_new()` - Empty map
- `dart_set_new()` - Empty set

## Examples

See the `dart/tests/` directory for complete examples of:

- Integer arithmetic
- String operations
- Collection manipulation
- Control flow
- Function calls

## Contributing

When adding new features to the compiler:

1. Update `codegen.c` to emit appropriate C code
2. Add tests in `dart/tests/`
3. Update this documentation
4. Ensure memory management is correct

## License

Part of the Portable C Compiler project.
See COPYING for license information.
