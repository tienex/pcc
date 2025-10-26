# Generalized ARC Library Support for PCC

This document describes the generalized Automatic Reference Counting (ARC) library infrastructure added to PCC, providing a front-end agnostic implementation usable by Objective-C, Objective-C++, and potentially other languages requiring automatic memory management.

## Overview

The ARC library provides a complete framework for implementing automatic memory management through reference counting. It is designed to be language-neutral and can be adapted to different runtime implementations (Apple, GNU, custom).

## Architecture

### Components

```
┌─────────────────────────────────────────────────────────┐
│                   Language Front End                    │
│            (Objective-C, Objective-C++, etc.)           │
└──────────────────────┬──────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────┐
│              ARC Library (arc.c / arc.h)                │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │   Ownership  │  │    Scope     │  │   Code Gen   │  │
│  │  Qualifiers  │  │  Management  │  │   Helpers    │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ Optimization │  │  Bridging    │  │ Diagnostics  │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└──────────────────────┬──────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────┐
│              Runtime Library Interface                  │
│         (Apple runtime, GNU runtime, Custom)            │
└─────────────────────────────────────────────────────────┘
```

### File Structure

- **cc/ccom/arc.h**: Public API and data structures
- **cc/ccom/arc.c**: Implementation of ARC transformations
- **Compiler integration**: Driver, lexer, grammar, type system

## Features

### 1. Ownership Qualifiers

ARC provides four ownership qualifiers:

#### `__strong` (Default)
- Strong ownership: retains objects
- Automatically released when variable goes out of scope
- Default for all object pointers under ARC

```objective-c
__strong id obj = [[MyClass alloc] init];  // Strong by default
// obj is automatically released at end of scope
```

#### `__weak`
- Weak reference: does not retain objects
- Automatically zeroed when object is deallocated
- Prevents retain cycles

```objective-c
__weak id delegate = self.delegate;  // Won't create retain cycle
// Automatically set to nil if delegate is deallocated
```

#### `__unsafe_unretained`
- No ownership: does not retain
- Not zeroed when object is deallocated (unsafe)
- Used for performance or legacy compatibility

```objective-c
__unsafe_unretained id unsafeRef = obj;  // Not retained, not zeroed
```

#### `__autoreleasing`
- Object is autoreleased when assigned
- Used for out-parameters and temporary values

```objective-c
- (BOOL)getValue:(__autoreleasing id *)outValue error:(NSError **)error {
    *outValue = [self someValue];  // Autoreleased on assignment
    return YES;
}
```

### 2. Scope Management

The ARC library tracks variables and their lifetimes across scopes:

```c
struct arc_scope {
    int level;                    /* Scope nesting level */
    struct arc_var *vars;         /* Variables in this scope */
    struct arc_scope *parent;     /* Parent scope */
};
```

**Key Functions:**
- `arc_scope_push(int level)`: Enter new scope
- `arc_scope_pop()`: Exit scope and generate cleanup
- `arc_scope_cleanup()`: Generate release calls for scope

### 3. Code Generation

The library provides helpers for inserting runtime calls:

```c
P1ND *arc_insert_retain(P1ND *expr);
P1ND *arc_insert_release(P1ND *expr);
P1ND *arc_insert_autorelease(P1ND *expr);
P1ND *arc_insert_store_strong(P1ND *dest, P1ND *src);
P1ND *arc_insert_store_weak(P1ND *dest, P1ND *src);
P1ND *arc_insert_load_weak(P1ND *expr);
```

**Example transformation:**
```objective-c
// Source code:
__strong id obj = [factory createObject];

// Generated (conceptual):
id temp = [factory createObject];
id obj = objc_retain(temp);
// ... use obj ...
objc_release(obj);
```

### 4. Runtime Abstraction

Support for multiple runtime implementations:

```c
struct arc_runtime {
    char *retain;              /* objc_retain */
    char *release;             /* objc_release */
    char *autorelease;         /* objc_autorelease */
    char *store_strong;        /* objc_storeStrong */
    char *store_weak;          /* objc_storeWeak */
    char *load_weak;           /* objc_loadWeak */
    /* ... more runtime functions ... */
};
```

**Configuration:**
```c
// Use Apple runtime (default)
arc_init();

// Use custom runtime
struct arc_runtime custom_rt = {
    .retain = "my_retain",
    .release = "my_release",
    /* ... */
};
arc_set_runtime(&custom_rt);
```

### 5. Bridging (Core Foundation Interop)

ARC provides bridging casts for Core Foundation types:

#### `__bridge`
No ownership transfer:
```objective-c
CFStringRef cfStr = ...;
NSString *nsStr = (__bridge NSString *)cfStr;  // No retain/release
```

#### `__bridge_retained` (CF → Objective-C)
Transfer ownership from CF to ARC:
```objective-c
CFStringRef cfStr = CFStringCreateCopy(NULL, ...);
NSString *nsStr = (__bridge_retained NSString *)cfStr;  // CF owns -> ARC owns
// ARC will release nsStr
```

#### `__bridge_transfer` (Objective-C → CF)
Transfer ownership from ARC to CF:
```objective-c
NSString *nsStr = [[NSString alloc] init];
CFStringRef cfStr = (__bridge_transfer CFStringRef)nsStr;  // ARC owns -> CF owns
// Must CFRelease(cfStr) manually
```

### 6. Exception Support

ARC integrates with exception handling:

```c
void arc_try_begin(void);
void arc_try_end(void);
void arc_catch_begin(void);
void arc_catch_end(void);
P1ND *arc_exception_cleanup(void);
```

**Usage:**
```objective-c
@try {
    __strong id obj = [[MyClass alloc] init];
    [obj doSomething];
}
@catch (NSException *e) {
    // obj is properly released even if exception thrown
}
```

### 7. Optimization

The ARC library includes optimization passes:

```c
arc_optimize_function();                  /* Function-level optimizations */
arc_eliminate_redundant_ops(tree);        /* Remove redundant retain/release */
arc_fold_retain_autorelease();            /* Fold retain+autorelease */
```

**Example optimization:**
```objective-c
// Before optimization:
id obj = objc_retain([factory create]);
objc_release(obj);
objc_retain(obj);

// After optimization:
id obj = [factory create];  // Redundant ops eliminated
```

### 8. Diagnostics

ARC provides comprehensive diagnostics:

```c
arc_warn_unsafe_assign(dest, src);      /* Unsafe assignment warning */
arc_warn_leak(expr);                     /* Potential memory leak */
arc_warn_double_release(expr);           /* Double release detected */
arc_error_retain_cycle(var);             /* Retain cycle error */
```

## Compilation

### Enabling ARC

```bash
# Enable ARC for Objective-C
pcc -fobjc-arc myfile.m -o myfile.o

# Disable ARC (manual memory management)
pcc -fno-objc-arc myfile.m -o myfile.o
```

### Preprocessor Macros

When ARC is enabled:
- `__OBJC_ARC__` is defined
- Can be used for conditional compilation:

```objective-c
#if __OBJC_ARC__
    // ARC code
    __strong id obj = ...;
#else
    // Manual memory management
    id obj = [[... alloc] init];
    [obj release];
#endif
```

### Compiler Flags

The ARC flag is passed internally:
- `-fobjc-arc` → `-xobjc-arc` to ccom
- Preprocessor receives `-D__OBJC_ARC__`

## Usage Examples

### Example 1: Basic ARC Usage

```objective-c
// Compiled with: pcc -fobjc-arc example.m

#import <Foundation/Foundation.h>

@interface Person : NSObject
@property (strong) NSString *name;  // Strong property
@property (weak) Person *spouse;    // Weak to avoid retain cycle
@end

@implementation Person

- (void)marry:(Person *)partner {
    self.spouse = partner;      // Weak assignment
    partner.spouse = self;      // No retain cycle!
}

@end

int main(void) {
    @autoreleasepool {
        Person *alice = [[Person alloc] init];  // Retained
        alice.name = @"Alice";                  // String retained

        Person *bob = [[Person alloc] init];
        bob.name = @"Bob";

        [alice marry:bob];  // No retain cycle due to weak reference

        // alice and bob automatically released at end of scope
    }
    return 0;
}
```

### Example 2: Bridging with Core Foundation

```objective-c
// Compiled with: pcc -fobjc-arc -framework CoreFoundation bridging.m

#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>

void example_bridging(void) {
    // Objective-C string (ARC managed)
    NSString *nsStr = @"Hello";

    // Transfer to Core Foundation (no ownership transfer)
    CFStringRef cfStr1 = (__bridge CFStringRef)nsStr;
    // nsStr is still owned by ARC

    // Transfer ownership from ARC to CF
    CFStringRef cfStr2 = (__bridge_transfer CFStringRef)nsStr;
    // Must manually CFRelease(cfStr2)
    CFRelease(cfStr2);

    // Transfer ownership from CF to ARC
    CFStringRef cfStr3 = CFStringCreateCopy(NULL, CFSTR("World"));
    NSString *nsStr2 = (__bridge_retained NSString *)cfStr3;
    // ARC now owns nsStr2, will release automatically
    CFRelease(cfStr3);  // Release original CF ownership
}
```

### Example 3: Weak References to Avoid Retain Cycles

```objective-c
// Compiled with: pcc -fobjc-arc weak_refs.m

#import <Foundation/Foundation.h>

@interface Parent : NSObject
@property (strong) NSMutableArray *children;
@end

@interface Child : NSObject
@property (weak) Parent *parent;  // Weak to avoid cycle
@end

@implementation Parent
- (instancetype)init {
    if ((self = [super init])) {
        _children = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)addChild:(Child *)child {
    [_children addObject:child];  // Strong reference in array
    child.parent = self;          // Child has weak reference to parent
}
@end

@implementation Child
@end

int main(void) {
    @autoreleasepool {
        Parent *parent = [[Parent alloc] init];

        for (int i = 0; i < 10; i++) {
            Child *child = [[Child alloc] init];
            [parent addChild:child];
        }

        // No retain cycle: children strongly reference parent
        // but parent only has weak references from children

    }  // parent and all children properly released
    return 0;
}
```

## Integration with Language Front Ends

### Objective-C Integration

The Objective-C front end integrates ARC as follows:

1. **Variable Declaration**:
```c
void arc_var_declared(struct symtab *var);
```

2. **Assignment**:
```c
P1ND *arc_handle_assign(P1ND *dest, P1ND *src);
```

3. **Return Statement**:
```c
P1ND *arc_handle_return(P1ND *expr);
```

4. **Scope Exit**:
```c
P1ND *arc_scope_cleanup(void);
```

### Objective-C++ Integration

Similar integration points, with additional support for C++ RAII:
- ARC-managed Objective-C objects in C++ classes
- Integration with C++ destructors
- Exception safety across language boundaries

### Custom Language Integration

To integrate ARC with a custom language:

1. **Initialize ARC**:
```c
#include "arc.h"

void mylang_init(void) {
    arc_enabled = 1;
    arc_init();
}
```

2. **Track Variables**:
```c
void mylang_declare_var(struct symtab *var) {
    if (is_managed_type(var->stype)) {
        arc_var_declared(var);
    }
}
```

3. **Transform Assignments**:
```c
P1ND *mylang_assign(P1ND *dest, P1ND *src) {
    if (arc_enabled && is_managed_type(dest->n_type)) {
        return arc_handle_assign(dest, src);
    }
    return biop(ASSIGN, dest, src);
}
```

4. **Generate Cleanup**:
```c
void mylang_exit_scope(void) {
    P1ND *cleanup = arc_scope_cleanup();
    if (cleanup) {
        emit_code(cleanup);
    }
    arc_scope_pop();
}
```

## Runtime Requirements

ARC requires a compatible runtime library providing these functions:

### Core Functions
- `id objc_retain(id obj)` - Increment reference count
- `void objc_release(id obj)` - Decrement reference count
- `id objc_autorelease(id obj)` - Add to autorelease pool

### Strong References
- `void objc_storeStrong(id *location, id obj)` - Store with retain/release

### Weak References
- `void objc_storeWeak(id *location, id obj)` - Store weak reference
- `id objc_loadWeak(id *location)` - Load weak reference
- `void objc_destroyWeak(id *location)` - Destroy weak reference
- `void objc_copyWeak(id *dest, id *src)` - Copy weak reference
- `void objc_moveWeak(id *dest, id *src)` - Move weak reference

### Optimization
- `id objc_retainAutorelease(id obj)` - Combined retain+autorelease

### Available Runtimes

1. **Apple Runtime** (macOS, iOS)
   - Default configuration
   - Full ARC support including zeroing weak references
   - Link with `-lobjc`

2. **GNU Runtime** (GCC)
   - Partial ARC support
   - May require custom runtime functions
   - Link with `-lobjc -lgcc`

3. **Custom Runtime**
   - Implement required functions
   - Configure via `arc_set_runtime()`

## Statistics and Debugging

### Enable Statistics

```c
arc_print_stats();
```

**Output:**
```
ARC Statistics:
  Retains:        142
  Releases:       142
  Autoreleases:   28
  Optimized away: 15
  Weak refs:      8
```

### Debugging

Set `arc_enabled` in debugger to trace ARC operations:

```gdb
(gdb) set arc_enabled=1
(gdb) b arc_insert_retain
(gdb) run
```

## Performance Considerations

### Overhead

ARC adds:
- **Runtime overhead**: Retain/release calls (usually negligible)
- **Compile-time overhead**: Analysis and transformation
- **Code size**: Additional runtime calls

### Optimizations

The ARC library includes several optimizations:

1. **Redundant Elimination**: Remove retain/release pairs
2. **Scope Merging**: Combine cleanup operations
3. **Return Optimization**: Avoid retain+autorelease for returns
4. **Weak Reference Caching**: Minimize weak reference operations

### Best Practices

1. Use `__weak` for delegates and parent references
2. Use `__unsafe_unretained` only when performance critical
3. Avoid excessive autoreleasing in tight loops
4. Use autorelease pools in long-running loops

## Limitations

1. **C++ Integration**: Incomplete support for Objective-C++ edge cases
2. **Blocks**: Block capture semantics not yet fully implemented
3. **C Pointers**: No tracking for `malloc`/`free` memory
4. **Runtime Detection**: Does not auto-detect runtime capabilities

## Future Enhancements

- [ ] Block capture semantics
- [ ] Advanced optimization passes
- [ ] Static analysis for retain cycles
- [ ] Integration with garbage collection
- [ ] Support for custom allocators
- [ ] Thread-safe weak reference implementation

## References

- [Clang ARC Documentation](https://clang.llvm.org/docs/AutomaticReferenceCounting.html)
- [Apple ARC Release Notes](https://developer.apple.com/library/archive/releasenotes/ObjectiveC/RN-TransitioningToARC/)
- [Objective-C Runtime](https://developer.apple.com/documentation/objectivec/objective-c_runtime)

## Version History

- **2025-10-26**: Initial generalized ARC library implementation
  - Complete ownership qualifier support
  - Scope management and cleanup generation
  - Bridging cast support
  - Runtime abstraction layer
  - Multiple front-end support
