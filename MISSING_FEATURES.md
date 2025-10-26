# PCC Missing Features - What's NOT Implemented

This document lists features from C99, C11, and C23 that are **NOT YET IMPLEMENTED** in PCC.

---

## C99 Missing Features

### Complex Numbers (Partial Support Only) ⚠️

**Missing:**
- Full `<complex.h>` library support
- Complex arithmetic operations
- Complex math functions

**Status:** Basic `_Complex` type exists but library is incomplete

**Example of what doesn't work:**
```c
#include <complex.h>

double complex z1 = 1.0 + 2.0*I;  // May not work fully
double complex z2 = csqrt(z1);     // Function may be missing
```

**Workaround:** Use manual real/imaginary part handling or external library

---

### Type-Generic Math (`<tgmath.h>`) ⚠️

**Missing:**
- Type-generic math macros
- Automatic type selection for math functions

**Example of what doesn't work:**
```c
#include <tgmath.h>

float f = 1.0f;
sqrt(f);  // Should call sqrtf() automatically - may not work
```

**Workaround:** Call type-specific functions directly (`sqrtf`, `sqrt`, `sqrtl`)

---

## C11 Missing Features

### 1. Generic Selections (`_Generic`) ❌ **NOT IMPLEMENTED**

**What it is:** Compile-time type-based selection (like C++ function overloading)

**Example of what doesn't work:**
```c
#define cbrt(X) _Generic((X), \
    long double: cbrtl, \
    default: cbrt,      \
    float: cbrtf        \
)(X)

// This syntax is NOT supported in PCC
float x = 8.0f;
float result = cbrt(x);  // Would call cbrtf() automatically
```

**Impact:** HIGH - Required for many C11 generic macros
**Workaround:** Call type-specific functions manually
**Implementation Difficulty:** Medium (parser + semantic analysis)

---

### 2. Full Atomic Operations (`_Atomic`) ⚠️ **PARTIAL**

**What's missing:**
- Full `<stdatomic.h>` support
- Atomic operations on complex types
- Memory ordering guarantees
- Atomic compound operations

**What works:**
- Basic `_Atomic` type qualifier (limited)

**Example of what doesn't work:**
```c
#include <stdatomic.h>

_Atomic int counter = 0;
atomic_fetch_add(&counter, 1);           // May not work
atomic_store_explicit(&counter, 5, memory_order_release);  // May not work

// Atomic structs
struct Data { int x; int y; };
_Atomic struct Data shared_data;  // Likely doesn't work
```

**Impact:** MEDIUM - Important for concurrent programming
**Workaround:** Use `libthread` mutexes or platform-specific atomics
**Implementation Difficulty:** Hard (requires compiler + platform support)

---

### 3. Alignment Specifiers (`_Alignas`, `_Alignof`) ❌ **NOT IMPLEMENTED**

**What's missing:**
- `_Alignas` - Specify alignment requirements
- `_Alignof` - Query alignment of types
- `aligned_alloc()` - Allocate aligned memory

**Example of what doesn't work:**
```c
#include <stdalign.h>

// Specify alignment
_Alignas(16) int aligned_array[100];  // NOT supported

// Query alignment
size_t alignment = _Alignof(max_align_t);  // NOT supported

// Aligned allocation
void *ptr = aligned_alloc(64, 1024);  // May not be available
```

**Impact:** LOW-MEDIUM - Important for SIMD, cache optimization
**Workaround:** Use platform-specific attributes (`__attribute__((aligned(N)))` on GCC)
**Implementation Difficulty:** Medium (compiler support needed)

---

### 4. Thread-Local Storage (`_Thread_local`) ⚠️ **LIBRARY ONLY**

**What's missing:**
- `_Thread_local` keyword in compiler
- `thread_local` convenience macro

**What works:**
- Thread-local storage via `tss_t` in `<threads.h>`

**Example of what doesn't work:**
```c
_Thread_local int per_thread_counter = 0;  // Compiler keyword NOT supported
thread_local int per_thread_data = 0;       // Also NOT supported
```

**What DOES work (workaround):**
```c
#include <threads.h>

tss_t key;
tss_create(&key, NULL);
tss_set(key, &data);
void *data = tss_get(key);
```

**Impact:** MEDIUM - But good workaround exists
**Workaround:** Use `tss_*` functions from `libthread`
**Implementation Difficulty:** Medium (compiler support needed)

---

### 5. Static Assertions in Declarations ⚠️ **LIMITED**

**What may not work:**
- Static assertions outside of functions
- Static assertions in struct declarations

**Example that may have issues:**
```c
// At file scope
_Static_assert(sizeof(int) == 4, "int must be 4 bytes");  // May not work everywhere

struct Data {
    int x;
    _Static_assert(offsetof(struct Data, x) == 0, "x must be first");  // May not work
    int y;
};
```

**Impact:** LOW - Basic `_Static_assert` in functions works
**Workaround:** Place assertions in functions or use preprocessor checks

---

## C23 Missing Features

### 1. Standard Attributes (`[[attributes]]`) ❌ **NOT IMPLEMENTED**

**What's missing:**
- `[[deprecated]]`
- `[[fallthrough]]`
- `[[nodiscard]]`
- `[[maybe_unused]]`
- `[[noreturn]]` (though `_Noreturn` exists)

**Example of what doesn't work:**
```c
// Mark function as deprecated
[[deprecated("Use new_function instead")]]
void old_function(void);  // NOT supported

// Mark switch fallthrough as intentional
switch (x) {
    case 1:
        do_something();
        [[fallthrough]];  // NOT supported
    case 2:
        do_other();
        break;
}

// Warn if return value ignored
[[nodiscard]]
int important_function(void);  // NOT supported

// Suppress unused warnings
[[maybe_unused]] int debugging_var = 0;  // NOT supported
```

**Impact:** MEDIUM - Helpful for warnings and documentation
**Workaround:** Use compiler-specific attributes (`__attribute__`, `__declspec`)
**Implementation Difficulty:** Medium (parser + attribute handling)

---

### 2. Binary File Inclusion (`#embed`) ❌ **NOT IMPLEMENTED**

**What's missing:**
- `#embed` preprocessor directive
- Embedding binary files directly in code

**Example of what doesn't work:**
```c
// Embed a binary file directly
const unsigned char icon_data[] = {
    #embed "icon.png"  // NOT supported
};

// With parameters
const char shader_code[] = {
    #embed "shader.glsl" limit(1024)  // NOT supported
};
```

**Impact:** LOW - Convenience feature
**Workaround:** Use `xxd -i` or similar tools to generate C arrays
**Implementation Difficulty:** Medium (preprocessor extension)

---

### 3. Binary Literals (`0b` prefix) ❌ **NOT IMPLEMENTED**

**What's missing:**
- Binary integer literal syntax

**Example of what doesn't work:**
```c
unsigned int flags = 0b10101010;  // NOT supported in compiler
unsigned int mask = 0b11110000;   // NOT supported in compiler
```

**What DOES work (workaround):**
```c
#include <metaware_syntax.h>

unsigned int flags = _BIN_10101010;  // Predefined macro
unsigned int mask = _BIN_11110000;   // Predefined macro

// Or just use hex
unsigned int flags = 0xAA;
unsigned int mask = 0xF0;
```

**Impact:** LOW - Hex works fine, macros available
**Workaround:** Use `_BIN_xxxx` macros from `metaware_syntax.h` or hex literals
**Implementation Difficulty:** Easy (lexer only)

---

### 4. Digit Separators (`_` in numbers) ❌ **NOT IN COMPILER**

**What's missing:**
- Underscore separators in numeric literals in compiler

**Example of what doesn't work:**
```c
int million = 1_000_000;      // NOT supported in compiler
int billion = 1_000_000_000;  // NOT supported in compiler
unsigned int color = 0xFF_AA_00;  // NOT supported in compiler
double avogadro = 6.022_140_76e23;  // NOT supported in compiler
```

**What DOES work (workaround):**
```c
#include <metaware_syntax.h>

int million = 1 * _1M;           // Use predefined constants
int billion = 1 * _1B;
unsigned int color = HEX8(F,F,A,A,0,0,0,0);  // Use macro
```

**Impact:** MEDIUM - Readability feature, but workaround exists
**Workaround:** Use macros from `metaware_syntax.h`
**Implementation Difficulty:** Easy (lexer only, **patch available!**)
**See:** `libmetaware/compiler_patches/01_numeric_separators.md`

---

### 5. `nullptr` Constant ❌ **NOT IMPLEMENTED**

**What's missing:**
- `nullptr` keyword
- `nullptr_t` type

**Example of what doesn't work:**
```c
#include <stddef.h>

int *ptr = nullptr;  // NOT supported

void func(nullptr_t);  // NOT supported
```

**Impact:** LOW - `NULL` works fine
**Workaround:** Use `NULL` from `<stddef.h>`
**Implementation Difficulty:** Easy (lexer + parser)

---

### 6. Improved `auto` Type Inference ❌ **NOT IMPLEMENTED**

**What's missing:**
- `auto` for type inference (like C++)
- Type deduction from initializers

**Example of what doesn't work:**
```c
auto x = 5;           // NOT supported (x should be int)
auto ptr = malloc(100);  // NOT supported (ptr should be void*)
```

**Impact:** LOW - Explicit types work fine
**Workaround:** Declare types explicitly
**Implementation Difficulty:** Hard (full type inference)

---

### 7. `typeof` Operator ⚠️ **CHECK STATUS**

**May not work:**
- `typeof` for type extraction
- `typeof_unqual` for unqualified types

**Example that may not work:**
```c
int x = 5;
typeof(x) y = 10;  // May not be supported

const int c = 5;
typeof_unqual(c) z = 10;  // Definitely not supported (C23)
```

**Impact:** MEDIUM - Useful for macros
**Workaround:** Use explicit types or `__typeof__` (GCC extension)
**Implementation Difficulty:** Medium

---

### 8. Empty Initializers `{}` ⚠️ **CHECK STATUS**

**May not work:**
- Empty braces for zero-initialization

**Example that may not work:**
```c
struct Data data = {};  // May not work (C23 feature)
int array[10] = {};     // May not work
```

**What DOES work:**
```c
struct Data data = {0};  // C99/C11 style - works
int array[10] = {0};     // Works
```

**Impact:** LOW - `{0}` works fine
**Workaround:** Use `{0}` instead

---

### 9. Enhanced `#elifdef` / `#elifndef` ⚠️ **CHECK STATUS**

**May not work:**
- `#elifdef` - Combination of `#elif` and `#ifdef`
- `#elifndef` - Combination of `#elif` and `#ifndef`

**Example that may not work:**
```c
#ifdef FEATURE_A
    // ...
#elifdef FEATURE_B  // May not be supported
    // ...
#endif
```

**What DOES work:**
```c
#ifdef FEATURE_A
    // ...
#elif defined(FEATURE_B)  // Standard C99 - works
    // ...
#endif
```

**Impact:** LOW - Standard `#elif defined()` works
**Workaround:** Use `#elif defined(MACRO)`

---

### 10. `constexpr` ❌ **NOT IMPLEMENTED**

**What's missing:**
- `constexpr` keyword (if proposed for C23)
- Compile-time constant expressions

**Example of what doesn't work:**
```c
constexpr int max = 100;  // NOT supported (if this was in C23)
```

**Impact:** LOW - Macros work
**Workaround:** Use `#define` or `const`

---

### 11. Improved `static_assert` ❌ **NOT IMPLEMENTED**

**What's missing:**
- Single-argument `static_assert` (no message)

**Example of what doesn't work:**
```c
static_assert(sizeof(int) == 4);  // NOT supported (C23 allows no message)
```

**What DOES work:**
```c
_Static_assert(sizeof(int) == 4, "int must be 4 bytes");  // Message required
```

**Impact:** LOW - Just add a message
**Workaround:** Always provide message string

---

## Summary of Missing Features by Impact

### HIGH IMPACT (Users will notice)
- ❌ `_Generic` (C11) - No type-generic selection
- ⚠️ Full `_Atomic` (C11) - Limited atomic operations
- ❌ `[[attributes]]` (C23) - No standard attributes

### MEDIUM IMPACT (Workarounds available)
- ⚠️ Complex numbers (C99) - Basic type exists, library incomplete
- ⚠️ `_Thread_local` (C11) - Use `tss_*` functions instead
- ⚠️ `_Alignas`/`_Alignof` (C11) - Use compiler-specific attributes
- ❌ Digit separators in compiler (C23) - **Macros work, patch available**
- ⚠️ `typeof` - May work with GCC extension

### LOW IMPACT (Easy workarounds)
- ❌ Binary literals (C23) - Use hex or macros
- ❌ `nullptr` (C23) - Use `NULL`
- ❌ `#embed` (C23) - Use `xxd -i`
- ❌ Improved `auto` (C23) - Use explicit types
- ⚠️ Empty initializers (C23) - Use `{0}`

---

## What IS Implemented (For Comparison)

✅ **C99:** ~95% complete (nearly everything works)
✅ **C11 Libraries:** Threading, Unicode (100% complete)
✅ **C23 Libraries:** Bit utilities, checked arithmetic, safe strings (100% complete)
✅ **Extensions:** Decimal FP, MetaWare, Apple Blocks (100% complete)

---

## Implementation Priority Recommendations

If implementing missing features, recommended order:

### Easy Wins (High Value, Low Effort)
1. **Binary literals** (`0b1010`) - Lexer only, 1 day
2. **Digit separators** (`1_000_000`) - Lexer only, 2 days (**patch available!**)
3. **`nullptr`** - Lexer + parser, 2-3 days

### Medium Effort (High Value)
4. **`_Generic`** - Parser + semantic, 2-3 weeks
5. **`[[attributes]]`** - Parser + attribute handling, 2-3 weeks
6. **`_Alignas`/`_Alignof`** - Parser + codegen, 2 weeks

### Hard (Complex Implementation)
7. **Full `_Atomic`** - Compiler + platform support, 4+ weeks
8. **`#embed`** - Preprocessor, 2-3 weeks
9. **Improved `auto`** - Type inference, 4+ weeks

---

## Workarounds Quick Reference

| Missing Feature | Workaround |
|----------------|------------|
| `_Generic` | Call type-specific functions manually |
| Full `_Atomic` | Use `libthread` mutexes |
| `_Thread_local` | Use `tss_*` functions from `libthread` |
| `_Alignas` | Use `__attribute__((aligned(N)))` (GCC) |
| `[[deprecated]]` | Use `__attribute__((deprecated))` (GCC) |
| Binary literals | Use hex: `0xAA` or macro: `_BIN_10101010` |
| Digit separators | Use macros: `1 * _1M`, `HEX8(F,F,A,A,0,0,0,0)` |
| `nullptr` | Use `NULL` |
| `#embed` | Use `xxd -i file.bin > file.h` |
| Empty `{}` | Use `{0}` |
| Complex math | Use manual real/imag or external library |

---

## Documentation

For what IS implemented, see:
- **`C_STANDARD_SUPPORT.md`** - Complete feature matrix
- **`IMPLEMENTATION_SUMMARY.md`** - All implemented features
- **`QUICKSTART.md`** - Getting started guide

For compiler patches (to add missing features):
- **`libmetaware/compiler_patches/01_numeric_separators.md`** - Add digit separators
- **`libmetaware/compiler_patches/02_case_ranges.md`** - Add case ranges
- **`libmetaware/compiler_patches/03_labeled_arguments.md`** - Add labeled arguments

---

**Bottom Line:** PCC has excellent C99/C11/C23 library support, but some compiler-level features (especially C11 `_Generic` and full atomics) are missing. Most missing features have good workarounds available.
