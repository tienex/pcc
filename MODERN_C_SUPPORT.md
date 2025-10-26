# Modern C Standards Support in PCC

## Overview

The Portable C Compiler (PCC) now includes comprehensive support for modern C standards (C11, C23), IEEE 754-2008 decimal floating-point arithmetic, **and MetaWare High C extensions from 1989**. This document provides a complete guide to all enhanced features.

---

## Table of Contents

1. [C11 Support](#c11-support)
2. [C23 Support](#c23-support)
3. [IEEE 754-2008 Decimal Floating Point](#ieee-754-2008-decimal-floating-point)
4. [MetaWare High C Extensions](#metaware-high-c-extensions)
5. [Future Language Extensions](#future-language-extensions)
6. [Quick Start Guide](#quick-start-guide)
7. [FAQ](#faq)

---

## C11 Support

PCC provides portable implementations of key C11 library features that automatically link when your system lacks native support.

### Features Implemented

| Feature | Header | Status | Description |
|---------|--------|--------|-------------|
| Unicode support | `<uchar.h>` | ✅ Complete | UTF-8/16/32 conversions |
| Threading | `<threads.h>` | ✅ Complete | Cross-platform threading primitives |

### Unicode Support (libunicode)

Convert between UTF-8, UTF-16, and UTF-32:

```c
#include <uchar.h>
#include <stdio.h>

int main(void) {
    const char *utf8 = "Hello 世界 🌍";
    char16_t utf16[100];
    mbstate_t state = {0};

    // Convert UTF-8 to UTF-16
    const char *src = utf8;
    char16_t *dst = utf16;
    while (*src) {
        size_t result = mbrtoc16(dst++, src, 4, &state);
        if (result > 0) src += result;
    }

    printf("Converted successfully!\n");
    return 0;
}
```

**Documentation:** See [C11_SUPPORT.md](C11_SUPPORT.md) for complete API reference.

### Threading Support (libthread)

Portable threading that works on POSIX and Windows:

```c
#include <threads.h>
#include <stdio.h>

int worker(void *arg) {
    int id = *(int *)arg;
    printf("Thread %d running\n", id);
    return 0;
}

int main(void) {
    thrd_t thread;
    int id = 1;

    thrd_create(&thread, worker, &id);
    thrd_join(thread, NULL);

    return 0;
}
```

**Features:**
- Threads (`thrd_t`)
- Mutexes (`mtx_t`)
- Condition variables (`cnd_t`)
- Thread-local storage (`tss_t`)

**Documentation:** See [C11_SUPPORT.md](C11_SUPPORT.md) for complete API reference.

---

## C23 Support

PCC implements C23 library features for bit manipulation and overflow-safe arithmetic.

### Features Implemented

| Feature | Header | Status | Description |
|---------|--------|--------|-------------|
| Bit utilities | `<stdbit.h>` | ✅ Complete | Bit manipulation functions |
| Checked arithmetic | `<stdckdint.h>` | ✅ Complete | Overflow detection |

### Bit Utilities (libstdbit)

Efficient bit manipulation:

```c
#include <stdbit.h>
#include <stdio.h>

int main(void) {
    unsigned int value = 0x00FF0000;

    printf("Leading zeros: %u\n", stdc_leading_zeros(value));   // 8
    printf("Trailing zeros: %u\n", stdc_trailing_zeros(value)); // 16
    printf("One bits: %u\n", stdc_count_ones(value));           // 8

    // Power of 2 operations
    unsigned int next_pow2 = stdc_bit_ceil(100);    // 128
    unsigned int prev_pow2 = stdc_bit_floor(100);   // 64

    printf("Next power of 2 after 100: %u\n", next_pow2);
    printf("Previous power of 2: %u\n", prev_pow2);

    return 0;
}
```

**Functions:**
- Leading/trailing zeros and ones
- Population count (count bits set)
- Bit width, floor, ceiling
- Power-of-2 detection
- Type-generic macros for all unsigned types

**Documentation:** See [C23_SUPPORT.md](C23_SUPPORT.md) for complete API reference.

### Checked Integer Arithmetic (libckdint)

Detect integer overflow before it happens:

```c
#include <stdckdint.h>
#include <stdio.h>
#include <limits.h>

int main(void) {
    int result;

    // Safe addition
    if (ckd_add(&result, INT_MAX, 1)) {
        printf("Overflow detected!\n");
        // Handle overflow safely
    } else {
        printf("Result: %d\n", result);
    }

    // Safe multiplication
    unsigned int array_size = 1000000;
    unsigned int element_size = 4;
    unsigned int total;

    if (ckd_mul(&total, array_size, element_size)) {
        fprintf(stderr, "Array too large!\n");
        return 1;
    }

    printf("Safe to allocate %u bytes\n", total);
    return 0;
}
```

**Functions:**
- `ckd_add()` - Addition with overflow check
- `ckd_sub()` - Subtraction with overflow check
- `ckd_mul()` - Multiplication with overflow check
- Type-generic for all signed and unsigned integer types

**Documentation:** See [C23_SUPPORT.md](C23_SUPPORT.md) for complete API reference.

---

## IEEE 754-2008 Decimal Floating Point

PCC now supports decimal floating-point arithmetic, essential for financial and commercial applications where binary floating-point's rounding errors are unacceptable.

### The Problem with Binary Floating Point

```c
float price = 0.1f;
float tax = 0.2f;
float total = price + tax;  // 0.30000001... NOT exactly 0.3!
```

### The Solution: Decimal Floating Point

```c
#include <decimal.h>

_Decimal64 price = dec64_from_string("0.10");
_Decimal64 tax = dec64_from_string("0.20");
_Decimal64 total = dec64_add(price, tax);  // Exactly 0.30!
```

### Decimal Types

| Type | Precision | Range | Use Case |
|------|-----------|-------|----------|
| `_Decimal32` | 7 digits | ±10^±96 | Basic decimal math |
| `_Decimal64` | 16 digits | ±10^±384 | Financial calculations |
| `_Decimal128` | 34 digits | ±10^±6144 | High-precision finance |

### Complete Example: Sales Tax

```c
#include <decimal.h>
#include <stdio.h>

int main(void) {
    // Price with tax calculation
    _Decimal64 price = dec64_from_string("19.99");
    _Decimal64 tax_rate = dec64_from_string("0.0825");  // 8.25%

    _Decimal64 one = dec64_one();
    _Decimal64 multiplier = dec64_add(one, tax_rate);   // 1.0825
    _Decimal64 total = dec64_mul(price, multiplier);    // 21.6388875

    // Display result
    char buf[32];
    dec64_to_string(total, buf, sizeof(buf));
    printf("Subtotal: $19.99\n");
    printf("Tax (8.25%%): $%.2f\n",
           (double)dec64_to_int64(dec64_mul(price, tax_rate)) / 100.0);
    printf("Total: $%s\n", buf);

    return 0;
}
```

**Automatic Detection:**
- On GCC 4.3+ with decimal support: Uses built-in `_Decimal64` types
- On other systems: Automatically builds and links portable libdecimal

**Documentation:** See [DECIMAL_FP_SUPPORT.md](DECIMAL_FP_SUPPORT.md) for complete API reference and examples.

---

## MetaWare High C Extensions

PCC now includes **libmetaware**, a runtime library providing MetaWare High C compiler extensions from 1989 - features that were **decades ahead of their time**!

### What are MetaWare Extensions?

MetaWare High C was a pioneering compiler that introduced advanced features in 1989:
- **Nested functions** (25 years before GCC extension became common)
- **Generator coroutines** (12 years before Python 2.2, 31 before C++20!)
- **Function values/closures** (22 years before C++11 lambdas)
- **Non-local goto** with stack unwinding (early exception handling)
- **Numeric literal separators** (25 years before C++14)

### Features Included

| Feature | Year | Modern Equivalent | PCC Status |
|---------|------|-------------------|------------|
| Nested Functions | 1989 | GCC extension | ✅ Runtime |
| Generator Coroutines | 1989 | Python (2001), C++20 (2020) | ✅ Runtime |
| Function Values | 1989 | C++11 lambdas (2011) | ✅ Runtime |
| Non-Local Goto | 1989 | C++ exceptions | ✅ Runtime |
| Numeric Separators | 1989 | C++14 (2014) | 📋 Documented |

### Example: Generator Coroutines (1989!)

```c
#include <metaware.h>
#include <stdio.h>

/* Define a Fibonacci generator */
MW_GENERATOR(fibonacci, int, int) {
    int count = *(int *)__args;
    int a = 0, b = 1;

    for (int i = 0; i < count; i++) {
        MW_YIELD(__gen, a);  /* Yield value - like Python! */

        int temp = a;
        a = b;
        b = temp + b;
    }
}

int main(void) {
    /* Create and iterate over generator */
    mw_generator_t *gen = fibonacci(10);

    int num;
    MW_FOR_EACH(num, gen) {
        printf("%d ", num);  /* Prints: 0 1 1 2 3 5 8 13 21 34 */
    }

    mw_free_generator(gen);
    return 0;
}
```

Compile and run:
```bash
pcc fibonacci.c -lmetaware -o fibonacci
./fibonacci
```

### Example: Nested Functions with Closures

```c
#include <metaware.h>
#include <stdio.h>

/* Context for multiplier closure */
typedef struct {
    int factor;
} multiplier_ctx_t;

/* Nested function implementation */
void multiply_impl(void *ctx, void *args, void *result) {
    multiplier_ctx_t *context = (multiplier_ctx_t *)ctx;
    int x = *(int *)args;
    *(int *)result = x * context->factor;
}

/* Create a multiplier function */
mw_function_t *make_multiplier(int factor) {
    multiplier_ctx_t ctx = { factor };
    return mw_create_function(multiply_impl, &ctx, sizeof(ctx));
}

int main(void) {
    /* Create closures with different factors */
    mw_function_t *times_5 = make_multiplier(5);
    mw_function_t *times_10 = make_multiplier(10);

    int arg = 7;
    int result;

    mw_call_function(times_5, &arg, &result);
    printf("5 × 7 = %d\n", result);  /* 35 */

    mw_call_function(times_10, &arg, &result);
    printf("10 × 7 = %d\n", result);  /* 70 */

    mw_free_function(times_5);
    mw_free_function(times_10);
    return 0;
}
```

### Example: Non-Local Goto with Cleanup

```c
#include <metaware.h>
#include <stdio.h>

void cleanup_handler(void *data) {
    printf("Cleaning up resources...\n");
    /* Cleanup code here */
}

int process_data(void) {
    MW_LABEL(error_handler);

    /* Register cleanup */
    mw_set_label_cleanup(&error_handler, cleanup_handler, NULL);

    /* Simulate nested function that encounters error */
    if (/* error condition */ 1) {
        printf("Error detected! Jumping to handler...\n");
        mw_goto_label(&error_handler, 1);
    }

    printf("This won't execute\n");
    return 0;

__mw_label_error_handler:
    printf("Error handled!\n");
    return -1;
}

int main(void) {
    return process_data();
}
```

Output:
```
Error detected! Jumping to handler...
Cleaning up resources...
Error handled!
```

### All MetaWare Features

**1. Nested Functions**
- Pascal-style nested functions
- Access parent scope variables
- Trampoline-based implementation

**2. Generator Coroutines**
- Python-style yield
- Recursive generators
- State preservation across yields

**3. Function Values (Closures)**
- First-class function objects
- Context capture
- Type-safe invocation

**4. Non-Local Goto**
- Multi-level jumps
- Automatic stack unwinding
- Cleanup handlers

**5. Numeric Literal Separators** (compiler feature)
- `1_000_000` instead of `1000000`
- Requires parser modifications
- Fully documented for implementation

**Documentation:** See [METAWARE_EXTENSIONS.md](METAWARE_EXTENSIONS.md) for complete documentation with syntax, examples, and implementation details.

---

## Future Language Extensions

Two additional advanced language extensions have been researched and documented for potential future implementation:

### Apple Blocks (Closures for C)

Blocks add lambda/closure syntax to C:

```c
// Block variable
int (^add)(int, int) = ^(int a, int b) {
    return a + b;
};

int result = add(5, 3);  // Returns 8

// Capturing variables
int multiplier = 10;
int (^multiply)(int) = ^(int value) {
    return value * multiplier;  // Captures multiplier
};
```

**Status:** Documented, not implemented (requires compiler frontend changes)
**Complexity:** High (6-12 months development)
**Reference:** [LANGUAGE_EXTENSIONS_ROADMAP.md](LANGUAGE_EXTENSIONS_ROADMAP.md#1-apple-blocks-extension)

### MetaWare Generator Coroutines

Python-style generators from 1989:

```c
void fibonacci(int count) -> (int number) {
    int a = 0, b = 1;
    for (int i = 0; i < count; i++) {
        yield(a);
        int temp = a;
        a = b;
        b = temp + b;
    }
}

// Iterate over yielded values
for num <- fibonacci(10) do {
    printf("%d ", num);
}
```

**Status:** Documented, not implemented (requires compiler frontend changes)
**Complexity:** Very High (12-18 months development)
**Reference:** [LANGUAGE_EXTENSIONS_ROADMAP.md](LANGUAGE_EXTENSIONS_ROADMAP.md#2-metaware-generator-coroutines)

---

## Quick Start Guide

### Installation

```bash
git clone https://github.com/yourrepo/pcc.git
cd pcc
./configure
make
sudo make install
```

The build system automatically:
- Detects C11/C23 compiler support
- Checks for native library support
- Builds portable libraries only when needed
- Configures automatic library linking

### Configuration Output

```
C11 support ...................... yes
C23 support ...................... yes
Build portable Unicode library ... no
Build portable Threading library . no
Build portable Bit utils library . yes
Build portable Checked int library yes
Build portable Decimal FP library  yes
```

### Your First Program

```c
/* modern_c_demo.c - Demonstrates C11, C23, and Decimal FP features */

#include <stdio.h>
#include <threads.h>      // C11 threading
#include <stdbit.h>       // C23 bit utilities
#include <stdckdint.h>    // C23 checked arithmetic
#include <decimal.h>      // IEEE 754-2008 decimal FP

// Thread worker using C11 threads
int worker(void *arg) {
    printf("Thread says: %s\n", (char *)arg);
    return 0;
}

int main(void) {
    printf("=== Modern C Features Demo ===\n\n");

    // C11: Threading
    printf("C11 Threading:\n");
    thrd_t thread;
    thrd_create(&thread, worker, "Hello from C11 thread!");
    thrd_join(thread, NULL);

    // C23: Bit manipulation
    printf("\nC23 Bit Utilities:\n");
    unsigned int value = 1000;
    printf("Bit width of %u: %u bits\n",
           value, stdc_bit_width(value));
    printf("Next power of 2: %u\n",
           stdc_bit_ceil(value));

    // C23: Overflow detection
    printf("\nC23 Overflow Detection:\n");
    int result;
    if (ckd_add(&result, 2000000000, 2000000000)) {
        printf("Overflow detected in addition!\n");
    }

    // IEEE 754-2008: Decimal arithmetic
    printf("\nIEEE 754-2008 Decimal Arithmetic:\n");
    _Decimal64 price = dec64_from_string("19.99");
    _Decimal64 quantity = dec64_from_int64(3);
    _Decimal64 total = dec64_mul(price, quantity);

    char buf[32];
    dec64_to_string(total, buf, sizeof(buf));
    printf("3 × $19.99 = $%s (exact!)\n", buf);

    printf("\nAll features working correctly!\n");
    return 0;
}
```

Compile and run:

```bash
pcc modern_c_demo.c -o demo
./demo
```

Output:
```
=== Modern C Features Demo ===

C11 Threading:
Thread says: Hello from C11 thread!

C23 Bit Utilities:
Bit width of 1000: 10 bits
Next power of 2: 1024

C23 Overflow Detection:
Overflow detected in addition!

IEEE 754-2008 Decimal Arithmetic:
3 × $19.99 = $59.97 (exact!)

All features working correctly!
```

---

## FAQ

### Q: Do I need to link these libraries manually?

**A:** No! PCC automatically links the necessary libraries based on what features you use and what your system provides natively.

### Q: Will this break my existing code?

**A:** No. All extensions are additive and don't affect existing C code. The compiler remains standards-compliant.

### Q: What if my system already has these features?

**A:** The configure script detects native support and only builds portable libraries when needed. On modern systems with full C11/C23 support, the portable libraries aren't used at all.

### Q: Can I use these features with other compilers?

**A:** The library headers are designed to work with GCC, Clang, and other compilers. The portable libraries can be used standalone.

### Q: What's the performance impact?

**A:** Minimal. The portable implementations use compiler built-ins when available (GCC, Clang intrinsics) for near-native performance. The fallback C implementations are well-optimized.

### Q: Are these features standard?

**A:**
- **C11 features** (`<uchar.h>`, `<threads.h>`): ISO C11 standard (ISO/IEC 9899:2011)
- **C23 features** (`<stdbit.h>`, `<stdckdint.h>`): ISO C23 standard (ISO/IEC 9899:2023)
- **Decimal FP** (`_Decimal64`, etc.): IEEE 754-2008 + ISO/IEC TR 24732
- **Blocks & Generators**: Non-standard extensions (documented for future work)

### Q: Can I help implement Blocks or Generators?

**A:** Yes! See [LANGUAGE_EXTENSIONS_ROADMAP.md](LANGUAGE_EXTENSIONS_ROADMAP.md) for implementation details and join the PCC mailing list.

### Q: Where can I find more documentation?

**A:**
- **C11:** [C11_SUPPORT.md](C11_SUPPORT.md)
- **C23:** [C23_SUPPORT.md](C23_SUPPORT.md)
- **Decimal FP:** [DECIMAL_FP_SUPPORT.md](DECIMAL_FP_SUPPORT.md)
- **Future Extensions:** [LANGUAGE_EXTENSIONS_ROADMAP.md](LANGUAGE_EXTENSIONS_ROADMAP.md)
- **User Guide:** [README_C11.md](README_C11.md)

---

## Summary of Added Features

### Fully Implemented ✅

| Feature | Standard | Description | Auto-Link |
|---------|----------|-------------|-----------|
| `<uchar.h>` | C11 | Unicode UTF-8/16/32 conversions | ✅ |
| `<threads.h>` | C11 | Cross-platform threading | ✅ |
| `<stdbit.h>` | C23 | Bit manipulation utilities | ✅ |
| `<stdckdint.h>` | C23 | Overflow-safe arithmetic | ✅ |
| `_Decimal32/64/128` | IEEE 754-2008 | Decimal floating-point | ✅ |

### Documented for Future Work 📋

| Feature | Origin | Complexity | Timeline |
|---------|--------|------------|----------|
| Blocks (`^{ }`) | Apple/WG14 N1451 | High | 6-12 months |
| MetaWare (full syntax) | MetaWare High C | Very High | 6-9 months |

**Note:** MetaWare generator coroutines and nested functions have **runtime support** via libmetaware. Full compiler syntax support would eliminate API overhead and provide native `yield`, `->`, and `for-<-` syntax.

---

## Contributing

We welcome contributions! Areas where you can help:

1. **Testing:** Test on different platforms (BSD, Linux, macOS, Windows)
2. **Documentation:** Improve examples and guides
3. **Bug Fixes:** Report and fix issues
4. **Features:** Implement Blocks or Generators (see roadmap)

**Mailing List:** pcc@lists.ludd.ltu.se
**Repository:** https://github.com/yourrepo/pcc

---

## License

All modern C support features are part of the Portable C Compiler (PCC) project and are distributed under the same BSD-style license. See [COPYING](COPYING) for details.

---

## Acknowledgments

- **libunicode & libthread:** Original C11 implementation for PCC
- **libstdbit & libckdint:** C23 portable implementations
- **libdecimal:** IEEE 754-2008 decimal floating-point support
- **Research:** Apple Blocks specification, MetaWare High C documentation
- **Community:** PCC development team and contributors

---

**Document Version:** 1.0
**Last Updated:** 2025
**Maintainer:** PCC Development Team
**Status:** Production Ready

For the latest updates and documentation, visit the PCC project website.
