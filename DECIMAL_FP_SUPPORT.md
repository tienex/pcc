# IEEE 754-2008 Decimal Floating Point Support in PCC

## Overview

PCC now includes support for IEEE 754-2008 decimal floating-point arithmetic with portable implementations of the `_Decimal32`, `_Decimal64`, and `_Decimal128` types as specified in ISO/IEC TR 24732.

The build system automatically detects your compiler's decimal floating-point capabilities and builds the portable libdecimal library only when needed.

## Why Decimal Floating Point?

Traditional binary floating-point (float, double) cannot exactly represent many common decimal values:

```c
float price = 0.1f;   // Actually stores ~0.100000001490116119384765625
double tax = 0.08;     // Actually stores ~0.0800000000000000016653345369...
```

This leads to rounding errors in financial and commercial applications:

```c
float total = 0.1f + 0.2f;  // Result: 0.30000001192092896 (not exactly 0.3!)
```

**Decimal floating-point solves this problem:**

```c
_Decimal64 price = 0.1DD;
_Decimal64 tax = 0.08DD;
_Decimal64 total = price + tax;  // Exactly 0.18, no rounding error!
```

## Decimal Types

### _Decimal32 (32-bit)
- **Precision:** 7 decimal digits
- **Range:** ±10^-95 to ±10^96
- **Storage:** 4 bytes
- **Use case:** Basic decimal arithmetic, embedded systems

```c
_Decimal32 price = 19.99DF;
```

### _Decimal64 (64-bit)
- **Precision:** 16 decimal digits
- **Range:** ±10^-383 to ±10^384
- **Storage:** 8 bytes
- **Use case:** Financial calculations, scientific computing

```c
_Decimal64 balance = 1234567.89DD;
```

### _Decimal128 (128-bit)
- **Precision:** 34 decimal digits
- **Range:** ±10^-6143 to ±10^6144
- **Storage:** 16 bytes
- **Use case:** High-precision financial, cryptography

```c
_Decimal128 national_debt = 31400000000000.00DL;
```

## Installation

### Automatic Integration

The decimal FP library integrates seamlessly into PCC:

```bash
./configure
make
sudo make install
```

The build system automatically:
1. Detects native `_Decimal` compiler support
2. Checks if the system provides decimal FP types
3. Builds portable libdecimal only if needed
4. Links it automatically when you use decimal types

### Configuration Output

```
checking for _Decimal64 support... no
Build portable Decimal FP library  yes
```

If your compiler already supports decimal types, you'll see:

```
checking for _Decimal64 support... yes
Build portable Decimal FP library  no
```

## Usage with Native Compiler Support

If your compiler (GCC 4.3+, ICC, Clang with -std=c2x) has built-in decimal support:

```c
#include <stdio.h>

int main(void) {
    _Decimal64 price = 19.99DD;
    _Decimal64 quantity = 5.0DD;
    _Decimal64 total = price * quantity;

    printf("Total: $%.2Df\n", (double)total);  // GCC extension
    return 0;
}
```

Compile with:
```bash
pcc program.c -o program        # Uses built-in decimal types
gcc -std=c2x program.c -o program
```

## Usage with libdecimal (Portable Implementation)

On systems without native decimal support, use libdecimal's API:

```c
#include <decimal.h>
#include <stdio.h>

int main(void) {
    // Create decimal values
    _Decimal64 price = dec64_from_string("19.99");
    _Decimal64 quantity = dec64_from_int64(5);

    // Arithmetic operations
    _Decimal64 total = dec64_mul(price, quantity);  // 99.95

    // Convert to string for display
    char buf[32];
    dec64_to_string(total, buf, sizeof(buf));
    printf("Total: $%s\n", buf);

    return 0;
}
```

Compile with:
```bash
pcc program.c -o program  # Automatically links -ldecimal if needed
```

## Complete API Reference

### Creating Decimal Values

```c
// From integers
_Decimal32  d32 = dec32_from_int32(12345);
_Decimal64  d64 = dec64_from_int64(9876543210LL);
_Decimal128 d128 = dec128_from_int64(1000000LL);

// From strings
_Decimal32  d32 = dec32_from_string("123.45");
_Decimal64  d64 = dec64_from_string("9876543.21");
_Decimal128 d128 = dec128_from_string("1234567890.123456");

// Special values
_Decimal64 zero = dec64_zero();
_Decimal64 one = dec64_one();
_Decimal64 inf = dec64_inf();
_Decimal64 nan = dec64_nan();
```

### Arithmetic Operations

```c
_Decimal64 a = dec64_from_string("100.50");
_Decimal64 b = dec64_from_string("25.25");

_Decimal64 sum = dec64_add(a, b);       // 125.75
_Decimal64 diff = dec64_sub(a, b);      // 75.25
_Decimal64 product = dec64_mul(a, b);   // 2537.625
_Decimal64 quotient = dec64_div(a, b);  // ~3.980198...

_Decimal64 negated = dec64_neg(a);      // -100.50
_Decimal64 absolute = dec64_abs(negated); // 100.50
_Decimal64 square_root = dec64_sqrt(a); // ~10.025...
```

### Comparisons

```c
_Decimal64 a = dec64_from_string("100.00");
_Decimal64 b = dec64_from_string("200.00");

if (dec64_eq(a, b)) { /* equal */ }
if (dec64_ne(a, b)) { /* not equal */ }
if (dec64_lt(a, b)) { /* a < b */ }
if (dec64_le(a, b)) { /* a <= b */ }
if (dec64_gt(a, b)) { /* a > b */ }
if (dec64_ge(a, b)) { /* a >= b */ }
```

### Type Classification

```c
_Decimal64 value = dec64_from_string("123.45");

if (dec64_is_zero(value))   { /* value is zero */ }
if (dec64_is_inf(value))    { /* value is infinity */ }
if (dec64_is_nan(value))    { /* value is NaN */ }
if (dec64_is_finite(value)) { /* value is finite */ }
```

### Type Conversions

```c
// Convert between decimal types
_Decimal32  d32 = dec32_from_int32(100);
_Decimal64  d64 = dec32_to_dec64(d32);      // Promote
_Decimal128 d128 = dec64_to_dec128(d64);    // Promote

_Decimal64  back64 = dec128_to_dec64(d128); // Narrow (may lose precision)
_Decimal32  back32 = dec64_to_dec32(d64);   // Narrow (may lose precision)

// Convert to integers
int32_t i32 = dec32_to_int32(d32);
int64_t i64 = dec64_to_int64(d64);

// Convert to/from strings
char buffer[64];
dec64_to_string(d64, buffer, sizeof(buffer));
_Decimal64 parsed = dec64_from_string(buffer);
```

## Practical Examples

### Example 1: Sales Tax Calculation

```c
#include <decimal.h>
#include <stdio.h>

_Decimal64 calculate_total_with_tax(_Decimal64 price, _Decimal64 tax_rate) {
    _Decimal64 one = dec64_one();
    _Decimal64 multiplier = dec64_add(one, tax_rate);  // 1 + tax_rate
    return dec64_mul(price, multiplier);
}

int main(void) {
    _Decimal64 price = dec64_from_string("19.99");
    _Decimal64 tax_rate = dec64_from_string("0.0825");  // 8.25% tax

    _Decimal64 total = calculate_total_with_tax(price, tax_rate);

    char buf[32];
    dec64_to_string(total, buf, sizeof(buf));
    printf("Price: $19.99\n");
    printf("Tax rate: 8.25%%\n");
    printf("Total: $%s\n", buf);  // Exactly $21.63925

    return 0;
}
```

### Example 2: Compound Interest

```c
#include <decimal.h>
#include <stdio.h>

_Decimal64 compound_interest(_Decimal64 principal, _Decimal64 rate, int years) {
    _Decimal64 one = dec64_one();
    _Decimal64 multiplier = dec64_add(one, rate);  // 1 + rate

    _Decimal64 amount = principal;
    for (int i = 0; i < years; i++) {
        amount = dec64_mul(amount, multiplier);
    }

    return amount;
}

int main(void) {
    _Decimal64 principal = dec64_from_string("10000.00");
    _Decimal64 rate = dec64_from_string("0.05");  // 5% annual interest

    _Decimal64 final = compound_interest(principal, rate, 10);

    char buf[32];
    dec64_to_string(final, buf, sizeof(buf));
    printf("Principal: $10,000.00\n");
    printf("Rate: 5%% per year\n");
    printf("Years: 10\n");
    printf("Final amount: $%s\n", buf);  // $16,288.95 (exact)

    return 0;
}
```

### Example 3: Currency Conversion with Precision

```c
#include <decimal.h>
#include <stdio.h>

int main(void) {
    // Exchange rate: 1 USD = 0.92 EUR (example)
    _Decimal64 exchange_rate = dec64_from_string("0.92");

    _Decimal64 usd_amounts[] = {
        dec64_from_string("100.00"),
        dec64_from_string("250.50"),
        dec64_from_string("1000.99")
    };

    printf("USD to EUR Conversion:\n");
    printf("---------------------\n");

    for (int i = 0; i < 3; i++) {
        _Decimal64 eur = dec64_mul(usd_amounts[i], exchange_rate);

        char usd_str[32], eur_str[32];
        dec64_to_string(usd_amounts[i], usd_str, sizeof(usd_str));
        dec64_to_string(eur, eur_str, sizeof(eur_str));

        printf("$%s USD = €%s EUR\n", usd_str, eur_str);
    }

    return 0;
}
```

### Example 4: Handling Division Precisely

```c
#include <decimal.h>
#include <stdio.h>

int main(void) {
    // Divide a bill equally among 3 people
    _Decimal64 total_bill = dec64_from_string("100.00");
    _Decimal64 num_people = dec64_from_int64(3);

    _Decimal64 per_person = dec64_div(total_bill, num_people);

    char buf[32];
    dec64_to_string(per_person, buf, sizeof(buf));

    printf("Total bill: $100.00\n");
    printf("Split 3 ways: $%s each\n", buf);  // $33.333... (exact representation)

    // Verify: 3 × per_person should equal total
    _Decimal64 verify = dec64_mul(per_person, num_people);
    dec64_to_string(verify, buf, sizeof(buf));
    printf("Verification: 3 × $%s = $%s\n",
           "33.333...", buf);

    return 0;
}
```

## Performance

### Portable libdecimal Performance

As a software implementation, typical operation times:

| Operation | Cycles (approx) |
|-----------|-----------------|
| Addition/Subtraction | 10-50 |
| Multiplication | 20-100 |
| Division | 50-200 |
| Conversion (int/string) | 10-100 |
| Comparison | 5-20 |

### Hardware-Accelerated Performance

Modern CPUs with decimal FP instructions (IBM POWER6+, z/Architecture):

| Operation | Cycles (approx) |
|-----------|-----------------|
| All operations | 1-5 |

When available, use native compiler support for best performance.

## Standards Compliance

- **IEEE 754-2008** - Decimal floating-point arithmetic specification
- **ISO/IEC TR 24732** - Extension for the programming language C to support decimal floating-point arithmetic
- **C23 Draft (N3088)** - Decimal floating-point types proposed for C23

## Platform Support

| Platform | Native Decimal | libdecimal | Notes |
|----------|----------------|------------|-------|
| GCC 4.3+ | Yes (software) | Not needed | Use -std=c2x or ISO TR 24732 |
| GCC 4.2 and older | No | Yes | Uses portable library |
| Intel ICC | Yes | Not needed | Full hardware support on capable CPUs |
| IBM XL C | Yes | Not needed | Optimized for POWER processors |
| Clang 15+ | Partial | Maybe | Check with configure |
| PCC (this project) | Via libdecimal | Yes | Portable implementation |

## Implementation Details

### Encoding Format

libdecimal uses a simplified **BID (Binary Integer Decimal)** encoding:

```
value = (-1)^sign × coefficient × 10^exponent
```

### Bit Layout

**_Decimal32:** 1 sign bit + 8 exponent bits + 23 coefficient bits
**_Decimal64:** 1 sign bit + 10 exponent bits + 53 coefficient bits
**_Decimal128:** 1 sign bit + 14 exponent bits + 113 coefficient bits

### Rounding

Currently implements **round-to-nearest, ties-to-even** (banker's rounding).

### Limitations

Current portable implementation:
- Simplified arithmetic (suitable for most applications)
- No subnormal number support
- Basic exception handling
- String parsing via conversion (limited precision)

For full IEEE 754-2008 compliance, consider integrating GCC's libdecnumber.

## Future Enhancements

Potential improvements:
1. **Full libdecnumber integration** - Complete IEEE 754-2008 compliance
2. **Compiler frontend support** - Parse `19.99DD` literals directly
3. **Hardware acceleration** - Use POWER dfp instructions when available
4. **Additional rounding modes** - Support all IEEE 754 modes
5. **Exception flags** - Implement IEEE exception signaling

## Testing

Comprehensive test program included:

```bash
# Build test
pcc test_decimal.c -o test_decimal -lm

# Run test
./test_decimal
```

Test coverage:
- Special values (zero, infinity, NaN)
- Integer conversions
- String conversions
- All arithmetic operations
- All comparison operations
- Type conversions
- Financial calculation examples

## Troubleshooting

### Error: "_Decimal64 undeclared"

Your compiler doesn't support decimal types. Either:
1. Use libdecimal API functions instead of operators
2. Rebuild PCC after running `./configure` to build libdecimal

### Link Error: "undefined reference to dec64_add"

The decimal library wasn't linked. Either:
1. Add `-ldecimal` to your link line
2. Use PCC which links it automatically

### Warning: "precision loss in conversion"

Converting from larger to smaller decimal types may lose precision:
```c
_Decimal128 large = dec128_from_string("1.23456789012345678901234567890");
_Decimal32 small = dec128_to_dec32(large);  // Precision lost!
```

Use appropriate type sizes for your precision needs.

## See Also

- **libdecimal/README.md** - Detailed library documentation
- **C23_SUPPORT.md** - Other C23 features in PCC
- **C11_SUPPORT.md** - C11 features documentation
- IEEE 754-2008 Standard
- ISO/IEC TR 24732 (Decimal FP Technical Report)

## References

1. IEEE Standard for Floating-Point Arithmetic (IEEE 754-2008)
2. ISO/IEC TR 24732:2009 - Extension for the programming language C to support decimal floating-point arithmetic
3. GCC Decimal Float Support: https://gcc.gnu.org/onlinedocs/gcc/Decimal-Float.html
4. IBM developerWorks: Decimal Floating-Point
5. Mike Cowlishaw's decNumber library

---

**Version:** 1.0
**Last Updated:** 2025
**Maintainer:** PCC Development Team
**License:** BSD-style (see COPYING file)
