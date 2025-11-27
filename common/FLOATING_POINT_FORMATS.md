# Floating Point Format Support in PCC

This document describes the status of the 50+ floating point formats supported by PCC's softfloat library.

## Implementation Status Overview

### ✅ Fully Implemented (Binary Formats)
These formats work with the generic softfloat arithmetic engine:

#### IEEE 754 Standard Formats
- **binary16** (half precision) - USE_IEEEFP_16
- **binary32** (single precision) - USE_IEEEFP_32
- **binary64** (double precision) - USE_IEEEFP_64
- **binary80/x80** (extended precision) - USE_IEEEFP_X80
- **binary128** (quad precision) - USE_IEEEFP_128

#### Modern AI/ML Formats
- **bfloat16** (Brain Float) - USE_BFLOAT16
- **TensorFloat-32 (TF32)** - USE_TENSORFLOAT32
- **FP8 E4M3** - USE_FP8_E4M3
- **FP8 E5M2** - USE_FP8_E5M2

#### OCP ML Format Variants (with helper functions)
- **OCP E4M3FN** - USE_OCP_E4M3FN (finite only, no INF)
- **OCP E5M2FNUZ** - USE_OCP_E5M2FNUZ (finite, no negative zero)
- **OCP E4M3FNUZ** - USE_OCP_E4M3FNUZ (finite, no negative zero)

#### Historic Computer Systems (Radix 2)
- **Microsoft Binary Format (MBF)** - 32/64-bit - USE_MSBFP_32/64
  - No INF/NaN support handled correctly
  - Bias 129, no implicit bit

- **DEC VAX formats** - USE_VAXFP_F/D/G/H
  - F_floating (32-bit), D_floating (64-bit)
  - G_floating (64-bit), H_floating (128-bit)
  - No INF/NaN support handled correctly
  - Note: Byte order conversion helpers present but not complete

- **Cray Floating Point** - USE_CRAYFP_64
  - 64-bit format with 48-bit mantissa
  - No INF/NaN, no denormals

- **Motorola 68k Extended** - USE_M68K_EXT96
  - 96-bit format with explicit leading bit
  - Same as x87 but 96-bit storage

#### Historic Computer Systems (Radix 16)
- **IBM Hexadecimal FP** - USE_IBMFP_32/64/128
  - 32/64/128-bit formats
  - Radix 16 arithmetic fully supported by softfloat
  - 7-bit exponent, bias 64

#### Specialized Formats
- **ARM Alternative Half** - USE_ARM_ALT_HALF (no INF/NaN)
- **Pixar PXR24** - USE_PXR24 (24-bit for image processing)
- **AMD 24-bit** - USE_AMD24
- **AMD 3DNow!** - USE_AMD_3DNOW (standard IEEE 32-bit)
- **HP-PA Quad** - USE_HPPA_QUAD (128-bit)
- **Intel Flexpoint16** - USE_FLEXPOINT16
- **Qualcomm Hexagon Half** - USE_HEXAGON_HALF
- **Google BF8** - USE_GOOGLE_BF8

#### Research/Educational Formats
- **Minifloat E3M2** - USE_MINIFLOAT_E3M2 (6-bit)
- **Minifloat E2M3** - USE_MINIFLOAT_E2M3 (6-bit)
- **NVIDIA MX9 E4M3** - USE_NVIDIA_MX9_E4M3
- **NVIDIA MX9 E5M2** - USE_NVIDIA_MX9_E5M2

### 🔧 Partially Implemented

#### Double-Double (High Precision)
- **Double-Double** - USE_DOUBLE_DOUBLE
- Status: **Arithmetic helper functions implemented**
- Basic operations: add, subtract, multiply, negate
- Uses error-free transformation algorithms
- Provides ~106 bits of mantissa precision
- TODO: Division, square root, transcendental functions
- TODO: Integration with FPI conversion routines

### ❌ Not Implemented (Require Specialized Libraries)

These formats have FPI specifications but require fundamentally different arithmetic:

#### IEEE 754-2008 Decimal Formats
- **decimal32/64/128** - USE_DECIMAL32/64/128
- Requires: DPD (Densely Packed Decimal) or BID (Binary Integer Decimal) arithmetic
- Would need: Complete decimal arithmetic library (e.g., Intel decNumber)

#### Alternative Number Systems
- **Posit formats** - USE_POSIT8/16/32/64
  - Requires: Complete posit arithmetic library
  - Different: Regime-based tapered precision, NaR instead of NaN

- **Unum Type I** - USE_UNUM16/32
  - Requires: Complete unum library with uncertainty tracking
  - Different: Variable-size format with ubit

- **Logarithmic Number System** - USE_LNS16/32
  - Requires: Specialized logarithmic arithmetic
  - Different: Value = sign * 2^exponent (no mantissa)
  - Easy: Multiplication/division
  - Hard: Addition/subtraction (requires antilog tables)

## How Softfloat Works

PCC's softfloat library is a **generic floating point emulator** that can handle any format described by the FPI (Floating Point Information) structure.

### FPI Structure Fields
```c
typedef struct FPI {
    int nbits;              // Mantissa bits
    int emin;               // Minimum exponent
    int emax;               // Maximum exponent
    int rounding;           // Rounding mode

    int sudden_underflow:1; // Underflow behavior
    int explicit_one:1;     // MSB explicitly stored
    int has_inf_nan:1;      // Supports INF and NaN
    int has_neg_zero:1;     // Supports negative zero
    int has_radix_16:1;     // Radix 16 (hex) floats
    int storage;            // Storage size in bits
    int exp_bias;           // Exponent bias
} FPI;
```

### Supported Characteristics

1. **Radix 2 (Binary)**: Standard floating point
2. **Radix 16 (Hexadecimal)**: IBM format support ✅
3. **No INF/NaN**: MBF, VAX, Cray formats ✅
4. **Explicit leading bit**: x87, 68k formats ✅
5. **Different exponent ranges**: All handled ✅
6. **Different biases**: All handled ✅

### Unsupported Characteristics

1. **Radix 10 (Decimal)**: Requires different arithmetic ❌
2. **Variable precision (Posit/Unum)**: Requires different arithmetic ❌
3. **Logarithmic encoding**: Requires different arithmetic ❌
4. **Double-pair (Double-Double)**: Partially supported 🔧

## Using Alternative Formats

To use a non-standard floating point format:

1. **In architecture macdefs.h**, define the format:
```c
#define USE_BFLOAT16    // Use bfloat16 for float type
#define USE_IBMFP_64    // Use IBM hex for double type
```

2. **Rebuild the compiler** with the new configuration

3. **Format constants** are automatically defined in softfloat.h

## Format Compatibility Matrix

| Format Category | Count | Softfloat Support | Special Requirements |
|----------------|-------|-------------------|---------------------|
| IEEE Binary | 5 | ✅ Full | None |
| AI/ML Binary | 11 | ✅ Full | OCP helpers included |
| Historic Binary | 11 | ✅ Full | VAX byte order TODO |
| Historic Hex | 3 | ✅ Full | Radix 16 implemented |
| Specialized | 10 | ✅ Full | None |
| Educational | 4 | ✅ Full | None |
| Double-Double | 1 | 🔧 Partial | Helpers implemented |
| Decimal | 3 | ❌ None | Need decimal library |
| Posit | 4 | ❌ None | Need posit library |
| Unum | 2 | ❌ None | Need unum library |
| Logarithmic | 2 | ❌ None | Need LNS library |

**Summary**: ~44 of 50+ formats fully usable, 1 partially implemented, 11 need external libraries

## References

- IEEE 754-2008 Standard
- Open Compute Project ML Formats Specification
- IBM System/360 Principles of Operation
- DEC VAX Architecture Reference Manual
- Cray Research Hardware Reference Manual
- Gustafson, J. (2017). "The End of Error: Unum Computing"
- Gustafson, J. (2022). "Posit Arithmetic"
- Bailey, D. (2007). "High-Precision Floating-Point Arithmetic in Scientific Computation"

## Contributing

To add support for currently unimplemented formats:

1. **Decimal formats**: Integrate Intel decNumber or similar library
2. **Posit formats**: Integrate SoftPosit or similar library
3. **Unum formats**: Implement unum arithmetic from specification
4. **LNS formats**: Implement logarithmic add/subtract algorithms
5. **Double-Double**: Complete division, sqrt, and transcendental functions

See the helper function templates in `common/softfloat.c` for examples.
