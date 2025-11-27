# Runtime Floating-Point Format Support Design

## Overview
This document describes the design for runtime floating-point format selection in PCC, allowing a single compiler binary to generate code for multiple FP formats.

## Supported Formats

### Currently Implemented (Compile-time)
1. **PDP10FLOAT** - PDP-10 native format
   - Single (36-bit): sign(1) + exp(8, excess-128) + frac(27)
   - Double (72-bit): sign(1) + exp(8, excess-1024) + frac(62)

2. **FDFLOAT** - VAX F/D-float
   - F-float (32-bit): sign(1) + exp(8, excess-128) + frac(23, hidden)
   - D-float (64-bit): sign(1) + exp(8, excess-128) + frac(55, hidden)

3. **IEEE 754 Binary**
   - binary32 (float): sign(1) + exp(8, bias 127) + frac(23, hidden)
   - binary64 (double): sign(1) + exp(11, bias 1023) + frac(52, hidden)
   - binary80 (long double): sign(1) + exp(15, bias 16383) + frac(64, explicit)

### To Be Added

4. **VAX G-float** (64-bit)
   - sign(1) + exp(11, excess-1024) + frac(52, hidden)
   - Extended range compared to D-float
   - Same mantissa precision as IEEE binary64 but different format

5. **VAX H-float** (128-bit)
   - sign(1) + exp(15, excess-16384) + frac(112, hidden)
   - Quadruple precision
   - Extended range compared to G-float

6. **IEEE 754 binary16** (half precision, 16-bit)
   - sign(1) + exp(5, bias 15) + frac(10, hidden)
   - Already defined in softfloat.c under `#ifdef notdef`
   - Useful for ML/GPU applications

7. **IEEE 754 binary128** (quad precision, 128-bit)
   - sign(1) + exp(15, bias 16383) + frac(112, hidden)
   - Already defined in softfloat.c under `#ifndef notyet`
   - Full quadruple precision

8. **IEEE 754-2008 8-bit formats** (research needed)
   - FP8 E4M3: sign(1) + exp(4) + mantissa(3) - ML training
   - FP8 E5M2: sign(1) + exp(5) + mantissa(2) - ML inference
   - Non-standard, used in NVIDIA/AMD GPUs
   - May need custom implementation

## Architecture Changes Required

### 1. FPI Structure Definitions
Define FPI structures for all formats in `common/softfloat.c`:

```c
/* VAX G-float (64-bit, extended range) */
FPI fpi_vax_g = { 53,   1-1024-53+1,
                        2046-1024-53+1, FPI_Round_near_from0, 0,
        0, 0, 0, 1,  64,     1024+53-1 };

/* VAX H-float (128-bit quad precision) */
FPI fpi_vax_h = { 113,   1-16384-113+1,
                         32766-16384-113+1, FPI_Round_near_from0, 0,
        0, 0, 0, 1,  128,     16384+113-1 };

/* IEEE binary16 (already exists, enable it) */
FPI fpi_binary16 = { 11, 1-15-11+1,
                        30-15-11+1, 1, 0,
        0, 1, 1, 0,  16,   15+11-1 };

/* IEEE binary128 (already exists) */
FPI fpi_binary128 = { 113,   1-16383-113+1,
                         32766-16383-113+1, 1, 0,
        0, 1, 1, 0,   128,     16383+113-1 };

/* FP8 formats (to be researched and implemented) */
FPI fpi_fp8_e4m3 = { 4, /* ... TBD ... */ };
FPI fpi_fp8_e5m2 = { 3, /* ... TBD ... */ };
```

### 2. Runtime Format Selection

Add global variables for FP format selection:
```c
extern int pdp10_fpfmt_float;   /* Format for FLOAT type */
extern int pdp10_fpfmt_double;  /* Format for DOUBLE type */
extern int pdp10_fpfmt_ldouble; /* Format for LDOUBLE type */
```

Format constants:
```c
#define PDP10_FP_PDP10     0  /* PDP-10 native */
#define PDP10_FP_VAX_FD    1  /* VAX F/D-float */
#define PDP10_FP_VAX_G     2  /* VAX G-float */
#define PDP10_FP_VAX_H     3  /* VAX H-float */
#define PDP10_FP_IEEE32    4  /* IEEE binary32 */
#define PDP10_FP_IEEE64    5  /* IEEE binary64 */
#define PDP10_FP_IEEE80    6  /* IEEE binary80 */
#define PDP10_FP_IEEE16    7  /* IEEE binary16 */
#define PDP10_FP_IEEE128   8  /* IEEE binary128 */
#define PDP10_FP_FP8_E4M3  9  /* FP8 E4M3 */
#define PDP10_FP_FP8_E5M2  10 /* FP8 E5M2 */
```

### 3. Runtime fpis[] Array

Make fpis[] array contents runtime-selectable:
```c
void pdp10_init_fp_formats(void) {
    switch (pdp10_fpfmt_float) {
    case PDP10_FP_IEEE32:
        fpis[0] = &fpi_binary32;
        break;
    case PDP10_FP_IEEE16:
        fpis[0] = &fpi_binary16;
        break;
    /* ... etc ... */
    }

    switch (pdp10_fpfmt_double) {
    case PDP10_FP_IEEE64:
        fpis[1] = &fpi_binary64;
        break;
    case PDP10_FP_VAX_G:
        fpis[1] = &fpi_vax_g;
        break;
    /* ... etc ... */
    }

    /* Similar for LDOUBLE */
}
```

### 4. Command-Line Flags

Add flags to select FP formats:
```
-mfp-float=<format>    # Format for float type
-mfp-double=<format>   # Format for double type
-mfp-ldouble=<format>  # Format for long double type

Where <format> is one of:
  pdp10      - PDP-10 native format
  vax-f      - VAX F-float (32-bit)
  vax-d      - VAX D-float (64-bit)
  vax-g      - VAX G-float (64-bit, extended range)
  vax-h      - VAX H-float (128-bit)
  ieee32     - IEEE 754 binary32 (float)
  ieee64     - IEEE 754 binary64 (double)
  ieee80     - IEEE 754 binary80 (long double)
  ieee16     - IEEE 754 binary16 (half)
  ieee128    - IEEE 754 binary128 (quad)
  fp8-e4m3   - FP8 E4M3 (8-bit, ML training)
  fp8-e5m2   - FP8 E5M2 (8-bit, ML inference)
```

### 5. Challenges

#### Format-Specific Operations
Some operations in softfloat.c are format-specific (e.g., PDP10FLOAT has two's complement negatives). Solutions:
1. Use function pointers for format-specific operations
2. Use runtime dispatch based on format ID
3. Generalize operations where possible

#### Storage Size Mismatches
Different formats have different storage sizes. SZFLOAT/SZDOUBLE/SZLDOUBLE macros need to be runtime-aware:
```c
#define SZFLOAT   (pdp10_fpfmt_float_size())
#define SZDOUBLE  (pdp10_fpfmt_double_size())
#define SZLDOUBLE (pdp10_fpfmt_ldouble_size())
```

#### Compatibility
Need to maintain backwards compatibility:
- Default to PDP-10 format in native mode
- Default to VAX F/D in POW2 mode (current behavior)
- Allow override with explicit flags

## Implementation Plan

### Phase 1: FPI Structure Definitions
1. Define FPI structures for VAX G and H
2. Enable existing binary16 and binary128 definitions
3. Research and define FP8 formats

### Phase 2: Runtime Infrastructure
1. Add global FP format variables
2. Implement pdp10_init_fp_formats()
3. Make fpis[] array runtime-configurable
4. Add format size functions

### Phase 3: Command-Line Flags
1. Add -mfp-* flags to mflags()
2. Parse format names and set globals
3. Call initialization function

### Phase 4: Format-Specific Operations
1. Audit softfloat.c for format-specific code
2. Implement runtime dispatch for format differences
3. Test all format combinations

### Phase 5: Testing
1. Create test cases for each format
2. Verify correct code generation
3. Test mixed-format scenarios

## Benefits

1. **Single Binary**: One PCC build supports all FP formats
2. **Flexibility**: Mix formats (e.g., IEEE float + VAX double)
3. **Cross-Platform**: Better support for Alpha, VAX, and other targets
4. **Modern Support**: IEEE 754 half/quad and FP8 for ML workloads
5. **Compatibility**: Maintain existing behavior as defaults

## Alpha Support

The user mentioned "It is also needed for alpha target":
- Alpha uses IEEE 754 formats by default
- VAX compatibility mode uses VAX G-float
- This runtime system allows single Alpha backend to support both

## Cross-Platform Type Tables

Related user request: "Alpha on Win32 uses 32-bit pointers"
- Same architecture, different ABI/platform
- Need runtime type table based on target platform
- Similar to pdp10_ptrsize approach

This can be generalized to:
```c
struct target_config {
    int ptrsize;      /* Pointer size */
    int fpfmt_float;  /* Float format */
    int fpfmt_double; /* Double format */
    int fpfmt_ldouble;/* Long double format */
    /* etc. */
};
```

Load configuration based on target triple (e.g., "alpha-unknown-linux", "alpha-unknown-win32").
