# PDP-10 POW2 Mode Code Generation Analysis

## Test Case Analysis

### Test 1: `int simple_add(int a, int b)`
```
Native mode:  int=36 bits -> 1 register
              Args: a in R1, b in R2
              Return: R1

POW2 mode:    int=32 bits -> 1 register
              Args: a in R1, b in R2
              Return: R1
              ✓ SAME - works correctly
```

### Test 2: `long long_add(long a, long b)`
```
Native mode:  long=36 bits -> 1 register
              Args: a in R1, b in R2
              Return: R1

POW2 mode:    long=64 bits -> 2 registers (pdp10_szty returns 2!)
              Args: a in XR1 (R1+R2 pair), b in XR3 (R3+R4 pair)
              Return: XR1
              ✓ DIFFERENT - correctly uses register pairs in POW2 mode
```

### Test 3: `int sum5(int a, int b, int c, int d, int e)`
```
Native mode:  All int=36 bits -> 1 register each
              Args: R1, R2, R3, R4, R5
              Return: R1

POW2 mode:    All int=32 bits -> 1 register each
              Args: R1, R2, R3, R4, R5
              Return: R1
              ✓ SAME - works correctly
```

### Test 4: `long long mixed(int a, long b, int c)`
```
Native mode:  int=36 bits (1 reg), long=36 bits (1 reg)
              Args: a in R1, b in R2, c in R3
              Return: long long (72 bits) in XR1

POW2 mode:    int=32 bits (1 reg), long=64 bits (2 regs!)
              Args: a in R1, b in XR2 (R2+R3 pair), c in R4
              Return: long long (64 bits) in XR1
              ✓ DIFFERENT - correctly handles mixed register allocation
```

### Test 5: `void* get_addr(int *p)`
```
Native mode:  pointer=36 bits -> 1 register
              Args: p in R1
              Return: R1

POW2 mode:    pointer=64 bits -> 2 registers (pdp10_szty checks ISPTR!)
              Args: p in XR1 (R1+R2 pair)
              Return: XR1
              ✓ DIFFERENT - correctly uses register pair for pointers
```

### Test 6: `long sum3_long(long a, long b, long c)`
```
Native mode:  long=36 bits -> 1 register each
              Args: a in R1, b in R2, c in R3
              Return: R1

POW2 mode:    long=64 bits -> 2 registers each
              Args: a in XR1 (R1+R2), b in XR3 (R3+R4), c in XR5 (R5+R6)
              Return: XR1
              ✓ DIFFERENT - correctly allocates 3 register pairs

              Critical check: regnum tracking in fixargs()
              - After arg a: regnum = 0 + pdp10_szty(LONG) = 0 + 2 = 2
              - After arg b: regnum = 2 + 2 = 4
              - After arg c: regnum = 4 + 2 = 6
              - All fit in registers (< 8) ✓
```

### Test 7: `long long return_longlong(void)`
```
Native mode:  long long=72 bits -> XR1 return register
              RETREG(LONGLONG) -> pdp10_szty(LONGLONG)==2 -> XR1

POW2 mode:    long long=64 bits -> XR1 return register
              RETREG(LONGLONG) -> pdp10_szty(LONGLONG)==2 -> XR1
              ✓ SAME - both use XR1 for return
```

## Critical Function Paths Verified

### 1. fixargs() in code.c:351,397,414
- Uses pdp10_szty() for argument register allocation ✓
- Correctly tracks regnum for register pairs ✓
- Spills to stack when regnum + szty > 8 ✓

### 2. mkreg() in code.c:377
- Uses pdp10_szty() to decide if register pair needed ✓
- If szty==2, adds 16 to convert R register to XR register ✓

### 3. gclass() in local2.c:1272
- Uses pdp10_szty() to return CLASSB vs CLASSA ✓
- CLASSB for register pairs, CLASSA for single registers ✓

### 4. PCLASS macro in macdefs.h:388
- Uses pdp10_szty() to select SBREG vs SAREG ✓
- Used in instruction template matching ✓

### 5. RETREG macro in macdefs.h:389
- Uses pdp10_szty() to select XR1 vs R1 ✓
- Determines return register based on type size ✓

## Floating-Point Handling

### Native Mode (PDP10FLOAT):
- float: PDP-10 36-bit format, uses 2 registers (architectural requirement)
- double: PDP-10 72-bit format, uses 2 registers

### POW2 Mode (FDFLOAT - VAX format):
- float: VAX F-floating 32-bit format, uses 1 register (fits in 36 bits)
- double: VAX D-floating 64-bit format, uses 2 registers

**Compile-time POW2 (-DPDP10_POW2)**: ✓ CORRECT
  - Uses FDFLOAT format
  - Type sizes match (SZFLOAT=32, SZDOUBLE=64)
  - pdp10_szty(FLOAT)=1, pdp10_szty(DOUBLE)=2
  - All consistent!

**Runtime POW2 (-mpow2 without -DPDP10_POW2)**: ✗ BROKEN
  - Still uses PDP10FLOAT format (36/72-bit)
  - But pdp10_szty claims FLOAT=1 register (32-bit)
  - Format mismatch will cause incorrect FP operations!

## Conclusion

The code generation logic is CORRECT when:
1. PCC is compiled with -DPDP10_POW2 for compile-time POW2 mode
2. All szty() calls replaced with pdp10_szty() (DONE ✓)
3. VAX floating-point format used for POW2 mode (DONE ✓)

The runtime -mpow2 flag will:
- Work for integer types and register allocation ✓
- FAIL for floating-point (format mismatch) ✗
- FAIL for structs (layout mismatch) ✗
- FAIL for arrays (element size mismatch) ✗

As documented: runtime flag is for TESTING ONLY, proper use requires recompiling with -DPDP10_POW2.
