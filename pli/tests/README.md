# PL/I Test Programs

This directory contains test programs for the PCC PL/I compiler.

## Test Programs

### PL/I Programs (.pli)

1. **hello.pli** - Simple "Hello, World!" program
   - Demonstrates basic program structure
   - Shows PUT statement for output

2. **factorial.pli** - Factorial calculator
   - Demonstrates recursive procedures
   - Shows function declaration with RETURNS
   - Uses RECURSIVE attribute

3. **fibonacci.pli** - Fibonacci sequence generator
   - Demonstrates iterative algorithms
   - Shows DO loops with TO clause
   - Uses local variable initialization

4. **array_demo.pli** - Array manipulation
   - Demonstrates array declarations
   - Shows array subscripting
   - Calculates sum and average

### PL/M Programs (.plm)

5. **plm_blink.plm** - LED blink pattern (PL/M-86)
   - Demonstrates PL/M syntax
   - Shows hardware I/O with OUTPUT
   - Uses PL/M-specific functions (SHL)

## Running Tests

To compile a PL/I program:
```bash
pli -d pli hello.pli -o hello
./hello
```

To compile a PL/M program:
```bash
pli -d plm86 plm_blink.plm -o blink
```

## Dialect Options

- `pli` - Standard PL/I (default)
- `plm` - Generic PL/M
- `plm86` - PL/M-86
- `plm386` - PL/M-386
- `plc` - PL/C (teaching dialect)
- `subset` - PL/I Subset G

## Language Features Demonstrated

- Procedure declarations with OPTIONS(MAIN)
- DECLARE statements with various data types
- FIXED BINARY (integer) types
- FLOAT BINARY (real) types
- CHARACTER VARYING (strings)
- Arrays with subscripting
- Recursive procedures
- DO loops (simple, TO, WHILE)
- IF-THEN-ELSE statements
- PUT statement for I/O
- Built-in functions
- PL/M-specific features (BYTE, WORD, OUTPUT, SHL)
