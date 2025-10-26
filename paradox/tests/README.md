# PAL/ObjectPAL Test Programs

This directory contains test programs for the PAL/ObjectPAL compiler.

## Test Files

1. **hello.pal** - Simple "Hello, World!" program demonstrating basic PAL syntax
   - Variable declarations
   - Procedure definition
   - Built-in functions (msginfo)

2. **objectpal_example.pal** - ObjectPAL example showing advanced features
   - Methods with parameters
   - Try-except exception handling
   - Database operations (scan, post)
   - String and currency types
   - Date/time functions
   - Switch statements

3. **loops.pal** - Loop examples
   - For loop
   - While loop
   - Foreach loop (ObjectPAL)
   - Array operations

## Running the Tests

To compile a test program:

```bash
palcom -d objectpal-latest -o output.o test_file.pal
```

## Dialect Support

The compiler supports multiple PAL dialects:

- `pal-1.0` - Original Paradox 1.0 PAL (limited features)
- `pal-3.0` - Paradox 3.0 PAL (added functions, local vars)
- `pal-4.5` - Paradox 4.5 PAL (added switch, events)
- `objectpal-1.0` - ObjectPAL 1.0 (added objects, methods)
- `objectpal-7.0` - ObjectPAL 7.0 (added private members, destructors)
- `objectpal-latest` - Latest ObjectPAL features (default)

Example with specific dialect:

```bash
palcom -d pal-3.0 hello.pal
```
