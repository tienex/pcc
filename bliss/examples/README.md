# BLISS Example Programs

This directory contains example BLISS programs demonstrating various features of the language, as well as working examples using the BLISS runtime library.

## Runtime Library Examples (C)

These examples demonstrate using the BLISS runtime library (libbliss) from C programs. They show the actual I/O and utility functions available.

### hello_runtime.c
Working Hello World using runtime library:
- Character and string output
- Numeric output (decimal, hex, octal)
- Demonstrates bliss_main() entry point

```bash
cd bliss/examples
make hello_runtime
./hello_runtime
```

### vector_runtime.c
Vector/array operations with runtime library:
- Vector allocation (bliss_alloc_vector)
- Element access and initialization
- Sum calculation
- Memory management (bliss_free_vector)

```bash
make vector_runtime
./vector_runtime
```

### string_runtime.c
String manipulation examples:
- String creation (bliss_string_from_cstr)
- String concatenation (bliss_string_concat)
- String comparison (bliss_string_compare)
- Substring extraction (bliss_string_substr)
- String output (bliss_put_string)

```bash
make string_runtime
./string_runtime
```

### bits_runtime.c
Bit field and bit counting operations:
- Field extraction (bliss_field_extract)
- Field insertion (bliss_field_insert)
- Bit counting (bliss_count_leading_zeros, bliss_count_trailing_zeros, bliss_popcount)
- Hexadecimal output

```bash
make bits_runtime
./bits_runtime
```

### Building All Runtime Examples

```bash
cd bliss/examples
make          # Build all examples
make run-all  # Build and run all examples
make clean    # Clean built files
```

## BLISS Language Examples

These are pure BLISS programs demonstrating language syntax (not yet fully functional as they require complete compiler implementation).

### hello.bli
Basic "Hello World" program demonstrating:
- Module structure
- Routine definitions
- External routine declarations
- Comments

```bash
bliss hello.bli -o hello
./hello
```

### factorial.bli
Factorial calculation showing:
- Recursive routines
- Iterative loops (WHILE)
- Conditional expressions (IF-THEN-ELSE)
- Local variables
- Arithmetic operations
- Return values

Implements both recursive and iterative factorial calculation.

```bash
bliss factorial.bli -o factorial
./factorial
```

### vector_ops.bli
Vector (array) operations demonstrating:
- Vector declarations
- Block vectors (BLOCKVECTOR)
- Vector subscripting
- INCR loops (counting up)
- DECR loops (counting down)
- Vector initialization
- Sum, max, and reverse operations

```bash
bliss vector_ops.bli -o vector_ops
./vector_ops
```

### control_flow.bli
Advanced control flow showing:
- CASE expressions
- SELECTONE expressions
- Loop constructs (DO-WHILE, WHILE-DO, INCR, DECR)
- LEAVE statements (loop exit)
- EXITLOOP (break)
- Complex conditional logic

```bash
bliss control_flow.bli -o control_flow
./control_flow
```

## BLISS Language Features Demonstrated

### Module Structure
```bliss
MODULE name =
BEGIN
    ! Declarations and code
END
ELUDOM
```

### Routine Definitions
```bliss
GLOBAL ROUTINE func(param1, param2) =
BEGIN
    ! Function body
    ! Last expression is the return value
END;

ROUTINE private_func : NOVALUE =
BEGIN
    ! No return value
END;
```

### Local Variables
```bliss
LOCAL var;
LOCAL vec : BLOCKVECTOR[10];
```

### Conditionals
```bliss
IF condition
THEN
    expression1
ELSE
    expression2
```

### Loops
```bliss
! INCR loop
INCR i FROM 0 TO 9 DO
    statement;

! DECR loop
DECR i FROM 9 TO 0 DO
    statement;

! WHILE loop
WHILE condition DO
    statement;

! DO-WHILE loop
DO
    statement
WHILE condition;
```

### CASE Expression
```bliss
CASE .n FROM 0 TO 6 OF
SET
    [0]: value0;
    [1, 2, 3]: value123;
    OUTRANGE: default_value
TES
```

### SELECTONE Expression
```bliss
SELECTONE .value OF
SET
    [1]: expr1;
    [2, 3]: expr23;
    OTHERWISE: default_expr
TES
```

### Vector Operations
```bliss
LOCAL vec : BLOCKVECTOR[10];
vec[0] = 42;
x = .vec[5];
```

### Comments
```bliss
! This is a comment in BLISS
```

## Compiling Examples

To compile any example:

```bash
# Compile to executable
bliss example.bli -o example

# Compile to assembly
bliss -S example.bli -o example.s

# Compile to object file
bliss -c example.bli -o example.o

# Link with runtime library
bliss example.bli -lbliss -o example
```

## Notes

- These examples demonstrate syntax and language features
- For actual I/O operations, link with libbliss runtime library
- BLISS expressions always have a value (last expression in block)
- The `.` prefix dereferences a variable (fetch its value)
- Assignment uses `=` not `:=`
- BLISS is expression-oriented, not statement-oriented

## Further Reading

- [BLISS Language Manual](../README.md)
- [BLISS Runtime Library](../../libbliss/README.md)
- Original DEC BLISS documentation
