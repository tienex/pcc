# COBOL Compiler Tests

This directory contains test programs for the COBOL compiler and runtime library.

## Test Programs

### Basic Tests
- **hello.cob**: Simple "Hello World" program
- **arithmetic.cob**: Arithmetic operations (ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE)
- **oo-test.cob**: Object-oriented COBOL features (CLASS, METHOD)

### Runtime Library Tests
- **test_runtime.c**: Unit tests for libcobol runtime functions

## Running Tests

### Build the runtime library test:
```bash
cd libcobol
make
cd ../cobol/tests
cc -I../../libcobol test_runtime.c -L../../libcobol -lcobol -o test_runtime
./test_runtime
```

### Test the COBOL compiler (when ready):
```bash
# First, configure and build
cd /home/user/pcc
./configure
make

# Compile COBOL programs
./cobol/cobol/cobol tests/hello.cob -o hello
./hello
```

## Expected Output

### hello.cob:
```
Hello from COBOL!
Counter: 42
```

### arithmetic.cob:
```
100 + 50 = 150
100 - 50 = 50
100 * 2 = 200
100 / 2 = 50
100 + 50 * 2 = 200
```

### test_runtime:
```
Testing COBOL Runtime Library...

Testing numeric operations...
  100 + 50 = 150
  100 - 50 = 50
  100 * 50 = 5000
  100 / 50 = 2
  ✓ Numeric operations passed

Testing string operations...
  MOVE: Hello
  STRING: HelloWorld
  COMPARE: -1
  ✓ String operations passed

Testing field conversions...
  Set/Get Int: 12345
  Set/Get Double: 123.45
  ✓ Field conversions passed

Testing intrinsic functions...
  LENGTH: 20
  UPPER-CASE: HELLO WORLD
  LOWER-CASE: hello world
  REVERSE: dlrow olleh
  ✓ Intrinsic functions passed

All tests passed!
```

## Test Coverage

- [x] Basic program structure
- [x] DISPLAY statement
- [x] MOVE statement
- [x] Arithmetic operations
- [x] COMPUTE statement
- [x] Numeric field handling
- [x] Alphanumeric field handling
- [x] String operations
- [x] Intrinsic functions
- [x] OO COBOL syntax (CLASS, METHOD)
- [ ] File I/O operations
- [ ] PERFORM loops
- [ ] IF/ELSE conditions
- [ ] ACCEPT from stdin
