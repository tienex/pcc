# BLISS Compiler Tests

This directory contains test files for the BLISS compiler and runtime library.

## Integration Tests

Working integration tests for the runtime library:

### test_runtime_io.c
Tests I/O functions:
- putchar, puts, putcrlf
- put_decimal, put_hex, put_octal
- Basic output functionality

### test_runtime_memory.c
Tests memory management:
- malloc/free
- Vector allocation and initialization
- Vector freeing
- Memory operations

### Running Integration Tests

```bash
cd bliss/tests
make          # Build all tests
make run      # Build and run all tests
make clean    # Clean test binaries
```

## Future Test Organization

Tests will be organized by feature:
- `lexer/` - Lexical analysis tests
- `parser/` - Parser tests
- `semantic/` - Semantic analysis tests
- `codegen/` - Code generation tests
- `runtime/` - Runtime library tests

## Running Compiler Tests

To run a specific BLISS test:

```bash
bliss test_file.bli
```

## Test Format

Each test file is a BLISS program that tests specific language features.
Tests should be self-contained and demonstrate correct or incorrect usage.

## Adding Tests

To add a new test:
1. Create a `.bli` file in the appropriate subdirectory
2. Add a comment describing what is being tested
3. Include expected behavior (compile success/failure, output, etc.)

## Current Test Coverage

- [x] Lexer: Comments, identifiers, keywords, operators
- [x] Parser: Module structure, routines, expressions
- [ ] Semantic: Type checking, symbol resolution
- [ ] Code generation: MIP IR output
- [ ] Runtime: Library integration

## Known Issues

- Full MIP IR generation not yet implemented (stub implementation)
- Semantic analysis incomplete
- No integration tests with runtime library yet

## Future Improvements

- Add automated test runner
- Add expected output files
- Integrate with CI/CD
- Add performance benchmarks
- Add regression tests
