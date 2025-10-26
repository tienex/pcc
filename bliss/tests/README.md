# BLISS Compiler Tests

This directory contains test files for the BLISS compiler.

## Test Organization

Tests are organized by feature:
- `lexer/` - Lexical analysis tests
- `parser/` - Parser tests
- `semantic/` - Semantic analysis tests
- `codegen/` - Code generation tests

## Running Tests

To run all tests:

```bash
cd bliss/tests
./run_tests.sh
```

To run a specific test:

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
