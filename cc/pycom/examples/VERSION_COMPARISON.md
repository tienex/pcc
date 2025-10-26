# Python 2 vs Python 3 Comparison

This directory contains examples demonstrating the differences between Python 2 and Python 3 syntax supported by the PCC Python compiler.

## Key Differences Implemented

### 1. Print Statement vs Print Function

**Python 2:**
```python
print "Hello, World!"
print "x =", x
print "a =", a, "b =", b
```

**Python 3:**
```python
print("Hello, World!")
print("x =", x)
print("a =", a, "b =", b)
```

### 2. Division Operator Semantics

**Python 2:**
- `/` performs integer division for integers: `10 / 3 = 3`
- `//` performs floor division: `10 // 3 = 3`

**Python 3:**
- `/` performs true division (would return float): `10 / 3 = 3.333...`
- `//` performs floor division: `10 // 3 = 3`

Note: The compiler currently treats `/` as integer division in both modes since we don't have full float support yet.

### 3. Keywords

**Python 2 Only:**
- `print` (statement)
- `exec` (statement)
- `xrange` (builtin)

**Python 3:**
- `print` is a regular function
- `exec` is a regular function
- `nonlocal` keyword (for closures)

## Usage

### Compiling Python 2 Code

```bash
./pycom -2 examples/python2/hello.py
./pycom -2 -v examples/python2/factorial.py
```

### Compiling Python 3 Code

```bash
./pycom -3 examples/python3/hello.py
./pycom examples/python3/hello.py  # Python 3 is default
```

## Examples Directory Structure

```
examples/
├── python2/
│   ├── hello.py          # Print statement demonstration
│   ├── arithmetic.py     # Integer division demonstration
│   └── factorial.py      # Recursive function with print
├── python3/
│   ├── hello.py          # Print function demonstration
│   ├── arithmetic.py     # Floor division demonstration
│   └── factorial.py      # Recursive function with print()
└── VERSION_COMPARISON.md # This file
```

## Planned Future Enhancements

1. **True division in Python 3**: `/` returns float
2. **Unicode strings**: Default string type differences
3. **Exception syntax**: `except E, e:` (Python 2) vs `except E as e:` (Python 3)
4. **Integer types**: `long` type in Python 2
5. **Range vs xrange**: Iterator differences
6. **Input functions**: `raw_input()` vs `input()`
7. **Imports**: Absolute vs relative import defaults

## Testing

To test both versions:

```bash
# Test Python 2 mode
cd cc/pycom
./pycom -2 -v examples/python2/hello.py
./pycom -2 -v examples/python2/factorial.py
./pycom -2 -v examples/python2/arithmetic.py

# Test Python 3 mode
./pycom -3 -v examples/python3/hello.py
./pycom -3 -v examples/python3/factorial.py
./pycom -3 -v examples/python3/arithmetic.py
```

## Compatibility Notes

### Features That Work in Both Modes

- Function definitions (`def`)
- Control flow (`if`, `while`, `for`)
- Arithmetic operators (`+`, `-`, `*`, `/`, `%`, `//`, `**`)
- Bitwise operators (`&`, `|`, `^`, `~`, `<<`, `>>`)
- Comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`)
- Logical operators (`and`, `or`, `not`)
- Variable assignment
- Return statements
- Function calls

### Mode-Specific Features

**Python 2 Mode (`-2` flag):**
- `print` is treated as a keyword/statement
- `exec` is treated as a keyword/statement
- `xrange` is recognized as a keyword

**Python 3 Mode (`-3` flag or default):**
- `print` is treated as a regular identifier (function)
- `exec` is treated as a regular identifier (function)
- `nonlocal` keyword is available

## Version Detection

The compiler automatically detects which Python version to use based on:
1. Command-line flag (`-2` or `--3`)
2. Default if no flag: Python 3

You can check which version is being used with the `-v` (verbose) flag:

```bash
$ ./pycom -2 -v test.py
Compiling: test.py
Python version: 2
...
```
