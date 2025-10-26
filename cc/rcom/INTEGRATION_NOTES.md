# Ruby Compiler Integration Notes

## What Has Been Implemented

The Ruby front-end compiler (rcom) has been fully implemented with the following components:

1. **Lexer** (`scan.l`) - Tokenizes Ruby source code
2. **Parser** (`cgram.y`) - Parses Ruby grammar and builds AST
3. **IR Definitions** (`pass1.h`) - Ruby-specific extensions to PCC IR
4. **Tree Operations** (`trees.c`) - AST manipulation functions
5. **Symbol Table** (`symtabs.c`) - Symbol management
6. **Main Entry** (`main.c`) - Compiler driver
7. **Support Files** (`pftn.c`, `init.c`) - Function and initialization handling
8. **Build System** (`Makefile.in`) - Build configuration
9. **Documentation** (`README.md`) - Comprehensive user guide

## Integration with PCC Build System

To fully integrate the Ruby compiler into the PCC build system, the following steps are needed:

### 1. Update `configure.ac`

Add Ruby compiler support to the main configuration file:

```autoconf
# Add after other compiler configurations
AC_ARG_ENABLE([ruby],
  [AS_HELP_STRING([--enable-ruby], [Build Ruby compiler (rcom)])],
  [enable_ruby=$enableval],
  [enable_ruby=no])

AM_CONDITIONAL(BUILD_RUBY, test x$enable_ruby = xyes)

# Add to CCNAMES variable
if test x$enable_ruby = xyes; then
  CCNAMES="$CCNAMES \$(BINPREFIX)rcom\$(EXEEXT)"
  RUBYCCNAMES="\$(BINPREFIX)rcom\$(EXEEXT)"
else
  RUBYCCNAMES=""
fi
AC_SUBST(RUBYCCNAMES)
```

### 2. Update `cc/Makefile.in`

Add rcom to the subdirectories:

```makefile
# Add to SUBDIRS
SUBDIRS = ccom cxxcom rcom

# Add conditional build
if BUILD_RUBY
RUBY_SUBDIR = rcom
endif
```

### 3. Update `configure.ac` AC_CONFIG_FILES

Add the Ruby Makefile to the list of generated files:

```autoconf
AC_CONFIG_FILES([
  ...
  cc/rcom/Makefile
  ...
])
```

### 4. Regenerate Build System

After making the above changes:

```bash
autoreconf -i
./configure --enable-ruby
make
```

## Missing Dependencies

The current implementation assumes the following functions are available from the PCC infrastructure:

1. **From mip/common.c**:
   - `talloc()` - Allocate tree node
   - `send_passt()` - Send tree to pass 2
   - `mkdope()` - Initialize operator table

2. **From pass2.h**:
   - P1ND structure definition
   - Operator definitions (PLUS, MINUS, CALL, etc.)

3. **From common files**:
   - `compat.c` - Portability functions
   - `softfloat.c` - Floating-point support
   - `unicode.c` - Unicode handling

These are provided by the existing PCC infrastructure and will be linked automatically by the Makefile.

## Testing the Compiler

### Manual Build (without configure integration)

You can build rcom manually for testing:

```bash
cd cc/rcom

# Generate parser and lexer
yacc -d cgram.y
mv y.tab.c cgram.c
mv y.tab.h cgram.h

flex scan.l
mv lex.yy.c scan.c

# Compile (adjust paths as needed)
gcc -I../../mip -I../../arch/amd64 -I../../common -I../ccom \
    -c main.c trees.c symtabs.c pftn.c init.c cgram.c scan.c

# Link (this requires proper paths to PCC object files)
gcc -o rcom *.o ../../mip/*.o ../../arch/amd64/*.o ../../common/*.o
```

### Test Program

Create a simple Ruby test:

```ruby
# test.rb
def add(a, b)
  return a + b
end

result = add(5, 3)
puts result
```

Compile:
```bash
./rcom test.rb test.s
```

## Known Issues and TODO

### Immediate Issues

1. **Missing Function Implementations**:
   - Some functions called in the code need proper implementations
   - `locctr()` - Location counter management
   - `defid()` - Identifier definition

2. **Type System**:
   - Need better type inference for Ruby expressions
   - Dynamic typing not fully supported

3. **Error Handling**:
   - Need better error messages
   - Recovery from parse errors

### Future Enhancements

1. **Language Features**:
   - Full block/proc support with closures
   - Module system
   - Metaclasses and singleton methods
   - Exception handling (begin/rescue/ensure)

2. **Optimizations**:
   - Type inference for better code generation
   - Constant folding
   - Dead code elimination
   - Inline expansion for small methods

3. **Runtime Support**:
   - Garbage collector integration
   - Ruby object model in C
   - Standard library bindings

4. **Debugging**:
   - DWARF debug information for Ruby
   - Source-level debugging support
   - Better error diagnostics

## Architecture Notes

The Ruby compiler follows PCC's two-pass architecture:

**Pass 1** (rcom):
- Lexical analysis (scan.l)
- Syntax analysis (cgram.y)
- Semantic analysis (trees.c)
- Generate P1ND IR

**Pass 2** (existing PCC backends):
- Register allocation
- Instruction selection
- Assembly generation

This means that once rcom generates valid P1ND IR, all 18 PCC backends automatically support Ruby compilation!

## References

- Main documentation: `README.md`
- Architecture guide: `../ARCHITECTURE_EXPLORATION_SUMMARY.md`
- Implementation guide: `../ruby_frontend_guide.txt`
- PCC documentation: `../../README`

## Status

**Status**: Alpha - Structural implementation complete, needs testing and debugging

**Last Updated**: 2025-10-26
