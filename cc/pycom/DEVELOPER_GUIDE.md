# Python Frontend Compiler - Developer Guide

## Quick Start for Developers

### Understanding the Codebase

The Python compiler is organized into several key modules:

```
pycom.h      → Main header with all data structures
lexer.c      → Tokenization (source → tokens)
parser.c     → Parsing (tokens → AST)
symtab.c     → Symbol table (variable/function tracking)
codegen.c    → Code generation (AST → PCC IR)
util.c       → Utilities (error handling, memory management)
main.c       → Driver program
```

### Adding New Language Features

#### 1. Adding a New Operator

**Step 1**: Add token type to `pycom.h`:
```c
typedef enum {
    // ... existing tokens
    TOK_NEW_OPERATOR,
} TokenType;
```

**Step 2**: Add lexer support in `lexer.c`:
```c
switch (c) {
    // ... existing cases
    case 'X':
        return make_token(TOK_NEW_OPERATOR, NULL, line, column);
}
```

**Step 3**: Add parser support in `parser.c`:
```c
static ASTNode *parse_expression_level(Parser *parser) {
    // ... handle new operator
    if (parser_match(parser, TOK_NEW_OPERATOR)) {
        // parse logic
    }
}
```

**Step 4**: Add codegen support in `codegen.c`:
```c
static NODE *codegen_expr(ASTNode *expr) {
    switch (expr->data.binop.op) {
        case TOK_NEW_OPERATOR:
            return make_binop(PCC_OP, left, right, type);
    }
}
```

#### 2. Adding a New Statement Type

**Step 1**: Add AST node type to `pycom.h`:
```c
typedef enum {
    // ... existing types
    AST_NEW_STATEMENT,
} ASTNodeType;
```

**Step 2**: Add AST node data structure:
```c
struct ASTNode {
    union {
        // ... existing
        struct {
            // fields for new statement
        } new_stmt;
    } data;
};
```

**Step 3**: Add keyword token (if needed):
```c
// In pycom.h
TOK_NEW_KEYWORD,

// In lexer.c keywords table
{"newkeyword", TOK_NEW_KEYWORD},
```

**Step 4**: Add parser function:
```c
static ASTNode *parse_new_stmt(Parser *parser) {
    ASTNode *node = ast_new(AST_NEW_STATEMENT);
    parser_expect(parser, TOK_NEW_KEYWORD);
    // parse statement components
    return node;
}
```

**Step 5**: Update main statement parser:
```c
static ASTNode *parse_stmt(Parser *parser) {
    if (parser_match(parser, TOK_NEW_KEYWORD)) {
        return parse_new_stmt(parser);
    }
    // ... existing cases
}
```

**Step 6**: Add code generation:
```c
static NODE *codegen_stmt(ASTNode *stmt) {
    switch (stmt->type) {
        case AST_NEW_STATEMENT:
            // generate PCC IR nodes
            return node;
    }
}
```

#### 3. Adding a New Data Type

**Step 1**: Map to PCC type in `codegen.c`:
```c
// Python lists might map to:
TWORD type = PTR | STRTY;  // Pointer to struct
```

**Step 2**: Add type checking in symbol table:
```c
Symbol *symtab_insert(SymbolTable *symtab, const char *name, TWORD type) {
    // Store type information
    sym->type = type;
}
```

**Step 3**: Add runtime support (if needed):
```c
// Create helper functions for type operations
NODE *create_list_node(int size) {
    // allocate and initialize
}
```

### PCC IR Reference

#### Common Node Types

```c
ICON     // Integer constant
NAME     // Variable name
PLUS     // Addition
MINUS    // Subtraction
MUL      // Multiplication
DIV      // Division
MOD      // Modulo
AND      // Bitwise AND
OR       // Bitwise OR
ER       // XOR (exclusive OR)
LS       // Left shift
RS       // Right shift
COMPL    // Bitwise complement (~)
UMINUS   // Unary minus (-)
EQ       // Equal
NE       // Not equal
LT       // Less than
LE       // Less or equal
GT       // Greater than
GE       // Greater or equal
ASSIGN   // Assignment
CALL     // Function call
RETURN   // Return statement
CBRANCH  // Conditional branch
GOTO     // Unconditional branch
CM       // Comma (arg separator)
```

#### Creating Nodes

```c
// Constant
NODE *icon = make_icon(42);

// Variable
NODE *var = make_name("x");

// Binary operation: x + 42
NODE *add = make_binop(PLUS, var, icon, LONGLONG);

// Assignment: x = 42
NODE *assign = make_binop(ASSIGN, var, icon, LONGLONG);

// Function call: foo(x, 42)
NODE *args = make_binop(CM, var, icon, LONGLONG);
NODE *func = make_name("foo");
NODE *call = xmalloc(sizeof(NODE));
call->n_op = CALL;
call->n_left = func;
call->n_right = args;
```

### Debugging Tips

#### 1. Enable Verbose Mode

```bash
./pycom -v test.py
```

This shows:
- Tokens (from lexer)
- AST structure
- Compilation stages

#### 2. Print AST

The `print_ast()` function in `main.c` displays the AST tree:

```
Module:
  FunctionDef: factorial
    Parameters:
      Name: n
    Body:
      If:
        Test:
          Compare: op=27
            Name: n
            Number: 1
        Body:
          Return:
            Number: 1
```

#### 3. Add Debug Output

```c
// In codegen.c
fprintf(stderr, "DEBUG: Generating code for %s\n", expr->data.name.id);
```

#### 4. Check Symbol Table

```c
Symbol *sym = symtab_lookup(symtab, "variable");
if (sym) {
    fprintf(stderr, "Found: %s at offset %d\n", sym->name, sym->offset);
}
```

### Common Patterns

#### Expression Precedence

Operator precedence (highest to lowest):
1. Primary (literals, identifiers, parentheses)
2. Unary (`-`, `not`, `~`)
3. Multiplicative (`*`, `/`, `%`, `//`)
4. Additive (`+`, `-`)
5. Shift (`<<`, `>>`)
6. Bitwise AND (`&`)
7. Bitwise XOR (`^`)
8. Bitwise OR (`|`)
9. Comparison (`==`, `!=`, `<`, `<=`, `>`, `>=`)
10. Logical (`and`, `or`)

#### Statement Block Parsing

```c
static ASTNode **parse_suite(Parser *parser, int *count) {
    parser_expect(parser, TOK_COLON);
    parser_skip_newlines(parser);
    parser_expect(parser, TOK_INDENT);

    // Parse statements until DEDENT
    while (!parser_match(parser, TOK_DEDENT)) {
        stmts[stmt_count++] = parse_stmt(parser);
    }

    parser_expect(parser, TOK_DEDENT);
    return stmts;
}
```

#### Indentation Tracking

The lexer maintains an indent stack:
```c
// Indent increased → emit INDENT
if (indent > current_indent) {
    lexer->indent_stack[lexer->indent_count++] = indent;
    return make_token(TOK_INDENT, ...);
}

// Indent decreased → emit DEDENT(s)
if (indent < current_indent) {
    while (lexer->indent_stack[...] > indent) {
        lexer->indent_count--;
        dedents++;
    }
    return make_token(TOK_DEDENT, ...);
}
```

### Testing Workflow

#### 1. Create Test File

```python
# test.py
def test():
    x = 42
    return x

test()
```

#### 2. Compile with Verbose

```bash
./pycom -v test.py
```

#### 3. Check Output

- AST should show correct structure
- No errors should be reported
- (Future) Assembly output should be valid

#### 4. Add to Test Suite

```bash
cp test.py examples/
# Update Makefile test target if needed
```

### Integration with PCC

#### Building with PCC

To integrate with PCC's build system:

1. Add to `cc/Makefile.in`:
```makefile
SUBDIRS = cc ccom cxxcom pycom ...
```

2. Create `cc/pycom/Makefile.in`:
```makefile
# Template for autotools integration
```

3. Update `configure.ac`:
```bash
AC_CONFIG_FILES([cc/pycom/Makefile])
```

#### Using PCC Backends

The generated IR can be processed by:

```bash
# Generate IR
./pycom -o output.ir program.py

# (Future) Pass to PCC backend
ccom -c output.ir -o output.s

# Assemble
as output.s -o output.o

# Link
ld output.o -o program
```

### Error Handling

#### Reporting Errors

```c
void error(const char *fmt, ...) {
    fprintf(stderr, "Error: ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    error_count++;
}
```

#### Error Recovery

Currently, the compiler stops after 10 errors. To improve:

1. **Skip to synchronization point**:
```c
while (!parser_match(parser, TOK_NEWLINE) &&
       !parser_match(parser, TOK_EOF)) {
    parser_advance(parser);
}
```

2. **Create error nodes**:
```c
if (error) {
    return ast_new(AST_ERROR);
}
```

### Performance Considerations

1. **Memory Management**: All allocations use `xmalloc`/`xrealloc`
2. **No Memory Leaks**: Free AST nodes with `ast_free()`
3. **Efficient Parsing**: Single-pass parser with lookahead
4. **Symbol Lookup**: Linear search (consider hash table for large programs)

### Future Enhancements

Priority features to implement:

1. **For loops**: Map to while with iterator
2. **Lists/tuples**: Heap allocation + runtime support
3. **String operations**: Concatenation, indexing
4. **Built-in functions**: `print()`, `len()`, `range()`
5. **Multiple assignment**: `a, b = 1, 2`
6. **Augmented assignment**: `+=`, `-=`, etc. (partially done)
7. **Boolean operators**: Short-circuit evaluation
8. **Comparison chaining**: `a < b < c`

### Resources

- [PCC Source Code](http://pcc.ludd.ltu.se/)
- [Python Grammar](https://docs.python.org/3/reference/grammar.html)
- [Dragon Book](https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools)
- [Crafting Interpreters](https://craftinginterpreters.com/)

### Getting Help

- Check `README.md` for user documentation
- Look at `examples/` for working code
- Study PCC's C frontend in `cc/ccom/`
- Examine IR nodes in `mip/manifest.h` and `mip/node.h`

## Quick Reference Card

### File → Module Mapping
- Lexer functions → `lexer.c`
- Parser functions → `parser.c`
- Symbol table → `symtab.c`
- Code generation → `codegen.c`
- Utilities → `util.c`
- Main driver → `main.c`
- All declarations → `pycom.h`

### Common Operations
- Create AST node: `ast_new(type)`
- Free AST node: `ast_free(node)`
- Create IR node: `make_icon()`, `make_name()`, `make_binop()`
- Lookup symbol: `symtab_lookup(symtab, name)`
- Report error: `error("message", ...)`
- Allocate memory: `xmalloc(size)`

### Build Commands
- Compile: `make`
- Clean: `make clean`
- Test: `make test`
- Install: `make install`
