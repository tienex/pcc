# PL/I Compiler Architecture

This document describes the architecture of the PCC PL/I compiler and how it integrates with the PCC infrastructure.

## Overview

The PL/I compiler follows the standard PCC two-pass architecture:

```
PL/I Source (.pli)
    ↓
[Frontend: plicom]
  ├─ Lexer (scan.l) → Tokens
  ├─ Parser (pligram.y) → Parse Tree
  ├─ Semantic Analysis → Type checking, Symbol resolution
  └─ IR Generation (ir.c) → PCC NODE structures
    ↓
[PCC Middle-End: mip/]
  ├─ Optimization (optim.c, optim2.c)
  ├─ Register allocation (regs.c)
  └─ Instruction selection (match.c)
    ↓
[PCC Backend: arch/TARGETARCH/]
  ├─ Code generation (code.c)
  ├─ Local optimization (local.c, local2.c)
  └─ Instruction templates (table.c)
    ↓
Assembly (.s) → [as] → Object (.o) → [ld + libpli.a] → Executable
```

## Why PCC IR?

**The PL/I compiler uses the PCC intermediate representation (NODE structures)** instead of generating assembly directly. This provides several critical advantages:

### 1. **Multi-Architecture Support**
By generating PCC IR, the PL/I compiler automatically works on all 16+ architectures supported by PCC:
- x86 (i386, i86)
- x86-64 (amd64)
- ARM, ARM64
- MIPS, MIPS64
- PowerPC, PowerPC64
- SPARC, SPARC64
- VAX
- M68K
- M16C
- PDP-11, PDP-10, PDP-7
- Nova
- WebAssembly

No architecture-specific code is needed in the PL/I frontend.

### 2. **Optimization Infrastructure**
The PCC middle-end provides:
- **Common subexpression elimination**
- **Dead code elimination**
- **Register allocation** (graph coloring)
- **Instruction scheduling**
- **Peephole optimization**
- **Strength reduction**
- **Loop optimization**

All of these work automatically with PL/I code.

### 3. **Code Quality**
The PCC backends have been highly optimized over decades. By using the same IR, PL/I benefits from:
- **Efficient instruction selection** via pattern matching
- **Architecture-specific optimizations** in each backend
- **ABI compliance** (calling conventions, stack layout)
- **Debug symbol generation** (30+ formats supported)

### 4. **Maintainability**
Using PCC IR means:
- **No duplication of code generation logic**
- **Single source of truth** for code generation
- **Bug fixes in PCC** automatically benefit PL/I
- **New architecture support** automatically available

## PCC IR (NODE Structures)

### NODE Definition

From `mip/node.h`:

```c
typedef struct node {
    int n_op;              // Operator type
    TWORD n_type;          // Data type
    TWORD n_qual;          // Type qualifiers
    int n_su;              // Size/alignment
    struct attr *n_ap;     // Attributes
    union {
        struct node *n_left;
        CONSZ n_val;       // Constant value
    } n_l;
    union {
        struct node *n_right;
        int n_rval;
    } n_r;
} NODE;
```

### Operators

From `mip/node.h`, some key operators:

**Value Nodes:**
- `NAME` - Variable/function reference
- `ICON` - Integer constant
- `FCON` - Floating-point constant
- `REG` - Register
- `TEMP` - Temporary

**Arithmetic:**
- `PLUS`, `MINUS`, `MUL`, `DIV`, `MOD`
- `UMINUS` - Unary minus

**Logical:**
- `AND`, `OR`, `ER` (XOR), `COMPL` (NOT)
- `LS` (left shift), `RS` (right shift)

**Comparison:**
- `EQ`, `NE`, `LT`, `LE`, `GT`, `GE`
- `ULT`, `ULE`, `UGT`, `UGE` (unsigned)

**Control Flow:**
- `CALL`, `UCALL` - Function calls
- `CBRANCH` - Conditional branch
- `GOTO` - Unconditional jump
- `RETURN` - Return from function

**Other:**
- `ASSIGN` - Assignment
- `STASG` - Structure assignment
- `CM` - Comma operator (argument lists)
- `SCONV` - Type conversion
- `ADDROF` - Address-of

### Type Encoding

From `mip/manifest.h`:

**Basic Types:**
- `CHAR`, `UCHAR`
- `SHORT`, `USHORT`
- `INT`, `UNSIGNED`
- `LONG`, `ULONG`
- `LONGLONG`, `ULONGLONG`
- `FLOAT`, `DOUBLE`, `LDOUBLE`
- `VOID`
- `STRTY` - Structure
- `UNIONTY` - Union

**Type Modifiers:**
- `PTR` - Pointer (use `INCREF(type)`)
- `FTN` - Function
- `ARY` - Array

**Macros:**
- `INCREF(t)` - Make pointer to type t
- `DECREF(t)` - Dereference pointer type
- `BTYPE(t)` - Get basic type
- `ISPTR(t)` - Check if pointer
- `ISFTN(t)` - Check if function
- `ISARY(t)` - Check if array

## PL/I to PCC Type Mapping

The `ir.c` module maps PL/I types to PCC types:

| PL/I Type          | PCC Type      | Notes                    |
|--------------------|---------------|--------------------------|
| FIXED BINARY(15)   | INT           | 32-bit signed integer    |
| FIXED BINARY(31)   | INT/LONG      | 32-bit signed integer    |
| FIXED BINARY(63)   | LONGLONG      | 64-bit signed integer    |
| FLOAT BINARY(24)   | FLOAT         | Single precision         |
| FLOAT BINARY(53)   | DOUBLE        | Double precision         |
| BIT(n)             | CHAR/UCHAR    | Bit string               |
| CHARACTER(n)       | CHAR          | Character string         |
| POINTER            | INCREF(INT)   | Pointer type             |
| BYTE (PL/M)        | UCHAR         | 8-bit unsigned           |
| WORD (PL/M)        | USHORT        | 16-bit unsigned          |
| DWORD (PL/M)       | UNSIGNED      | 32-bit unsigned          |
| ADDRESS (PL/M)     | INCREF(VOID)  | Generic pointer          |

## IR Generation Functions

### Initialization
```c
void ir_init(void);                    // Initialize IR generator
int ir_newlabel(void);                 // Allocate new label
void ir_label(int label);              // Emit label
```

### Node Creation
```c
NODE *ir_icon(CONSZ val);              // Integer constant
NODE *ir_fcon(double val);             // Float constant
NODE *ir_name(char *name, TWORD type); // Variable reference
NODE *ir_assign(NODE *var, NODE *expr);// Assignment
NODE *ir_binop(int op, NODE *l, NODE *r, TWORD t); // Binary op
NODE *ir_unop(int op, NODE *operand, TWORD t);     // Unary op
NODE *ir_call(char *func, NODE *args, TWORD ret);  // Function call
NODE *ir_arg(NODE *arg, NODE *next);   // Build argument list
NODE *ir_cbranch(NODE *cond, int label); // Conditional branch
NODE *ir_goto(int label);              // Unconditional jump
NODE *ir_return(NODE *expr);           // Return statement
```

### Function Management
```c
void ir_function_start(struct symtab *sp, int is_main);
void ir_function_end(int is_main);
```

### Emitting to Backend
```c
void ir_emit(NODE *p);                 // Send node to pass2
```

## Communication with Pass2

The `send_passt()` function (from `mip/manifest.h`) sends information to the backend:

```c
void send_passt(int type, ...);
```

**Message Types (from `mip/pass2.h`):**

- `IP_NODE` - Send a NODE tree for code generation
- `IP_PROLOG` - Function prologue
- `IP_EPILOG` - Function epilogue
- `IP_DEFLABEL` - Define a label
- `IP_ASM` - Inline assembly

**Example:**
```c
// Send assignment statement to backend
NODE *p = ir_assign(var, expr);
send_passt(IP_NODE, p);

// Emit a label
send_passt(IP_DEFLABEL, label_num);

// Function prologue
send_passt(IP_PROLOG, func_symtab);
```

## Example: PL/I to IR

### PL/I Code:
```pli
HELLO: PROCEDURE OPTIONS(MAIN);
    DECLARE MSG CHARACTER(20) INITIAL('Hello, World!');
    DECLARE I FIXED BINARY(31);

    I = 42;
    PUT SKIP LIST('Value:', I);
END HELLO;
```

### Generated IR (Conceptual):
```c
// Function start
ir_function_start(hello_sym, 1);  // 1 = OPTIONS(MAIN)

// Declarations create symbol table entries (already done)

// I = 42
NODE *i_var = ir_name("I", INT);
NODE *forty_two = ir_icon(42);
NODE *assign = ir_assign(i_var, forty_two);
ir_emit(assign);

// PUT SKIP
NODE *put_skip = ir_call("pli_put_skip", NIL, VOID);
ir_emit(put_skip);

// PUT LIST('Value:')
NODE *msg = ir_name(".LC0", INCREF(CHAR));  // String constant
NODE *put_str = ir_call("pli_put_string", msg, VOID);
ir_emit(put_str);

// PUT LIST(I)
NODE *i_val = ir_name("I", INT);
NODE *put_int = ir_call("pli_put_fixed", i_val, VOID);
ir_emit(put_int);

// Function end
ir_function_end(1);  // 1 = OPTIONS(MAIN), calls pli_finish()
```

### PCC Backend Output (x86-64):

```asm
    .text
    .globl HELLO
HELLO:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $256, %rsp

    call    pli_init@PLT

    movl    $42, -4(%rbp)           # I = 42
    call    pli_put_skip@PLT
    leaq    .LC0(%rip), %rdi
    call    pli_put_string@PLT
    movl    -4(%rbp), %edi
    call    pli_put_fixed@PLT

    call    pli_finish@PLT
    xorl    %eax, %eax
    leave
    ret
```

## Runtime Library Integration

### Automatic Linking

The PL/I driver (`pli/pli/pli.c`) automatically links with:
- `-lpli` - PL/I runtime library
- `-lm` - Math library (for trigonometric functions)

### Runtime Functions

All PL/I built-in functions map to `libpli.a` functions:

| PL/I Built-in | Runtime Function    | Signature                        |
|---------------|---------------------|----------------------------------|
| PUT SKIP      | pli_put_skip()      | void pli_put_skip(void)          |
| PUT LIST(n)   | pli_put_fixed()     | void pli_put_fixed(int32_t)      |
| GET LIST(n)   | pli_get_fixed()     | int32_t pli_get_fixed(void)      |
| ABS(n)        | pli_abs_fixed()     | int32_t pli_abs_fixed(int32_t)   |
| SQRT(x)       | pli_sqrt()          | double pli_sqrt(double)          |
| SIN(x)        | pli_sin()           | double pli_sin(double)           |
| INDEX(s1,s2)  | pli_index()         | int32_t pli_index(char*, char*)  |
| LENGTH(s)     | pli_length()        | int32_t pli_length(char*)        |
| ALLOCATE      | pli_allocate()      | void* pli_allocate(size_t)       |
| FREE          | pli_free()          | void pli_free(void*)             |

### Main Procedure Handling

For `OPTIONS(MAIN)` procedures, the IR generator creates:

```c
int main(void) {
    pli_init();        // Initialize runtime
    PROCEDURE_NAME();  // Call PL/I procedure
    pli_finish();      // Cleanup runtime
    return 0;
}
```

## Benefits Summary

### For Users:
- **Write once, run anywhere** - Same PL/I code works on all architectures
- **High performance** - PCC's optimizations produce efficient code
- **Standard compliance** - Follows PCC ABI conventions

### For Developers:
- **Simpler maintenance** - No architecture-specific code in frontend
- **Easier debugging** - Standard IR format, well-understood
- **Extensibility** - Easy to add new PL/I features

### For the Ecosystem:
- **Consistency** - All PCC languages use same IR and backends
- **Shared improvements** - Optimizations benefit all languages
- **Reduced code duplication** - Single backend codebase

## See Also

- `mip/manifest.h` - Operator and type definitions
- `mip/node.h` - NODE structure definition
- `mip/pass2.h` - Backend interface
- `pli/plicom/ir.c` - PL/I IR generation
- `pli/libpli/` - Runtime library
- PCC Architecture Documentation

## References

1. PCC Source Code: https://pcc.ludd.ltu.se/
2. PCC Paper: "A Portable C Compiler"
3. NODE-based IR: Classic compiler design
4. Backend Architecture: Pattern-matching code generation
