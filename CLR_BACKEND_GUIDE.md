# PCC CLR (Common Language Runtime) Backend Implementation Guide

## Quick Reference: What Goes in Each File

### 1. arch/clr/macdefs.h (600-800 lines)

Define CLR's type system and constants:

```c
#ifndef _MACDEFS_H_
#define _MACDEFS_H_

/* Character/byte handling */
#define makecc(val,i)   lastcon = (lastcon<<8)|((val<<24)>>24);

/* Type sizes in bits (CLR is typically 32-bit stack-based) */
#define SZCHAR       8
#define SZBOOL       32    /* bool on CLR stack */
#define SZINT        32
#define SZSHORT      16
#define SZLONG       32    /* or 64 if targeting CLR on 64-bit */
#define SZLONGLONG   64
#define SZFLOAT      32
#define SZDOUBLE     64
#define SZLDOUBLE    64
#define SZPOINT(t)   32    /* pointers are object references */

/* Alignment requirements */
#define ALCHAR       8
#define ALBOOL       32
#define ALINT        32
#define ALSHORT      16
#define ALLONG       32
#define ALPOINT      32
#define ALSTACK      32

/* Min/max constants */
#define MIN_CHAR     -128
#define MAX_CHAR     127
#define MAX_UCHAR    255
#define MIN_SHORT    -32768
#define MAX_SHORT    32767
#define MAX_USHORT   65535
#define MIN_INT      (-2147483647 - 1)
#define MAX_INT      2147483647
#define MAX_UINT     4294967295U

/* Register allocation (CLR has virtual stack, not physical regs) */
#define NUMREGS      8       /* Virtual evaluation stack depth */
#define RETREG       0       /* Return value "register" */

/* Argument passing */
#define ARGINIT      0       /* Arguments start at offset 0 */
#define AUTOINIT     0       /* Locals start at offset 0 */

/* Output format */
#define LABFMT       "L%d"   /* Label format */

/* Special CLR considerations */
#define TARGET_ILASM         /* Generate ILASM output */

#endif
```

### 2. arch/clr/code.c (2000-3000 lines)

Implement function prologue/epilogue and entry/exit code:

```c
#include "pass1.h"

/*
 * Beginning of job - called once at start of compilation
 */
void
bjobcode(void)
{
    printf(".assembly extern mscorlib { }\n");
    printf(".assembly CLRCode { }\n");
    printf(".namespace PCC\n");
    printf("{\n");
}

/*
 * End of job - called once at end of compilation
 */
void
ejobcode(int flag)
{
    printf("} // end namespace\n");
}

/*
 * Beginning of function prologue
 * s = array of symbols (parameters)
 * cnt = number of parameters
 */
void
bfcode(struct symtab **s, int cnt)
{
    int i, lcloff;
    
    printf(".method public static ");
    
    /* Return type */
    switch (s[cnt]->stype) {
        case VOID: printf("void"); break;
        case INT: printf("int32"); break;
        case FLOAT: printf("float64"); break;
        /* ... handle other types ... */
    }
    
    printf(" %s(\n", s[cnt]->sname);
    
    /* Arguments */
    for (i = 0; i < cnt; i++) {
        printf("    [in] ");
        switch (s[i]->stype) {
            case INT: printf("int32"); break;
            case FLOAT: printf("float64"); break;
            /* ... */
        }
        printf(" %s", s[i]->sname);
        if (i < cnt - 1) printf(",\n");
    }
    
    printf(")\n");
    printf("{\n");
    printf("    .locals init (\n");
    
    /* Local variables */
    /* ... emit .locals for auto variables ... */
    printf("    )\n");
}

/*
 * End of function
 */
void
efcode(void)
{
    printf("}\n\n");
}

/*
 * Code generation for function
 * Called during Pass 1 for architecture-specific setup
 */
void
funcode(NODE *p)
{
    /* Optional: Called for each function declaration */
}
```

### 3. arch/clr/local.c (1500-2000 lines)

Handle symbols and section directives:

```c
#include "pass1.h"

/*
 * Set segment (section) for code output
 */
void
setseg(int seg, char *name)
{
    switch (seg) {
        case PROG:  /* .text */
            break;
        case DATA:  /* .data */
        case LDATA:
            break;
        case UDATA: /* .bss */
            break;
    }
}

/*
 * Declare external symbols
 */
void
genextern(struct symtab *sp)
{
    if (sp->sclass == EXTERN) {
        if (sp->stype == STRTY || sp->stype == UNIONTY) {
            /* Handle struct/union types */
        } else {
            /* Handle global functions/variables */
            printf(".method extern %s\n", sp->sname);
        }
    }
}

/*
 * Declare global symbols
 */
void
globname(struct symtab *sp)
{
    if (sp->sclass == EXTDEF || sp->sclass == EXTERN) {
        if (sp->sflags & SFUNC) {
            printf(".method public static");
        } else {
            printf(".field public static");
        }
        printf(" %s\n", sp->sname);
    }
}
```

### 4. arch/clr/local2.c (1500-2000 lines)

Handle register allocation for CLR stack:

```c
#include "pass2.h"

/*
 * Called during Pass 2 instruction selection
 * CLR doesn't use traditional registers, but we can use this
 * to manage the evaluation stack depth
 */
void
local2init(void)
{
    /* Initialize any local structures */
}

/*
 * Allocate stack slots for temporaries
 */
int
alloctemp(int sz)
{
    static int depth = 0;
    int off = depth;
    depth += sz;
    return off;
}

/*
 * Perform any CLR-specific optimizations
 */
void
rmove(int s, int t, TWORD t1)
{
    /* Handle register moves (or stack moves in CLR) */
}

/*
 * Output helper - convert register/location to CLR stack notation
 */
void
outaddr(NODE *p)
{
    printf("[");
    if (p->n_name)
        printf("%s", p->n_name);
    else if (p->n_op == OREG)
        printf("bp+%d", p->n_su);
    printf("]");
}
```

### 5. arch/clr/table.c (2000-3000+ lines)

Define instruction selection rules:

```c
#include "pass2.h"

/* Type macros */
#define TWORD   TINT|TUNSIGNED
#define TLONG   TLONG|TULONG
#define ANYFIXED TINT|TUNSIGNED|TSHORT|TUSHORT|TCHAR|TUCHAR
#define ANYFLOAT TFLOAT|TDOUBLE|TLDOUBLE

struct optab table[] = {
    /* First entry MUST be empty */
    { -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

    /* ===== INTEGER OPERATIONS ===== */
    
    /* Integer addition: reg + reg = reg */
    { PLUS, INAREG,
        SAREG, TWORD,
        SAREG, TWORD,
        NAREG, RESC1,
        "add\n", },
    
    /* Integer addition: reg + const = reg */
    { PLUS, INAREG,
        SAREG, TWORD,
        SCON, TWORD,
        0, RESC1,
        "ldc.i4 AR\n"
        "add\n", },
    
    /* Subtraction */
    { MINUS, INAREG,
        SAREG, TWORD,
        SAREG, TWORD,
        NAREG, RESC1,
        "sub\n", },
    
    /* Multiplication */
    { MUL, INAREG,
        SAREG, TWORD,
        SAREG, TWORD,
        NAREG, RESC1,
        "mul\n", },
    
    /* Division */
    { DIV, INAREG,
        SAREG, TWORD,
        SAREG, TWORD,
        NAREG, RESC1,
        "div\n", },
    
    /* ===== LOAD/STORE ===== */
    
    /* Load integer constant */
    { ICON, INAREG,
        SCON, TWORD,
        SANY, TANY,
        0, RESC1,
        "ldc.i4 AL\n", },
    
    /* Load from global variable */
    { NAME, INAREG,
        SNAME, TWORD,
        SANY, TANY,
        0, RESC1,
        "ldsfld int32 PCC::AL\n", },
    
    /* Load from local variable */
    { OREG, INAREG,
        SOREG, TWORD,
        SANY, TANY,
        0, RESC1,
        "ldloc.s AL\n", },
    
    /* Store to global variable */
    { ASSIGN, FOREFF,
        SNAME, TWORD,
        SAREG, TWORD,
        0, RLEFT,
        "stsfld int32 PCC::AL\n", },
    
    /* Store to local variable */
    { ASSIGN, FOREFF,
        SOREG, TWORD,
        SAREG, TWORD,
        0, RLEFT,
        "stloc.s AL\n", },
    
    /* ===== COMPARISONS ===== */
    
    /* Equality test */
    { EQ, FORCC,
        SAREG, TWORD,
        SAREG, TWORD,
        0, RESC1,
        "ceq\n", },
    
    /* Not equal */
    { NE, FORCC,
        SAREG, TWORD,
        SAREG, TWORD,
        0, RESC1,
        "ceq\n"
        "ldc.i4.0\n"
        "ceq\n", },
    
    /* ===== FUNCTION CALLS ===== */
    
    /* Function call */
    { CALL, INAREG,
        SAREG, TPOINT,  /* Function pointer */
        SANY, TANY,      /* Arguments */
        NAREG|NASL, RESC1,
        "call int32 AL\n", },
    
    /* ===== CONTROL FLOW ===== */
    
    /* Branch if condition */
    { CBRANCH, FOREFF,
        FORCC, TWORD,
        SCON, TINT,
        0, RLEFT,
        "brtrue AL\n", },
    
    /* Return */
    { RETURN, FOREFF,
        SAREG, TWORD,
        SANY, TANY,
        0, RLEFT,
        "ret\n", },
    
    /* Return void */
    { RETURN, FOREFF,
        SANY, TVOID,
        SANY, TANY,
        0, RLEFT,
        "ret\n", },
    
    /* Type conversions */
    { SCONV, INAREG,
        SAREG, TCHAR,
        SAREG, TINT,
        0, RLEFT,
        "conv.i1\n", },
    
    { SCONV, INAREG,
        SAREG, TUCHAR,
        SAREG, TUNSIGNED,
        0, RLEFT,
        "conv.u1\n", },
    
    /* ... add more operators as needed ... */
};

int tblsize = sizeof(table) / sizeof(table[0]);
int tblents = sizeof(table) / sizeof(table[0]);
```

### 6. arch/clr/order.c (600-1000 lines)

Define cost metrics and evaluation order:

```c
#include "pass2.h"

/*
 * Sethi-Ullman numbers for CLR
 * - CLR is stack-based, so costs are different from register machines
 * - Focus on minimizing stack depth
 */

int
suflags(int op)
{
    switch (op) {
        /* Leaf nodes - no children */
        case NAME:
        case ICON:
        case FCON:
            return 0;
        
        /* Unary operators */
        case UMINUS:
        case COMPL:
        case ADDROF:
            return 01;
        
        /* Binary operators - evaluate left first */
        case PLUS:
        case MINUS:
        case MUL:
        case DIV:
        case MOD:
        case AND:
        case OR:
        case ER:
            return 02;
        
        /* Function calls - expensive */
        case CALL:
        case UCALL:
            return 04;
        
        default:
            return 01;
    }
}

/*
 * Return cost to put temporary storage for tree
 */
int
cost(NODE *p)
{
    switch (p->n_op) {
        case ICON:
            return 1;  /* Loading constants is cheap */
        case NAME:
            return 2;  /* Loading from memory is medium */
        case CALL:
            return 10; /* Function calls are expensive */
        default:
            return 3;
    }
}
```

## Minimal Viable Implementation Checklist

- [ ] **macdefs.h**: Type definitions (200 lines minimum)
- [ ] **code.c**: bjobcode(), efcode(), bfcode(), ejobcode() (500 lines)
- [ ] **local.c**: setseg(), genextern(), globname() (400 lines)
- [ ] **local2.c**: local2init(), alloctemp(), basic stubs (300 lines)
- [ ] **table.c**: At least 30-50 optab entries (1000+ lines)
- [ ] **order.c**: suflags() and cost() functions (200 lines)

## Testing the Implementation

```bash
# Build with CLR target
./configure --target=x86_64-clr-win32
make

# Test with simple C program
echo 'int main() { return 42; }' > test.c
./cc/cc/cc test.c

# Check if CLR IL is generated
# Should see: ldc.i4.s 42, ret
```

## Key CLR IL Instructions Reference

| Operation | ILASM Instruction | Example |
|-----------|-------------------|---------|
| Load const | `ldc.i4` | `ldc.i4 10` |
| Load var | `ldloc.s` | `ldloc.s 0` |
| Load static | `ldsfld` | `ldsfld int32 X` |
| Store | `stloc.s` | `stloc.s 0` |
| Store static | `stsfld` | `stsfld int32 X` |
| Add | `add` | (stack-based) |
| Multiply | `mul` | (stack-based) |
| Compare | `ceq` | `ceq` |
| Branch | `brtrue` | `brtrue Label` |
| Return | `ret` | `ret` |
| Call | `call` | `call int32 Func()` |

## References

1. **CLR IL Specification**: Partition III of ECMA-335
2. **ILASM Grammar**: Details in ECMA-335
3. **PCC ARM Backend**: `/home/user/pcc/arch/arm/` - Similar scope to CLR
4. **PCC i386 Backend**: `/home/user/pcc/arch/i386/` - Good register management example

## Common Pitfalls

1. **Empty First Table Entry**: The first entry in table[] must have op = -1
2. **Stack-Based vs Register-Based**: CLR doesn't allocate physical registers like traditional backends
3. **Type Annotations**: CLR IL requires explicit type information in many instructions
4. **Local Variable Layout**: Must track local variable offsets for ldloc/stloc instructions
5. **Assembly Metadata**: Need proper .assembly, .namespace, and .class directives

