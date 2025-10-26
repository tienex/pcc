# Implementation Status: Memory Models and Extensions

## Overview

This document details what has been implemented versus what is still needed for complete, working support of memory models and x86 extensions in PCC.

---

## ✅ What's COMPLETE (Frontend/Parser)

### 1. **Keyword Recognition** ✅
All keywords are recognized by the lexer/parser:

| Architecture | Keywords | Status |
|--------------|----------|--------|
| **i86** | `__far`, `__near`, `__huge`, `__based`, `__segment`, `__self` | ✅ Parsing works |
| **i86** | `__cdecl`, `__pascal`, `__fortran`, `__syscall`, `__watcall` | ✅ Parsing works |
| **i86** | `__interrupt`, `__loadds`, `__saveregs`, `__export` | ✅ Parsing works |
| **i86** | `__ss`, `__cs`, `__ds`, `__es` | ✅ Parsing works |
| **i86** | `__emit`, `__asm` | ✅ Parsing works |
| **i86+i386** | `__far16` | ✅ Parsing works |
| **i86+i386+amd64** | `__fs`, `__gs` | ✅ Parsing works |
| **amd64** | `__near32` | ✅ Parsing works |

**Files**: `cc/ccom/gcc_compat.c`, `cc/ccom/pass1.h`

### 2. **Attribute System** ✅
All keywords map to GCC-style attributes:

```c
int __far *ptr;
// Equivalent to:
int __attribute__((far)) *ptr;
```

**Status**: ✅ Complete - all 52 keywords work as attributes

### 3. **Memory Model Flags** ✅
Command-line parsing and flag storage:

```bash
# i86
pcc -mcmodel=tiny    # Sets mcmodel = MCTINY
pcc -mcmodel=large   # Sets mcmodel = MCLARGE

# i386
pcc -mcmodel=small   # Sets mcmodel = MCI386_SMALL
pcc -mcmodel=large   # Sets mcmodel = MCI386_LARGE
```

**Files**:
- `cc/cc/cc.c` - Command-line parsing ✅
- `arch/i86/macdefs.h` - Flag definitions ✅
- `arch/i86/local2.c` - mcmodel variable ✅
- `arch/i386/macdefs.h` - Flag definitions ✅
- `arch/i386/local2.c` - mcmodel variable ✅

### 4. **SZPOINT Macro** ✅
Dynamic pointer sizing based on memory model:

```c
// arch/i86/macdefs.h
#define SZPOINT(t)  (ISFTN(DECREF(t)) ? \
                     ((mcmodel & (MCMEDIUM|MCLARGE|MCHUGE)) ? 32 : 16) : \
                     ((mcmodel & (MCCOMPACT|MCLARGE|MCHUGE)) ? 32 : 16))

// arch/i386/macdefs.h
#define SZPOINT(t)  ((mcmodel & MCI386_LARGE) ? 48 : 32)
```

**Status**: ✅ Macros defined and used by frontend

### 5. **Documentation** ✅
Comprehensive documentation created:
- `arch/i86/DOS_KEYWORDS.md` (320+ lines) ✅
- `arch/i86/BORLAND_MSVC_EXTENSIONS.md` (600+ lines) ✅
- `arch/i386/I386_EXTENSIONS.md` (340+ lines) ✅
- `MEMORY_MODELS_AND_EXTENSIONS.md` (260+ lines) ✅

### 6. **Test Files** ✅
Syntax examples for all features:
- `arch/i86/test_i86_complete.c` (250+ lines) ✅
- `arch/i386/test_i386_extensions.c` (60+ lines) ✅
- `arch/amd64/test_amd64_extensions.c` (85+ lines) ✅

---

## ❌ What's MISSING (Backend/Code Generation)

### 1. **Pointer Size Code Generation** ❌

**What works**:
- Parser knows pointer should be 16/32/48 bits
- SZPOINT macro returns correct size

**What's missing**:
- Backend doesn't actually generate different code for different pointer sizes
- No special handling in register allocation
- No address calculation differences

**Example**:
```c
// i86 large model
int __far *ptr;  // Parser knows this is 32-bit
*ptr = 42;       // ❌ Backend generates same code as 16-bit pointer
```

**Files that need changes**:
- `arch/i86/local2.c` - Address mode selection
- `arch/i86/code.c` - Instruction generation
- `arch/i86/order.c` - Pattern matching

### 2. **Calling Convention Implementation** ❌

**What works**:
- Parser recognizes `__cdecl`, `__pascal`, `__fortran`, etc.
- Attributes are stored

**What's missing**:
- No actual calling convention changes in code generation
- Arguments still pushed/passed the same way
- Stack cleanup still handled the same way

**Example**:
```c
// Should reverse argument order and callee-clean stack
int __pascal PascalFunc(int a, int b, int c) {
    return a + b + c;
}

// ❌ Currently generates C calling convention code
```

**What needs implementation**:

#### `__pascal` / `__fortran`:
- [ ] Reverse argument push order (right-to-left → left-to-right)
- [ ] Callee cleans stack (add RET n instruction)
- [ ] Name mangling (uppercase for Pascal, uppercase with _ for Fortran)

#### `__syscall`:
- [ ] Pass args in registers (EAX, EDX, EBX, ECX)
- [ ] Caller cleans stack for stack args
- [ ] Specific register usage pattern

#### `__watcall`:
- [ ] Pass first 4 args in EAX, EDX, EBX, ECX
- [ ] Remaining args on stack
- [ ] Callee cleans stack

**Files that need changes**:
- `arch/i86/local2.c` - `gencall()`, `lastcall()`
- `arch/i86/code.c` - Prologue/epilogue generation

### 3. **Interrupt Handler Code Generation** ❌

**What works**:
- Parser recognizes `__interrupt`

**What's missing**:
- No IRET instruction generation
- No automatic register save/restore
- No interrupt stack frame setup

**Example**:
```c
void __interrupt timer_handler(void) {
    // Handle timer
}

// ❌ Should generate:
// - Save all registers (pusha)
// - Function body
// - Restore all registers (popa)
// - IRET (not RET)
//
// Currently generates normal RET
```

**What needs implementation**:
- [ ] Generate PUSHA at function entry
- [ ] Generate POPA before return
- [ ] Generate IRET instead of RET
- [ ] Handle interrupt number attribute
- [ ] Preserve all segment registers

**Files that need changes**:
- `arch/i86/code.c` - `prologue()`, `eoftn()`

### 4. **Function Modifier Code Generation** ❌

#### `__loadds`:
**Missing**:
- [ ] Generate `PUSH DS` / `MOV DS, SS` / `POP DS` sequence
- [ ] Save/restore DS around function body

#### `__saveregs`:
**Missing**:
- [ ] Force save/restore of ALL registers (even caller-saved)
- [ ] Override normal register usage

#### `__export`:
**Missing**:
- [ ] Generate DLL export tables
- [ ] Mark symbols as exported
- [ ] Linker integration

**Files that need changes**:
- `arch/i86/code.c` - Prologue/epilogue
- Linker integration (external)

### 5. **Segment Register Access** ❌

**What works**:
- Parser recognizes `__fs`, `__gs`, `__ss`, `__cs`, `__ds`, `__es`

**What's missing**:
- No segment override prefix generation
- No special address mode handling

**Example**:
```c
// i386 or amd64
int __fs *ptr = (int __fs *)0;
int value = *ptr;

// ❌ Should generate: mov eax, fs:[0]
// Currently generates: mov eax, [0] (no segment override)
```

**What needs implementation**:
- [ ] Detect segment-qualified pointers
- [ ] Generate segment override prefixes (0x64 for FS, 0x65 for GS)
- [ ] Handle in all addressing modes

**Files that need changes**:
- `arch/i386/local2.c` - `adrput()`, address mode functions
- `arch/amd64/local2.c` - Same as i386
- `arch/i86/local2.c` - For i86 segment registers

### 6. **`__emit` Code Generation** ❌

**What works**:
- Parser recognizes `__emit(bytes...)`
- Multiple arguments supported

**What's missing**:
- No actual byte emission
- Arguments not processed

**Example**:
```c
__emit(0x90);              // NOP
__emit(0x0F, 0x31);        // RDTSC
__emit(0xCD, 0x21);        // INT 21h

unsigned char opcode = 0x90;
__emit(opcode);            // Variable

// ❌ None of these actually emit bytes
```

**What needs implementation**:
- [ ] Process attribute arguments
- [ ] Emit raw bytes to instruction stream
- [ ] Handle constant and variable arguments
- [ ] Validate byte values (0-255)

**Files that need changes**:
- `arch/i86/code.c` - Special case in instruction emission

### 7. **MSVC `__asm` Inline Assembly** ❌

**What works**:
- Parser recognizes `__asm` keyword

**What's missing**:
- No MSVC-style assembly parsing
- Falls back to GCC asm() syntax

**Example**:
```c
// MSVC syntax
__asm {
    mov eax, 1
    mov ebx, 2
    add eax, ebx
}

// ❌ Parser doesn't handle braces and statement list
// Currently expects GCC __asm__("...") syntax
```

**What needs implementation**:
- [ ] Parse MSVC brace syntax
- [ ] Parse assembly statement list
- [ ] Convert to internal representation
- [ ] Generate assembly output

**Files that need changes**:
- `cc/ccom/scan.l` - Lexer for assembly tokens
- `cc/ccom/cgram.y` - Grammar for MSVC __asm blocks

### 8. **`__near32` for x86-64** ❌

**What works**:
- Parser recognizes `__near32`

**What's missing**:
- No 32-bit pointer generation
- No calling convention changes
- No ABI thunking

**Example**:
```c
// amd64
int __near32 legacy_api(int x, int y) {
    return x + y;
}

// ❌ Should generate:
// - 32-bit calling convention
// - 32-bit pointer handling
// - Thunk layer for 64-bit calls
//
// Currently generates normal 64-bit code
```

**What needs implementation**:
- [ ] 32-bit pointer size in 64-bit code
- [ ] 32-bit calling convention (i386 System V ABI)
- [ ] Thunk generation for 64→32 calls
- [ ] Stack alignment (16-byte vs 4-byte)
- [ ] Register usage differences

**Files that need changes**:
- `arch/amd64/local2.c` - Calling convention
- `arch/amd64/code.c` - Prologue/epilogue
- Linker support (external)

### 9. **`__far16` for i386** ❌

**What works**:
- Parser recognizes `__far16`

**What's missing**:
- No 16-bit far pointer generation
- No segment:offset handling in 32-bit code

**Example**:
```c
// i386
void __far16 *ptr = (void __far16 *)0xB8000000L;
*ptr = 'A';

// ❌ Should generate:
// - 32-bit segment:offset (16:16)
// - Far pointer operations
//
// Currently treats as 32-bit pointer
```

**What needs implementation**:
- [ ] 16-bit segment + 16-bit offset representation
- [ ] Far pointer arithmetic
- [ ] Segment manipulation

**Files that need changes**:
- `arch/i386/local2.c` - Pointer handling

---

## 📋 **Implementation Priority**

### **High Priority** (Most Impact)

1. **Pointer size code generation** (i86/i386)
   - Essential for memory model correctness
   - Affects all pointer operations

2. **Segment register access** (`__fs`, `__gs`)
   - Needed for TLS on x86-64
   - Relatively simple (just segment override prefixes)

3. **`__pascal` calling convention**
   - Common in DOS/Windows code
   - Moderate complexity

### **Medium Priority**

4. **`__interrupt` handler generation**
   - Important for embedded/DOS development
   - Self-contained feature

5. **`__emit` byte emission**
   - Useful for inline assembly
   - Simple implementation

6. **`__loadds` / `__saveregs`**
   - Less common but useful
   - Moderate complexity

### **Low Priority** (Complex/Niche)

7. **`__near32` / `__far16`**
   - Very specialized use cases
   - Requires complex thunking

8. **MSVC `__asm` syntax**
   - Large parser changes
   - Alternative (GCC asm) already works

---

## 🔧 **What Would a Complete Implementation Look Like?**

### Example: `__fs` Segment Override

**Current state**:
```c
int __fs *ptr = (int __fs *)0x28;
int value = *ptr;
```
Parser accepts this, but generates:
```asm
mov eax, [0x28]          ; Wrong - no segment override
```

**Complete implementation** would generate:
```asm
mov eax, fs:[0x28]       ; Correct - FS segment override (prefix 0x64)
```

**Implementation**:
```c
// In arch/amd64/local2.c - adrput() function
void adrput(FILE *io, NODE *p) {
    struct attr *ap;

    // Check for segment override attributes
    if ((ap = attr_find(p->n_ap, GCC_ATYP_FS)) != NULL) {
        fprintf(io, "%%fs:");  // Emit FS prefix
    } else if ((ap = attr_find(p->n_ap, GCC_ATYP_GS)) != NULL) {
        fprintf(io, "%%gs:");  // Emit GS prefix
    }

    // ... rest of address generation
}
```

### Example: `__pascal` Calling Convention

**Current state**:
```c
int __pascal Add(int a, int b, int c) {
    return a + b + c;
}

// Call: Add(1, 2, 3)
```

Generates (C calling convention):
```asm
push 3              ; Right to left
push 2
push 1
call _Add
add esp, 12         ; Caller cleans
```

**Complete implementation** would generate:
```asm
push 1              ; Left to right (reversed)
push 2
push 3
call ADD            ; Uppercase name
                    ; Callee cleans (no add esp)
```

### Example: `__interrupt` Handler

**Current state**:
```c
void __interrupt timer_isr(void) {
    // Handle timer
}
```

Generates:
```asm
_timer_isr:
    push ebp
    mov ebp, esp
    ; ... function body ...
    pop ebp
    ret                 ; Wrong - should be IRET
```

**Complete implementation** would generate:
```asm
_timer_isr:
    pusha               ; Save all registers
    push ds
    push es
    ; ... function body ...
    pop es
    pop ds
    popa                ; Restore all registers
    iret                ; Interrupt return
```

---

## 📊 **Summary: What You Have**

| Component | Frontend | Backend | Notes |
|-----------|----------|---------|-------|
| Keyword parsing | ✅ 100% | N/A | All keywords recognized |
| Attribute system | ✅ 100% | N/A | All map to attributes |
| Memory model flags | ✅ 100% | ❌ 0% | Flags set but not used |
| SZPOINT macro | ✅ 100% | ⚠️ 50% | Macro exists, partial backend use |
| Pointer types | ✅ 100% | ❌ 0% | Parsed but same codegen |
| Calling conventions | ✅ 100% | ❌ 0% | Parsed but same codegen |
| Function modifiers | ✅ 100% | ❌ 0% | Parsed but same codegen |
| Segment registers | ✅ 100% | ❌ 0% | Parsed but no override |
| `__emit` | ✅ 100% | ❌ 0% | Parsed but no emission |
| `__asm` (MSVC) | ✅ 50% | ❌ 0% | Basic recognition only |
| Documentation | ✅ 100% | N/A | Comprehensive docs |
| Test files | ✅ 100% | N/A | Syntax examples only |

**Overall**: Frontend ~95% complete, Backend ~5% complete

---

## 🚀 **Next Steps**

To make this **actually work**, you would need to:

### Phase 1: Basic Code Generation (2-3 weeks)
1. Implement segment register overrides (`__fs`, `__gs`)
2. Implement `__emit` byte emission
3. Basic pointer size handling in i86 backend

### Phase 2: Calling Conventions (2-3 weeks)
4. Implement `__pascal` calling convention
5. Implement `__cdecl` explicitly
6. Implement `__interrupt` handlers

### Phase 3: Advanced Features (3-4 weeks)
7. Complete memory model code generation
8. Implement `__loadds`, `__saveregs`
9. Implement `__watcall`, `__syscall`

### Phase 4: Complex Features (4-6 weeks)
10. MSVC `__asm` parser
11. `__near32` thunking for x86-64
12. `__far16` for i386

**Total estimated effort**: 3-4 months of development

---

## 📝 **Conclusion**

**You have built an excellent foundation!** The frontend/parser work is essentially complete and very well documented. However, the **backend code generation is entirely missing**.

**What works**: Source code with these keywords will **parse** successfully
**What doesn't work**: The generated assembly/object code **ignores the keywords**

**Analogy**: You've built a car with a beautiful dashboard and all the controls, but the controls aren't connected to the engine yet. The buttons all look right and click nicely, but pressing them doesn't actually do anything. 🚗

To make this production-ready, significant backend work is needed in the code generation phase.
