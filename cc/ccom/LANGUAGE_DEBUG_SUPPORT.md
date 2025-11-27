# Language-Specific Debug Symbol Support

## Overview

This document outlines language-specific debug symbol support across all 24 debug format families for 22+ programming languages.

## Language Categories

### Systems Programming Languages
1. **C** - Already fully supported (base implementation)
2. **C++** - Object-oriented extension of C
3. **Objective-C** - Object-oriented extension for macOS/iOS
4. **Objective-C++** - Combines C++ and Objective-C
5. **Rust** - Modern systems language
6. **Zig** - Modern systems language
7. **D** - Systems language with GC
8. **Crystal** - Compiled Ruby-like language

### Application Programming Languages
9. **Go** - Google's concurrent language
10. **Java** - JVM bytecode (special handling)
11. **Kotlin** - Modern JVM language
12. **C#** - .NET managed language
13. **Dart** - Google's UI language

### Scientific/Numeric Languages
14. **Fortran** - Numeric computation (77, 90, 95, 2003, 2008, 2018)

### Procedural/Modular Languages
15. **Pascal** - Structured programming
16. **Modula-2** - Module-based Pascal successor
17. **Modula-3** - Object-oriented Modula
18. **Oberon** - Simplified Modula-2
19. **Ada** - Military/safety-critical
20. **COBOL** - Business applications

### Functional Languages
21. **Haskell** - Pure functional, lazy evaluation
22. **OCaml** - ML-family functional language
23. **Prolog** - Logic programming

## Language Support Matrix

| Language | DWARF | CodeView/PDB | STABS | Notes |
|----------|-------|--------------|-------|-------|
| C | ✓ | ✓ | ✓ | Base implementation |
| C++ | ✓ | ✓ | ✓ | Classes, templates, namespaces |
| Objective-C | ✓ | - | ✓ | Apple platforms |
| Objective-C++ | ✓ | - | ✓ | Apple platforms |
| Fortran | ✓ | ✓ | ✓ | Array descriptors, modules |
| Pascal | ✓ | ✓ | ✓ | Sets, subranges |
| Modula-2 | ✓ | - | - | Modules, opaque types |
| Modula-3 | ✓ | - | - | Objects, GC |
| Oberon | ✓ | - | - | Type extensions |
| Ada | ✓ | - | - | Tasks, packages, generics |
| Go | ✓ | - | - | Goroutines, interfaces, slices |
| Dart | ✓ | - | - | Async, isolates |
| Rust | ✓ | ✓ | - | Traits, lifetimes, ownership |
| Crystal | ✓ | - | - | Macros, unions |
| Zig | ✓ | - | - | Comptime, error unions |
| D | ✓ | - | - | Templates, mixins |
| Haskell | ✓ | - | - | Lazy evaluation, type classes |
| OCaml | ✓ | - | - | Variants, modules |
| Prolog | - | - | - | Limited debug support |
| COBOL | ✓ | ✓ | - | Records, paragraphs |
| Java | Special | Special | - | Bytecode (not native) |
| Kotlin | Special | Special | - | Bytecode (not native) |
| C# | Special | ✓ | - | Managed code |

## DWARF Language Codes (DW_LANG_*)

```c
// Standard DWARF language codes
#define DW_LANG_C89              0x0001  // ANSI C (1989)
#define DW_LANG_C                0x0002  // Non-ANSI C
#define DW_LANG_Ada83            0x0003  // Ada 83
#define DW_LANG_C_plus_plus      0x0004  // C++
#define DW_LANG_Cobol74          0x0005  // COBOL 74
#define DW_LANG_Cobol85          0x0006  // COBOL 85
#define DW_LANG_Fortran77        0x0007  // Fortran 77
#define DW_LANG_Fortran90        0x0008  // Fortran 90
#define DW_LANG_Pascal83         0x0009  // Pascal 83
#define DW_LANG_Modula2          0x000a  // Modula-2
#define DW_LANG_Java             0x000b  // Java
#define DW_LANG_C99              0x000c  // C99
#define DW_LANG_Ada95            0x000d  // Ada 95
#define DW_LANG_Fortran95        0x000e  // Fortran 95
#define DW_LANG_PLI              0x000f  // PL/I
#define DW_LANG_ObjC             0x0010  // Objective-C
#define DW_LANG_ObjC_plus_plus   0x0011  // Objective-C++
#define DW_LANG_UPC              0x0012  // Unified Parallel C
#define DW_LANG_D                0x0013  // D
#define DW_LANG_Python           0x0014  // Python
#define DW_LANG_OpenCL           0x0015  // OpenCL
#define DW_LANG_Go               0x0016  // Go
#define DW_LANG_Modula3          0x0017  // Modula-3
#define DW_LANG_Haskell          0x0018  // Haskell
#define DW_LANG_C_plus_plus_03   0x0019  // C++03
#define DW_LANG_C_plus_plus_11   0x001a  // C++11
#define DW_LANG_OCaml            0x001b  // OCaml
#define DW_LANG_Rust             0x001c  // Rust
#define DW_LANG_C11              0x001d  // C11
#define DW_LANG_Swift            0x001e  // Swift
#define DW_LANG_Julia            0x001f  // Julia
#define DW_LANG_Dylan            0x0020  // Dylan
#define DW_LANG_C_plus_plus_14   0x0021  // C++14
#define DW_LANG_Fortran03        0x0022  // Fortran 2003
#define DW_LANG_Fortran08        0x0023  // Fortran 2008
#define DW_LANG_RenderScript     0x0024  // RenderScript
#define DW_LANG_BLISS            0x0025  // BLISS
#define DW_LANG_Kotlin           0x0026  // Kotlin
#define DW_LANG_Zig              0x0027  // Zig
#define DW_LANG_Crystal          0x0028  // Crystal
#define DW_LANG_C_plus_plus_17   0x002a  // C++17
#define DW_LANG_C_plus_plus_20   0x002b  // C++20
#define DW_LANG_C17              0x002c  // C17
#define DW_LANG_Fortran18        0x002d  // Fortran 2018
#define DW_LANG_Ada2005          0x002e  // Ada 2005
#define DW_LANG_Ada2012          0x002f  // Ada 2012
#define DW_LANG_Mojo             0x0033  // Mojo
```

## Language-Specific Features by Language

### C++ (DW_LANG_C_plus_plus*)
**Unique Features**:
- Classes (DW_TAG_class_type)
- Virtual functions and vtables
- Inheritance (DW_AT_inheritance)
- Templates (DW_TAG_template_type_parameter)
- Namespaces (DW_TAG_namespace)
- References (DW_TAG_reference_type)
- Exceptions (DW_TAG_thrown_type)
- Operator overloading
- RTTI (Run-Time Type Information)
- Move semantics (C++11+)
- Lambda expressions (C++11+)
- Constexpr functions (C++11+)

**Example Tags**:
```c
DW_TAG_class_type
DW_TAG_namespace
DW_TAG_template_type_parameter
DW_TAG_template_value_parameter
DW_TAG_inheritance
DW_TAG_friend
DW_TAG_ptr_to_member_type
```

### Fortran (DW_LANG_Fortran*)
**Unique Features**:
- Multi-dimensional arrays with non-1 lower bounds
- Array sections and slicing
- COMMON blocks (DW_TAG_common_block)
- Modules (DW_TAG_module)
- Derived types (structures)
- ALLOCATABLE arrays
- POINTER variables
- Assumed-shape arrays
- Coarrays (Fortran 2008)
- Submodules (Fortran 2008)

**Array Descriptors**:
```c
DW_TAG_array_type with:
  DW_AT_lower_bound (can be non-1)
  DW_AT_upper_bound
  DW_AT_stride
```

### Go (DW_LANG_Go)
**Unique Features**:
- Goroutines (lightweight threads)
- Channels (DW_TAG_channel_type - custom)
- Interfaces (DW_TAG_interface_type)
- Slices (dynamic arrays)
- Maps (hash tables)
- Defer statements
- Multiple return values
- Struct embedding (composition)

**Custom Extensions**:
```c
DW_TAG_string_type (Go strings)
DW_TAG_interface_type (Go interfaces)
// Custom tags for slices, maps, channels
```

### Rust (DW_LANG_Rust)
**Unique Features**:
- Ownership and borrowing
- Lifetimes (DW_AT_lifetime - custom)
- Traits (similar to interfaces)
- Enums with data (sum types)
- Pattern matching
- Option<T> and Result<T, E>
- Macros
- Unsafe blocks

**Custom Attributes**:
```c
DW_AT_rust_trait
DW_AT_rust_lifetime
DW_AT_rust_ownership
```

### Objective-C (DW_LANG_ObjC)
**Unique Features**:
- Classes (@interface)
- Protocols (DW_TAG_protocol - custom)
- Categories (DW_TAG_category - custom)
- Properties (@property)
- Message passing (selectors)
- Dynamic typing (id type)
- Blocks (closures)
- ARC (Automatic Reference Counting)

**Apple Extensions**:
```c
DW_TAG_APPLE_property
DW_TAG_APPLE_protocol
```

### Ada (DW_LANG_Ada*)
**Unique Features**:
- Tasks (concurrent programming)
- Protected types
- Packages and package bodies
- Generic units
- Discriminated records
- Variant records
- Constrained subtypes
- Access types (pointers)
- Renaming declarations

**Tags**:
```c
DW_TAG_ada_task_type
DW_TAG_ada_protected_type
DW_TAG_package
DW_TAG_generic_subprogram
```

### Haskell (DW_LANG_Haskell)
**Unique Features**:
- Lazy evaluation
- Type classes
- Higher-kinded types
- Algebraic data types
- Pattern matching
- Monads
- Currying
- List comprehensions

**Special Considerations**:
- Thunks (unevaluated expressions)
- Closure representations
- Type class dictionaries

### OCaml (DW_LANG_OCaml)
**Unique Features**:
- Variant types (sum types)
- Module system (functors)
- Polymorphic variants
- Objects (optional)
- First-class modules
- GADTs (Generalized Algebraic Data Types)

### Pascal (DW_LANG_Pascal83)
**Unique Features**:
- Sets (DW_TAG_set_type)
- Subranges (DW_TAG_subrange_type)
- Variant records
- Nested procedures
- WITH statements

### Modula-2/3 (DW_LANG_Modula2/3)
**Unique Features**:
- Modules (DEFINITION and IMPLEMENTATION)
- Opaque types
- Coroutines (Modula-2)
- Objects and methods (Modula-3)
- Garbage collection (Modula-3)

### Oberon (Custom)
**Unique Features**:
- Type extensions
- Type guards
- Module imports
- System modules

### COBOL (DW_LANG_Cobol*)
**Unique Features**:
- PICTURE clauses for data formatting
- Level numbers (01-49, 66, 77, 88)
- OCCURS clauses (arrays)
- REDEFINES (unions/overlays)
- Paragraphs and sections
- File descriptions (FD)

### D (DW_LANG_D)
**Unique Features**:
- Templates
- Mixins (compile-time code injection)
- Contracts (in/out/invariant)
- Scope guards
- Unittest blocks
- Module system
- Garbage collection

### Zig (DW_LANG_Zig)
**Unique Features**:
- Comptime (compile-time execution)
- Error unions (try/catch alternative)
- Optional types (?T)
- Packed structs
- Explicit allocators
- No hidden control flow

### Crystal (DW_LANG_Crystal)
**Unique Features**:
- Union types
- Macros (AST transformation)
- Fibers (lightweight concurrency)
- Type inference
- Nilable types

### Dart (Custom - likely uses DWARF extensions)
**Unique Features**:
- Async/await
- Isolates (actor model)
- Mixins
- Extension methods
- Null safety (Sound type system)

### Java/Kotlin (JVM Bytecode)
**Special Handling**:
- Not native code compilation
- Uses class file debug attributes:
  - LineNumberTable
  - LocalVariableTable
  - LocalVariableTypeTable
- Out of scope for native compiler

### C# (.NET Managed)
**Special Handling**:
- Managed PDB format (different from native)
- IL (Intermediate Language) not native code
- May use PDB for mixed-mode debugging

### Prolog (Logic Programming)
**Limited Support**:
- Typically interpreted
- Limited native compilation
- Debug support varies by implementation

## Implementation Strategy

### Phase 1: Core Language Support
1. Add language enumeration to debugsym.h
2. Add language-specific type encodings
3. Implement C++ extensions (classes, templates, namespaces)
4. Implement Fortran extensions (arrays, modules, COMMON)

### Phase 2: Modern Systems Languages
5. Implement Rust extensions (traits, lifetimes, ownership)
6. Implement Go extensions (goroutines, channels, interfaces)
7. Implement Zig extensions (comptime, error unions)
8. Implement D extensions (templates, mixins)
9. Implement Crystal extensions (union types, macros)

### Phase 3: Apple Ecosystem
10. Implement Objective-C extensions (protocols, categories, properties)
11. Implement Objective-C++ combined features

### Phase 4: Procedural/Modular Languages
12. Implement Pascal extensions (sets, subranges, variant records)
13. Implement Modula-2/3 extensions (modules, opaque types)
14. Implement Oberon extensions (type extensions)

### Phase 5: Scientific/Business Languages
15. Implement Ada extensions (tasks, packages, generics)
16. Implement COBOL extensions (PICTURE, level numbers, OCCURS)

### Phase 6: Functional Languages
17. Implement Haskell extensions (lazy eval, type classes)
18. Implement OCaml extensions (variants, modules, functors)

### Phase 7: Documentation
19. Update all documentation with language support matrices
20. Create language-specific examples

## File Structure

```
debugsym_lang.h          - Language enumerations and type encodings
debugsym_lang_cpp.c      - C++ specific extensions
debugsym_lang_fortran.c  - Fortran specific extensions
debugsym_lang_go.c       - Go specific extensions
debugsym_lang_rust.c     - Rust specific extensions
debugsym_lang_objc.c     - Objective-C specific extensions
debugsym_lang_ada.c      - Ada specific extensions
debugsym_lang_pascal.c   - Pascal/Modula/Oberon extensions
debugsym_lang_haskell.c  - Haskell specific extensions
debugsym_lang_ocaml.c    - OCaml specific extensions
debugsym_lang_cobol.c    - COBOL specific extensions
debugsym_lang_modern.c   - D/Zig/Crystal/Dart extensions
```

## Statistics

- **Languages Supported**: 22+
- **Native Compilation Languages**: 20
- **Bytecode/Managed Languages**: 3 (Java, Kotlin, C# - special handling)
- **New Files**: ~12
- **Estimated New Code**: ~8,000-10,000 lines
- **Language Epochs Covered**: 1950s (COBOL) to 2020s (Zig, Mojo)

## Benefits

1. **Universal Language Support**: Single debug system for all languages
2. **Cross-Language Debugging**: Debug polyglot applications
3. **Language-Aware Tools**: Better debugger integration
4. **Future-Proof**: Easy to add new languages
5. **Educational**: Reference implementation for language debug features

## Next Steps

1. Implement debugsym_lang.h with all language codes and enumerations
2. Create language-specific extension files
3. Integrate with existing format implementations
4. Add language selection to API
5. Create comprehensive tests
6. Update documentation

