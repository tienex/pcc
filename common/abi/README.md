# PCC ABI Library

Generic Application Binary Interface (ABI) library for cross-language and cross-platform compatibility.

## Overview

The PCC ABI library provides a uniform interface for handling:
- **Name mangling** for C++, Pascal Object, and other OOP languages
- **Class/struct layout** with proper field ordering, padding, and alignment
- **Virtual table (vtable)** construction and management
- **Calling conventions** across different platforms
- **Type size and alignment** calculations

## Supported ABIs

### 1. Itanium C++ ABI
**Used by**: GCC, Clang (Unix/Linux/macOS), most Unix compilers

**Features**:
- Standard name mangling scheme (`_Z...`)
- Virtual base support
- RTTI (Run-Time Type Information)
- Empty base optimization
- Primary base class optimization

**Example mangled names**:
```
void foo(int)                     -> _Z3fooi
std::string::length()             -> _ZNSs6lengthEv
Class::method(int, float) const   -> _ZNK5Class6methodEif
```

### 2. Microsoft Visual C++ ABI
**Used by**: MSVC on Windows, Clang with `-fms-compatibility`

**Features**:
- Different mangling scheme (`?...`)
- `__thiscall` calling convention for methods
- Multiple vftables for multiple inheritance
- Virtual base tables (vbtable)
- Different RTTI structure

**Example mangled names**:
```
void foo(int)                     -> ?foo@@YAXH@Z
Class::method(int)                -> ?method@Class@@QAEXH@Z
Class::~Class()                   -> ??1Class@@QAE@XZ
```

### 3. Watcom C++ ABI
**Used by**: Watcom C/C++, Open Watcom

**Features**:
- Register-based calling conventions
- Simplified mangling (`W?...`)
- Efficient code generation for DOS/OS2
- Support for 16-bit and 32-bit modes

**Example mangled names**:
```
void foo(int)                     -> _foo
Class::method(int)                -> W?method$5_Class$i
```

## API Usage

### Basic Initialization

```c
#include "abi.h"

/* Initialize ABI context */
abi_context_t *ctx = abi_init(ABI_ITANIUM);

/* Or auto-detect based on target */
abi_kind_t abi = abi_detect("x86_64-pc-linux-gnu");
ctx = abi_init(abi);
```

### Name Mangling

```c
/* Create a function descriptor */
abi_function_t *func = abi_create_function("myFunction");
func->return_type = abi_create_type(ABI_TYPE_INT);

/* Add parameters */
abi_param_t *param = calloc(1, sizeof(abi_param_t));
param->name = "x";
param->type = abi_create_type(ABI_TYPE_DOUBLE);
abi_add_param(func, param);

/* Mangle the name */
char *mangled = abi_mangle_function(ctx, func);
printf("Mangled: %s\n", mangled);

/* Result (Itanium): _Z10myFunctiond */
/* Result (MSVC):    ?myFunction@@YAHN@Z */
```

### Class Layout

```c
/* Create a class */
abi_class_t *cls = abi_create_class("MyClass");

/* Add fields */
abi_field_t *field1 = calloc(1, sizeof(abi_field_t));
field1->name = "x";
field1->type = abi_create_type(ABI_TYPE_INT);
abi_add_field(cls, field1);

abi_field_t *field2 = calloc(1, sizeof(abi_field_t));
field2->name = "y";
field2->type = abi_create_type(ABI_TYPE_DOUBLE);
abi_add_field(cls, field2);

/* Layout the class */
abi_layout_class(ctx, cls);

printf("Class size: %zu bytes\n", cls->size);
printf("Alignment: %zu bytes\n", cls->alignment);
printf("Field x offset: %zu\n", field1->offset);
printf("Field y offset: %zu\n", field2->offset);
```

### Virtual Table Management

```c
/* Create a class with virtual functions */
abi_class_t *cls = abi_create_class("Base");
cls->has_vtable = 1;

/* Add virtual function */
abi_virtual_func_t *vfunc = calloc(1, sizeof(abi_virtual_func_t));
vfunc->name = "virtual_method";
vfunc->func = abi_create_function("virtual_method");
abi_add_virtual(cls, vfunc);

/* Build vtable */
void **vtable = abi_build_vtable(ctx, cls);
size_t vtable_size = abi_vtable_size(ctx, cls);

printf("Vtable size: %zu entries\n", vtable_size);
```

### Inheritance

```c
/* Create base class */
abi_class_t *base = abi_create_class("Base");
abi_layout_class(ctx, base);

/* Create derived class */
abi_class_t *derived = abi_create_class("Derived");

/* Add base class */
abi_base_t *base_desc = calloc(1, sizeof(abi_base_t));
base_desc->base_class = base;
base_desc->is_virtual = 0;  /* Non-virtual inheritance */
base_desc->is_primary = 1;  /* Primary base */
abi_add_base(derived, base_desc);

/* Layout derived class */
abi_layout_class(ctx, derived);

printf("Base offset in derived: %zu\n", base_desc->offset);
```

## Cross-Language Integration

### C++ and Pascal Object

```c
/* In C++ compiler (cxxcom) */
abi_context_t *cpp_ctx = abi_init(ABI_ITANIUM);

/* In Pascal compiler (pcom) for Object Pascal/Delphi */
abi_context_t *pas_ctx = abi_init(ABI_ITANIUM);  /* Use same ABI */

/* Both can interoperate if using same ABI */
if (abi_can_interoperate(ABI_ITANIUM, ABI_ITANIUM, func)) {
    /* Call from Pascal to C++ or vice versa */
}
```

### Example: Shared Class Definition

**C++ (cxxcom)**:
```cpp
class Animal {
public:
    virtual void speak() = 0;
    int age;
};
```

**Pascal (pcom)**:
```pascal
type
  Animal = class
    age: integer;
    procedure speak; virtual; abstract;
  end;
```

Both compile to the same binary layout when using the same ABI!

## Compatibility Matrix

| Source ABI | Target ABI | Compatible? | Notes |
|------------|------------|-------------|-------|
| Itanium    | Itanium    | ✅ Yes      | Full compatibility |
| Itanium    | ARM        | ✅ Yes      | ARM is Itanium variant |
| Itanium    | MSVC       | ❌ No       | Different mangling & layout |
| Itanium    | Watcom     | ❌ No       | Different mangling & layout |
| MSVC       | MSVC       | ✅ Yes      | Full compatibility |
| MSVC       | Watcom     | ❌ No       | Different mangling |
| Watcom     | Watcom     | ✅ Yes      | Full compatibility |

## ABI Comparison

### Name Mangling

| Language Feature | Itanium | MSVC | Watcom |
|------------------|---------|------|--------|
| `void foo(int)` | `_Z3fooi` | `?foo@@YAXH@Z` | `_foo` |
| `Class::method()` | `_ZN5Class6methodEv` | `?method@Class@@QAEXXZ` | `W?method$5_Class$` |
| Constructor | `_ZN5ClassC1Ev` | `??0Class@@QAE@XZ` | `W?$ct$5_Class$` |
| Destructor | `_ZN5ClassD1Ev` | `??1Class@@QAE@XZ` | `W?$dt$5_Class$` |
| Vtable | `_ZTV5Class` | `??_7Class@@6B@` | `W?$vft$5_Class` |
| RTTI | `_ZTI5Class` | `??_R4Class@@6B@` | `W?$rtti$5_Class` |

### Class Layout

**Example class**:
```cpp
class Example {
    int a;
    double b;
    virtual void f();
};
```

**Itanium Layout**:
```
Offset  Content
0       vtable pointer (8 bytes)
8       int a (4 bytes)
12      [padding] (4 bytes)
16      double b (8 bytes)
Total: 24 bytes
```

**MSVC Layout** (similar):
```
Offset  Content
0       vtable pointer (8 bytes)
8       int a (4 bytes)
12      [padding] (4 bytes)
16      double b (8 bytes)
Total: 24 bytes
```

## Advanced Features

### Virtual Inheritance

```c
/* Diamond inheritance problem */
abi_class_t *top = abi_create_class("Top");
abi_class_t *left = abi_create_class("Left");
abi_class_t *right = abi_create_class("Right");
abi_class_t *bottom = abi_create_class("Bottom");

/* Left and Right virtually inherit from Top */
abi_base_t *left_base = calloc(1, sizeof(abi_base_t));
left_base->base_class = top;
left_base->is_virtual = 1;  /* Virtual inheritance */
abi_add_base(left, left_base);

abi_base_t *right_base = calloc(1, sizeof(abi_base_t));
right_base->base_class = top;
right_base->is_virtual = 1;
abi_add_base(right, right_base);

/* Bottom inherits from both */
abi_base_t *bottom_left = calloc(1, sizeof(abi_base_t));
bottom_left->base_class = left;
abi_add_base(bottom, bottom_left);

abi_base_t *bottom_right = calloc(1, sizeof(abi_base_t));
bottom_right->base_class = right;
abi_add_base(bottom, bottom_right);

/* Layout resolves virtual base only once */
abi_layout_class(ctx, bottom);
```

### Calling Conventions

```c
abi_function_t *func = abi_create_function("callback");

/* Set calling convention */
func->calling_conv = ABI_CC_STDCALL;  /* Windows stdcall */
/* or */
func->calling_conv = ABI_CC_CDECL;    /* C calling convention */
/* or */
func->calling_conv = ABI_CC_THISCALL; /* C++ method (MSVC) */

char *mangled = abi_mangle_function(ctx, func);
```

## Debug Output

```c
/* Dump class layout */
abi_dump_class(ctx, cls, stdout);

/* Output:
 * class MyClass {
 *   size: 24 bytes
 *   alignment: 8 bytes
 *   mangled: _ZN7MyClassE
 *
 *   Fields:
 *     x at offset 0
 *     y at offset 8
 * }
 */

/* Dump vtable */
abi_dump_vtable(ctx, cls, stdout);

/* Output:
 * vtable for MyClass {
 *   [0] speak
 *   [1] move
 * }
 */
```

## Integration with Compilers

### C++ Compiler (cxxcom)

```c
#include "abi.h"

/* Initialize ABI based on target */
abi_context_t *abi_ctx = abi_init(abi_detect(target_triple));

/* When compiling a class */
abi_class_t *cls = abi_create_class(class_name);
/* ... add members ... */
abi_layout_class(abi_ctx, cls);

/* Generate mangled names */
char *vtable_name = abi_mangle_vtable(abi_ctx, cls);
emit_symbol(vtable_name);
```

### Pascal Compiler (pcom)

```c
#include "abi.h"

/* Initialize ABI (use same as C++ for interop) */
abi_context_t *abi_ctx = abi_init(ABI_ITANIUM);

/* When compiling Object Pascal class */
abi_class_t *cls = abi_create_class(class_name);
/* ... add fields and methods ... */
abi_layout_class(abi_ctx, cls);

/* Generate C++-compatible mangled names */
char *method_name = abi_mangle_function(abi_ctx, method);
```

## Building

```bash
make
```

This produces `libpccabi.a` which can be linked with any frontend.

## Testing

See `tests/` directory for comprehensive test cases:
- `test_mangling.c` - Name mangling tests for all ABIs
- `test_layout.c` - Class layout tests
- `test_interop.c` - Cross-language compatibility tests

## References

- [Itanium C++ ABI Specification](https://itanium-cxx-abi.github.io/cxx-abi/)
- [MSVC Name Mangling](https://en.wikiversity.org/wiki/Visual_C%2B%2B_name_mangling)
- [ARM C++ ABI](https://github.com/ARM-software/abi-aa)

## License

Part of the Portable C Compiler project. See COPYING for details.
