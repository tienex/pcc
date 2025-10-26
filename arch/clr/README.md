# PCC CLR Backend - Microsoft Common Language Runtime Support

## Overview

This backend enables the Portable C Compiler (PCC) to generate **Microsoft CIL/MSIL** (Common Intermediate Language) code for the **.NET Framework** and **Mono** runtime environments. It brings C++/CLI-inspired features to C and Fortran, allowing these languages to leverage the .NET Framework's extensive runtime capabilities.

## Features

### Core Functionality

- **ILASM Code Generation**: Generates human-readable IL Assembly (.il files) that can be assembled with `ilasm.exe`
- **Full Type System Mapping**: Maps C/Fortran types to CLR types (int32, int64, float32, float64, etc.)
- **Stack-Based Architecture**: Optimized for CLR's stack-based virtual machine
- **Automatic Memory Management**: Integration with CLR's garbage collector
- **Cross-Language Interoperability**: Generated IL can be linked with C#, VB.NET, F#, and other .NET languages

### C++/CLI-Inspired Features

The CLR backend introduces several features inspired by C++/CLI to enable C and Fortran code to take full advantage of the .NET Framework runtime:

#### 1. **P/Invoke (Platform Invocation Services)**

Allows calling native C libraries from managed CLR code:

```c
// The backend will generate appropriate P/Invoke stubs
extern void native_function(int x);  // Automatically marshaled
```

Generated IL includes:
```il
.method public static pinvokeimpl("nativelibrary.dll")
    void native_function(int32 x)
{
}
```

#### 2. **Boxing and Unboxing**

Value types can be automatically boxed to object references when needed:

- **Boxing**: Converts value types (int, float, struct) to `System.Object`
- **Unboxing**: Extracts value types from object references
- Enables use of .NET collections and interfaces with C/Fortran types

#### 3. **Exception Handling**

Maps C/Fortran error handling to .NET exception model:

```c
// Traditional C error handling can be mapped to try/catch
int result = risky_operation();
if (result < 0) {
    // Backend can optionally generate throw instruction
}
```

Generated IL supports:
- `.try` / `catch` / `finally` blocks
- Exception propagation through .NET stack
- Integration with .NET exception hierarchy

#### 4. **Array Support**

.NET array features accessible from C/Fortran:

- **Safe arrays**: CLR performs bounds checking
- **Multidimensional arrays**: Native .NET array support
- **Array methods**: Access to `Length`, `Clone`, etc.

```c
int arr[100];  // Mapped to int32[] in CLR
// Generates: newarr int32
```

#### 5. **Delegates and Function Pointers**

Function pointers map to .NET delegates:

```c
typedef int (*callback_t)(int);
callback_t cb = &my_function;  // Creates delegate
```

Generated IL creates delegate types for type-safe callbacks.

#### 6. **Reference Parameters**

Pass-by-reference semantics using CLR managed references:

```c
void swap(int *a, int *b);  // Can use & syntax in IL for ref params
```

#### 7. **Garbage Collection Integration**

- Automatic memory management via CLR GC
- Manual GC control through runtime calls:
  ```c
  // Backend provides intrinsics for GC control
  __clr_gc_collect();      // Force garbage collection
  __clr_gc_suppress(ptr);  // Suppress finalization
  ```

#### 8. **Value Types and Reference Types**

Structs can be emitted as either:

- **Value Types** (.NET `struct`): Stack-allocated, passed by value
- **Reference Types** (.NET `class`): Heap-allocated, garbage collected

```c
struct Point { int x, y; };  // Emits as value type
// .class public sequential ansi sealed beforefieldinit
//        Point extends [mscorlib]System.ValueType
```

#### 9. **Interoperability Attributes**

Support for .NET marshaling attributes:

- `[MarshalAs]` for P/Invoke parameter marshaling
- `[ThreadStatic]` for thread-local storage
- `[DllImport]` for native library imports
- Custom attributes for metadata

#### 10. **Mixed-Mode Assembly**

Generated code can be mixed with managed C#/VB.NET code in the same assembly, enabling:

- C/Fortran business logic with .NET UI
- Integration with .NET libraries (WinForms, WPF, ASP.NET)
- Use of Entity Framework, LINQ, etc. from C code

## Architecture

The backend consists of 6 main files:

1. **macdefs.h**: Type definitions and constants for CLR
2. **code.c**: Function prologue/epilogue and IL structure generation
3. **local.c**: Symbol handling and external declarations (Pass 1)
4. **local2.c**: Stack management and optimization (Pass 2)
5. **table.c**: Instruction selection table (IR → IL mapping)
6. **order.c**: Expression evaluation order and cost metrics

## Usage

### Building with CLR Target

```bash
# Configure PCC for CLR target
./configure --target=i386-clr-win32

# Or for 64-bit CLR
./configure --target=x86_64-clr-win64

# Build
make

# Compile C code to IL
./pcc -target clr myprogram.c -o myprogram.il

# Assemble IL to executable
ilasm myprogram.il
```

### Platform Support

- **Windows**: Native support with .NET Framework or .NET Core
- **Linux/macOS**: Via Mono runtime
- **Cross-compilation**: Can generate IL on any platform for CLR

### Runtime Requirements

- .NET Framework 4.0 or later
- OR Mono 5.0 or later
- OR .NET Core / .NET 5+ runtime

## Generated IL Structure

Example C code:
```c
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    return factorial(5);
}
```

Generated IL:
```il
.assembly extern mscorlib { }
.assembly PCCOutput { .ver 1:0:0:0 }
.module PCCOutput.exe

.namespace PCC
{
    .class public abstract sealed auto ansi beforefieldinit
           GeneratedCode extends [mscorlib]System.Object
    {
        .method public static int32 factorial(int32 n) cil managed
        {
            .maxstack 8
            ldarg.0
            ldc.i4.1
            cgt
            brfalse.s IL_L1
            ldarg.0
            ldarg.0
            ldc.i4.1
            sub
            call int32 PCC.GeneratedCode::factorial(int32)
            mul
            ret
        IL_L1:
            ldc.i4.1
            ret
        }

        .method public static int32 Main() cil managed
        {
            .entrypoint
            .maxstack 8
            ldc.i4.5
            call int32 PCC.GeneratedCode::factorial(int32)
            ret
        }
    }
}
```

## Advanced Features

### Custom Attributes

```c
// Attributes can be added via pragmas or special syntax
#pragma clr attribute("Serializable")
struct Data {
    int value;
};
```

### Thread-Local Storage

```c
// Thread-static variables
#pragma clr threadstatic
static int thread_counter;
```

### Unsafe Code

For direct memory manipulation (mapped to CLR unsafe context):

```c
// Pinning for P/Invoke or direct memory access
void* ptr = __clr_pin(&my_struct);
// ... use pointer ...
__clr_unpin(ptr);
```

### Reflection and Metadata

Generated assemblies have full metadata, allowing:

- Reflection from other .NET languages
- Runtime type inspection
- Dynamic invocation
- Serialization

## Limitations

1. **Inline Assembly**: Not supported (CLR has no inline assembly)
2. **Direct Hardware Access**: Must use P/Invoke to native code
3. **Signal Handling**: Must use .NET event model instead
4. **Memory Layout**: Some C tricks with memory layout may not work due to CLR's managed memory

## Performance Considerations

- **JIT Compilation**: Code is JIT-compiled at runtime (initial startup cost)
- **Garbage Collection**: Periodic GC pauses
- **Boxing Overhead**: Value type boxing has performance cost
- **Optimization**: Use NGen or AOT compilation for better startup time

## Integration with .NET Ecosystem

### Calling .NET Libraries from C

```c
// String operations via .NET
extern void System_Console_WriteLine(const char* str);
// Backend generates: call void [mscorlib]System.Console::WriteLine(string)

void my_func() {
    System_Console_WriteLine("Hello from C!");
}
```

### Collections

```c
// Use .NET List<T> instead of manual arrays
extern void* System_Collections_Generic_List_int_Create();
extern void System_Collections_Generic_List_int_Add(void* list, int item);
// Generates proper IL for generic collections
```

### LINQ Integration

While LINQ is primarily for C#/VB.NET, generated IL can be consumed by LINQ queries in other .NET languages.

## Future Enhancements

Planned features:

- [ ] Generic type support from C templates
- [ ] Async/await mapping for async functions
- [ ] LINQ provider for C
- [ ] Automatic interface generation
- [ ] COM interop support
- [ ] UWP/Windows Runtime support
- [ ] Xamarin mobile support

## Examples

See `examples/clr/` directory for:

- Hello World in C targeting CLR
- P/Invoke examples
- Mixed C and C# projects
- .NET library integration
- Fortran with CLR backend

## References

- [ECMA-335: Common Language Infrastructure (CLI)](https://www.ecma-international.org/publications-and-standards/standards/ecma-335/)
- [Microsoft CIL Instruction Set](https://docs.microsoft.com/en-us/dotnet/api/system.reflection.emit.opcodes)
- [Mono Project](https://www.mono-project.com/)
- [C++/CLI Language Specification](https://docs.microsoft.com/en-us/cpp/dotnet/)

## License

Same as PCC (BSD-style license). See COPYING file.

## Authors

PCC CLR Backend Team
- Initial implementation: 2025

## Contributing

Contributions welcome! Areas needing work:

- Fortran-specific .NET integration
- Optimization passes for stack-based IL
- Better exception handling mapping
- Generic type support
- Additional .NET library bindings

Contact: pcc@lists.ludd.ltu.se
