# libwirth - Unified Runtime Library for Wirth Languages and Ada

A comprehensive runtime library supporting the entire Wirth language family (Pascal, Modula-2, Modula-3, Oberon) and Ada, with full interoperability.

## Overview

libwirth provides a unified runtime environment for:
- **31 language dialects** across 5 language families
- **Full interoperability** between all supported languages
- **Automatic type conversions** for strings, arrays, and basic types
- **Unified I/O system** compatible with all language conventions
- **Exception handling** across language boundaries
- **Concurrency support** (coroutines, threads, tasks, active objects)

## Supported Languages

### Pascal Family
- ISO 7185, ISO 10206, UCSD, Microsoft, Think, Clascal, MacPascal, Borland, Delphi, Free Pascal

### Modula-2 Family
- PIM 2, PIM 3, PIM 4, ISO, GNU, R10

### Modula-3 Family
- Modula-3 (DEC SRC), Critical Mass M3, Polytechnique M3

### Oberon Family
- Oberon, Oberon-2, Oberon-07, Component Pascal, Active Oberon, A2 Oberon

### Ada Family
- Ada 83, Ada 95, Ada 2005, Ada 2012, Ada 2022, SPARK

## Features

### Type System Support
- Integer types: INTEGER, CARDINAL, LONGINT (16/32/64-bit)
- Real types: REAL, LONGREAL, EXTENDED
- String types: Pascal string, ARRAY OF CHAR, TEXT, Ada String
- Pointer types: POINTER, REF, access types
- Set types: Pascal SET, Modula-2 BITSET
- Record/Object types: with type extension support

### I/O Operations
- Text I/O compatible with all languages
- Binary file I/O
- Standard input/output/error
- Formatted output with field widths

### String Operations
- Automatic conversion between all string types
- Pascal short/long strings
- Modula-2/Oberon ARRAY OF CHAR
- Modula-3 TEXT (reference-counted)
- Ada String (constrained arrays)
- Unicode/wide string support

### Memory Management
- Traced allocation (garbage-collected for M3)
- Untraced allocation (manual management)
- Compatible with all language semantics

### Concurrency
- **Coroutines** (Modula-2)
- **PROCESS types** (Modula-2)
- **Threads** (Modula-3, Ada tasks)
- **Active Objects** (Active Oberon)
- **Synchronization** (mutexes, conditions, semaphores)

### Exception Handling
- Unified exception system
- TRY/EXCEPT (Modula-3, Component Pascal)
- Ada exception propagation
- Signal handling

## API Reference

See `wirthrt.h` for the complete API.

### Example Usage

#### Pascal
```pascal
uses WirthRT;

var
  s: String;
begin
  WriteLn('Hello from Pascal');
  s := 'Test string';
end.
```

#### Modula-2
```modula2
MODULE Example;
FROM WirthRT IMPORT WriteInt, WriteLn;

BEGIN
  WriteInt(42, 10);
  WriteLn
END Example.
```

#### Oberon
```oberon
MODULE Example;
IMPORT Out := WirthRT;

BEGIN
  Out.Int(42, 0);
  Out.Ln
END Example.
```

#### Ada
```ada
with WirthRT;

procedure Example is
begin
  WirthRT.Put_Line("Hello from Ada");
end Example;
```

## Building

```bash
cd libwirth
make
sudo make install
```

## Linking

Link with `-lwirth` and `-lm`:

```bash
pcc myprogram.pas -lwirth -lm
```

## Interoperability Examples

### Calling Modula-2 from Pascal
```pascal
// Pascal
program Main;
{$IMPORT MyModule}  // Modula-2 module

begin
  MyModule.DoSomething;
end.
```

### Calling Pascal from Oberon
```oberon
MODULE Main;
IMPORT PascalUnit;

BEGIN
  PascalUnit.MyProcedure
END Main.
```

## Implementation Status

| Feature | Status |
|---------|--------|
| Core runtime | ✓ Complete |
| I/O operations | ✓ Complete |
| String conversions | ✓ Complete |
| Memory management | ✓ Complete |
| Exception handling | ✓ Complete |
| Set operations | ✓ Complete |
| Mathematical functions | ✓ Complete |
| Coroutines | ⚠ Partial |
| Threads/Tasks | ⚠ Partial |
| Garbage collection | ⚠ Planned |

## Thread Safety

The current implementation is not thread-safe. Thread-safe variants are planned for:
- Modula-3 programs using threads
- Ada programs using tasks
- Active Oberon programs

## Performance

libwirth is designed for:
- **Low overhead** - Minimal abstraction cost
- **Efficient string conversions** - Lazy copying where possible
- **Stack-based allocation** - For temporary conversions
- **Inline-friendly** - Small functions can be inlined

## License

Part of the Portable C Compiler (PCC) project.
See the main PCC LICENSE file for details.

## See Also

- PCC Pascal Compiler: `../pascal/README.md`
- Wirth Languages Guide: `../pascal/WIRTH_LANGUAGES.md`
- Individual language documentation in `../pascal/docs/`
