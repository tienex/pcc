# PCC Pascal Compiler

A multi-dialect Pascal compiler frontend for the Portable C Compiler (PCC) framework.

## Features

- **Multiple Dialect Support**: Compile code written for various Pascal implementations
- **Clang-style Error Reporting**: Clear, colorful diagnostics with source context
- **Comprehensive Language Support**: From strict ISO Pascal to modern Object Pascal
- **Integration with PCC Backend**: Generates optimized code for multiple architectures

## Supported Dialects

### 1. ISO 7185 Standard Pascal
The original Pascal standard from 1983.
```bash
pascal -d iso program.pas
```

Features:
- Strict type checking
- Basic procedural programming
- No extensions

### 2. ISO 10206 Extended Pascal
Enhanced version of Standard Pascal.
```bash
pascal -d extended program.pas
```

Additional features:
- String type
- Conformant arrays
- Module system
- Enhanced I/O

### 3. Microsoft Pascal 4.0
Microsoft's Pascal implementation from the 1980s.
```bash
pascal -d microsoft program.pas
```

Extensions:
- Inline assembly
- Far/near pointers
- C-style string literals
- Hexadecimal constants ($FF)

### 4. Clascal (Classic Object Pascal)
Apple's Object Pascal for classic Macintosh.
```bash
pascal -d clascal program.pas
```

Features:
- Object-oriented programming (pre-class objects)
- Mac Toolbox integration support
- Unit system

### 5. MacPascal / MPW Pascal
Apple's Macintosh Programmer's Workshop Pascal.
```bash
pascal -d macpascal program.pas
```

Extensions:
- Objects
- Inline functions
- Extended floating-point types

### 6. Borland Pascal / Turbo Pascal 7.0
The most popular DOS Pascal compiler.
```bash
pascal -d borland program.pas
```

Features:
- Units
- Objects (pre-class OOP)
- Inline assembly
- String type
- Break/continue statements
- C++ style comments (//)

### 7. Delphi Object Pascal
Modern Object Pascal with full OOP support.
```bash
pascal -d delphi program.pas
```

Advanced features:
- Classes with inheritance
- Interfaces
- Properties
- Operator overloading
- Generic programming
- Dynamic arrays
- 64-bit integers
- Currency type
- Resourcestrings
- Namespaces

### 8. Free Pascal
Compatible with Turbo Pascal and Delphi.
```bash
pascal -d freepascal program.pas
```

Comprehensive feature set:
- All Delphi features
- Generic programming
- Advanced type inference
- Modern syntax extensions

## Usage

### Basic Compilation

```bash
# Compile a Pascal program
pascal program.pas

# Specify output file
pascal -o myprogram program.pas

# Compile to object file only
pascal -c program.pas

# Compile to assembly only
pascal -S program.pas
```

### Dialect Selection

```bash
# Use specific dialect
pascal -d delphi myapp.pas

# ISO Pascal (default)
pascal -d iso classic.pas

# Borland/Turbo Pascal
pascal -d borland legacy.pas
```

### Warning Control

```bash
# Enable all warnings
pascal -Wall program.pas

# Enable specific warnings
pascal -Wunused -Wstrict program.pas

# Suppress all warnings
pascal -w program.pas

# Warn about non-standard extensions
pascal -Wextensions program.pas
```

### Error Reporting Options

```bash
# Disable colored output
pascal -fno-color program.pas

# Disable caret diagnostics
pascal -fno-caret program.pas

# Set error limit
pascal -ferror-limit=50 program.pas

# Verbose output
pascal -v program.pas
```

## Error Reporting

The Pascal compiler uses clang-style diagnostics for clear, helpful error messages:

```
program.pas:15:12: error: type mismatch in assignment
    count := 'invalid';
             ^~~~~~~~
program.pas:10:5: note: 'count' declared here
    count: integer;
    ^
```

Features:
- Source location with line and column
- Source context display
- Caret pointing to error location
- Color-coded severity levels (error/warning/note)
- Related location notes
- Automatic terminal color detection

## Architecture

```
pascal/
├── pascal/          # Driver program (orchestrates compilation)
│   └── pascal.c     # Command-line interface and build pipeline
├── pcom/            # Pascal compiler proper (frontend)
│   ├── main.c       # Entry point
│   ├── scan.l       # Lexical analyzer (Flex)
│   ├── pgram.y      # Parser grammar (Bison/Yacc)
│   ├── error.c/h    # Clang-style error reporting
│   ├── dialect.c/h  # Dialect configuration
│   ├── symtab.c     # Symbol table management
│   ├── types.c      # Type system
│   ├── builtins.c   # Built-in functions/procedures
│   └── pass1.h      # Frontend data structures
└── tests/           # Test programs for each dialect
```

## Compilation Pipeline

```
Source.pas
    ↓
[pcom] Pascal Compiler Frontend
    ↓ (generates intermediate representation)
[PCC Backend] Code Generation
    ↓ (generates assembly)
[as] Assembler
    ↓ (generates object file)
[ld] Linker
    ↓
Executable
```

## Built-in Functions and Procedures

### Standard I/O
- `read`, `readln`, `write`, `writeln`
- `reset`, `rewrite`, `get`, `put`
- `eof`, `eoln`, `page`

### Mathematical
- `abs`, `sqr`, `sqrt`, `sin`, `cos`, `exp`, `ln`, `arctan`
- `round`, `trunc`

### Ordinal Operations
- `odd`, `ord`, `chr`, `succ`, `pred`

### String Functions (dialect-dependent)
- `length`, `concat`, `copy`, `pos`
- `upcase`, `lowercase` (Borland/Delphi)
- `trim`, `insert`, `delete` (Delphi)

### Memory Management
- `new`, `dispose`, `sizeof`

### Type Conversion
- `int`, `real`, `str`, `val`

### Extended Functions (Borland/Delphi/FreePascal)
- `inc`, `dec`
- `exit`, `break`, `continue`
- `fillchar`, `move`
- `hi`, `lo`, `swap`

## Examples

### Hello World (ISO Pascal)
```pascal
program HelloWorld;
begin
  writeln('Hello, World!');
end.
```

### Fibonacci (Borland Pascal)
```pascal
program Fibonacci;

function Fib(n: integer): integer;
begin
  if n <= 1 then
    Fib := n
  else
    Fib := Fib(n-1) + Fib(n-2);
end;

var
  i: integer;

begin
  for i := 0 to 10 do
    writeln('Fib(', i, ') = ', Fib(i));
end.
```

### Object-Oriented (Delphi)
```pascal
type
  TAnimal = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    procedure Speak; virtual; abstract;
    property Name: string read FName;
  end;

  TDog = class(TAnimal)
  public
    procedure Speak; override;
  end;

implementation

constructor TAnimal.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

procedure TDog.Speak;
begin
  WriteLn(Name, ' says: Woof!');
end;
```

## Building from Source

```bash
# Configure
./configure

# Build
make

# Install
make install
```

## Testing

```bash
# Test ISO Pascal
pascal -d iso tests/hello_iso.pas
./a.out

# Test Borland Pascal
pascal -d borland tests/hello_borland.pas

# Test Delphi
pascal -d delphi tests/hello_delphi.pas

# Test FreePascal
pascal -d freepascal tests/hello_freepascal.pas
```

## Compatibility Notes

### ISO Pascal
- Strict conformance to ISO 7185
- Requires `program` header
- No extensions allowed
- Limited to basic Pascal features

### Borland Pascal
- Compatible with Turbo Pascal 7.0
- Units from Turbo Pascal may need adaptation
- Some platform-specific features not supported

### Delphi
- Supports Delphi language features
- VCL/FMX components not included
- RTL may need separate implementation

### Free Pascal
- Most language features supported
- Some compiler directives may differ
- RTL compatibility ongoing

## License

Part of the Portable C Compiler project.
See COPYING for license information.

## Contributing

Contributions are welcome! Areas for improvement:
- Additional dialect support
- Extended RTL implementation
- Optimization improvements
- Bug fixes and testing

## References

- ISO 7185:1990 - Programming languages — Pascal
- ISO 10206:1990 - Extended Pascal
- Borland Turbo Pascal 7.0 documentation
- Delphi Language Guide
- Free Pascal documentation
