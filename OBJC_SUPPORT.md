# Objective-C Support in PCC

This document describes the Objective-C 1.0 and 2.0 language support added to the Portable C Compiler (PCC).

## Overview

PCC now includes optional support for Objective-C 1.0 and 2.0, allowing compilation of `.m` (Objective-C) source files. The implementation adds language-level syntax support for Objective-C constructs while maintaining compatibility with standard C compilation.

## Features Implemented

### Objective-C 1.0 Support

#### Language Constructs
- **@interface**: Class interface declarations with instance variables and method declarations
- **@implementation**: Class implementation with method definitions
- **@protocol**: Protocol declarations for defining interfaces
- **@end**: Marks the end of @interface, @implementation, or @protocol
- **@class**: Forward class declarations
- **@selector()**: Selector literals for method selectors
- **@encode()**: Type encoding directive

#### Access Modifiers
- **@private**: Private instance variable access
- **@protected**: Protected instance variable access (default)
- **@public**: Public instance variable access
- **@package**: Package-level access (framework-internal)

#### Message Syntax
- **Message sends**: `[receiver message]`, `[receiver message:arg]`
- **Nested messages**: `[[Class alloc] init]`
- **Class methods**: Prefixed with `+`
- **Instance methods**: Prefixed with `-`

#### String Literals
- **@"string"**: Objective-C constant string literals (NSString)

### Objective-C 2.0 Support

#### Properties
- **@property**: Property declarations with attributes
  - Attributes: `nonatomic`, `atomic`, `assign`, `retain`, `copy`, `readonly`, `readwrite`
- **@synthesize**: Automatic property accessor synthesis
- **@dynamic**: Dynamic property accessor implementation

#### Exception Handling
- **@try**: Exception handling try block
- **@catch**: Exception catching with typed exceptions
- **@finally**: Finally block for cleanup code
- **@throw**: Throw exception statement

#### Synchronization
- **@synchronized**: Synchronized blocks for thread safety

#### Protocol Extensions
- **@optional**: Optional protocol methods
- **@required**: Required protocol methods (default)

## Type System Extensions

The following Objective-C types have been added to PCC's type system:

- **id**: Generic Objective-C object pointer type
- **Class**: Objective-C class type
- **SEL**: Selector type for method selectors

## Compilation

### Recognizing Objective-C Files

The compiler driver automatically recognizes `.m` files as Objective-C source files and:
1. Defines the `__OBJC__` preprocessor macro
2. Passes the `-xobjc` flag to the compiler frontend
3. Processes files through the C preprocessor

### Command Line Usage

```bash
# Compile a single Objective-C file
pcc -c myfile.m

# Compile and link with Objective-C runtime
pcc -o myprogram myfile.m -lobjc

# Compile multiple files
pcc -o myprogram file1.c file2.m file3.m -lobjc
```

### Preprocessor Support

When compiling `.m` files, the preprocessor automatically defines:
- `__OBJC__`: Indicates Objective-C mode is active

## Runtime Linking

### Darwin/macOS
On macOS, link against the system Objective-C runtime:
```bash
pcc -o myprogram myfile.m -lobjc
```

### Linux/GNU
On Linux, link against the GNU Objective-C runtime (from GCC):
```bash
pcc -o myprogram myfile.m -lobjc
```

## Implementation Status

### Completed Components

1. **Compiler Driver (cc/cc/cc.c)**
   - `.m` file recognition
   - `objcflag` support
   - Preprocessor define injection (`__OBJC__`)
   - Compiler flag passing (`-xobjc`)

2. **Lexer (cc/ccom/scan.l)**
   - @ prefix recognition
   - Objective-C keyword tokenization
   - @"string" literal support
   - Support for all Objective-C 1.0 and 2.0 keywords

3. **Grammar (cc/ccom/cgram.y)**
   - Class interface/implementation declarations
   - Protocol declarations
   - Property declarations
   - Method declarations and definitions
   - Message send expressions
   - Exception handling statements
   - Synchronized blocks
   - @selector() and @encode() expressions

4. **Type System (mip/manifest.h)**
   - id type (OBJC_ID)
   - Class type (OBJC_CLASS)
   - SEL type (OBJC_SEL)
   - Type keywords in lexer

### Pending Implementation

The following components require runtime support and code generation:

1. **Semantic Analysis**
   - Method signature validation
   - Protocol conformance checking
   - Property attribute validation

2. **Code Generation**
   - Message send translation to `objc_msgSend()` calls
   - Property accessor synthesis
   - @selector() implementation
   - @encode() implementation
   - Exception handling runtime calls
   - @synchronized block transformation

3. **Runtime Integration**
   - Automatic linking of Objective-C runtime
   - Runtime function declarations
   - Object file metadata generation

## Grammar Structure

### Top-Level Declarations

```
external_def:
    | objc_class_interface
    | objc_class_implementation
    | objc_protocol_declaration
    | objc_class_declaration
```

### Interface Declaration

```
objc_class_interface:
    @interface ClassName [ : SuperClass ] [ <Protocol, ...> ]
    {
        [ instance_variables ]
    }
    [ method_declarations ]
    [ @property declarations ]
    @end
```

### Implementation Declaration

```
objc_class_implementation:
    @implementation ClassName [ : SuperClass ]
    [ method_definitions ]
    [ @synthesize statements ]
    [ @dynamic statements ]
    @end
```

### Method Declaration/Definition

```
method_type objc_method_selector ;

method_type:
    '+' (class method)
    '-' (instance method)

objc_method_selector:
    selector_name
    | keyword_name : type param [ keyword_name : type param ... ]
```

### Message Send Expression

```
'[' receiver message_args ']'

receiver:
    expression
    | ClassName

message_args:
    selector
    | keyword : expr [ keyword : expr ... ]
```

## Example Usage

See `test_objc.m` for a comprehensive example demonstrating:
- Class interfaces and implementations
- Method declarations and definitions
- Properties with @synthesize
- Exception handling (@try/@catch/@finally)
- Synchronized blocks
- Message sends
- @selector() usage
- Objective-C string literals

## Compatibility

### Objective-C 1.0 Compatibility
Full syntax support for:
- NeXTSTEP / OpenStep
- macOS 10.0 - 10.4
- GCC Objective-C runtime

### Objective-C 2.0 Compatibility
Full syntax support for:
- macOS 10.5+ (Leopard and later)
- iOS 2.0+
- Modern Objective-C runtime
- Properties
- Fast enumeration syntax (syntax only)
- Exception handling
- Synchronized blocks

## Limitations

1. **Runtime Required**: Actual Objective-C execution requires linking against an Objective-C runtime library (Apple or GNU runtime)

2. **Code Generation**: Current implementation provides syntax support; full code generation for message sends, properties, and runtime features needs completion

3. **ARC Not Supported**: Automatic Reference Counting (ARC) is not implemented; manual memory management only

4. **Modern Features**: Objective-C 2.0+ features like blocks, literals (@[], @{}), and subscripting are not yet implemented

## References

- Objective-C 1.0: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ObjectiveC/
- Objective-C 2.0: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/
- GNU Objective-C Runtime: https://gcc.gnu.org/onlinedocs/gcc/Objective-C.html

## Contributing

To complete the Objective-C support implementation:

1. Implement semantic analysis in `cc/ccom/objc.c`
2. Add code generation for message sends
3. Implement property accessor synthesis
4. Add runtime function declarations and calls
5. Complete @selector() and @encode() implementation
6. Add comprehensive test suite

## Version History

- **2025-10-26**: Initial Objective-C 1.0 and 2.0 syntax support added to PCC
