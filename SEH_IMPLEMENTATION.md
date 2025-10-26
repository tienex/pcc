# Structured Exception Handling (SEH) Support in PCC

This document describes the Structured Exception Handling (SEH) implementation added to the Portable C Compiler (PCC).

## Overview

SEH is a Microsoft-specific extension to C that provides structured exception handling capabilities. This implementation adds syntactic and semantic support for SEH constructs across all PCC targets.

## Supported Keywords

The following SEH keywords have been added:

- `__try` - Marks the beginning of a guarded code block
- `__except(filter)` - Defines an exception handler with a filter expression
- `__finally` - Defines a termination handler that always executes
- `__leave` - Transfers control to the end of the enclosing `__try` block

## Syntax

### Try-Except Block

```c
__try {
    // Guarded code
} __except (filter_expression) {
    // Exception handler
}
```

The `filter_expression` can evaluate to:
- `EXCEPTION_EXECUTE_HANDLER` (1) - Execute the handler
- `EXCEPTION_CONTINUE_SEARCH` (0) - Continue searching for a handler
- `EXCEPTION_CONTINUE_EXECUTION` (-1) - Resume execution

### Try-Finally Block

```c
__try {
    // Guarded code
} __finally {
    // Termination handler (always executes)
}
```

### Leave Statement

```c
__try {
    if (condition) {
        __leave; // Jump to end of __try block
    }
    // More code
} __except (EXCEPTION_EXECUTE_HANDLER) {
    // Handler
}
```

## Implementation Details

### Compiler Frontend (Pass 1)

#### Node Operators (mip/node.h)
Added new IR node types:
- `TRYBLOCK` (59) - Represents a `__try` block
- `EXCEPT` (60) - Represents an `__except` clause
- `FINALLY` (61) - Represents a `__finally` clause
- `LEAVE` (62) - Represents a `__leave` statement
- `EXCEPTEXPR` (63) - Represents the exception filter expression

#### Keywords (cc/ccom/scan.l)
Added SEH keywords to the keyword table with proper token mappings.

#### Grammar Rules (cc/ccom/cgram.y)
Added production rules for:
- `tryexceptstmt` - Try-except statement parsing
- `tryfinallystmt` - Try-finally statement parsing
- `tryprefix` - Common prefix for try blocks
- `C_LEAVE` - Leave statement parsing

#### Label Management
SEH blocks use a set of labels for control flow:
- `sehtrylab` - Start of try block
- `sehexcept` - Start of except handler
- `sehfinally` - Start of finally handler
- `sehendlab` - End of SEH block
- `sehleavlab` - Target for `__leave` statements

These labels are managed via `saveseh()` and `resetseh()` functions to support nested try blocks.

### Code Generation (Pass 2)

The architecture-specific code generation for SEH is target-dependent:

#### Windows x86 (i386)
Uses Frame-based Exception Handling (FEH):
- Exception registration record on stack at FS:[0]
- Chain of exception handlers
- Stack unwinding via frame pointers

#### Windows x64 (amd64)
Uses Table-based Exception Handling:
- `.pdata` section for function table entries
- `.xdata` section for unwind information
- No runtime registration required

#### Other Targets
For non-Windows targets, SEH constructs can either:
1. Generate equivalent setjmp/longjmp code
2. Map to target-specific exception mechanisms
3. Emit a warning if unsupported

## Cross-Target Compatibility

The SEH implementation is designed to work across all PCC targets:

1. **Syntax Support**: All targets can parse SEH constructs
2. **Semantic Analysis**: Validates proper nesting and usage
3. **Code Generation**: Target-specific backends generate appropriate code

## Current Status

### Completed
- ✅ Keyword recognition (`__try`, `__except`, `__finally`, `__leave`)
- ✅ Grammar rules and parsing
- ✅ AST node types
- ✅ Label management for control flow
- ✅ Nested try block support

### Pending
- ⚠️ Architecture-specific code generation (i386, amd64)
- ⚠️ Exception registration record generation
- ⚠️ Unwind information tables
- ⚠️ Runtime library integration
- ⚠️ Filter expression evaluation
- ⚠️ Finally block execution guarantees

## Usage Example

```c
#include <windows.h>
#include <stdio.h>

int main() {
    __try {
        int *p = NULL;
        printf("About to cause an exception\n");
        *p = 42;  // Access violation
        printf("This won't execute\n");
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        printf("Exception caught!\n");
        return 1;
    }

    return 0;
}
```

## File Modifications

### Core Files Modified
- `mip/node.h` - Added SEH node operator definitions
- `cc/ccom/scan.l` - Added SEH keywords
- `cc/ccom/cgram.y` - Added SEH grammar rules and helper functions
- `cc/ccom/pftn.c` - Added SEH label variables
- `cc/ccom/pass1.h` - Added SEH extern declarations

### Architecture Files (Future Work)
- `arch/i386/code.c` - i386 SEH code generation
- `arch/amd64/code.c` - AMD64 SEH code generation
- `os/win32/*` - Windows-specific SEH support

## Testing

To test SEH support:

```bash
# Compile a program with SEH
pcc -c test_seh.c

# The compiler should accept SEH syntax without errors
```

## References

- Microsoft SEH Documentation: https://docs.microsoft.com/en-us/cpp/cpp/structured-exception-handling-c-cpp
- Windows x86 Exception Handling: Frame-based (FS:[0] chain)
- Windows x64 Exception Handling: Table-based (.pdata/.xdata)
- PCC Architecture: Multi-pass compiler with target-specific backends

## Notes

- SEH is primarily a Windows feature but can be adapted for other platforms
- The implementation allows for graceful degradation on non-Windows targets
- Full code generation will require platform-specific runtime support
- Nested try blocks are supported via the saveseh/resetseh mechanism

## Author

SEH support added: 2025-10-26
