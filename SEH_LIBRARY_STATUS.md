# SEH Library Build Status

**Date:** 2025-10-26
**Status:** ⚠️ PARTIAL - Build issues need resolution

---

## Overview

The libseh (Structured Exception Handling) runtime library is available in `/home/user/pcc/libseh/` but has build issues that need to be resolved before it can be fully integrated with the C++ compiler.

---

## Build Attempt

### Command
```bash
cd /home/user/pcc/libseh
make CC=gcc CXX=g++ clean all
```

### Issues Encountered

#### 1. Missing External Declaration ✅ FIXED

**File:** `seh_dwarf.c`
**Error:**
```
error: '_seh_current_exception' undeclared
```

**Root Cause:**
- Variable `_seh_current_exception` defined in `seh.c` (line 51)
- Used in `seh_dwarf.c` and `seh_cxx.cpp`
- No extern declaration in headers

**Fix Applied:**
- Added extern declaration to `seh_helpers.h`:
```c
#ifndef _WIN32
extern __thread struct _seh_exception_record _seh_current_exception;
#endif
```
- Added `#include "seh_helpers.h"` to `seh_dwarf.c`

#### 2. Platform-Specific Register Issues ⚠️ NEEDS FIXING

**File:** `seh_context.c`
**Errors:**
```
error: 'REG_R8' undeclared
error: 'REG_R9' undeclared
error: 'REG_R10' undeclared
error: 'REG_R11' undeclared
error: 'REG_R12' undeclared
error: 'REG_R13' undeclared
error: 'REG_R14' undeclared
error: 'REG_R15' undeclared
```

**Root Cause:**
- Code assumes Linux/glibc register name macros
- These macros are architecture and libc specific
- Not all platforms define REG_R8-REG_R15

**Affected Functions:**
- `_seh_get_register()` - lines 285-295
- `_seh_get_ip()` - line 217
- `_seh_get_sp()` - line 241

**Potential Solutions:**

1. **Add Platform Detection:**
```c
#ifdef __x86_64__
  #ifdef __linux__
    // Use REG_R8 through REG_R15
  #elif defined(__FreeBSD__)
    // Use BSD register names
  #endif
#elif defined(__i386__)
  // 32-bit x86 registers
#elif defined(__arm__) || defined(__aarch64__)
  // ARM registers
#endif
```

2. **Use Direct Offset Access:**
```c
// Instead of symbolic names, use direct array indices
// This requires knowing the layout of ucontext_t for each platform
```

3. **Conditional Compilation:**
```c
#ifndef REG_R8
  #define REG_R8 0  // Fallback value
  #warning "REG_R8 not defined, using fallback"
#endif
```

---

## Current Workaround

Since the SEH library has unresolved build issues, the compiler implementation uses **stub functions** for exception handling:

### In `cxxcode.c`:
```c
/* Stub implementations - to be completed when SEH library is ready */
void cxxtry(NODE *try_body, NODE *catch_list) {
    uerror("Exception handling requires libseh (not yet built)");
    // Generate placeholder code
}
```

### Benefits of Stub Approach:
1. ✅ Compiler continues to build
2. ✅ Code generation architecture can be designed
3. ✅ Integration points clearly defined
4. ✅ Easy to replace stubs later

---

## Modified Files

### libseh/seh_helpers.h
- Added extern declaration for `_seh_current_exception`

### libseh/seh_dwarf.c
- Added `#include "seh_helpers.h"`

These changes fix issue #1 but issue #2 remains.

---

## Next Steps

### Short Term (Current Approach):
1. ✅ Use stub implementations in compiler
2. ✅ Design and document code generation architecture
3. ✅ Define integration points with SEH library
4. ✅ Complete compiler without runtime library

### Medium Term (SEH Library Fixes):
1. ⚠️ Fix platform-specific register issues in `seh_context.c`
2. ⚠️ Add proper platform detection (#ifdef logic)
3. ⚠️ Test on multiple platforms (Linux, BSD, macOS)
4. ⚠️ Build and test complete library

### Long Term (Integration):
1. ⚠️ Replace stub implementations with real SEH calls
2. ⚠️ Link libseh.a with compiler
3. ⚠️ End-to-end testing with exception handling
4. ⚠️ Performance optimization

---

## Platform Compatibility Matrix

| Platform | Arch | Status | Notes |
|----------|------|--------|-------|
| Linux (glibc) | x86_64 | ⚠️ Partial | REG_R8-R15 issues |
| Linux (glibc) | i386 | ❓ Unknown | Not tested |
| FreeBSD | x86_64 | ❓ Unknown | Different register names |
| OpenBSD | x86_64 | ❓ Unknown | Different ucontext layout |
| macOS | x86_64 | ❓ Unknown | Darwin-specific ABI |
| macOS | aarch64 | ❓ Unknown | ARM64 registers |

---

## Code Quality Issues

### Minor Issues:
- ⚠️ Unused variables in `seh_dwarf.c` (lines 107, 109)
- ⚠️ Functions missing return statements (lines 217, 241)

### Can Be Fixed With:
```bash
# Add -Wno-unused-variable to CFLAGS temporarily
# Or fix the actual code
```

---

## Testing Status

### What Works:
- ✅ Headers compile
- ✅ seh.c compiles
- ✅ seh_cxx.cpp compiles (with fixes)
- ✅ seh_dwarf.c compiles (with fixes)

### What Doesn't Work:
- ❌ seh_context.c fails to compile
- ❌ Library cannot be built
- ❌ Cannot link with compiler

---

## Alternative Approaches

### 1. Use Existing Exception Libraries
- Link with libc++abi (LLVM)
- Link with libstdc++ (GCC)
- **Pros:** Already built and tested
- **Cons:** Not PCC-specific, license issues

### 2. Minimal Exception Support
- Implement only C++ exceptions (not full SEH)
- Use DWARF unwinding directly
- Skip signal-to-exception mapping
- **Pros:** Simpler, fewer platform issues
- **Cons:** Less compatible with Windows SEH

### 3. Defer Exception Handling
- Complete other C++ features first
- Come back to exceptions later
- **Pros:** Make progress on other fronts
- **Cons:** Incomplete C++ support

---

## Recommendation

**Current Path:** Proceed with stub implementations

**Rationale:**
1. Syntax support (Phase 5.1) is complete ✅
2. Code generation architecture can be designed without library
3. Stubs clearly show what needs to be implemented
4. SEH library can be fixed independently
5. Integration is straightforward once library works

**Action Items:**
1. ✅ Document library issues (this file)
2. ⏭️ Implement code generation stubs
3. ⏭️ Design complete architecture
4. ⏭️ Defer library fixes to future work

---

## References

- libseh README: `/home/user/pcc/libseh/README.md`
- SEH API: `/home/user/pcc/libseh/seh.h`
- C++ Interop: `/home/user/pcc/libseh/seh_cxx.cpp`

---

## Summary

The SEH library has platform-specific issues in `seh_context.c` that prevent it from building. Two files have been fixed (`seh_helpers.h` and `seh_dwarf.c`) but register name issues remain.

The compiler will proceed with stub implementations for exception handling code generation, deferring full SEH library integration until the platform issues are resolved.

**Status:** Exception handling syntax complete, code generation in progress with stubs.
