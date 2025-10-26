# PCC Multi-Stage Bootstrap - Current Status

## Summary

Multi-stage bootstrapping infrastructure for PCC has been implemented with support for native, cross, and Canadian cross builds. The build system has been reorganized to support incremental compilation stages.

## ✅ Completed

### 1. Bootstrap Infrastructure (100%)
- **bootstrap.sh script** (450+ lines)
  - Supports 1-10 stage builds
  - Native, cross, and Canadian cross modes
  - Automatic build system detection
  - Stage comparison for reproducibility
  - Parallel builds with configurable job count
  - Colored output and progress logging

### 2. Build System Organization (100%)
- **Stage separation**:
  - Stage 0: Minimal C compiler only (common/abi + cc/{cc,cpp,ccom})
  - Stage 1+: All languages (C, C++, Pascal, F77)

- **New Makefile targets**:
  - `make all-c install-c clean-c` - C compiler only
  - `make all install clean` - C + C++ (standard)
  - `make all-full install-full clean-full` - All languages
  - `make bootstrap` - 3-stage with verification
  - `make bootstrap-quick` - Fast 2-stage build

### 3. Autoconf Improvements (100%)
- Fixed obsolete macro warnings:
  - `AC_CONFIG_HEADER` → `AC_CONFIG_HEADERS`
  - `AC_PROG_LEX` → `AC_PROG_LEX([noyywrap])`
- Added `AC_PROG_RANLIB` for library support
- Configure generates with zero warnings

### 4. Compilation Fixes (100%)
- Fixed `aliasmap()` return type conflict (int → long)
- Fixed `P1ND` forward declaration issues
- Fixed `lineno` multiple definition errors
- Fixed `pragma_aux_info` struct redefinitions
- Added missing `#include <stdarg.h>` to Pascal compiler
- PCC C compiler builds successfully

## ⚠️ Known Issues

### Runtime Library Linking (Blocks Stage 1+)

**Problem**: Stage 0 PCC cannot link executables because it's looking for:
1. `crtbegin.o` / `crtend.o` - GCC C runtime files
2. `libpcc.a` - PCC runtime library

**Error**:
```
ld: cannot find crtbegin.o: No such file or directory
ld: cannot find -lpcc: No such file or directory
```

**Root Cause**:
- PCC driver (cc/driver/) hardcodes these files but doesn't know where to find them
- GCC has these files in `/usr/lib/gcc/x86_64-linux-gnu/13/`
- PCC needs to either:
  1. Build its own runtime library
  2. Find and use GCC's runtime files
  3. Skip these files for bootstrap

**Impact**:
- Stage 0 compiler builds successfully
- Stage 0 compiler cannot compile/link programs
- Bootstrap cannot proceed to Stage 1

**Files Involved**:
- `cc/driver/platform.c` - Defines crt file lists
- `cc/driver/driver.c` - Linker command construction

## Current Bootstrap Status

### Stage 0: ✅ Builds Successfully
```bash
$ ls -lh /tmp/pcc-test/stage0/
bin/pcc         44K   # PCC driver
libexec/ccom   491K   # C compiler
libexec/cpp     56K   # Preprocessor
```

**What Works**:
- Compilation to assembly (.s files)
- Assembly to object (.o files)

**What Doesn't Work**:
- Linking executables (missing runtime libraries)

### Stage 1: ❌ Blocked
Configure fails with "C compiler cannot create executables" because
Stage 0 PCC cannot link test programs.

### Stage 2: ❌ Not Reached
Cannot proceed without Stage 1.

## Solutions Being Considered

### Option 1: Build PCC Runtime Library
Create a minimal runtime library with necessary startup code.
- **Pros**: Self-contained, no external dependencies
- **Cons**: Requires implementing C runtime (non-trivial)

### Option 2: Use GCC Runtime Files
Modify driver to find and use GCC's crtbegin.o/crtend.o.
- **Pros**: Leverages existing infrastructure
- **Cons**: Dependency on GCC installation

### Option 3: Make Runtime Files Optional
Allow PCC to work without crtbegin.o for bootstrap.
- **Pros**: Quick fix for bootstrap
- **Cons**: May break certain features

### Option 4: Build Minimal libpcc
Create stub libpcc.a with essential functions.
- **Pros**: Minimal implementation
- **Cons**: Still requires solving crt*.o issue

## Testing Status

### What's Been Tested
- ✅ Configure script generation (no warnings)
- ✅ Stage 0 C compiler builds
- ✅ Makefile target separation
- ✅ Bootstrap script stage handling
- ❌ Stage 0 compiler linking (fails)
- ❌ Stage 1 configuration (blocked)
- ❌ Multi-stage bootstrap (blocked)

### Test Commands
```bash
# Build stage 0 (WORKS)
./configure
make all-c

# Test stage 0 compiler (FAILS at linking)
echo 'int main() { return 0; }' | /tmp/pcc-test/stage0/bin/pcc -x c -

# Run bootstrap (FAILS at stage 1 configure)
./bootstrap.sh --stages=2
```

## Next Steps

1. **Immediate**: Fix runtime library linking for Stage 0
   - Investigate driver.c linker command construction
   - Determine best approach (Options 1-4 above)
   - Implement solution

2. **Testing**: Verify Stage 1 builds with Stage 0 compiler
   - Test C-only builds
   - Test full builds (C, C++, Pascal, F77)

3. **Verification**: Complete 3-stage bootstrap
   - Stage 0 → Stage 1 → Stage 2
   - Compare Stage 1 and Stage 2 binaries
   - Verify reproducibility

4. **Documentation**: Update BOOTSTRAP.md with findings
   - Document runtime library requirements
   - Add troubleshooting section
   - Update examples

## Files Changed

### New Files
- `bootstrap.sh` - Multi-stage bootstrap script
- `BOOTSTRAP.md` - Bootstrap documentation
- `BOOTSTRAP_TEST_RESULTS.md` - Test results
- `BOOTSTRAP_STATUS.md` - This file
- `examples/bootstrap-*.sh` - Example scripts
- `examples/README.md` - Examples documentation

### Modified Files
- `configure.ac` → `configure` - Autoconf improvements
- `Makefile.in` - Stage separation, new targets
- `cc/Makefile.in` - C vs C++ separation
- `cc/ccom/pass1.h` - Type fixes
- `cc/ccom/scan.l` - Duplicate definition fixes
- `cc/cxxcom/pass1.h` - Type fixes
- `cc/cxxcom/scan.l` - Duplicate definition fixes
- `mip/pass2.h` - Return type fixes
- `pascal/pascal/pascal.c` - Header fixes
- `config.guess` - Made executable
- `config.sub` - Made executable

## Commits

1. **79ae472** - Add multi-stage bootstrap support
2. **5b7f6f9** - Fix PCC compilation errors + AC_PROG_RANLIB
3. **148f51b** - Improve bootstrap: separate C-only and full builds

## References

- GCC Bootstrap: https://gcc.gnu.org/install/build.html
- Autoconf Manual: https://www.gnu.org/software/autoconf/manual/
- PCC Mailing List: pcc@lists.ludd.ltu.se

---

**Last Updated**: October 26, 2025
**Status**: Stage 0 builds, Stage 1 blocked on runtime libraries
