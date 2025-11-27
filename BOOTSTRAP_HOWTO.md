# Multi-Stage Bootstrap Build System

PCC now includes GCC-style integrated bootstrap support through the `--enable-bootstrap` configure option.

## Overview

The bootstrap build system allows PCC to build itself in multiple stages to verify correctness and reproducibility:

- **Stage 1**: Build PCC using the system compiler (GCC, Clang, etc.)
- **Stage 2**: Build PCC using the Stage 1 compiler
- **Stage 3**: Build PCC using the Stage 2 compiler
- **Comparison**: Verify that Stage 2 and Stage 3 produce identical binaries

## Configure Options

### `--enable-bootstrap[=MODE]`

Enable multi-stage bootstrap build. Modes:

- **`yes`** (default when flag is used): 3-stage bootstrap with Stage 2/3 comparison
- **`lean`**: 2-stage bootstrap without comparison (faster)
- **`no`** (default): Single-stage build

### `--disable-bootstrap`

Explicitly disable bootstrap even if enabled by default.

## Usage Examples

### Full Bootstrap with Verification

```bash
./configure --enable-bootstrap
make
make install
```

This will:
1. Build Stage 1 with system compiler
2. Build Stage 2 with Stage 1 compiler
3. Build Stage 3 with Stage 2 compiler
4. Compare Stage 2 and Stage 3 binaries
5. Install from Stage 3

### Lean Bootstrap (Faster)

```bash
./configure --enable-bootstrap=lean
make
make install
```

This performs a 2-stage bootstrap without comparison, which is faster but less thorough.

### Normal Build (No Bootstrap)

```bash
./configure
make
make install
```

Without `--enable-bootstrap`, this performs a standard single-stage build.

## Make Targets

When `--enable-bootstrap` is used, `make` and `make install` automatically run the bootstrap process. You can also use these targets explicitly:

- `make bootstrap-build` - Run the multi-stage bootstrap
- `make bootstrap-install` - Install from the final bootstrap stage
- `make bootstrap-clean` - Clean bootstrap build directories
- `make bootstrap-stage1` - Build only Stage 1
- `make bootstrap-stage2` - Build only Stage 2
- `make bootstrap-stage3` - Build only Stage 3
- `make bootstrap-compare` - Compare Stage 2 and Stage 3

## Legacy Bootstrap Script

The `bootstrap.sh` script is still available for more control:

```bash
# 3-stage with comparison (equivalent to --enable-bootstrap)
./bootstrap.sh --stages=3 --compare-stages

# 2-stage without comparison (equivalent to --enable-bootstrap=lean)
./bootstrap.sh --stages=2

# Via make targets
make bootstrap        # 3-stage with comparison
make bootstrap-quick  # 2-stage
make bootstrap-full   # 3-stage with comparison
```

## Build Directories

Bootstrap builds use `.bootstrap/` in the source directory:

- `.bootstrap/stage1/` - Stage 1 build and installation
- `.bootstrap/stage2/` - Stage 2 build and installation
- `.bootstrap/stage3/` - Stage 3 build and installation

These are automatically ignored by git.

## Bootstrap Comparison

When enabled, the bootstrap comparison verifies that:

1. Stage 2 and Stage 3 compilers are byte-for-byte identical
2. The compiler can reliably reproduce itself
3. There are no hidden dependencies on the host compiler

If comparison fails, it indicates a potential bug in the compiler.

## Performance Notes

- **Full bootstrap**: ~3x build time (builds compiler 3 times)
- **Lean bootstrap**: ~2x build time (builds compiler 2 times)
- **Normal build**: 1x build time

For development, use normal builds. For releases and verification, use full bootstrap.

## Cross-Compilation

Bootstrap is currently supported only for native builds. For cross-compilation, use the normal build process without `--enable-bootstrap`.

## Troubleshooting

### Bootstrap comparison fails

This means Stage 2 and Stage 3 produce different binaries. Possible causes:
- Compiler bug producing non-deterministic output
- Timestamp or path dependencies in binaries
- Different optimization behaviors

### Stage N build fails

Check the build log in `.bootstrap/stageN/`. Common issues:
- Missing dependencies
- System header compatibility
- Code generation bugs in previous stage

## Integration with CI/CD

Example GitHub Actions workflow:

```yaml
- name: Configure with bootstrap
  run: ./configure --enable-bootstrap

- name: Build
  run: make

- name: Install
  run: sudo make install
```

The bootstrap will run automatically and fail the build if comparison fails.
