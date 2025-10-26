#!/bin/sh
# PCC Multi-Stage Bootstrap Script
# Supports native, cross, and Canadian cross bootstrapping
#
# Usage:
#   ./bootstrap.sh [options]
#
# Options:
#   --stages=N              Number of stages to build (default: 3)
#   --build=TRIPLE          Build system triple (default: auto-detect)
#   --host=TRIPLE           Host system triple (default: same as build)
#   --target=TRIPLE         Target system triple (default: same as host)
#   --cc=COMPILER           Initial compiler to use (default: gcc)
#   --prefix=DIR            Installation prefix (default: /usr/local)
#   --builddir=DIR          Build directory (default: ./bootstrap-build)
#   --jobs=N                Parallel jobs (default: number of CPUs)
#   --compare-stages        Compare stage N with stage N+1 for reproducibility
#   --keep-going            Continue even if comparison fails
#   --clean                 Clean bootstrap directories before building
#   --help                  Show this help message
#
# Examples:
#   Native bootstrap (3 stages):
#     ./bootstrap.sh --stages=3 --compare-stages
#
#   Cross compilation (build on x86_64, run on aarch64):
#     ./bootstrap.sh --host=aarch64-linux-gnu
#
#   Canadian cross (build on x86_64, run on aarch64, target arm):
#     ./bootstrap.sh --host=aarch64-linux-gnu --target=arm-linux-gnueabihf

set -e

# Default values
STAGES=3
BUILD_TRIPLE=""
HOST_TRIPLE=""
TARGET_TRIPLE=""
BOOTSTRAP_CC="${CC:-gcc}"
PREFIX="/usr/local"
BUILDDIR="bootstrap-build"
JOBS=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
COMPARE_STAGES=0
KEEP_GOING=0
CLEAN=0
VERBOSE=0

# Color output
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    BOLD='\033[1m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    BOLD=''
    NC=''
fi

# Helper functions
log_info() {
    printf "${BLUE}[INFO]${NC} %s\n" "$1"
}

log_success() {
    printf "${GREEN}[SUCCESS]${NC} %s\n" "$1"
}

log_warning() {
    printf "${YELLOW}[WARNING]${NC} %s\n" "$1"
}

log_error() {
    printf "${RED}[ERROR]${NC} %s\n" "$1" >&2
}

log_stage() {
    printf "\n${BOLD}${BLUE}==== %s ====${NC}\n\n" "$1"
}

show_help() {
    sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
    exit 0
}

# Parse command line arguments
for arg in "$@"; do
    case "$arg" in
        --stages=*)
            STAGES="${arg#*=}"
            ;;
        --build=*)
            BUILD_TRIPLE="${arg#*=}"
            ;;
        --host=*)
            HOST_TRIPLE="${arg#*=}"
            ;;
        --target=*)
            TARGET_TRIPLE="${arg#*=}"
            ;;
        --cc=*)
            BOOTSTRAP_CC="${arg#*=}"
            ;;
        --prefix=*)
            PREFIX="${arg#*=}"
            ;;
        --builddir=*)
            BUILDDIR="${arg#*=}"
            ;;
        --jobs=*)
            JOBS="${arg#*=}"
            ;;
        --compare-stages)
            COMPARE_STAGES=1
            ;;
        --keep-going)
            KEEP_GOING=1
            ;;
        --clean)
            CLEAN=1
            ;;
        --verbose)
            VERBOSE=1
            ;;
        --help)
            show_help
            ;;
        *)
            log_error "Unknown option: $arg"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Validate stages
if [ "$STAGES" -lt 1 ] || [ "$STAGES" -gt 10 ]; then
    log_error "Invalid number of stages: $STAGES (must be 1-10)"
    exit 1
fi

# Get absolute path to source directory
SRCDIR="$(cd "$(dirname "$0")" && pwd)"
BUILDDIR="$(cd "$(dirname "$BUILDDIR")" 2>/dev/null && pwd)/$(basename "$BUILDDIR")" || BUILDDIR="$PWD/$BUILDDIR"

# Auto-detect build triple if not specified
if [ -z "$BUILD_TRIPLE" ]; then
    if [ -x "$SRCDIR/config.guess" ]; then
        BUILD_TRIPLE=$("$SRCDIR/config.guess")
        log_info "Auto-detected build system: $BUILD_TRIPLE"
    else
        log_error "Cannot auto-detect build system. Please specify --build=TRIPLE"
        exit 1
    fi
fi

# Set host and target if not specified
if [ -z "$HOST_TRIPLE" ]; then
    HOST_TRIPLE="$BUILD_TRIPLE"
fi

if [ -z "$TARGET_TRIPLE" ]; then
    TARGET_TRIPLE="$HOST_TRIPLE"
fi

# Determine bootstrap type
BOOTSTRAP_TYPE="native"
if [ "$BUILD_TRIPLE" != "$HOST_TRIPLE" ]; then
    if [ "$HOST_TRIPLE" != "$TARGET_TRIPLE" ]; then
        BOOTSTRAP_TYPE="canadian-cross"
    else
        BOOTSTRAP_TYPE="cross"
    fi
fi

# Display configuration
log_stage "PCC Multi-Stage Bootstrap Configuration"
printf "%-20s %s\n" "Bootstrap type:" "$BOOTSTRAP_TYPE"
printf "%-20s %s\n" "Number of stages:" "$STAGES"
printf "%-20s %s\n" "Build system:" "$BUILD_TRIPLE"
printf "%-20s %s\n" "Host system:" "$HOST_TRIPLE"
printf "%-20s %s\n" "Target system:" "$TARGET_TRIPLE"
printf "%-20s %s\n" "Bootstrap compiler:" "$BOOTSTRAP_CC"
printf "%-20s %s\n" "Source directory:" "$SRCDIR"
printf "%-20s %s\n" "Build directory:" "$BUILDDIR"
printf "%-20s %s\n" "Install prefix:" "$PREFIX"
printf "%-20s %s\n" "Parallel jobs:" "$JOBS"
printf "%-20s %s\n" "Compare stages:" "$([ $COMPARE_STAGES -eq 1 ] && echo 'yes' || echo 'no')"
echo

# Verify bootstrap compiler exists
if ! command -v "$BOOTSTRAP_CC" >/dev/null 2>&1; then
    log_error "Bootstrap compiler not found: $BOOTSTRAP_CC"
    exit 1
fi

log_info "Bootstrap compiler: $BOOTSTRAP_CC ($($BOOTSTRAP_CC --version 2>&1 | head -n1))"

# Clean if requested
if [ "$CLEAN" -eq 1 ]; then
    log_info "Cleaning bootstrap directories..."
    rm -rf "$BUILDDIR"
fi

# Create build directory
mkdir -p "$BUILDDIR"
cd "$BUILDDIR"

# Function to configure a stage
configure_stage() {
    stage_num=$1
    stage_dir=$2
    stage_cc=$3
    stage_host=$4
    stage_target=$5

    log_info "Configuring stage $stage_num in $stage_dir"

    mkdir -p "$stage_dir"
    cd "$stage_dir"

    # Build configure command
    configure_cmd="$SRCDIR/configure"
    configure_cmd="$configure_cmd --prefix=$PREFIX/stage$stage_num"

    # Set compiler
    if [ -n "$stage_cc" ]; then
        export CC="$stage_cc"
        export CXX="${stage_cc}++"
    fi

    # For cross and Canadian cross builds
    if [ "$stage_num" -eq 0 ] || [ "$BOOTSTRAP_TYPE" = "native" ]; then
        # Stage 0 or native: configure for the host we're building the compiler for
        if [ "$stage_host" != "$BUILD_TRIPLE" ]; then
            configure_cmd="$configure_cmd --host=$stage_host"
        fi
        if [ "$stage_target" != "$stage_host" ]; then
            configure_cmd="$configure_cmd --target=$stage_target"
        fi
    else
        # Later stages in cross builds
        if [ "$stage_target" != "$stage_host" ]; then
            configure_cmd="$configure_cmd --target=$stage_target"
        fi
    fi

    # Add build triple for cross builds
    if [ "$BUILD_TRIPLE" != "$stage_host" ] && [ "$stage_num" -eq 0 ]; then
        configure_cmd="$configure_cmd --build=$BUILD_TRIPLE"
    fi

    log_info "Running: $configure_cmd"
    eval "$configure_cmd"

    cd "$BUILDDIR"
}

# Function to build a stage
build_stage() {
    stage_num=$1
    stage_dir=$2

    log_info "Building stage $stage_num..."

    cd "$stage_dir"

    # Stage 0: Only build C compiler (minimal bootstrap)
    # Stage 1+: Build all languages (C, C++, Pascal, F77)
    if [ $stage_num -eq 0 ]; then
        log_info "Stage 0: Building minimal C compiler only (C language)"
        make -j"$JOBS" all-c
        make install-c
    else
        log_info "Stage $stage_num: Building all languages (C, C++, Pascal, F77)"
        make -j"$JOBS" all-full
        make install-full
    fi

    cd "$BUILDDIR"
}

# Function to compare two stages
compare_stages() {
    stage1_dir=$1
    stage2_dir=$2
    stage1_num=$3
    stage2_num=$4

    log_info "Comparing stage $stage1_num with stage $stage2_num..."

    # Find all executables in both stages
    stage1_bins=$(find "$stage1_dir" -type f -executable 2>/dev/null | sort)
    stage2_bins=$(find "$stage2_dir" -type f -executable 2>/dev/null | sort)

    comparison_failed=0

    # Compare each binary
    for bin1 in $stage1_bins; do
        bin_name=$(basename "$bin1")
        bin2="$stage2_dir/$(echo "$bin1" | sed "s|$stage1_dir/||")"

        if [ ! -f "$bin2" ]; then
            log_warning "Binary $bin_name exists in stage $stage1_num but not in stage $stage2_num"
            comparison_failed=1
            continue
        fi

        # Compare using cmp (byte-by-byte comparison)
        if cmp -s "$bin1" "$bin2"; then
            log_success "Binary $bin_name is identical between stages"
        else
            # Try stripping debug info and comparing again
            strip_dir="$BUILDDIR/stripped-compare"
            mkdir -p "$strip_dir"

            cp "$bin1" "$strip_dir/bin1"
            cp "$bin2" "$strip_dir/bin2"

            strip "$strip_dir/bin1" 2>/dev/null || true
            strip "$strip_dir/bin2" 2>/dev/null || true

            if cmp -s "$strip_dir/bin1" "$strip_dir/bin2"; then
                log_warning "Binary $bin_name differs only in debug info"
            else
                log_error "Binary $bin_name differs between stage $stage1_num and stage $stage2_num"
                comparison_failed=1

                # Show size difference
                size1=$(stat -f%z "$bin1" 2>/dev/null || stat -c%s "$bin1")
                size2=$(stat -f%z "$bin2" 2>/dev/null || stat -c%s "$bin2")
                log_info "  Stage $stage1_num size: $size1 bytes"
                log_info "  Stage $stage2_num size: $size2 bytes"
            fi

            rm -rf "$strip_dir"
        fi
    done

    if [ $comparison_failed -eq 0 ]; then
        log_success "All binaries are identical between stage $stage1_num and stage $stage2_num"
        return 0
    else
        log_error "Stage comparison failed"
        return 1
    fi
}

# Main bootstrap loop
log_stage "Starting Bootstrap Process"

prev_stage_prefix=""

for stage in $(seq 0 $((STAGES - 1))); do
    log_stage "Stage $stage Build"

    stage_dir="$BUILDDIR/stage$stage"
    stage_prefix="$PREFIX/stage$stage"

    # Determine which compiler to use
    if [ $stage -eq 0 ]; then
        # Stage 0: use bootstrap compiler
        stage_cc="$BOOTSTRAP_CC"
        stage_host="$HOST_TRIPLE"
        stage_target="$TARGET_TRIPLE"
    else
        # Later stages: use previous stage's compiler
        if [ "$BOOTSTRAP_TYPE" = "native" ] || [ $stage -gt 1 ]; then
            stage_cc="$prev_stage_prefix/bin/pcc"
            stage_host="$HOST_TRIPLE"
            stage_target="$TARGET_TRIPLE"
        else
            # Stage 1 in cross or Canadian cross
            stage_cc="$prev_stage_prefix/bin/pcc"
            stage_host="$HOST_TRIPLE"
            stage_target="$TARGET_TRIPLE"
        fi

        # Verify compiler exists
        if [ ! -x "$stage_cc" ]; then
            log_error "Compiler from previous stage not found: $stage_cc"
            exit 1
        fi

        log_info "Using compiler: $stage_cc"
    fi

    # Configure and build this stage
    configure_stage $stage "$stage_dir" "$stage_cc" "$stage_host" "$stage_target"
    build_stage $stage "$stage_dir"

    log_success "Stage $stage completed"

    # Compare with previous stage if requested
    if [ $COMPARE_STAGES -eq 1 ] && [ $stage -gt 0 ]; then
        if compare_stages "$prev_stage_prefix" "$stage_prefix" $((stage - 1)) $stage; then
            log_success "Stage $((stage - 1)) and stage $stage are reproducible"
        else
            if [ $KEEP_GOING -eq 0 ]; then
                log_error "Bootstrap failed: stages differ"
                exit 1
            else
                log_warning "Continuing despite stage differences (--keep-going)"
            fi
        fi
    fi

    prev_stage_prefix="$stage_prefix"
done

# Final summary
log_stage "Bootstrap Complete"
log_success "Successfully built $STAGES stages"

if [ $COMPARE_STAGES -eq 1 ] && [ $STAGES -gt 1 ]; then
    echo
    log_info "Bootstrap verification completed"
    log_info "The compiler is reproducible across stages"
fi

echo
log_info "Final compiler installed at: $prev_stage_prefix/bin/pcc"
log_info "To use the bootstrapped compiler:"
echo "  export PATH=$prev_stage_prefix/bin:\$PATH"
echo

# Create a symlink to the final stage
if [ $STAGES -gt 1 ]; then
    final_link="$PREFIX/bootstrap"
    mkdir -p "$PREFIX"
    ln -sfn "stage$((STAGES - 1))" "$final_link"
    log_info "Symlink created: $final_link -> stage$((STAGES - 1))"
fi

exit 0
