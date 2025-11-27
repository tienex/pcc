#!/bin/bash
# Multi-Architecture Bootstrap Testing Script for PCC
# Tests: MinGW, Wine, i386, and multi-arch configurations

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_DIR="${TEST_DIR:-$SCRIPT_DIR/test-multiarch}"
LOG_DIR="$TEST_DIR/logs"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results tracking
declare -A TEST_RESULTS

log() {
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $*"
}

log_error() {
    echo -e "${RED}[✗]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[!]${NC} $*"
}

check_tool() {
    local tool=$1
    local package=$2
    if command -v "$tool" &> /dev/null; then
        log_success "$tool found"
        return 0
    else
        log_warning "$tool not found (install: $package)"
        return 1
    fi
}

check_dependencies() {
    log "Checking dependencies..."
    
    local all_ok=true
    
    # MinGW compilers
    check_tool "x86_64-w64-mingw32-gcc" "gcc-mingw-w64-x86-64" || all_ok=false
    check_tool "i686-w64-mingw32-gcc" "gcc-mingw-w64-i686" || all_ok=false
    
    # Wine
    check_tool "wine" "wine" || all_ok=false
    check_tool "wine64" "wine64" || all_ok=false
    
    # Multilib for i386
    if [ -f /usr/lib/gcc/x86_64-linux-gnu/*/32/libgcc.a ]; then
        log_success "gcc-multilib support found"
    else
        log_warning "gcc-multilib not found (install: gcc-multilib g++-multilib)"
        all_ok=false
    fi
    
    if [ "$all_ok" = false ]; then
        log ""
        log "To install all dependencies on Ubuntu/Debian:"
        echo "  sudo dpkg --add-architecture i386"
        echo "  sudo apt-get update"
        echo "  sudo apt-get install gcc-mingw-w64 g++-mingw-w64 wine wine32 wine64 gcc-multilib g++-multilib"
        log ""
        read -p "Continue anyway? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
}

test_mingw_x86_64() {
    log "========================================"
    log "Testing MinGW x86_64 Cross-Compilation"
    log "========================================"
    
    local build_dir="$TEST_DIR/mingw-x86_64"
    local log_file="$LOG_DIR/mingw-x86_64.log"
    
    mkdir -p "$build_dir" "$LOG_DIR"
    cd "$build_dir"
    
    log "Configuring for MinGW x86_64..."
    if CC=x86_64-w64-mingw32-gcc \
       AR=x86_64-w64-mingw32-ar \
       RANLIB=x86_64-w64-mingw32-ranlib \
       "$SCRIPT_DIR/configure" \
       --host=x86_64-w64-mingw32 \
       --prefix="$build_dir/install" \
       > "$log_file" 2>&1; then
        log_success "Configure succeeded"
    else
        log_error "Configure failed (see $log_file)"
        TEST_RESULTS[mingw_x86_64_configure]="FAIL"
        return 1
    fi
    TEST_RESULTS[mingw_x86_64_configure]="PASS"
    
    log "Building..."
    if make -j$(nproc) >> "$log_file" 2>&1; then
        log_success "Build succeeded"
        TEST_RESULTS[mingw_x86_64_build]="PASS"
    else
        log_error "Build failed (see $log_file)"
        TEST_RESULTS[mingw_x86_64_build]="FAIL"
        return 1
    fi
    
    # Test with Wine
    if command -v wine64 &> /dev/null; then
        log "Testing with Wine64..."
        cat > test.c << 'EOF'
#include <stdio.h>
int main(void) {
    printf("Hello from MinGW x86_64!\n");
    return 0;
}
EOF
        if ./cc/cc/cc.exe -o test.exe test.c >> "$log_file" 2>&1; then
            log_success "Compilation with pcc.exe succeeded"
            if wine64 test.exe >> "$log_file" 2>&1; then
                log_success "Wine execution succeeded"
                TEST_RESULTS[mingw_x86_64_wine]="PASS"
            else
                log_error "Wine execution failed"
                TEST_RESULTS[mingw_x86_64_wine]="FAIL"
            fi
        else
            log_error "Compilation with pcc.exe failed"
            TEST_RESULTS[mingw_x86_64_wine]="FAIL"
        fi
    else
        log_warning "Wine64 not available, skipping runtime test"
        TEST_RESULTS[mingw_x86_64_wine]="SKIP"
    fi
}

test_mingw_i686() {
    log "========================================"
    log "Testing MinGW i686 Cross-Compilation"
    log "========================================"
    
    local build_dir="$TEST_DIR/mingw-i686"
    local log_file="$LOG_DIR/mingw-i686.log"
    
    mkdir -p "$build_dir" "$LOG_DIR"
    cd "$build_dir"
    
    log "Configuring for MinGW i686..."
    if CC=i686-w64-mingw32-gcc \
       AR=i686-w64-mingw32-ar \
       RANLIB=i686-w64-mingw32-ranlib \
       "$SCRIPT_DIR/configure" \
       --host=i686-w64-mingw32 \
       --prefix="$build_dir/install" \
       > "$log_file" 2>&1; then
        log_success "Configure succeeded"
        TEST_RESULTS[mingw_i686_configure]="PASS"
    else
        log_error "Configure failed (see $log_file)"
        TEST_RESULTS[mingw_i686_configure]="FAIL"
        return 1
    fi
    
    log "Building..."
    if make -j$(nproc) >> "$log_file" 2>&1; then
        log_success "Build succeeded"
        TEST_RESULTS[mingw_i686_build]="PASS"
    else
        log_error "Build failed (see $log_file)"
        TEST_RESULTS[mingw_i686_build]="FAIL"
        return 1
    fi
    
    # Test with Wine
    if command -v wine &> /dev/null; then
        log "Testing with Wine32..."
        cat > test.c << 'EOF'
#include <stdio.h>
int main(void) {
    printf("Hello from MinGW i686!\n");
    return 0;
}
EOF
        if ./cc/cc/cc.exe -o test.exe test.c >> "$log_file" 2>&1; then
            log_success "Compilation with pcc.exe succeeded"
            if wine test.exe >> "$log_file" 2>&1; then
                log_success "Wine execution succeeded"
                TEST_RESULTS[mingw_i686_wine]="PASS"
            else
                log_error "Wine execution failed"
                TEST_RESULTS[mingw_i686_wine]="FAIL"
            fi
        else
            log_error "Compilation with pcc.exe failed"
            TEST_RESULTS[mingw_i686_wine]="FAIL"
        fi
    else
        log_warning "Wine not available, skipping runtime test"
        TEST_RESULTS[mingw_i686_wine]="SKIP"
    fi
}

test_i386_native() {
    log "========================================"
    log "Testing i386 Native Build (32-bit)"
    log "========================================"
    
    local build_dir="$TEST_DIR/i386-native"
    local log_file="$LOG_DIR/i386-native.log"
    
    mkdir -p "$build_dir" "$LOG_DIR"
    cd "$build_dir"
    
    log "Configuring for i386..."
    if CFLAGS="-m32" \
       LDFLAGS="-m32" \
       "$SCRIPT_DIR/configure" \
       --build=i686-pc-linux-gnu \
       --prefix="$build_dir/install" \
       > "$log_file" 2>&1; then
        log_success "Configure succeeded"
        TEST_RESULTS[i386_configure]="PASS"
    else
        log_error "Configure failed (see $log_file)"
        TEST_RESULTS[i386_configure]="FAIL"
        return 1
    fi
    
    log "Building..."
    if make -j$(nproc) >> "$log_file" 2>&1; then
        log_success "Build succeeded"
        TEST_RESULTS[i386_build]="PASS"
    else
        log_error "Build failed (see $log_file)"
        TEST_RESULTS[i386_build]="FAIL"
        return 1
    fi
    
    log "Testing 32-bit executable..."
    cat > test.c << 'EOF'
#include <stdio.h>
int main(void) {
    printf("Hello from i386!\n");
    printf("sizeof(void*) = %zu\n", sizeof(void*));
    return 0;
}
EOF
    if ./cc/cc/cc -o test test.c >> "$log_file" 2>&1; then
        log_success "Compilation succeeded"
        if file test | grep -q "32-bit"; then
            log_success "Binary is 32-bit"
            if ./test >> "$log_file" 2>&1; then
                log_success "Execution succeeded"
                TEST_RESULTS[i386_runtime]="PASS"
            else
                log_error "Execution failed"
                TEST_RESULTS[i386_runtime]="FAIL"
            fi
        else
            log_error "Binary is not 32-bit!"
            TEST_RESULTS[i386_runtime]="FAIL"
        fi
    else
        log_error "Compilation failed"
        TEST_RESULTS[i386_runtime]="FAIL"
    fi
}

test_multiarch() {
    log "========================================"
    log "Testing Multi-Arch Configuration"
    log "========================================"
    
    local build_dir="$TEST_DIR/multiarch"
    local log_file="$LOG_DIR/multiarch.log"
    
    mkdir -p "$build_dir" "$LOG_DIR"
    cd "$build_dir"
    
    log "Configuring with multiarch support..."
    if "$SCRIPT_DIR/configure" \
       --enable-multiarch \
       --prefix="$build_dir/install" \
       > "$log_file" 2>&1; then
        log_success "Configure succeeded"
        TEST_RESULTS[multiarch_configure]="PASS"
    else
        log_error "Configure failed (see $log_file)"
        TEST_RESULTS[multiarch_configure]="FAIL"
        return 1
    fi
    
    log "Building..."
    if make -j$(nproc) >> "$log_file" 2>&1; then
        log_success "Build succeeded"
        TEST_RESULTS[multiarch_build]="PASS"
    else
        log_error "Build failed (see $log_file)"
        TEST_RESULTS[multiarch_build]="FAIL"
        return 1
    fi
    
    # Check multiarch paths
    if grep -q "x86_64-linux-gnu" Makefile; then
        log_success "Multi-arch path detected in Makefile"
        TEST_RESULTS[multiarch_paths]="PASS"
    else
        log_warning "Multi-arch path not found"
        TEST_RESULTS[multiarch_paths]="WARN"
    fi
}

print_summary() {
    log ""
    log "========================================"
    log "Test Summary"
    log "========================================"
    
    local total=0
    local passed=0
    local failed=0
    local skipped=0
    
    for test in "${!TEST_RESULTS[@]}"; do
        result="${TEST_RESULTS[$test]}"
        printf "%-30s " "$test"
        case "$result" in
            PASS)
                echo -e "${GREEN}✓ PASS${NC}"
                ((passed++))
                ;;
            FAIL)
                echo -e "${RED}✗ FAIL${NC}"
                ((failed++))
                ;;
            SKIP)
                echo -e "${YELLOW}○ SKIP${NC}"
                ((skipped++))
                ;;
            WARN)
                echo -e "${YELLOW}! WARN${NC}"
                ;;
        esac
        ((total++))
    done
    
    log ""
    log "Total:   $total"
    log_success "Passed:  $passed"
    log_error "Failed:  $failed"
    log_warning "Skipped: $skipped"
    log ""
    
    if [ $failed -eq 0 ]; then
        log_success "All tests passed!"
        return 0
    else
        log_error "Some tests failed"
        return 1
    fi
}

main() {
    log "PCC Multi-Architecture Bootstrap Testing"
    log "Test directory: $TEST_DIR"
    log ""
    
    check_dependencies
    
    # Run tests
    test_mingw_x86_64 || true
    test_mingw_i686 || true
    test_i386_native || true
    test_multiarch || true
    
    # Print summary
    print_summary
}

# Parse arguments
SKIP_DEPS=false
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-deps)
            SKIP_DEPS=true
            shift
            ;;
        --test-dir)
            TEST_DIR="$2"
            LOG_DIR="$TEST_DIR/logs"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--skip-deps] [--test-dir DIR]"
            exit 1
            ;;
    esac
done

main
