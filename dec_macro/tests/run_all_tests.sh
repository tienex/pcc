#!/bin/bash
# Test runner for DEC MACRO compiler - Individual test files
# Runs all tests organized by architecture

set -e

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0

echo "========================================"
echo "DEC MACRO Compiler Individual Test Suite"
echo "========================================"
echo ""

# Path to compiler
MCOM="../mcom/mcom"

if [ ! -f "$MCOM" ]; then
    echo -e "${RED}ERROR: Compiler not found at $MCOM${NC}"
    echo "Please build the compiler first: cd ../mcom && make"
    exit 1
fi

echo "Using compiler: $MCOM"
echo ""

# Function to run a single test
run_test() {
    local test_file=$1
    local test_name=$(basename "$test_file" .mac)
    local test_dir=$(dirname "$test_file")

    TOTAL=$((TOTAL + 1))

    # Run the compiler
    if $MCOM "$test_file" > "${test_dir}/${test_name}.out" 2>&1; then
        echo -e "  ${GREEN}✓${NC} $test_name"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗${NC} $test_name"
        FAILED=$((FAILED + 1))
        if [ -n "$VERBOSE" ]; then
            echo "    Error:"
            cat "${test_dir}/${test_name}.out" | head -5 | sed 's/^/      /'
        fi
    fi
}

# Run tests by architecture
echo -e "${BLUE}=== PDP-10 Tests ===${NC}"
if [ -d "pdp10" ]; then
    for test in pdp10/test_*.mac; do
        if [ -f "$test" ]; then
            run_test "$test"
        fi
    done
else
    echo "  No PDP-10 tests found"
fi
echo ""

echo -e "${BLUE}=== PDP-11 Tests ===${NC}"
if [ -d "pdp11" ]; then
    for test in pdp11/test_*.mac; do
        if [ -f "$test" ]; then
            run_test "$test"
        fi
    done
else
    echo "  No PDP-11 tests found"
fi
echo ""

echo -e "${BLUE}=== VAX Tests ===${NC}"
if [ -d "vax" ]; then
    for test in vax/test_*.mac; do
        if [ -f "$test" ]; then
            run_test "$test"
        fi
    done
else
    echo "  No VAX tests found"
fi
echo ""

# Summary
echo "========================================"
echo "Test Summary"
echo "========================================"
echo "Total tests:  $TOTAL"
echo -e "Passed:       ${GREEN}$PASSED${NC} ($((PASSED * 100 / TOTAL))%)"
echo -e "Failed:       ${RED}$FAILED${NC} ($((FAILED * 100 / TOTAL))%)"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    echo -e "${YELLOW}Note: Some tests may fail due to parser limitations${NC}"
    echo -e "${YELLOW}Check individual .out files for details${NC}"
    exit 1
fi
