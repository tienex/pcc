#!/bin/bash
# Test runner for DEC MACRO compiler
# Compiles all test files and reports results

set -e

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0

echo "========================================"
echo "DEC MACRO Compiler Test Suite"
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

    TOTAL=$((TOTAL + 1))

    echo -n "Testing $test_name ... "

    # Run the compiler
    if $MCOM "$test_file" > "${test_name}.out" 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))

        # Check output file was created and has content
        if [ ! -s "${test_name}.out" ]; then
            echo -e "  ${YELLOW}WARNING: Output file is empty${NC}"
        fi

        # Count generated instructions
        local inst_count=$(grep -c "^\s*[A-Z]" "${test_name}.out" 2>/dev/null || echo "0")
        echo "  Generated $inst_count instructions"

    else
        echo -e "${RED}FAIL${NC}"
        FAILED=$((FAILED + 1))
        echo "  Error output:"
        cat "${test_name}.out" | head -20 | sed 's/^/    /'
    fi

    echo ""
}

# Run all test files
echo "Running Architecture Test Suites:"
echo "=================================="
echo ""

# Test PDP-10
if [ -f "test_pdp10_all.mac" ]; then
    echo "--- PDP-10 Comprehensive Tests ---"
    run_test "test_pdp10_all.mac"
fi

# Test individual PDP-10 files
for test in test_pdp10_*.mac; do
    if [ -f "$test" ] && [ "$test" != "test_pdp10_all.mac" ]; then
        run_test "$test"
    fi
done

# Test PDP-11
if [ -f "test_pdp11_all.mac" ]; then
    echo "--- PDP-11 Comprehensive Tests ---"
    run_test "test_pdp11_all.mac"
fi

# Test VAX
if [ -f "test_vax_all.mac" ]; then
    echo "--- VAX Comprehensive Tests ---"
    run_test "test_vax_all.mac"
fi

# Test minimal examples
echo "--- Basic Functionality Tests ---"
for test in minimal*.mac; do
    if [ -f "$test" ]; then
        run_test "$test"
    fi
done

# Summary
echo "========================================"
echo "Test Summary"
echo "========================================"
echo "Total tests:  $TOTAL"
echo -e "Passed:       ${GREEN}$PASSED${NC}"
echo -e "Failed:       ${RED}$FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
