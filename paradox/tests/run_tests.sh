#!/bin/bash

# Test script for PAL compiler
# Runs various test cases and reports results

set -e  # Exit on error

echo "========================================="
echo "PAL Compiler Test Suite"
echo "========================================="
echo ""

PALCOM="../palcom/palcom"
TEST_DIR="."
PASSED=0
FAILED=0
TOTAL=0

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if compiler exists
if [ ! -f "$PALCOM" ]; then
    echo "Error: PAL compiler not found at $PALCOM"
    echo "Please build the compiler first:"
    echo "  cd ../palcom && make"
    exit 1
fi

# Function to run a test
run_test() {
    local test_file=$1
    local test_name=$(basename "$test_file" .pal)
    local dialect=${2:-"objectpal-latest"}

    TOTAL=$((TOTAL + 1))

    echo -n "Testing $test_name (dialect: $dialect)... "

    # Try to compile the test file
    if $PALCOM -d "$dialect" -o "/tmp/${test_name}.o" "$test_file" 2>/tmp/${test_name}.err; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
        rm -f "/tmp/${test_name}.o" "/tmp/${test_name}.err"
    else
        echo -e "${RED}FAIL${NC}"
        echo "  Error output:"
        sed 's/^/    /' "/tmp/${test_name}.err"
        FAILED=$((FAILED + 1))
    fi
}

# Test individual files
echo "Lexer Tests:"
echo "------------"
if [ -f "$TEST_DIR/test_lexer.pal" ]; then
    run_test "$TEST_DIR/test_lexer.pal" "objectpal-latest"
else
    echo "  test_lexer.pal not found, skipping"
fi

echo ""
echo "Parser Tests:"
echo "-------------"
if [ -f "$TEST_DIR/test_parser.pal" ]; then
    run_test "$TEST_DIR/test_parser.pal" "objectpal-latest"
else
    echo "  test_parser.pal not found, skipping"
fi

echo ""
echo "ObjectPAL Features Tests:"
echo "-------------------------"
if [ -f "$TEST_DIR/test_objectpal.pal" ]; then
    run_test "$TEST_DIR/test_objectpal.pal" "objectpal-latest"
else
    echo "  test_objectpal.pal not found, skipping"
fi

echo ""
echo "Example Programs:"
echo "-----------------"
for example in hello.pal loops.pal objectpal_example.pal; do
    if [ -f "$TEST_DIR/$example" ]; then
        run_test "$TEST_DIR/$example" "objectpal-latest"
    fi
done

echo ""
echo "Dialect Tests:"
echo "--------------"
# Test with different dialects
if [ -f "$TEST_DIR/hello.pal" ]; then
    run_test "$TEST_DIR/hello.pal" "pal-3.0"
    run_test "$TEST_DIR/hello.pal" "pal-4.5"
    run_test "$TEST_DIR/hello.pal" "objectpal-1.0"
    run_test "$TEST_DIR/hello.pal" "objectpal-7.0"
fi

# Summary
echo ""
echo "========================================="
echo "Test Summary"
echo "========================================="
echo "Total tests:  $TOTAL"
echo -e "Passed:       ${GREEN}$PASSED${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "Failed:       ${RED}$FAILED${NC}"
else
    echo -e "Failed:       $FAILED"
fi
echo "========================================="

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
