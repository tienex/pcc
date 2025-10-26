#!/bin/bash
#
# Test runner for Perl 5 compiler
#

PERL5=../perl5/perl5
TESTS_DIR=$(dirname "$0")

if [ ! -x "$PERL5" ]; then
    echo "Error: perl5 compiler not found at $PERL5"
    echo "Please build the compiler first"
    exit 1
fi

echo "Running Perl 5 compiler tests..."
echo

passed=0
failed=0

for test in $TESTS_DIR/test_*.pl; do
    testname=$(basename "$test")
    echo -n "Testing $testname... "

    # Try to compile the test
    if $PERL5 -v -S -o "${test%.pl}.s" "$test" 2>&1 | grep -q "error"; then
        echo "FAILED (compilation error)"
        failed=$((failed + 1))
    else
        echo "PASSED"
        passed=$((passed + 1))
        # Clean up assembly file if not saving temps
        if [ -z "$SAVE_TEMPS" ]; then
            rm -f "${test%.pl}.s"
        fi
    fi
done

echo
echo "Results: $passed passed, $failed failed"

if [ $failed -eq 0 ]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed"
    exit 1
fi
