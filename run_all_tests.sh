#!/bin/bash

echo "========================================="
echo "Running ALL GC Tests"
echo "========================================="

failed=0
passed=0

run_test() {
    echo ""
    echo "Running: $1"
    echo "-----------------------------------------"
    if ./$1; then
        echo "✅ $1 PASSED"
        ((passed++))
    else
        echo "❌ $1 FAILED"
        ((failed++))
    fi
}

# Run all tests
run_test "test_simple"
run_test "test_weak_simple"
run_test "test_gc_pools"
run_test "test_gc_weak"
run_test "test_gc_edge_cases"
run_test "test_gc_pascal_integration"

echo ""
echo "========================================="
echo "Test Summary"
echo "========================================="
echo "Passed: $passed"
echo "Failed: $failed"
echo "========================================="

if [ $failed -eq 0 ]; then
    echo "✅ ALL TESTS PASSED!"
    exit 0
else
    echo "❌ SOME TESTS FAILED"
    exit 1
fi
