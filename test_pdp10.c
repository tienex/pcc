/*
 * Test program for PDP-10 backend POW2 mode validation
 * This tests register allocation, argument passing, and type sizes.
 */

/* Test 1: Simple integer operations (should work in both modes) */
int simple_add(int a, int b) {
    return a + b;
}

/* Test 2: Long/pointer operations (different register allocation) */
long long_add(long a, long b) {
    return a + b;
}

/* Test 3: Multiple arguments (tests register allocation) */
int sum5(int a, int b, int c, int d, int e) {
    return a + b + c + d + e;
}

/* Test 4: Mixed types (tests szty() usage) */
long long mixed(int a, long b, int c) {
    return (long long)a + b + (long long)c;
}

/* Test 5: Pointer operations */
void* get_addr(int *p) {
    return (void*)p;
}

/* Test 6: Function with many long arguments (tests register pairs) */
long sum3_long(long a, long b, long c) {
    return a + b + c;
}

/* Test 7: Return value handling */
long long return_longlong(void) {
    return 0x123456789ABCDEFLL;
}
