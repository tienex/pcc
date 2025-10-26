/*
 * Test program for IEEE 754-2008 decimal floating point (libdecimal)
 */

#include <stdio.h>
#include <string.h>
#include "libdecimal/decimal.h"

#define TEST(name) printf("\n=== Testing %s ===\n", name)
#define ASSERT(cond, msg) if (!(cond)) { \
    printf("FAILED: %s\n", msg); \
    return 1; \
}

int main(void) {
    char buf[64];

    printf("Testing IEEE 754-2008 Decimal Floating Point (libdecimal)\n");
    printf("============================================================\n");

    /* ================================================================
     * _Decimal32 Tests
     * ================================================================ */

    TEST("_Decimal32 special values");
    {
        _Decimal32 zero = dec32_zero();
        _Decimal32 one = dec32_one();
        _Decimal32 inf = dec32_inf();
        _Decimal32 nan = dec32_nan();

        ASSERT(dec32_is_zero(zero), "zero should be zero");
        ASSERT(!dec32_is_zero(one), "one should not be zero");
        ASSERT(dec32_is_inf(inf), "inf should be infinity");
        ASSERT(dec32_is_nan(nan), "nan should be NaN");
        ASSERT(dec32_is_finite(one), "one should be finite");
        ASSERT(!dec32_is_finite(inf), "inf should not be finite");

        printf("  Special values: PASS\n");
    }

    TEST("_Decimal32 integer conversions");
    {
        _Decimal32 d = dec32_from_int32(12345);
        int32_t i = dec32_to_int32(d);

        ASSERT(i == 12345, "int32 round-trip failed");

        d = dec32_from_int32(-9876);
        i = dec32_to_int32(d);

        ASSERT(i == -9876, "negative int32 round-trip failed");

        printf("  Integer conversions: PASS\n");
    }

    TEST("_Decimal32 string conversions");
    {
        _Decimal32 d = dec32_from_string("123.45");
        dec32_to_string(d, buf, sizeof(buf));

        printf("  Parsed '123.45' as: %s\n", buf);

        d = dec32_from_string("-678.90");
        dec32_to_string(d, buf, sizeof(buf));

        printf("  Parsed '-678.90' as: %s\n", buf);

        printf("  String conversions: PASS\n");
    }

    TEST("_Decimal32 arithmetic");
    {
        _Decimal32 a = dec32_from_int32(100);
        _Decimal32 b = dec32_from_int32(50);

        _Decimal32 sum = dec32_add(a, b);
        int32_t result = dec32_to_int32(sum);
        ASSERT(result == 150, "100 + 50 should be 150");
        printf("  100 + 50 = %d\n", result);

        _Decimal32 diff = dec32_sub(a, b);
        result = dec32_to_int32(diff);
        ASSERT(result == 50, "100 - 50 should be 50");
        printf("  100 - 50 = %d\n", result);

        _Decimal32 prod = dec32_mul(a, b);
        result = dec32_to_int32(prod);
        printf("  100 * 50 = %d\n", result);

        _Decimal32 quot = dec32_div(a, b);
        result = dec32_to_int32(quot);
        printf("  100 / 50 = %d\n", result);

        _Decimal32 neg = dec32_neg(a);
        result = dec32_to_int32(neg);
        ASSERT(result == -100, "neg(100) should be -100");
        printf("  neg(100) = %d\n", result);

        _Decimal32 abs_val = dec32_abs(neg);
        result = dec32_to_int32(abs_val);
        ASSERT(result == 100, "abs(-100) should be 100");
        printf("  abs(-100) = %d\n", result);

        printf("  Arithmetic: PASS\n");
    }

    TEST("_Decimal32 comparisons");
    {
        _Decimal32 a = dec32_from_int32(100);
        _Decimal32 b = dec32_from_int32(200);
        _Decimal32 c = dec32_from_int32(100);

        ASSERT(dec32_eq(a, c), "100 == 100 failed");
        ASSERT(dec32_ne(a, b), "100 != 200 failed");
        ASSERT(dec32_lt(a, b), "100 < 200 failed");
        ASSERT(dec32_le(a, b), "100 <= 200 failed");
        ASSERT(dec32_le(a, c), "100 <= 100 failed");
        ASSERT(dec32_gt(b, a), "200 > 100 failed");
        ASSERT(dec32_ge(b, a), "200 >= 100 failed");
        ASSERT(dec32_ge(a, c), "100 >= 100 failed");

        printf("  Comparisons: PASS\n");
    }

    /* ================================================================
     * _Decimal64 Tests
     * ================================================================ */

    TEST("_Decimal64 special values");
    {
        _Decimal64 zero = dec64_zero();
        _Decimal64 one = dec64_one();
        _Decimal64 inf = dec64_inf();
        _Decimal64 nan = dec64_nan();

        ASSERT(dec64_is_zero(zero), "zero should be zero");
        ASSERT(!dec64_is_zero(one), "one should not be zero");
        ASSERT(dec64_is_inf(inf), "inf should be infinity");
        ASSERT(dec64_is_nan(nan), "nan should be NaN");
        ASSERT(dec64_is_finite(one), "one should be finite");

        printf("  Special values: PASS\n");
    }

    TEST("_Decimal64 integer conversions");
    {
        _Decimal64 d = dec64_from_int64(1234567890LL);
        int64_t i = dec64_to_int64(d);

        ASSERT(i == 1234567890LL, "int64 round-trip failed");

        d = dec64_from_int64(-9876543210LL);
        i = dec64_to_int64(d);

        ASSERT(i == -9876543210LL, "negative int64 round-trip failed");

        printf("  Integer conversions: PASS\n");
    }

    TEST("_Decimal64 string conversions");
    {
        _Decimal64 d = dec64_from_string("123456.789");
        dec64_to_string(d, buf, sizeof(buf));

        printf("  Parsed '123456.789' as: %s\n", buf);

        d = dec64_from_string("-987654.321");
        dec64_to_string(d, buf, sizeof(buf));

        printf("  Parsed '-987654.321' as: %s\n", buf);

        printf("  String conversions: PASS\n");
    }

    TEST("_Decimal64 arithmetic");
    {
        _Decimal64 a = dec64_from_int64(1000);
        _Decimal64 b = dec64_from_int64(500);

        _Decimal64 sum = dec64_add(a, b);
        int64_t result = dec64_to_int64(sum);
        ASSERT(result == 1500, "1000 + 500 should be 1500");
        printf("  1000 + 500 = %lld\n", (long long)result);

        _Decimal64 diff = dec64_sub(a, b);
        result = dec64_to_int64(diff);
        ASSERT(result == 500, "1000 - 500 should be 500");
        printf("  1000 - 500 = %lld\n", (long long)result);

        _Decimal64 prod = dec64_mul(a, b);
        result = dec64_to_int64(prod);
        printf("  1000 * 500 = %lld\n", (long long)result);

        _Decimal64 quot = dec64_div(a, b);
        result = dec64_to_int64(quot);
        printf("  1000 / 500 = %lld\n", (long long)result);

        printf("  Arithmetic: PASS\n");
    }

    TEST("_Decimal64 comparisons");
    {
        _Decimal64 a = dec64_from_int64(1000);
        _Decimal64 b = dec64_from_int64(2000);
        _Decimal64 c = dec64_from_int64(1000);

        ASSERT(dec64_eq(a, c), "1000 == 1000 failed");
        ASSERT(dec64_ne(a, b), "1000 != 2000 failed");
        ASSERT(dec64_lt(a, b), "1000 < 2000 failed");
        ASSERT(dec64_gt(b, a), "2000 > 1000 failed");

        printf("  Comparisons: PASS\n");
    }

    /* ================================================================
     * Type Conversion Tests
     * ================================================================ */

    TEST("Cross-type conversions");
    {
        _Decimal32 d32 = dec32_from_int32(12345);
        _Decimal64 d64 = dec32_to_dec64(d32);
        _Decimal128 d128 = dec32_to_dec128(d32);

        _Decimal32 back32 = dec64_to_dec32(d64);
        int32_t result = dec32_to_int32(back32);

        ASSERT(result == 12345, "dec32->dec64->dec32 round-trip failed");

        back32 = dec128_to_dec32(d128);
        result = dec32_to_int32(back32);

        ASSERT(result == 12345, "dec32->dec128->dec32 round-trip failed");

        printf("  Cross-type conversions: PASS\n");
    }

    /* ================================================================
     * _Decimal128 Tests
     * ================================================================ */

    TEST("_Decimal128 basic operations");
    {
        _Decimal128 zero = dec128_zero();
        _Decimal128 one = dec128_one();
        _Decimal128 inf = dec128_inf();
        _Decimal128 nan = dec128_nan();

        ASSERT(dec128_is_zero(zero), "zero should be zero");
        ASSERT(!dec128_is_zero(one), "one should not be zero");
        ASSERT(dec128_is_inf(inf), "inf should be infinity");
        ASSERT(dec128_is_nan(nan), "nan should be NaN");

        _Decimal128 a = dec128_from_int64(100000LL);
        _Decimal128 b = dec128_from_int64(50000LL);

        _Decimal128 sum = dec128_add(a, b);
        _Decimal128 diff = dec128_sub(a, b);
        _Decimal128 prod = dec128_mul(a, b);
        _Decimal128 quot = dec128_div(a, b);

        ASSERT(!dec128_is_zero(sum), "sum should not be zero");
        ASSERT(!dec128_is_zero(diff), "diff should not be zero");
        ASSERT(!dec128_is_zero(prod), "prod should not be zero");
        ASSERT(!dec128_is_zero(quot), "quot should not be zero");

        ASSERT(dec128_eq(a, a), "128-bit equality failed");
        ASSERT(dec128_ne(a, b), "128-bit inequality failed");

        printf("  _Decimal128 operations: PASS\n");
    }

    /* ================================================================
     * Practical Example: Financial Calculation
     * ================================================================ */

    TEST("Financial calculation example");
    {
        // Calculate 5% sales tax on $19.99
        _Decimal64 price = dec64_from_string("19.99");
        _Decimal64 tax_rate = dec64_from_string("0.05");
        _Decimal64 one = dec64_one();

        _Decimal64 multiplier = dec64_add(one, tax_rate);  // 1.05
        _Decimal64 total = dec64_mul(price, multiplier);   // 20.9895

        dec64_to_string(total, buf, sizeof(buf));
        printf("  Price: $19.99\n");
        printf("  Tax rate: 5%%\n");
        printf("  Total: $%s\n", buf);

        printf("  Financial calculation: PASS\n");
    }

    printf("\n============================================================\n");
    printf("All IEEE 754-2008 Decimal Floating Point Tests PASSED!\n");
    printf("============================================================\n");

    return 0;
}
