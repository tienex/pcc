/*
 * Test Suite for MetaWare Syntax Extensions Workarounds
 *
 * Tests the preprocessor-based approximations of MetaWare syntax features
 * that require full compiler support for proper implementation.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libmetaware/metaware_syntax.h"

#define TEST(name) printf("\n=== Testing %s ===\n", name)
#define ASSERT(cond, msg) if (!(cond)) { \
    printf("FAILED: %s\n", msg); \
    return 1; \
}
#define PASS(msg) printf("  %s: PASS\n", msg)

/* ================================================================
 * TEST 1: Labeled/Named Arguments
 * ================================================================ */

/* Declare a function with labeled arguments */
DECLARE_LABELED_FUNC(drawRect,
    int x;
    int y;
    int width;
    int height;
    int color;
);

DEFINE_LABELED_FUNC(drawRect) {
    printf("  Drawing rectangle:\n");
    printf("    Position: (%d, %d)\n", args.x, args.y);
    printf("    Size: %dx%d\n", args.width, args.height);
    printf("    Color: 0x%06X\n", args.color);
}

/* Another example with different types */
DECLARE_LABELED_FUNC(createWindow,
    const char *title;
    int width;
    int height;
    int visible;
);

DEFINE_LABELED_FUNC(createWindow) {
    printf("  Creating window:\n");
    printf("    Title: \"%s\"\n", args.title);
    printf("    Size: %dx%d\n", args.width, args.height);
    printf("    Visible: %s\n", args.visible ? "yes" : "no");
}

int test_labeled_arguments(void) {
    TEST("Labeled/Named Arguments");

    /* Call with arguments in any order */
    CALL(drawRect,
        .width = 200,
        .height = 100,
        .x = 50,
        .y = 75,
        .color = 0xFF0000
    );

    /* Call with different order */
    CALL(drawRect,
        .color = 0x00FF00,
        .x = 10,
        .y = 20,
        .height = 50,
        .width = 100
    );

    /* Call with string argument */
    CALL(createWindow,
        .title = "MetaWare Test",
        .width = 800,
        .height = 600,
        .visible = 1
    );

    PASS("Labeled arguments");
    return 0;
}

/* ================================================================
 * TEST 2: Numeric Literal Separators (Macro-based)
 * ================================================================ */

int test_numeric_separators(void) {
    TEST("Numeric Literal Separators");

    /* Test decimal separators */
    int one_thousand = _1K;
    int ten_thousand = _10K;
    int one_million = _1M;
    long long one_trillion = _1T;

    ASSERT(one_thousand == 1000, "1K should be 1000");
    ASSERT(ten_thousand == 10000, "10K should be 10000");
    ASSERT(one_million == 1000000, "1M should be 1000000");
    ASSERT(one_trillion == 1000000000000LL, "1T should be 1 trillion");

    printf("  1K = %d\n", one_thousand);
    printf("  10K = %d\n", ten_thousand);
    printf("  1M = %d\n", one_million);
    printf("  1T = %lld\n", one_trillion);

    /* Test binary literals */
    int bin_10 = _BIN_1010;
    int bin_15 = _BIN_1111;
    int bin_aa = _BIN_10101010;

    ASSERT(bin_10 == 10, "Binary 1010 should be 10");
    ASSERT(bin_15 == 15, "Binary 1111 should be 15");
    ASSERT(bin_aa == 0xAA, "Binary 10101010 should be 0xAA");

    printf("  Binary 1010 = %d\n", bin_10);
    printf("  Binary 1111 = %d\n", bin_15);
    printf("  Binary 10101010 = 0x%02X\n", bin_aa);

    /* Test hex separators */
    int hex_deadbeef = HEX8(D,E,A,D,B,E,E,F);
    int hex_cafe = HEX4(C,A,F,E);
    int hex_pair = HEX2(A,B);

    ASSERT(hex_deadbeef == 0xDEADBEEF, "HEX8 should produce 0xDEADBEEF");
    ASSERT(hex_cafe == 0xCAFE, "HEX4 should produce 0xCAFE");
    ASSERT(hex_pair == 0xAB, "HEX2 should produce 0xAB");

    printf("  HEX8(D,E,A,D,B,E,E,F) = 0x%08X\n", hex_deadbeef);
    printf("  HEX4(C,A,F,E) = 0x%04X\n", hex_cafe);
    printf("  HEX2(A,B) = 0x%02X\n", hex_pair);

    PASS("Numeric separators");
    return 0;
}

/* ================================================================
 * TEST 3: Case Ranges
 * ================================================================ */

const char *classify_char(int c) {
    switch (c) {
        CASE_LOWERCASE:
            return "lowercase letter";

        CASE_UPPERCASE:
            return "uppercase letter";

        CASE_DIGIT:
            return "digit";

        default:
            return "other";
    }
}

int test_case_ranges(void) {
    TEST("Case Ranges");

    /* Test lowercase */
    ASSERT(strcmp(classify_char('a'), "lowercase letter") == 0,
           "'a' should be lowercase");
    ASSERT(strcmp(classify_char('m'), "lowercase letter") == 0,
           "'m' should be lowercase");
    ASSERT(strcmp(classify_char('z'), "lowercase letter") == 0,
           "'z' should be lowercase");

    printf("  'a' -> %s\n", classify_char('a'));
    printf("  'm' -> %s\n", classify_char('m'));
    printf("  'z' -> %s\n", classify_char('z'));

    /* Test uppercase */
    ASSERT(strcmp(classify_char('A'), "uppercase letter") == 0,
           "'A' should be uppercase");
    ASSERT(strcmp(classify_char('M'), "uppercase letter") == 0,
           "'M' should be uppercase");
    ASSERT(strcmp(classify_char('Z'), "uppercase letter") == 0,
           "'Z' should be uppercase");

    printf("  'A' -> %s\n", classify_char('A'));
    printf("  'M' -> %s\n", classify_char('M'));
    printf("  'Z' -> %s\n", classify_char('Z'));

    /* Test digits */
    ASSERT(strcmp(classify_char('0'), "digit") == 0,
           "'0' should be digit");
    ASSERT(strcmp(classify_char('5'), "digit") == 0,
           "'5' should be digit");
    ASSERT(strcmp(classify_char('9'), "digit") == 0,
           "'9' should be digit");

    printf("  '0' -> %s\n", classify_char('0'));
    printf("  '5' -> %s\n", classify_char('5'));
    printf("  '9' -> %s\n", classify_char('9'));

    /* Test other */
    ASSERT(strcmp(classify_char('!'), "other") == 0,
           "'!' should be other");
    ASSERT(strcmp(classify_char(' '), "other") == 0,
           "' ' should be other");

    printf("  '!' -> %s\n", classify_char('!'));
    printf("  ' ' -> %s\n", classify_char(' '));

    PASS("Case ranges");
    return 0;
}

/* ================================================================
 * TEST 4: CASE_RANGE macros with custom ranges
 * ================================================================ */

char *get_grade(int score) {
    static char grade[3];

    switch (score) {
        CASE_RANGE_10(90):  /* 90-99 */
        case 100:
            strcpy(grade, "A");
            break;

        CASE_RANGE_10(80):  /* 80-89 */
            strcpy(grade, "B");
            break;

        CASE_RANGE_10(70):  /* 70-79 */
            strcpy(grade, "C");
            break;

        CASE_RANGE_10(60):  /* 60-69 */
            strcpy(grade, "D");
            break;

        default:
            strcpy(grade, "F");
            break;
    }

    return grade;
}

int test_custom_ranges(void) {
    TEST("Custom Case Ranges");

    ASSERT(strcmp(get_grade(95), "A") == 0, "95 should be A");
    ASSERT(strcmp(get_grade(85), "B") == 0, "85 should be B");
    ASSERT(strcmp(get_grade(75), "C") == 0, "75 should be C");
    ASSERT(strcmp(get_grade(65), "D") == 0, "65 should be D");
    ASSERT(strcmp(get_grade(55), "F") == 0, "55 should be F");
    ASSERT(strcmp(get_grade(100), "A") == 0, "100 should be A");

    printf("  Score 95 -> %s\n", get_grade(95));
    printf("  Score 85 -> %s\n", get_grade(85));
    printf("  Score 75 -> %s\n", get_grade(75));
    printf("  Score 65 -> %s\n", get_grade(65));
    printf("  Score 55 -> %s\n", get_grade(55));
    printf("  Score 100 -> %s\n", get_grade(100));

    PASS("Custom case ranges");
    return 0;
}

/* ================================================================
 * TEST 5: IN_RANGE utility macro
 * ================================================================ */

int test_in_range(void) {
    TEST("IN_RANGE Utility");

    ASSERT(IN_RANGE(5, 1, 10), "5 should be in range [1,10]");
    ASSERT(IN_RANGE(1, 1, 10), "1 should be in range [1,10]");
    ASSERT(IN_RANGE(10, 1, 10), "10 should be in range [1,10]");
    ASSERT(!IN_RANGE(0, 1, 10), "0 should NOT be in range [1,10]");
    ASSERT(!IN_RANGE(11, 1, 10), "11 should NOT be in range [1,10]");

    printf("  IN_RANGE(5, 1, 10) = %d\n", IN_RANGE(5, 1, 10));
    printf("  IN_RANGE(0, 1, 10) = %d\n", IN_RANGE(0, 1, 10));
    printf("  IN_RANGE(11, 1, 10) = %d\n", IN_RANGE(11, 1, 10));

    /* Test with RANGE_CASE */
    int score = 85;
    printf("  Score %d: ", score);

    RANGE_CASE(score, 90, 100) {
        printf("Grade A\n");
    }
    else RANGE_CASE(score, 80, 89) {
        printf("Grade B\n");
    }
    else RANGE_CASE(score, 70, 79) {
        printf("Grade C\n");
    }
    else {
        printf("Grade D or F\n");
    }

    PASS("IN_RANGE utility");
    return 0;
}

/* ================================================================
 * TEST 6: Complex Example - All Features Combined
 * ================================================================ */

DECLARE_LABELED_FUNC(processData,
    const char *filename;
    int buffer_size;
    int timeout_ms;
    int verbose;
);

DEFINE_LABELED_FUNC(processData) {
    printf("  Processing data file:\n");
    printf("    Filename: %s\n", args.filename);
    printf("    Buffer: %d bytes (%d KB)\n",
           args.buffer_size, args.buffer_size / _1K);
    printf("    Timeout: %d ms\n", args.timeout_ms);
    printf("    Verbose: %s\n", args.verbose ? "yes" : "no");

    /* Simulate processing different file types */
    const char *ext = strrchr(args.filename, '.');
    if (ext) {
        ext++; /* Skip the dot */
        char first = ext[0];

        switch (first) {
            CASE_LOWERCASE:
            CASE_UPPERCASE:
                printf("    File type: Text-based\n");
                break;

            CASE_DIGIT:
                printf("    File type: Numbered archive\n");
                break;

            default:
                printf("    File type: Unknown\n");
                break;
        }
    }
}

int test_combined_features(void) {
    TEST("Combined Features");

    /* Use labeled arguments with numeric separator macros */
    CALL(processData,
        .filename = "data.txt",
        .buffer_size = 4 * _1K,     /* 4KB buffer */
        .timeout_ms = 5000,
        .verbose = 1
    );

    printf("\n");

    CALL(processData,
        .filename = "archive.001",
        .buffer_size = _1M,          /* 1MB buffer */
        .timeout_ms = 10000,
        .verbose = 0
    );

    PASS("Combined features");
    return 0;
}

/* ================================================================
 * MAIN TEST RUNNER
 * ================================================================ */

int main(void) {
    int failures = 0;

    printf("==================================================\n");
    printf("  MetaWare Syntax Extensions Test Suite\n");
    printf("  (Preprocessor-based Workarounds)\n");
    printf("==================================================\n");

    failures += test_labeled_arguments();
    failures += test_numeric_separators();
    failures += test_case_ranges();
    failures += test_custom_ranges();
    failures += test_in_range();
    failures += test_combined_features();

    printf("\n==================================================\n");
    if (failures == 0) {
        printf("  ALL TESTS PASSED!\n");
        printf("==================================================\n");
        printf("\nNOTE: These are preprocessor-based workarounds.\n");
        printf("For true MetaWare syntax support, compiler changes\n");
        printf("are required. See METAWARE_COMPLETE_FEATURES.md\n");
        printf("for full implementation details.\n");
        return 0;
    } else {
        printf("  %d TEST(S) FAILED\n", failures);
        printf("==================================================\n");
        return 1;
    }
}
