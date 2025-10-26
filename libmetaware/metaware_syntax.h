/*
 * MetaWare Syntax Extensions - Preprocessor Workarounds
 *
 * This header provides macro-based approximations of MetaWare syntax features
 * that require compiler support. These are workarounds until full compiler
 * implementation is available.
 */

#ifndef _METAWARE_SYNTAX_H
#define _METAWARE_SYNTAX_H

#ifdef __cplusplus
extern "C" {
#endif

/* ================================================================
 * LABELED/NAMED ARGUMENTS (Keyword Arguments)
 * ================================================================ */

/*
 * Macro-based labeled arguments using C99 designated initializers
 *
 * Example:
 *   DECLARE_LABELED_FUNC(drawRect,
 *       int x; int y; int width; int height; int color;
 *   )
 *
 *   DEFINE_LABELED_FUNC(drawRect) {
 *       printf("Drawing rect at (%d,%d) size %dx%d color 0x%X\n",
 *              ARGS.x, ARGS.y, ARGS.width, ARGS.height, ARGS.color);
 *   }
 *
 *   // Call with labeled arguments:
 *   CALL(drawRect,
 *       .width = 200,
 *       .height = 100,
 *       .x = 50,
 *       .y = 75,
 *       .color = 0xFF0000
 *   );
 */

/* Declare a function that accepts labeled arguments */
#define DECLARE_LABELED_FUNC(name, ...) \
    typedef struct name##_args_t { \
        __VA_ARGS__ \
    } name##_args_t

/* Declare the function prototype */
#define DECLARE_LABELED_FUNC_PROTO(name) \
    void name(name##_args_t args)

/* Define the implementation (use 'args' to access parameters) */
#define DEFINE_LABELED_FUNC(name) \
    void name(name##_args_t args)

/* Call with labeled arguments */
#define CALL(func, ...) \
    func((func##_args_t){ __VA_ARGS__ })

/* Alternative: Use ARG macro for clarity */
#define ARG(name, value) .name = value

/* ================================================================
 * NUMERIC LITERAL SEPARATORS (Macro-based)
 * ================================================================ */

/*
 * Workaround using macros for common large numbers
 * Real implementation requires lexer changes
 */

/* Decimal separators */
#define _1K     1000
#define _10K    10000
#define _100K   100000
#define _1M     1000000
#define _10M    10000000
#define _100M   100000000
#define _1B     1000000000
#define _1T     1000000000000LL

/* Binary literals (if not supported) */
#define BIN(x) _BIN_##x
#define _BIN_0 0
#define _BIN_1 1
#define _BIN_00 0
#define _BIN_01 1
#define _BIN_10 2
#define _BIN_11 3
#define _BIN_0000 0
#define _BIN_0001 1
#define _BIN_0010 2
#define _BIN_0011 3
#define _BIN_0100 4
#define _BIN_0101 5
#define _BIN_0110 6
#define _BIN_0111 7
#define _BIN_1000 8
#define _BIN_1001 9
#define _BIN_1010 10
#define _BIN_1011 11
#define _BIN_1100 12
#define _BIN_1101 13
#define _BIN_1110 14
#define _BIN_1111 15

/* Full byte binary */
#define _BIN_00000000 0x00
#define _BIN_11111111 0xFF
#define _BIN_10101010 0xAA
#define _BIN_01010101 0x55
#define _BIN_11110000 0xF0
#define _BIN_00001111 0x0F

/* Hex with visual separators (via macros) */
#define HEX2(a,b) 0x##a##b
#define HEX4(a,b,c,d) 0x##a##b##c##d
#define HEX8(a,b,c,d,e,f,g,h) 0x##a##b##c##d##e##f##g##h

/* Usage examples:
 * int x = 5 * _1M;  // 5 million
 * int y = HEX4(DEAD, BEEF);  // 0xDEADBEEF
 * int z = _BIN_1010;  // 10
 */

/* ================================================================
 * CASE RANGES (Macro-based workaround)
 * ================================================================ */

/*
 * Macro-based case ranges using fall-through
 * Real implementation requires parser changes
 */

/* Generate consecutive case labels */
#define CASE_RANGE_2(start) \
    case (start): case ((start)+1)

#define CASE_RANGE_3(start) \
    case (start): case ((start)+1): case ((start)+2)

#define CASE_RANGE_4(start) \
    case (start): case ((start)+1): case ((start)+2): case ((start)+3)

#define CASE_RANGE_5(start) \
    CASE_RANGE_4(start): case ((start)+4)

#define CASE_RANGE_10(start) \
    CASE_RANGE_5(start): CASE_RANGE_5((start)+5)

#define CASE_RANGE_26(start) \
    CASE_RANGE_10(start): CASE_RANGE_10((start)+10): \
    CASE_RANGE_5((start)+20): case ((start)+25)

/* Character ranges */
#define CASE_LOWERCASE \
    CASE_RANGE_26('a')

#define CASE_UPPERCASE \
    CASE_RANGE_26('A')

#define CASE_DIGIT \
    CASE_RANGE_10('0')

#define CASE_ALPHA \
    CASE_LOWERCASE: CASE_UPPERCASE

#define CASE_ALNUM \
    CASE_ALPHA: CASE_DIGIT

/* Usage:
 * switch (c) {
 *     CASE_LOWERCASE:
 *         handle_lower();
 *         break;
 *     CASE_UPPERCASE:
 *         handle_upper();
 *         break;
 *     CASE_DIGIT:
 *         handle_digit();
 *         break;
 * }
 */

/* ================================================================
 * UTILITY MACROS
 * ================================================================ */

/* Check if in range (runtime check) */
#define IN_RANGE(value, start, end) \
    ((value) >= (start) && (value) <= (end))

/* Switch with range checks (instead of case ranges) */
#define RANGE_CASE(value, start, end) \
    if (IN_RANGE(value, start, end))

/* Example:
 * RANGE_CASE(grade, 90, 100) {
 *     printf("A\n");
 * }
 * else RANGE_CASE(grade, 80, 89) {
 *     printf("B\n");
 * }
 */

#ifdef __cplusplus
}
#endif

#endif /* _METAWARE_SYNTAX_H */
