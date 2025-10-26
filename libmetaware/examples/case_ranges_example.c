/*
 * MetaWare Case Ranges Example
 *
 * Demonstrates the use of case range syntax for cleaner switch statements.
 *
 * In real MetaWare High C (and GCC extension), you would write:
 *   case 'a' ... 'z':
 *   case 'A' ... 'Z':
 *   case '0' ... '9':
 *
 * With our workaround, we provide macros that expand to multiple case labels.
 */

#include <stdio.h>
#include <ctype.h>
#include "../metaware_syntax.h"

/* Character classification using case ranges */
const char *classify_character(int c) {
    switch (c) {
        CASE_LOWERCASE:
            return "lowercase letter";

        CASE_UPPERCASE:
            return "uppercase letter";

        CASE_DIGIT:
            return "digit";

        case ' ':
        case '\t':
        case '\n':
        case '\r':
            return "whitespace";

        case '.':
        case ',':
        case '!':
        case '?':
        case ';':
        case ':':
            return "punctuation";

        default:
            return "other";
    }
}

/* Grade classification */
char get_letter_grade(int score) {
    switch (score) {
        CASE_RANGE_10(90):  /* 90-99 */
        case 100:
            return 'A';

        CASE_RANGE_10(80):  /* 80-89 */
            return 'B';

        CASE_RANGE_10(70):  /* 70-79 */
            return 'C';

        CASE_RANGE_10(60):  /* 60-69 */
            return 'D';

        default:
            return 'F';
    }
}

/* HTTP status code handling */
const char *http_status_category(int code) {
    switch (code) {
        CASE_RANGE_10(100):  /* 100-109 (informational) */
            return "Informational";

        CASE_RANGE_10(200):  /* 200-209 (success) */
        CASE_RANGE_10(210):  /* 210-219 */
            return "Success";

        CASE_RANGE_10(300):  /* 300-309 (redirection) */
        CASE_RANGE_10(310):  /* 310-319 */
            return "Redirection";

        CASE_RANGE_10(400):  /* 400-409 (client error) */
        CASE_RANGE_10(410):  /* 410-419 */
        CASE_RANGE_10(420):  /* 420-429 */
            return "Client Error";

        CASE_RANGE_10(500):  /* 500-509 (server error) */
        CASE_RANGE_10(510):  /* 510-519 */
            return "Server Error";

        default:
            return "Unknown";
    }
}

/* Lexer token classification */
typedef enum {
    TOKEN_LETTER,
    TOKEN_DIGIT,
    TOKEN_OPERATOR,
    TOKEN_WHITESPACE,
    TOKEN_OTHER
} TokenType;

TokenType classify_token(char c) {
    switch (c) {
        CASE_ALPHA:  /* Both upper and lower case letters */
            return TOKEN_LETTER;

        CASE_DIGIT:
            return TOKEN_DIGIT;

        case '+':
        case '-':
        case '*':
        case '/':
        case '=':
        case '<':
        case '>':
            return TOKEN_OPERATOR;

        case ' ':
        case '\t':
        case '\n':
            return TOKEN_WHITESPACE;

        default:
            return TOKEN_OTHER;
    }
}

/* Test character classification */
void test_char_classification(void) {
    printf("=== Character Classification ===\n");

    char test_chars[] = "aZ5 .!@";
    for (int i = 0; test_chars[i] != '\0'; i++) {
        char c = test_chars[i];
        if (c == ' ') {
            printf("'%c' (space) -> %s\n", c, classify_character(c));
        } else {
            printf("'%c' -> %s\n", c, classify_character(c));
        }
    }
}

/* Test grade classification */
void test_grades(void) {
    printf("\n=== Grade Classification ===\n");

    int scores[] = {95, 85, 75, 65, 55, 100, 60, 0};
    for (int i = 0; i < 8; i++) {
        printf("Score %3d -> Grade %c\n", scores[i], get_letter_grade(scores[i]));
    }
}

/* Test HTTP status codes */
void test_http_status(void) {
    printf("\n=== HTTP Status Code Categories ===\n");

    int codes[] = {100, 200, 201, 301, 404, 500, 503, 999};
    for (int i = 0; i < 8; i++) {
        printf("HTTP %d -> %s\n", codes[i], http_status_category(codes[i]));
    }
}

/* Test lexer tokens */
void test_lexer(void) {
    printf("\n=== Lexer Token Classification ===\n");

    const char *input = "x = 42 + y";
    printf("Input: \"%s\"\n", input);
    printf("Tokens:\n");

    for (int i = 0; input[i] != '\0'; i++) {
        char c = input[i];
        TokenType type = classify_token(c);

        const char *type_str;
        switch (type) {
            case TOKEN_LETTER:     type_str = "LETTER"; break;
            case TOKEN_DIGIT:      type_str = "DIGIT"; break;
            case TOKEN_OPERATOR:   type_str = "OPERATOR"; break;
            case TOKEN_WHITESPACE: type_str = "WHITESPACE"; break;
            default:               type_str = "OTHER"; break;
        }

        if (c == ' ') {
            printf("  '%c' (space) -> %s\n", c, type_str);
        } else {
            printf("  '%c' -> %s\n", c, type_str);
        }
    }
}

/* Demonstrate RANGE_CASE alternative */
void test_range_case_alternative(void) {
    printf("\n=== RANGE_CASE Alternative (if-else style) ===\n");

    int scores[] = {95, 85, 75, 65, 55};

    for (int i = 0; i < 5; i++) {
        int score = scores[i];
        printf("Score %d: ", score);

        RANGE_CASE(score, 90, 100) {
            printf("Excellent (A)\n");
        }
        else RANGE_CASE(score, 80, 89) {
            printf("Good (B)\n");
        }
        else RANGE_CASE(score, 70, 79) {
            printf("Satisfactory (C)\n");
        }
        else RANGE_CASE(score, 60, 69) {
            printf("Passing (D)\n");
        }
        else {
            printf("Failing (F)\n");
        }
    }
}

/* Character range validation */
void test_in_range(void) {
    printf("\n=== IN_RANGE Utility ===\n");

    char c = 'm';
    printf("Testing character '%c':\n", c);
    printf("  Is lowercase? %s\n", IN_RANGE(c, 'a', 'z') ? "yes" : "no");
    printf("  Is uppercase? %s\n", IN_RANGE(c, 'A', 'Z') ? "yes" : "no");
    printf("  Is digit? %s\n", IN_RANGE(c, '0', '9') ? "yes" : "no");
    printf("  Is alphabetic? %s\n",
           (IN_RANGE(c, 'a', 'z') || IN_RANGE(c, 'A', 'Z')) ? "yes" : "no");
}

int main(void) {
    printf("=== MetaWare Case Ranges Examples ===\n\n");

    test_char_classification();
    test_grades();
    test_http_status();
    test_lexer();
    test_range_case_alternative();
    test_in_range();

    printf("\n=== Benefits of Case Ranges ===\n");
    printf("1. Cleaner switch statements for consecutive values\n");
    printf("2. More readable than long lists of case labels\n");
    printf("3. Self-documenting (shows intent: a range of values)\n");
    printf("4. Fewer lines of code\n");
    printf("5. Less error-prone (no missing case labels)\n");

    printf("\nNOTE: This workaround uses macro expansion.\n");
    printf("True syntax support would be more flexible:\n");
    printf("  case 'a' ... 'z':  (any range expression)\n");
    printf("  case 0 ... 100:    (numeric ranges)\n");
    printf("  case 'A' ... 'Z':\n");

    printf("\nCompare to standard C:\n");
    printf("  case 'a': case 'b': case 'c': ... case 'z':  (26 labels!)\n");
    printf("vs MetaWare:\n");
    printf("  case 'a' ... 'z':  (single range)\n");

    return 0;
}
