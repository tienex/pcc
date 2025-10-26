/*
 * Test program for C11 Unicode support (uchar.h)
 * This tests the portable libunicode implementation
 */

#include <stdio.h>
#include <string.h>
#include <uchar.h>
#include <locale.h>

int main(void) {
    /* Test UTF-32 conversion with ASCII */
    const char *utf8_string = "Hello, World!";
    char32_t c32;
    mbstate_t state = {0};
    size_t len;
    const char *p = utf8_string;
    int count = 0;

    printf("Testing C11 Unicode support (uchar.h)\n");
    printf("Input UTF-8 string: %s\n", utf8_string);
    printf("Converting to UTF-32 codepoints:\n");

    while (*p) {
        len = mbrtoc32(&c32, p, 4, &state);

        if (len == (size_t)-1 || len == (size_t)-2) {
            fprintf(stderr, "Error: Invalid UTF-8 sequence\n");
            return 1;
        }

        if (len == 0)
            break;

        printf("  Codepoint %d: U+%04X ('%c')\n", ++count, (unsigned)c32, (char)c32);
        p += len;
    }

    /* Test round-trip conversion */
    printf("\nTesting round-trip conversion:\n");
    char buf[16];
    mbstate_t state2 = {0};

    c32 = 0x0041;  /* 'A' */
    len = c32rtomb(buf, c32, &state2);
    if (len != (size_t)-1) {
        buf[len] = '\0';
        printf("  UTF-32 U+%04X -> UTF-8: \"%s\" (%zu bytes)\n", c32, buf, len);
    }

    /* Test UTF-16 conversion */
    printf("\nTesting UTF-16 conversion:\n");
    char16_t c16_buf[4];
    mbstate_t state16 = {0};
    const char *test_str = "A";

    len = mbrtoc16(&c16_buf[0], test_str, 1, &state16);
    if (len != (size_t)-1 && len != (size_t)-2) {
        printf("  'A' -> UTF-16: 0x%04X\n", c16_buf[0]);

        /* Convert back */
        char back_buf[4];
        mbstate_t state16b = {0};
        size_t back_len = c16rtomb(back_buf, c16_buf[0], &state16b);
        if (back_len != (size_t)-1) {
            back_buf[back_len] = '\0';
            printf("  UTF-16 0x%04X -> UTF-8: \"%s\"\n", c16_buf[0], back_buf);
        }
    }

    printf("\nC11 Unicode test passed!\n");
    return 0;
}
