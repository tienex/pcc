/*
 * C11 Unicode character utilities implementation
 * Portable implementation for systems lacking C11 Unicode support
 */

#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include "uchar.h"

/* Internal state structure for UTF-16 surrogate pairs */
typedef struct {
    mbstate_t mbs;
    char16_t high_surrogate;
} utf16_state_t;

static utf16_state_t internal_c16_state = {{0}, 0};

/*
 * UTF-8 validation and decoding
 * Returns the number of bytes in the UTF-8 sequence, or 0 on error
 */
static size_t
utf8_decode(const char *s, size_t n, char32_t *result)
{
    unsigned char c;
    char32_t codepoint;
    size_t len;
    size_t i;

    if (n == 0)
        return (size_t)-2;  /* incomplete sequence */

    c = (unsigned char)s[0];

    /* Single-byte character (ASCII) */
    if (c < 0x80) {
        *result = c;
        return 1;
    }

    /* Determine sequence length */
    if ((c & 0xE0) == 0xC0) {
        len = 2;
        codepoint = c & 0x1F;
    } else if ((c & 0xF0) == 0xE0) {
        len = 3;
        codepoint = c & 0x0F;
    } else if ((c & 0xF8) == 0xF0) {
        len = 4;
        codepoint = c & 0x07;
    } else {
        errno = EILSEQ;
        return (size_t)-1;  /* invalid sequence */
    }

    if (n < len)
        return (size_t)-2;  /* incomplete sequence */

    /* Decode continuation bytes */
    for (i = 1; i < len; i++) {
        c = (unsigned char)s[i];
        if ((c & 0xC0) != 0x80) {
            errno = EILSEQ;
            return (size_t)-1;
        }
        codepoint = (codepoint << 6) | (c & 0x3F);
    }

    /* Check for overlong encodings and invalid codepoints */
    if ((len == 2 && codepoint < 0x80) ||
        (len == 3 && codepoint < 0x800) ||
        (len == 4 && codepoint < 0x10000) ||
        codepoint > 0x10FFFF ||
        (codepoint >= 0xD800 && codepoint <= 0xDFFF)) {
        errno = EILSEQ;
        return (size_t)-1;
    }

    *result = codepoint;
    return len;
}

/*
 * Encode a codepoint to UTF-8
 */
static size_t
utf8_encode(char *s, char32_t c32)
{
    if (c32 < 0x80) {
        s[0] = (char)c32;
        return 1;
    } else if (c32 < 0x800) {
        s[0] = (char)(0xC0 | (c32 >> 6));
        s[1] = (char)(0x80 | (c32 & 0x3F));
        return 2;
    } else if (c32 < 0x10000) {
        if (c32 >= 0xD800 && c32 <= 0xDFFF) {
            errno = EILSEQ;
            return (size_t)-1;
        }
        s[0] = (char)(0xE0 | (c32 >> 12));
        s[1] = (char)(0x80 | ((c32 >> 6) & 0x3F));
        s[2] = (char)(0x80 | (c32 & 0x3F));
        return 3;
    } else if (c32 < 0x110000) {
        s[0] = (char)(0xF0 | (c32 >> 18));
        s[1] = (char)(0x80 | ((c32 >> 12) & 0x3F));
        s[2] = (char)(0x80 | ((c32 >> 6) & 0x3F));
        s[3] = (char)(0x80 | (c32 & 0x3F));
        return 4;
    } else {
        errno = EILSEQ;
        return (size_t)-1;
    }
}

size_t
mbrtoc16(char16_t *restrict pc16, const char *restrict s, size_t n,
         mbstate_t *restrict ps)
{
    utf16_state_t *state;
    char32_t c32;
    size_t result;

    if (ps == NULL) {
        state = &internal_c16_state;
    } else {
        state = (utf16_state_t *)ps;
    }

    if (s == NULL) {
        if (state->high_surrogate != 0) {
            errno = EILSEQ;
            return (size_t)-1;
        }
        memset(state, 0, sizeof(*state));
        return 0;
    }

    /* If we have a pending high surrogate, return the low surrogate */
    if (state->high_surrogate != 0) {
        if (pc16 != NULL)
            *pc16 = state->high_surrogate;
        state->high_surrogate = 0;
        return (size_t)-3;
    }

    if (n == 0)
        return (size_t)-2;

    /* Decode UTF-8 to UTF-32 */
    result = utf8_decode(s, n, &c32);
    if (result == (size_t)-1 || result == (size_t)-2)
        return result;

    /* Convert UTF-32 to UTF-16 */
    if (c32 < 0x10000) {
        if (pc16 != NULL)
            *pc16 = (char16_t)c32;
        return result;
    } else {
        /* Surrogate pair needed */
        c32 -= 0x10000;
        if (pc16 != NULL)
            *pc16 = (char16_t)(0xD800 + (c32 >> 10));
        state->high_surrogate = (char16_t)(0xDC00 + (c32 & 0x3FF));
        return result;
    }
}

size_t
c16rtomb(char *restrict s, char16_t c16, mbstate_t *restrict ps)
{
    utf16_state_t *state;
    char32_t c32;
    static char internal_buf[MB_LEN_MAX];

    if (ps == NULL) {
        state = &internal_c16_state;
    } else {
        state = (utf16_state_t *)ps;
    }

    if (s == NULL) {
        s = internal_buf;
        c16 = 0;
    }

    if (c16 == 0) {
        memset(state, 0, sizeof(*state));
        s[0] = '\0';
        return 1;
    }

    /* Check for surrogate pairs */
    if (c16 >= 0xD800 && c16 <= 0xDBFF) {
        /* High surrogate */
        if (state->high_surrogate != 0) {
            errno = EILSEQ;
            return (size_t)-1;
        }
        state->high_surrogate = c16;
        return 0;
    } else if (c16 >= 0xDC00 && c16 <= 0xDFFF) {
        /* Low surrogate */
        if (state->high_surrogate == 0) {
            errno = EILSEQ;
            return (size_t)-1;
        }
        c32 = 0x10000;
        c32 += (state->high_surrogate - 0xD800) << 10;
        c32 += c16 - 0xDC00;
        state->high_surrogate = 0;
    } else {
        /* BMP character */
        if (state->high_surrogate != 0) {
            errno = EILSEQ;
            return (size_t)-1;
        }
        c32 = c16;
    }

    return utf8_encode(s, c32);
}

size_t
mbrtoc32(char32_t *restrict pc32, const char *restrict s, size_t n,
         mbstate_t *restrict ps)
{
    static mbstate_t internal_state = {0};
    char32_t c32;
    size_t result;

    if (ps == NULL)
        ps = &internal_state;

    if (s == NULL) {
        memset(ps, 0, sizeof(*ps));
        return 0;
    }

    if (n == 0)
        return (size_t)-2;

    result = utf8_decode(s, n, &c32);
    if (result == (size_t)-1 || result == (size_t)-2)
        return result;

    if (pc32 != NULL)
        *pc32 = c32;

    if (c32 == 0)
        return 0;

    return result;
}

size_t
c32rtomb(char *restrict s, char32_t c32, mbstate_t *restrict ps)
{
    static mbstate_t internal_state = {0};
    static char internal_buf[MB_LEN_MAX];

    if (ps == NULL)
        ps = &internal_state;

    if (s == NULL) {
        s = internal_buf;
        c32 = 0;
    }

    if (c32 == 0) {
        memset(ps, 0, sizeof(*ps));
        s[0] = '\0';
        return 1;
    }

    return utf8_encode(s, c32);
}
