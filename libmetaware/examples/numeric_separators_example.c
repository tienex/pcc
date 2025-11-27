/*
 * MetaWare Numeric Literal Separators Example
 *
 * Demonstrates the use of numeric literal separators for improved readability.
 *
 * In real MetaWare High C (and modern C++14/C23), you would write:
 *   int population = 7_800_000_000;
 *   int hex_color = 0xFF_AA_00;
 *   int binary = 0b1010_1010_1111_0000;
 *
 * With our workaround, we provide common constants and helper macros.
 */

#include <stdio.h>
#include "../metaware_syntax.h"

/* Demonstrate various numeric constants */

/* File sizes */
#define BUFFER_SIZE    (4 * _1K)      /* 4 KB */
#define CACHE_SIZE     (16 * _1M)     /* 16 MB */
#define MAX_FILE_SIZE  (2 * _1B)      /* 2 GB */

/* Network timeouts (milliseconds) */
#define CONNECT_TIMEOUT   (5 * 1000)
#define READ_TIMEOUT      (30 * 1000)
#define IDLE_TIMEOUT      (5 * 60 * 1000)   /* 5 minutes */

/* Hardware registers (common patterns) */
#define REG_ENABLE      _BIN_11111111   /* All bits on */
#define REG_DISABLE     _BIN_00000000   /* All bits off */
#define REG_PATTERN_A   _BIN_10101010   /* Alternating */
#define REG_PATTERN_B   _BIN_01010101   /* Alternating inverse */
#define REG_HIGH_NIBBLE _BIN_11110000   /* Upper 4 bits */
#define REG_LOW_NIBBLE  _BIN_00001111   /* Lower 4 bits */

/* Color constants */
#define COLOR_RED       HEX8(F,F,0,0,0,0,0,0)
#define COLOR_GREEN     HEX8(0,0,F,F,0,0,0,0)
#define COLOR_BLUE      HEX8(0,0,0,0,F,F,0,0)
#define COLOR_WHITE     HEX8(F,F,F,F,F,F,0,0)
#define COLOR_BLACK     HEX8(0,0,0,0,0,0,0,0)

/* UUID-like constants */
#define UUID_PART1      HEX8(1,2,3,4,5,6,7,8)
#define UUID_PART2      HEX4(9,A,B,C)
#define UUID_PART3      HEX4(D,E,F,0)

void print_memory_sizes(void) {
    printf("=== Memory Sizes ===\n");
    printf("BUFFER_SIZE:    %10d bytes (%d KB)\n", BUFFER_SIZE, BUFFER_SIZE / _1K);
    printf("CACHE_SIZE:     %10d bytes (%d MB)\n", CACHE_SIZE, CACHE_SIZE / _1M);
    printf("MAX_FILE_SIZE:  %10d bytes (%d GB)\n", MAX_FILE_SIZE, MAX_FILE_SIZE / _1B);
}

void print_timeouts(void) {
    printf("\n=== Network Timeouts ===\n");
    printf("CONNECT_TIMEOUT: %6d ms\n", CONNECT_TIMEOUT);
    printf("READ_TIMEOUT:    %6d ms\n", READ_TIMEOUT);
    printf("IDLE_TIMEOUT:    %6d ms (%d seconds)\n", IDLE_TIMEOUT, IDLE_TIMEOUT / 1000);
}

void print_hardware_registers(void) {
    printf("\n=== Hardware Register Patterns ===\n");
    printf("REG_ENABLE:      0x%02X (binary: 11111111)\n", REG_ENABLE);
    printf("REG_DISABLE:     0x%02X (binary: 00000000)\n", REG_DISABLE);
    printf("REG_PATTERN_A:   0x%02X (binary: 10101010)\n", REG_PATTERN_A);
    printf("REG_PATTERN_B:   0x%02X (binary: 01010101)\n", REG_PATTERN_B);
    printf("REG_HIGH_NIBBLE: 0x%02X (binary: 11110000)\n", REG_HIGH_NIBBLE);
    printf("REG_LOW_NIBBLE:  0x%02X (binary: 00001111)\n", REG_LOW_NIBBLE);
}

void print_colors(void) {
    printf("\n=== Color Constants ===\n");
    printf("COLOR_RED:       0x%08X\n", COLOR_RED);
    printf("COLOR_GREEN:     0x%08X\n", COLOR_GREEN);
    printf("COLOR_BLUE:      0x%08X\n", COLOR_BLUE);
    printf("COLOR_WHITE:     0x%08X\n", COLOR_WHITE);
    printf("COLOR_BLACK:     0x%08X\n", COLOR_BLACK);
}

void print_uuid(void) {
    printf("\n=== UUID-like Constants ===\n");
    printf("UUID: %08X-%04X-%04X\n", UUID_PART1, UUID_PART2, UUID_PART3);
}

/* Demonstrate large number constants */
void print_world_statistics(void) {
    printf("\n=== World Statistics (using _1M, _1B) ===\n");

    long long world_population = 8 * _1B;          /* 8 billion */
    long long internet_users = 5 * _1B;            /* 5 billion */
    long long websites = 1 * _1B + 800 * _1M;      /* 1.8 billion */
    long long emails_per_day = 300 * _1B;          /* 300 billion */

    printf("World population:  %lld\n", world_population);
    printf("Internet users:    %lld\n", internet_users);
    printf("Websites:          %lld\n", websites);
    printf("Emails per day:    %lld\n", emails_per_day);
}

/* Demonstrate binary patterns */
void print_binary_patterns(void) {
    printf("\n=== Binary Patterns ===\n");

    /* Common bit masks */
    int mask_bit0 = _BIN_0001;
    int mask_bit1 = _BIN_0010;
    int mask_bit2 = _BIN_0100;
    int mask_bit3 = _BIN_1000;

    printf("Bit 0 mask: 0x%X (binary: 0001)\n", mask_bit0);
    printf("Bit 1 mask: 0x%X (binary: 0010)\n", mask_bit1);
    printf("Bit 2 mask: 0x%X (binary: 0100)\n", mask_bit2);
    printf("Bit 3 mask: 0x%X (binary: 1000)\n", mask_bit3);

    /* Test bit operations */
    int flags = _BIN_1010;  /* 10 in decimal */
    printf("\nFlags: 0x%X (binary: 1010)\n", flags);
    printf("  Bit 0 set? %s\n", (flags & mask_bit0) ? "yes" : "no");
    printf("  Bit 1 set? %s\n", (flags & mask_bit1) ? "yes" : "no");
    printf("  Bit 2 set? %s\n", (flags & mask_bit2) ? "yes" : "no");
    printf("  Bit 3 set? %s\n", (flags & mask_bit3) ? "yes" : "no");
}

int main(void) {
    printf("=== MetaWare Numeric Literal Separators Examples ===\n\n");

    print_memory_sizes();
    print_timeouts();
    print_hardware_registers();
    print_colors();
    print_uuid();
    print_world_statistics();
    print_binary_patterns();

    printf("\n=== Benefits of Numeric Separators ===\n");
    printf("1. Large numbers are easier to read\n");
    printf("2. Hex constants can be grouped by bytes\n");
    printf("3. Binary constants show bit patterns clearly\n");
    printf("4. Fewer mistakes when transcribing large numbers\n");
    printf("5. Self-documenting (e.g., 1_000_000 vs 1000000)\n");

    printf("\nNOTE: This workaround uses predefined constants.\n");
    printf("True syntax support would allow arbitrary separators:\n");
    printf("  1_234_567_890  (any grouping)\n");
    printf("  0xFF_AA_00     (hex grouping)\n");
    printf("  0b1010_1010    (binary grouping)\n");

    return 0;
}
