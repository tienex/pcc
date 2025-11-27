/*
 * Combined MetaWare Syntax Features Example
 *
 * Demonstrates using all three syntax extensions together in a realistic scenario.
 * This shows how these features work together to create more readable,
 * maintainable code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../metaware_syntax.h"

/* ================================================================
 * Configuration System with Labeled Arguments
 * ================================================================ */

DECLARE_LABELED_FUNC(configureServer,
    const char *hostname;
    int port;
    int max_connections;
    int buffer_size;
    int timeout_ms;
    int enable_ssl;
    int log_level;
);

DEFINE_LABELED_FUNC(configureServer) {
    printf("Server Configuration:\n");
    printf("  Hostname: %s:%d\n", args.hostname, args.port);
    printf("  Max connections: %d\n", args.max_connections);
    printf("  Buffer size: %d bytes (%d KB)\n",
           args.buffer_size, args.buffer_size / _1K);
    printf("  Timeout: %d ms\n", args.timeout_ms);
    printf("  SSL: %s\n", args.enable_ssl ? "enabled" : "disabled");
    printf("  Log level: %d\n", args.log_level);
}

/* ================================================================
 * Request Parser with Case Ranges
 * ================================================================ */

typedef enum {
    PARSE_METHOD,
    PARSE_PATH,
    PARSE_VERSION,
    PARSE_DONE,
    PARSE_ERROR
} ParseState;

ParseState parse_http_char(char c, ParseState current_state) {
    switch (current_state) {
        case PARSE_METHOD:
            switch (c) {
                CASE_UPPERCASE:  /* Method names are uppercase */
                    return PARSE_METHOD;
                case ' ':
                    return PARSE_PATH;
                default:
                    return PARSE_ERROR;
            }

        case PARSE_PATH:
            switch (c) {
                CASE_ALNUM:  /* Path can have letters and numbers */
                case '/':
                case '-':
                case '_':
                case '.':
                    return PARSE_PATH;
                case ' ':
                    return PARSE_VERSION;
                default:
                    return PARSE_ERROR;
            }

        case PARSE_VERSION:
            switch (c) {
                CASE_ALNUM:
                case '/':
                case '.':
                    return PARSE_VERSION;
                case '\r':
                case '\n':
                    return PARSE_DONE;
                default:
                    return PARSE_ERROR;
            }

        default:
            return PARSE_ERROR;
    }
}

/* ================================================================
 * Data Processing with Numeric Constants
 * ================================================================ */

DECLARE_LABELED_FUNC(processDataStream,
    const char *source;
    int chunk_size;
    int max_total_size;
    int compression_level;
    int priority;
);

DEFINE_LABELED_FUNC(processDataStream) {
    printf("\nData Stream Processing:\n");
    printf("  Source: %s\n", args.source);
    printf("  Chunk size: %d bytes", args.chunk_size);

    /* Use case ranges to describe chunk size */
    if (IN_RANGE(args.chunk_size, 0, _1K - 1)) {
        printf(" (< 1 KB)\n");
    } else if (IN_RANGE(args.chunk_size, _1K, _1M - 1)) {
        printf(" (%d KB)\n", args.chunk_size / _1K);
    } else if (IN_RANGE(args.chunk_size, _1M, _1B - 1)) {
        printf(" (%d MB)\n", args.chunk_size / _1M);
    } else {
        printf(" (%d GB)\n", args.chunk_size / _1B);
    }

    printf("  Max total: %d MB\n", args.max_total_size / _1M);
    printf("  Compression: level %d\n", args.compression_level);

    /* Priority classification using case ranges */
    printf("  Priority: ");
    switch (args.priority) {
        CASE_RANGE_3(0):  /* 0-2: Low */
            printf("Low\n");
            break;
        CASE_RANGE_3(3):  /* 3-5: Medium */
            printf("Medium\n");
            break;
        CASE_RANGE_4(6):  /* 6-9: High */
            printf("High\n");
            break;
        case 10:
            printf("Critical\n");
            break;
        default:
            printf("Invalid\n");
            break;
    }
}

/* ================================================================
 * Hardware Register Configuration
 * ================================================================ */

DECLARE_LABELED_FUNC(configureHardware,
    unsigned int device_id;
    unsigned int control_reg;
    unsigned int status_mask;
    unsigned int data_pattern;
    int enable;
);

DEFINE_LABELED_FUNC(configureHardware) {
    printf("\nHardware Configuration:\n");
    printf("  Device ID: 0x%04X\n", args.device_id);
    printf("  Control Register: 0x%02X\n", args.control_reg);
    printf("  Status Mask: 0x%02X\n", args.status_mask);
    printf("  Data Pattern: 0x%02X\n", args.data_pattern);
    printf("  Enabled: %s\n", args.enable ? "yes" : "no");

    /* Analyze control register bits */
    printf("  Control bits:\n");
    if (args.control_reg & _BIN_0001) printf("    - Auto-increment enabled\n");
    if (args.control_reg & _BIN_0010) printf("    - Interrupt enabled\n");
    if (args.control_reg & _BIN_0100) printf("    - DMA enabled\n");
    if (args.control_reg & _BIN_1000) printf("    - High-speed mode\n");
}

/* ================================================================
 * Example Application: Web Server Setup
 * ================================================================ */

void setup_development_server(void) {
    printf("=== Development Server Setup ===\n");

    /* Configure with labeled arguments - order doesn't matter! */
    CALL(configureServer,
        .port = 8080,
        .hostname = "localhost",
        .buffer_size = 8 * _1K,      /* 8 KB buffer */
        .max_connections = 100,
        .timeout_ms = 30 * 1000,     /* 30 seconds */
        .enable_ssl = 0,
        .log_level = 3               /* Debug level */
    );

    /* Process incoming data */
    CALL(processDataStream,
        .source = "http://localhost:8080/data",
        .chunk_size = 64 * _1K,      /* 64 KB chunks */
        .max_total_size = 100 * _1M, /* 100 MB max */
        .compression_level = 6,
        .priority = 5                /* Medium priority */
    );
}

void setup_production_server(void) {
    printf("\n=== Production Server Setup ===\n");

    /* Production has different requirements - labeled args make it clear */
    CALL(configureServer,
        .enable_ssl = 1,              /* SSL required in production */
        .hostname = "api.example.com",
        .port = 443,
        .max_connections = 10 * _1K,  /* 10,000 connections */
        .buffer_size = _1M,           /* 1 MB buffer */
        .timeout_ms = 60 * 1000,      /* 60 seconds */
        .log_level = 1                /* Errors only */
    );

    /* Production data processing */
    CALL(processDataStream,
        .priority = 9,                /* High priority */
        .source = "database://prod/stream",
        .max_total_size = 10 * _1B,   /* 10 GB max */
        .chunk_size = _1M,            /* 1 MB chunks */
        .compression_level = 9        /* Max compression */
    );
}

void configure_hardware_device(void) {
    printf("\n=== Hardware Device Configuration ===\n");

    /* Configure network card with binary patterns */
    CALL(configureHardware,
        .device_id = HEX4(AB, CD),
        .control_reg = _BIN_1111,      /* All features on */
        .status_mask = _BIN_11110000,  /* Upper nibble */
        .data_pattern = _BIN_10101010, /* Alternating pattern */
        .enable = 1
    );
}

void test_http_parser(void) {
    printf("\n=== HTTP Request Parser Test ===\n");

    const char *request = "GET /index.html HTTP/1.1\r\n";
    printf("Parsing: \"%s\"\n", request);

    ParseState state = PARSE_METHOD;
    int pos = 0;

    printf("Character analysis:\n");
    for (int i = 0; request[i] != '\0' && i < 25; i++) {
        char c = request[i];

        if (c == '\r' || c == '\n') {
            printf("  [%2d] '\\%c' -> ", i, c == '\r' ? 'r' : 'n');
        } else if (c == ' ') {
            printf("  [%2d] ' ' -> ", i);
        } else {
            printf("  [%2d] '%c' -> ", i, c);
        }

        state = parse_http_char(c, state);

        switch (state) {
            case PARSE_METHOD:  printf("parsing method\n"); break;
            case PARSE_PATH:    printf("parsing path\n"); break;
            case PARSE_VERSION: printf("parsing version\n"); break;
            case PARSE_DONE:    printf("done\n"); break;
            case PARSE_ERROR:   printf("ERROR\n"); break;
        }

        if (state == PARSE_DONE || state == PARSE_ERROR) {
            break;
        }
    }

    printf("Final state: %s\n",
           state == PARSE_DONE ? "SUCCESS" : "ERROR");
}

/* ================================================================
 * Main
 * ================================================================ */

int main(void) {
    printf("==================================================\n");
    printf("  Combined MetaWare Syntax Features Demo\n");
    printf("==================================================\n\n");

    setup_development_server();
    setup_production_server();
    configure_hardware_device();
    test_http_parser();

    printf("\n==================================================\n");
    printf("  Summary of MetaWare Features Used\n");
    printf("==================================================\n");
    printf("\n1. LABELED ARGUMENTS:\n");
    printf("   - Arguments can be in any order\n");
    printf("   - Self-documenting code\n");
    printf("   - Compare production vs development configs\n");
    printf("\n2. NUMERIC SEPARATORS:\n");
    printf("   - _1K, _1M, _1B for readable sizes\n");
    printf("   - HEX4, HEX8 for grouped hex constants\n");
    printf("   - _BIN_xxxx for binary patterns\n");
    printf("\n3. CASE RANGES:\n");
    printf("   - CASE_UPPERCASE, CASE_LOWERCASE, CASE_DIGIT\n");
    printf("   - CASE_ALNUM for alphanumeric characters\n");
    printf("   - Custom ranges with CASE_RANGE_n macros\n");
    printf("   - IN_RANGE for runtime range checks\n");

    printf("\n==================================================\n");
    printf("  Why These Features Matter\n");
    printf("==================================================\n");
    printf("\nThese MetaWare extensions from 1989 pioneered concepts\n");
    printf("that modern languages now consider essential:\n\n");
    printf("• Labeled arguments (Python kwargs, Swift, etc.)\n");
    printf("• Numeric separators (C++14, C23, Rust, etc.)\n");
    printf("• Case ranges (GCC extension, Rust, Swift, etc.)\n\n");
    printf("MetaWare High C was truly ahead of its time!\n");

    return 0;
}
