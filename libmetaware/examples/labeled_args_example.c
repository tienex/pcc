/*
 * MetaWare Labeled Arguments Example
 *
 * Demonstrates the use of labeled/named arguments (keyword arguments)
 * using the preprocessor-based workaround.
 *
 * In real MetaWare High C, you would write:
 *   drawRect(width => 200, height => 100, x => 50, y => 75, color => 0xFF0000);
 *
 * With our workaround, you write:
 *   CALL(drawRect, .width = 200, .height = 100, .x = 50, .y = 75, .color = 0xFF0000);
 */

#include <stdio.h>
#include "../metaware_syntax.h"

/* Declare a GUI drawing function with labeled arguments */
DECLARE_LABELED_FUNC(drawRect,
    int x;
    int y;
    int width;
    int height;
    int color;
    int border_width;
    int filled;
);

DEFINE_LABELED_FUNC(drawRect) {
    printf("Drawing rectangle:\n");
    printf("  Position: (%d, %d)\n", args.x, args.y);
    printf("  Size: %dx%d\n", args.width, args.height);
    printf("  Color: #%06X\n", args.color);
    printf("  Border: %dpx\n", args.border_width);
    printf("  Filled: %s\n", args.filled ? "yes" : "no");
}

/* HTTP request builder with labeled arguments */
DECLARE_LABELED_FUNC(httpRequest,
    const char *method;
    const char *url;
    const char *content_type;
    const char *body;
    int timeout;
    int follow_redirects;
);

DEFINE_LABELED_FUNC(httpRequest) {
    printf("\nHTTP Request:\n");
    printf("  %s %s\n", args.method, args.url);
    printf("  Content-Type: %s\n", args.content_type);
    printf("  Timeout: %d seconds\n", args.timeout);
    printf("  Follow redirects: %s\n", args.follow_redirects ? "yes" : "no");
    if (args.body) {
        printf("  Body: %s\n", args.body);
    }
}

/* Database connection with labeled arguments */
DECLARE_LABELED_FUNC(dbConnect,
    const char *host;
    int port;
    const char *database;
    const char *username;
    const char *password;
    int pool_size;
    int ssl;
);

DEFINE_LABELED_FUNC(dbConnect) {
    printf("\nDatabase Connection:\n");
    printf("  Host: %s:%d\n", args.host, args.port);
    printf("  Database: %s\n", args.database);
    printf("  User: %s\n", args.username);
    printf("  Pool size: %d\n", args.pool_size);
    printf("  SSL: %s\n", args.ssl ? "enabled" : "disabled");
}

int main(void) {
    printf("=== MetaWare Labeled Arguments Examples ===\n\n");

    /* Arguments can be specified in any order! */
    CALL(drawRect,
        .width = 200,
        .height = 100,
        .x = 50,
        .y = 75,
        .color = 0xFF0000,
        .border_width = 2,
        .filled = 1
    );

    /* Different order, same result */
    CALL(drawRect,
        .filled = 0,
        .border_width = 5,
        .color = 0x00FF00,
        .height = 150,
        .width = 300,
        .y = 10,
        .x = 20
    );

    /* HTTP request example */
    CALL(httpRequest,
        .method = "POST",
        .url = "https://api.example.com/data",
        .content_type = "application/json",
        .body = "{\"name\": \"test\"}",
        .timeout = 30,
        .follow_redirects = 1
    );

    /* Database connection example */
    CALL(dbConnect,
        .host = "localhost",
        .port = 5432,
        .database = "myapp",
        .username = "admin",
        .password = "secret",
        .pool_size = 10,
        .ssl = 1
    );

    printf("\n=== Benefits of Labeled Arguments ===\n");
    printf("1. Arguments can be in any order\n");
    printf("2. Self-documenting code (argument names visible at call site)\n");
    printf("3. Easier to maintain (adding arguments doesn't break order)\n");
    printf("4. Prevents argument confusion in functions with many parameters\n");

    return 0;
}
