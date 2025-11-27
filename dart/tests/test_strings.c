/*
 * Test string operations with Dart runtime
 */

#include <dart.h>
#include <stdio.h>

void _dart_main(void) {
    /* Create strings */
    DartObject *greeting = (DartObject *)dart_string_new("Hello");
    DartObject *world = (DartObject *)dart_string_new(" World!");

    /* Concatenate */
    DartObject *message = (DartObject *)dart_string_concat(
        (DartString *)greeting,
        (DartString *)world
    );

    /* Print */
    dart_print(message);

    /* Cleanup */
    dart_object_release(greeting);
    dart_object_release(world);
    dart_object_release(message);
}

int main(int argc, char **argv) {
    dart_runtime_init();
    _dart_main();
    dart_runtime_cleanup();
    return 0;
}
