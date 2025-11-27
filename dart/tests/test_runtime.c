/*
 * Test program for Dart runtime integration
 * This demonstrates the generated code pattern
 */

#include <dart.h>
#include <stdio.h>
#include <stdlib.h>

void _dart_main(void) {
    /* Variable declarations */
    DartObject *x = dart_null();
    DartObject *y = dart_null();
    DartObject *result = dart_null();

    /* Assignments */
    x = dart_int_new(42);
    y = dart_int_new(10);

    /* Binary operation (addition) */
    DartObject *_t0 = NULL;
    _t0 = dart_int_new(dart_int_value(x) + dart_int_value(y));
    result = _t0;

    /* Print result */
    dart_print(result);

    /* Cleanup */
    dart_object_release(x);
    dart_object_release(y);
    dart_object_release(result);
}

int main(int argc, char **argv) {
    dart_runtime_init();
    _dart_main();
    dart_runtime_cleanup();
    return 0;
}
