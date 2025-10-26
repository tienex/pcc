/*
 * Test collections with Dart runtime
 */

#include <dart.h>
#include <stdio.h>

void _dart_main(void) {
    /* Create a list */
    DartList *numbers = dart_list_new();

    /* Add numbers */
    dart_list_add(numbers, dart_int_new(10));
    dart_list_add(numbers, dart_int_new(20));
    dart_list_add(numbers, dart_int_new(30));

    /* Print list info */
    printf("List length: %zu\n", dart_list_length(numbers));

    /* Print elements */
    for (size_t i = 0; i < dart_list_length(numbers); i++) {
        DartObject *elem = dart_list_get(numbers, i);
        printf("  [%zu] = %lld\n", i, dart_int_value(elem));
    }

    /* Cleanup */
    dart_object_release((DartObject *)numbers);
}

int main(int argc, char **argv) {
    dart_runtime_init();
    _dart_main();
    dart_runtime_cleanup();
    return 0;
}
