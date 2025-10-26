/*
 * Hello World using COBOL Runtime Library
 * Compiled from hello.cob
 */

#include <stdio.h>
#include <string.h>
#include "../../libcobol/cobolrt.h"

int main(void)
{
    /* Initialize COBOL runtime */
    __cobol_init();

    /* WORKING-STORAGE SECTION */
    /* 01 GREETING PIC X(30) VALUE "Hello from COBOL!". */
    char greeting_data[30] = "Hello from COBOL!         ";
    cobol_field_t GREETING = {
        .data = greeting_data,
        .size = 30,
        .type = COB_TYPE_ALPHANUMERIC,
        .digits = 0,
        .scale = 0,
        .sign = 0
    };

    /* 01 COUNTER PIC 9(3) VALUE 0. */
    char counter_data[3] = "000";
    cobol_field_t COUNTER = {
        .data = counter_data,
        .size = 3,
        .type = COB_TYPE_NUMERIC,
        .digits = 3,
        .scale = 0,
        .sign = 0
    };

    /* PROCEDURE DIVISION */
    printf("\n========== COBOL HELLO WORLD ==========\n\n");

    /* DISPLAY GREETING. */
    printf("Executing: DISPLAY GREETING\n");
    printf("Output:    ");
    __cobol_display(&GREETING);
    printf("\n\n");

    /* MOVE 42 TO COUNTER. */
    printf("Executing: MOVE 42 TO COUNTER\n");
    __cobol_set_int(&COUNTER, 42);
    printf("Counter value set to: %d\n\n", __cobol_get_int(&COUNTER));

    /* DISPLAY "Counter: " COUNTER. */
    printf("Executing: DISPLAY \"Counter: \" COUNTER\n");
    printf("Output:    Counter: ");
    __cobol_display(&COUNTER);
    printf("\n\n");

    /* STOP RUN. */
    printf("Executing: STOP RUN\n");
    printf("\n========== PROGRAM COMPLETED ==========\n\n");

    /* Cleanup */
    __cobol_cleanup();

    return 0;
}
