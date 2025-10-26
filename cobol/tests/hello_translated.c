/* Generated from COBOL */

#include <stdio.h>
#include <string.h>
#include "cobol.h"

int main(void)
{
    /* WORKING-STORAGE SECTION */
    char GREETING[31] = "Hello from COBOL!              ";
    char COUNTER[4] = "000";

    /* PROCEDURE DIVISION */
    /* DISPLAY GREETING */
    printf("%s\n", GREETING);

    /* MOVE 42 TO COUNTER */
    sprintf(COUNTER, "%03d", 42);

    /* DISPLAY "Counter: " COUNTER */
    printf("Counter: %s\n", COUNTER);

    /* STOP RUN */
    return 0;
}
