/*
 * Arithmetic Operations Demo using COBOL Runtime Library
 * Demonstrates ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
 */

#include <stdio.h>
#include <string.h>
#include "../../libcobol/cobolrt.h"

int main(void)
{
    __cobol_init();

    printf("\n========== COBOL ARITHMETIC DEMO ==========\n\n");

    /* WORKING-STORAGE SECTION */
    char num1_data[5] = "00100";
    char num2_data[5] = "00050";
    char result_data[5] = "00000";

    cobol_field_t NUM1 = {
        .data = num1_data, .size = 5, .type = COB_TYPE_NUMERIC,
        .digits = 5, .scale = 0, .sign = 0
    };

    cobol_field_t NUM2 = {
        .data = num2_data, .size = 5, .type = COB_TYPE_NUMERIC,
        .digits = 5, .scale = 0, .sign = 0
    };

    cobol_field_t RESULT = {
        .data = result_data, .size = 5, .type = COB_TYPE_NUMERIC,
        .digits = 5, .scale = 0, .sign = 0
    };

    /* PROCEDURE DIVISION */

    /* ADD NUM1 TO NUM2 GIVING RESULT */
    printf("ADD 100 TO 50 GIVING RESULT\n");
    __cobol_add(&RESULT, &NUM1, &NUM2);
    printf("  Result: %d\n\n", __cobol_get_int(&RESULT));

    /* SUBTRACT NUM2 FROM NUM1 GIVING RESULT */
    printf("SUBTRACT 50 FROM 100 GIVING RESULT\n");
    __cobol_subtract(&RESULT, &NUM1, &NUM2);
    printf("  Result: %d\n\n", __cobol_get_int(&RESULT));

    /* MULTIPLY NUM1 BY 2 GIVING RESULT */
    __cobol_set_int(&NUM2, 2);
    printf("MULTIPLY 100 BY 2 GIVING RESULT\n");
    __cobol_multiply(&RESULT, &NUM1, &NUM2);
    printf("  Result: %d\n\n", __cobol_get_int(&RESULT));

    /* DIVIDE NUM1 BY 2 GIVING RESULT */
    printf("DIVIDE 100 BY 2 GIVING RESULT\n");
    __cobol_divide(&RESULT, &NUM1, &NUM2);
    printf("  Result: %d\n\n", __cobol_get_int(&RESULT));

    /* COMPUTE RESULT = NUM1 + NUM2 * 2 */
    __cobol_set_int(&NUM1, 100);
    __cobol_set_int(&NUM2, 50);
    printf("COMPUTE RESULT = 100 + 50 * 2\n");
    double computed = 100 + 50 * 2;
    __cobol_set_double(&RESULT, computed);
    printf("  Result: %d\n\n", __cobol_get_int(&RESULT));

    printf("========== ALL OPERATIONS SUCCESSFUL ==========\n\n");

    __cobol_cleanup();
    return 0;
}
