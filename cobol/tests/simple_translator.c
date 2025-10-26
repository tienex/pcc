/*
 * Simple COBOL to C translator for hello world
 * This is a simplified version for demonstration
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void)
{
	/* Initialize COBOL runtime */
	printf("/* Generated from COBOL */\n\n");
	printf("#include <stdio.h>\n");
	printf("#include <string.h>\n");
	printf("#include \"cobol.h\"\n\n");

	printf("int main(void)\n");
	printf("{\n");
	printf("    /* WORKING-STORAGE SECTION */\n");
	printf("    char GREETING[31] = \"Hello from COBOL!              \";\n");
	printf("    char COUNTER[4] = \"000\";\n\n");

	printf("    /* PROCEDURE DIVISION */\n");
	printf("    /* DISPLAY GREETING */\n");
	printf("    printf(\"%%s\\n\", GREETING);\n\n");

	printf("    /* MOVE 42 TO COUNTER */\n");
	printf("    sprintf(COUNTER, \"%%03d\", 42);\n\n");

	printf("    /* DISPLAY \"Counter: \" COUNTER */\n");
	printf("    printf(\"Counter: %%s\\n\", COUNTER);\n\n");

	printf("    /* STOP RUN */\n");
	printf("    return 0;\n");
	printf("}\n");

	return 0;
}
