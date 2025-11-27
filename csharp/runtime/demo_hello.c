/*
 * Hello World Demo using C# Runtime Library
 * This demonstrates what compiled C# code would look like
 */

#include <stdio.h>
#include "include/csruntime.h"

int main() {
	printf("\n========== C# Runtime Hello World Demo ==========\n\n");

	/* Initialize the C# runtime */
	struct cs_runtime_config config = {
		.initial_heap_size = 1024 * 1024,
		.max_heap_size = 1024 * 1024 * 1024,
		.enable_gc = 1,
		.enable_arc = 1,
		.gc_threshold = 512 * 1024,
		.debug_mode = 0,
	};

	CSRuntime_Init(&config);

	/* Simulate: string greeting = "Hello, World!"; */
	CSString *greeting = CS_String_Create("Hello, World!");
	printf("Created string: '%s'\n", CS_String_ToUTF8(greeting));
	printf("String length: %d characters\n", CS_String_Length(greeting));

	/* Simulate: string name = "C# Runtime"; */
	CSString *name = CS_String_Create("C# Runtime");

	/* Simulate: string message = greeting + " from " + name; */
	CSString *from = CS_String_Create(" from ");
	CSString *temp = CS_String_Concat(greeting, from);
	CSString *message = CS_String_Concat(temp, name);

	printf("\nConcatenated message: '%s'\n", CS_String_ToUTF8(message));

	/* Simulate: Console.WriteLine(message.ToUpper()); */
	CSString *upper = CS_String_ToUpper(message);
	printf("Uppercase: '%s'\n", CS_String_ToUTF8(upper));

	/* Simulate: Console.WriteLine(message.ToLower()); */
	CSString *lower = CS_String_ToLower(message);
	printf("Lowercase: '%s'\n", CS_String_ToUTF8(lower));

	/* Demonstrate reference counting */
	printf("\n--- Reference Counting Demo ---\n");
	printf("Initial retain of greeting\n");
	CS_Retain((CSObject *)greeting);
	printf("Releasing greeting twice (back to ref count 1)\n");
	CS_Release((CSObject *)greeting);
	CS_Release((CSObject *)greeting);

	/* Clean up */
	CS_Release((CSObject *)name);
	CS_Release((CSObject *)from);
	CS_Release((CSObject *)temp);
	CS_Release((CSObject *)message);
	CS_Release((CSObject *)upper);
	CS_Release((CSObject *)lower);

	/* Print runtime statistics */
	printf("\n--- Runtime Statistics ---\n");
	CS_ARC_PrintStats();
	CS_Memory_PrintStats();

	/* Shutdown runtime */
	CSRuntime_Shutdown();

	printf("\n========== Demo Complete ==========\n\n");
	return 0;
}
