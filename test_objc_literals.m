/*
 * Objective-C Literals Test Suite
 * Tests all @ literal syntax added to PCC
 *
 * Compile with: pcc -fobjc-arc test_objc_literals.m -o test_objc_literals -lobjc
 */

#import <Foundation/Foundation.h>

/* ============================================================================
 * Test 1: NSString Literals
 * ============================================================================ */

void test_string_literals(void) {
    printf("\n=== Testing NSString Literals ===\n");

    // Basic string literal
    NSString *str1 = @"Hello, World!";
    printf("String 1: %s\n", [str1 UTF8String]);

    // Empty string
    NSString *str2 = @"";
    printf("String 2 (empty): '%s'\n", [str2 UTF8String]);

    // String with escape sequences
    NSString *str3 = @"Line 1\nLine 2\tTabbed";
    printf("String 3:\n%s\n", [str3 UTF8String]);

    // String with unicode
    NSString *str4 = @"Hello 世界 🌍";
    printf("String 4 (unicode): %s\n", [str4 UTF8String]);

    // String concatenation still uses traditional syntax
    NSString *str5 = @"Hello, ";
    NSString *str6 = @"PCC!";
    NSString *combined = [str5 stringByAppendingString:str6];
    printf("Combined: %s\n", [combined UTF8String]);
}

/* ============================================================================
 * Test 2: NSNumber Integer Literals
 * ============================================================================ */

void test_number_integer_literals(void) {
    printf("\n=== Testing NSNumber Integer Literals ===\n");

    // Decimal integer
    NSNumber *num1 = @42;
    printf("Decimal: %lld\n", [num1 longLongValue]);

    // Negative integer
    NSNumber *num2 = @-17;
    printf("Negative: %lld\n", [num2 longLongValue]);

    // Octal integer
    NSNumber *num3 = @0755;
    printf("Octal (0755): %lld\n", [num3 longLongValue]);

    // Hexadecimal integer
    NSNumber *num4 = @0xFF;
    printf("Hexadecimal (0xFF): %lld\n", [num4 longLongValue]);

    // Binary integer (if supported)
    NSNumber *num5 = @0b1010;
    printf("Binary (0b1010): %lld\n", [num5 longLongValue]);

    // Large integer
    NSNumber *num6 = @9223372036854775807LL;
    printf("Large int: %lld\n", [num6 longLongValue]);

    // Unsigned integer
    NSNumber *num7 = @42U;
    printf("Unsigned: %llu\n", [num7 unsignedLongLongValue]);
}

/* ============================================================================
 * Test 3: NSNumber Floating Point Literals
 * ============================================================================ */

void test_number_float_literals(void) {
    printf("\n=== Testing NSNumber Float Literals ===\n");

    // Simple float
    NSNumber *num1 = @3.14;
    printf("Float: %f\n", [num1 doubleValue]);

    // Negative float
    NSNumber *num2 = @-2.718;
    printf("Negative float: %f\n", [num2 doubleValue]);

    // Scientific notation
    NSNumber *num3 = @1.23e-4;
    printf("Scientific: %e\n", [num3 doubleValue]);

    // Very small number
    NSNumber *num4 = @0.000001;
    printf("Small: %f\n", [num4 doubleValue]);

    // Very large number
    NSNumber *num5 = @1e10;
    printf("Large: %e\n", [num5 doubleValue]);

    // Float suffix
    NSNumber *num6 = @3.14f;
    printf("Float suffix: %f\n", [num6 floatValue]);
}

/* ============================================================================
 * Test 4: NSNumber Boolean Literals
 * ============================================================================ */

void test_bool_literals(void) {
    printf("\n=== Testing Boolean Literals ===\n");

    // YES literal
    NSNumber *yes = @YES;
    printf("@YES: %s\n", [yes boolValue] ? "true" : "false");
    printf("@YES intValue: %lld\n", [yes longLongValue]);

    // NO literal
    NSNumber *no = @NO;
    printf("@NO: %s\n", [no boolValue] ? "true" : "false");
    printf("@NO intValue: %lld\n", [no longLongValue]);

    // Comparison
    if ([yes boolValue] && ![no boolValue]) {
        printf("Boolean logic works correctly\n");
    }
}

/* ============================================================================
 * Test 5: NSNumber Character Literals
 * ============================================================================ */

void test_char_literals(void) {
    printf("\n=== Testing Character Literals ===\n");

    // Simple character
    NSNumber *ch1 = @'A';
    printf("Character 'A': %c (value: %lld)\n",
           (char)[ch1 longLongValue], [ch1 longLongValue]);

    // Lowercase
    NSNumber *ch2 = @'z';
    printf("Character 'z': %c (value: %lld)\n",
           (char)[ch2 longLongValue], [ch2 longLongValue]);

    // Digit
    NSNumber *ch3 = @'5';
    printf("Character '5': %c (value: %lld)\n",
           (char)[ch3 longLongValue], [ch3 longLongValue]);

    // Special characters
    NSNumber *ch4 = @'\n';
    printf("Newline character value: %lld\n", [ch4 longLongValue]);

    NSNumber *ch5 = @'\t';
    printf("Tab character value: %lld\n", [ch5 longLongValue]);
}

/* ============================================================================
 * Test 6: Boxed Expressions @(expr)
 * ============================================================================ */

void test_boxed_expressions(void) {
    printf("\n=== Testing Boxed Expressions ===\n");

    int x = 10;
    int y = 20;

    // Box a variable
    NSNumber *boxedX = @(x);
    printf("Boxed x (%d): %lld\n", x, [boxedX longLongValue]);

    // Box an expression
    NSNumber *sum = @(x + y);
    printf("Boxed (x + y): %lld\n", [sum longLongValue]);

    // Box a complex expression
    NSNumber *result = @(x * y + 5);
    printf("Boxed (x * y + 5): %lld\n", [result longLongValue]);

    // Box a float expression
    double pi = 3.14159;
    NSNumber *boxedPi = @(pi);
    printf("Boxed pi: %f\n", [boxedPi doubleValue]);

    // Box a comparison (boolean)
    NSNumber *comparison = @(x < y);
    printf("Boxed (x < y): %s\n", [comparison boolValue] ? "true" : "false");

    // Box a function call result
    NSNumber *boxedLength = @(strlen("Hello"));
    printf("Boxed strlen: %lld\n", [boxedLength longLongValue]);
}

/* ============================================================================
 * Test 7: Array Literals @[...]
 * ============================================================================ */

void test_array_literals(void) {
    printf("\n=== Testing Array Literals ===\n");

    // Empty array
    NSArray *empty = @[];
    printf("Empty array count: %lu\n", (unsigned long)[empty count]);

    // Array of numbers
    NSArray *numbers = @[@1, @2, @3, @4, @5];
    printf("Numbers array count: %lu\n", (unsigned long)[numbers count]);
    for (NSNumber *num in numbers) {
        printf("  %lld\n", [num longLongValue]);
    }

    // Array of strings
    NSArray *strings = @[@"Hello", @"World", @"PCC"];
    printf("Strings array count: %lu\n", (unsigned long)[strings count]);
    for (NSString *str in strings) {
        printf("  %s\n", [str UTF8String]);
    }

    // Mixed array
    NSArray *mixed = @[@42, @"Answer", @3.14, @YES];
    printf("Mixed array count: %lu\n", (unsigned long)[mixed count]);
    for (id obj in mixed) {
        printf("  Object: %s\n", [[obj description] UTF8String]);
    }

    // Nested arrays
    NSArray *nested = @[@[@1, @2], @[@3, @4], @[@5, @6]];
    printf("Nested array count: %lu\n", (unsigned long)[nested count]);

    // Array with expressions
    int a = 10, b = 20;
    NSArray *withExpr = @[@(a), @(b), @(a + b)];
    printf("Array with expressions:\n");
    for (NSNumber *num in withExpr) {
        printf("  %lld\n", [num longLongValue]);
    }
}

/* ============================================================================
 * Test 8: Dictionary Literals @{...}
 * ============================================================================ */

void test_dictionary_literals(void) {
    printf("\n=== Testing Dictionary Literals ===\n");

    // Empty dictionary
    NSDictionary *empty = @{};
    printf("Empty dictionary count: %lu\n", (unsigned long)[empty count]);

    // Simple dictionary
    NSDictionary *dict1 = @{
        @"name": @"PCC",
        @"version": @"1.2.0",
        @"language": @"Objective-C"
    };
    printf("Dictionary 1 count: %lu\n", (unsigned long)[dict1 count]);
    printf("  name: %s\n", [[dict1 objectForKey:@"name"] UTF8String]);
    printf("  version: %s\n", [[dict1 objectForKey:@"version"] UTF8String]);

    // Dictionary with number values
    NSDictionary *dict2 = @{
        @"one": @1,
        @"two": @2,
        @"three": @3
    };
    printf("\nDictionary 2 (numbers):\n");
    for (NSString *key in dict2) {
        NSNumber *value = [dict2 objectForKey:key];
        printf("  %s: %lld\n", [key UTF8String], [value longLongValue]);
    }

    // Mixed types
    NSDictionary *dict3 = @{
        @"count": @42,
        @"name": @"Test",
        @"pi": @3.14,
        @"active": @YES
    };
    printf("\nDictionary 3 (mixed types):\n");
    for (NSString *key in dict3) {
        id value = [dict3 objectForKey:key];
        printf("  %s: %s\n", [key UTF8String],
               [[value description] UTF8String]);
    }

    // Nested dictionaries
    NSDictionary *nested = @{
        @"person": @{
            @"name": @"John",
            @"age": @30
        },
        @"address": @{
            @"city": @"New York",
            @"zip": @"10001"
        }
    };
    printf("\nNested dictionary count: %lu\n", (unsigned long)[nested count]);
}

/* ============================================================================
 * Test 9: Complex Combinations
 * ============================================================================ */

void test_complex_combinations(void) {
    printf("\n=== Testing Complex Combinations ===\n");

    // Array of dictionaries
    NSArray *arrayOfDicts = @[
        @{@"name": @"Alice", @"age": @25},
        @{@"name": @"Bob", @"age": @30},
        @{@"name": @"Charlie", @"age": @35}
    ];

    printf("Array of dictionaries:\n");
    for (NSDictionary *person in arrayOfDicts) {
        printf("  %s is %lld years old\n",
               [[person objectForKey:@"name"] UTF8String],
               [[person objectForKey:@"age"] longLongValue]);
    }

    // Dictionary with array values
    NSDictionary *dictWithArrays = @{
        @"numbers": @[@1, @2, @3],
        @"colors": @[@"red", @"green", @"blue"],
        @"flags": @[@YES, @NO, @YES]
    };

    printf("\nDictionary with array values:\n");
    NSArray *numbers = [dictWithArrays objectForKey:@"numbers"];
    printf("  Numbers count: %lu\n", (unsigned long)[numbers count]);

    // Boxed expressions in arrays
    int x = 5, y = 10;
    NSArray *calculations = @[
        @(x + y),
        @(x * y),
        @(y - x),
        @(y / x)
    ];

    printf("\nCalculations array:\n");
    printf("  x + y = %lld\n", [[calculations objectAtIndex:0] longLongValue]);
    printf("  x * y = %lld\n", [[calculations objectAtIndex:1] longLongValue]);
    printf("  y - x = %lld\n", [[calculations objectAtIndex:2] longLongValue]);
    printf("  y / x = %lld\n", [[calculations objectAtIndex:3] longLongValue]);
}

/* ============================================================================
 * Test 10: Practical Examples
 * ============================================================================ */

void test_practical_examples(void) {
    printf("\n=== Testing Practical Examples ===\n");

    // Configuration dictionary
    NSDictionary *config = @{
        @"debug": @YES,
        @"timeout": @30,
        @"server": @"localhost",
        @"port": @8080,
        @"retries": @3
    };

    printf("Configuration:\n");
    printf("  Debug mode: %s\n",
           [[config objectForKey:@"debug"] boolValue] ? "enabled" : "disabled");
    printf("  Server: %s:%lld\n",
           [[config objectForKey:@"server"] UTF8String],
           [[config objectForKey:@"port"] longLongValue]);
    printf("  Timeout: %lld seconds\n",
           [[config objectForKey:@"timeout"] longLongValue]);

    // Data points array
    NSArray *dataPoints = @[@0.0, @0.5, @1.0, @1.5, @2.0];
    printf("\nData points:\n");
    for (NSNumber *point in dataPoints) {
        printf("  %f\n", [point doubleValue]);
    }

    // JSON-like structure
    NSDictionary *user = @{
        @"id": @12345,
        @"username": @"pccuser",
        @"active": @YES,
        @"score": @98.5,
        @"tags": @[@"developer", @"tester", @"admin"],
        @"preferences": @{
            @"theme": @"dark",
            @"notifications": @YES
        }
    };

    printf("\nUser profile:\n");
    printf("  ID: %lld\n", [[user objectForKey:@"id"] longLongValue]);
    printf("  Username: %s\n",
           [[user objectForKey:@"username"] UTF8String]);
    printf("  Active: %s\n",
           [[user objectForKey:@"active"] boolValue] ? "yes" : "no");
    printf("  Score: %.1f\n",
           [[user objectForKey:@"score"] doubleValue]);
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        printf("====================================\n");
        printf("Objective-C Literals Test Suite\n");
        printf("Testing PCC @ Literal Support\n");
        printf("====================================\n");

        test_string_literals();
        test_number_integer_literals();
        test_number_float_literals();
        test_bool_literals();
        test_char_literals();
        test_boxed_expressions();
        test_array_literals();
        test_dictionary_literals();
        test_complex_combinations();
        test_practical_examples();

        printf("\n====================================\n");
        printf("All literal tests completed!\n");
        printf("====================================\n");
    }

    return 0;
}
