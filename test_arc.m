/*
 * ARC (Automatic Reference Counting) Test File
 * Demonstrates generalized ARC support in PCC
 *
 * Compile with: pcc -fobjc-arc test_arc.m -o test_arc -lobjc
 */

#import <Foundation/Foundation.h>

/* ============================================================================
 * Example 1: Basic ARC with Strong References
 * ============================================================================ */

@interface Counter : NSObject {
    int count;
}
- (void)increment;
- (int)getCount;
@end

@implementation Counter
- (instancetype)init {
    if ((self = [super init])) {
        count = 0;
    }
    return self;  // ARC handles retain/autorelease
}

- (void)increment {
    count++;
}

- (int)getCount {
    return count;
}
@end

void test_strong_references(void) {
    printf("Testing strong references...\n");

    // Strong reference (default under ARC)
    __strong Counter *counter = [[Counter alloc] init];

    // No manual retain needed - ARC inserts it
    [counter increment];
    [counter increment];

    printf("Counter value: %d\n", [counter getCount]);

    // No manual release needed - ARC inserts it at scope exit
}

/* ============================================================================
 * Example 2: Weak References to Avoid Retain Cycles
 * ============================================================================ */

@interface Parent : NSObject
@property (strong) NSString *name;
@property (strong) NSMutableArray *children;
@end

@interface Child : NSObject
@property (strong) NSString *name;
@property (weak) Parent *parent;  // Weak to avoid retain cycle
@end

@implementation Parent
- (instancetype)initWithName:(NSString *)n {
    if ((self = [super init])) {
        _name = n;
        _children = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)addChild:(Child *)child {
    [_children addObject:child];  // Strong reference in array
    child.parent = self;          // Weak reference from child to parent
}
@end

@implementation Child
- (instancetype)initWithName:(NSString *)n {
    if ((self = [super init])) {
        _name = n;
    }
    return self;
}
@end

void test_weak_references(void) {
    printf("\nTesting weak references...\n");

    Parent *parent = [[Parent alloc] initWithName:@"Parent"];

    for (int i = 0; i < 3; i++) {
        NSString *name = [NSString stringWithFormat:@"Child %d", i];
        Child *child = [[Child alloc] initWithName:name];
        [parent addChild:child];
        printf("Added child: %s\n", [name UTF8String]);
    }

    printf("Parent has %lu children\n", (unsigned long)[parent.children count]);

    // No retain cycle - children have weak reference to parent
    // All objects properly cleaned up at scope exit
}

/* ============================================================================
 * Example 3: Unsafe Unretained References
 * ============================================================================ */

void test_unsafe_unretained(void) {
    printf("\nTesting unsafe unretained references...\n");

    Counter *counter = [[Counter alloc] init];

    // Unsafe unretained - no retain, not zeroed on dealloc
    __unsafe_unretained Counter *unsafeRef = counter;

    [counter increment];
    [counter increment];

    // Use unsafeRef while counter is still alive
    printf("Count via unsafe ref: %d\n", [unsafeRef getCount]);

    // After counter is released, unsafeRef becomes dangling
    // (This is why it's "unsafe")
}

/* ============================================================================
 * Example 4: Autoreleasing
 * ============================================================================ */

- (Counter *)createCounterWithValue:(int)value __autoreleasing {
    Counter *counter = [[Counter alloc] init];
    for (int i = 0; i < value; i++) {
        [counter increment];
    }
    // ARC inserts autorelease here for __autoreleasing return
    return counter;
}

void test_autoreleasing(void) {
    printf("\nTesting autoreleasing...\n");

    @autoreleasepool {
        // Counter is autoreleased, then retained by strong assignment
        Counter *counter = [self createCounterWithValue:5];
        printf("Counter value: %d\n", [counter getCount]);
        // counter is released at end of autoreleasepool
    }
}

/* ============================================================================
 * Example 5: Bridging with Core Foundation
 * ============================================================================ */

#if __has_include(<CoreFoundation/CoreFoundation.h>)
#import <CoreFoundation/CoreFoundation.h>

void test_bridging(void) {
    printf("\nTesting CF bridging...\n");

    // Objective-C string (ARC managed)
    NSString *nsStr = @"Hello, World!";

    // Bridge to Core Foundation (no ownership transfer)
    CFStringRef cfStr1 = (__bridge CFStringRef)nsStr;
    printf("CF String (bridged): %s\n",
           CFStringGetCStringPtr(cfStr1, kCFStringEncodingUTF8));
    // nsStr still owned by ARC - no CFRelease needed for cfStr1

    // Transfer ownership from ARC to CF
    NSString *nsStr2 = @"Transferred";
    CFStringRef cfStr2 = (__bridge_transfer CFStringRef)nsStr2;
    // Must manually CFRelease(cfStr2)
    printf("CF String (transferred): %s\n",
           CFStringGetCStringPtr(cfStr2, kCFStringEncodingUTF8));
    CFRelease(cfStr2);

    // Transfer ownership from CF to ARC
    CFStringRef cfStr3 = CFStringCreateWithCString(NULL, "Retained",
                                                   kCFStringEncodingUTF8);
    NSString *nsStr3 = (__bridge_retained NSString *)cfStr3;
    // ARC now owns nsStr3, will release automatically
    printf("NS String (retained): %s\n", [nsStr3 UTF8String]);
    CFRelease(cfStr3);  // Release original CF ownership
    // nsStr3 released automatically by ARC
}
#endif

/* ============================================================================
 * Example 6: ARC with Properties
 * ============================================================================ */

@interface Vehicle : NSObject
@property (strong, nonatomic) NSString *make;
@property (strong, nonatomic) NSString *model;
@property (weak, nonatomic) id owner;  // Weak to avoid cycle
@end

@implementation Vehicle
@synthesize make = _make;
@synthesize model = _model;
@synthesize owner = _owner;

- (instancetype)initWithMake:(NSString *)mk model:(NSString *)mdl {
    if ((self = [super init])) {
        _make = mk;    // ARC handles retain
        _model = mdl;  // ARC handles retain
    }
    return self;
}

- (NSString *)description {
    return [NSString stringWithFormat:@"%@ %@", _make, _model];
}
@end

void test_properties(void) {
    printf("\nTesting properties with ARC...\n");

    Vehicle *car = [[Vehicle alloc] initWithMake:@"Tesla" model:@"Model 3"];
    printf("Vehicle: %s\n", [[car description] UTF8String]);

    // Properties are automatically retained/released by ARC
    car.make = @"Ford";
    car.model = @"Mustang";
    printf("Updated vehicle: %s\n", [[car description] UTF8String]);

    // car and its properties automatically released at scope exit
}

/* ============================================================================
 * Example 7: ARC with Exception Handling
 * ============================================================================ */

void test_exceptions(void) {
    printf("\nTesting ARC with exceptions...\n");

    @try {
        Counter *counter = [[Counter alloc] init];
        [counter increment];

        // Simulate error
        @throw [NSException exceptionWithName:@"TestException"
                                      reason:@"Testing exception handling"
                                    userInfo:nil];

        // This line won't execute
        [counter increment];
    }
    @catch (NSException *e) {
        printf("Caught exception: %s\n", [[e name] UTF8String]);
        // counter is properly released even though exception was thrown
    }
    @finally {
        printf("Finally block executed\n");
    }
}

/* ============================================================================
 * Example 8: Collections and ARC
 * ============================================================================ */

void test_collections(void) {
    printf("\nTesting collections with ARC...\n");

    NSMutableArray *array = [[NSMutableArray alloc] init];

    for (int i = 0; i < 5; i++) {
        Counter *counter = [[Counter alloc] init];
        for (int j = 0; j <= i; j++) {
            [counter increment];
        }
        [array addObject:counter];  // Array retains counter
        // counter released at end of iteration, but array keeps it alive
    }

    printf("Array has %lu counters\n", (unsigned long)[array count]);

    // Enumerate array
    for (Counter *counter in array) {
        printf("Counter value: %d\n", [counter getCount]);
    }

    // array and all counters released at scope exit
}

/* ============================================================================
 * Example 9: Optimizations
 * ============================================================================ */

- (Counter *)getCounter {
    Counter *counter = [[Counter alloc] init];
    [counter increment];
    return counter;  // ARC optimizes: no retain+autorelease
}

void test_optimizations(void) {
    printf("\nTesting ARC optimizations...\n");

    // ARC optimizes the return value handling
    Counter *counter = [self getCounter];
    printf("Counter value: %d\n", [counter getCount]);

    // Multiple assignments optimized
    Counter *c1 = [[Counter alloc] init];
    Counter *c2 = c1;  // No retain/release
    Counter *c3 = c2;  // No retain/release

    [c1 increment];
    printf("All references point to same counter: %d\n", [c3 getCount]);
}

/* ============================================================================
 * Example 10: Memory Management Statistics
 * ============================================================================ */

void print_arc_statistics(void) {
    printf("\n" "=" "=");
    printf(" ARC Statistics ");
    printf("=" "=");
    printf("\n");

#ifdef __OBJC_ARC__
    printf("ARC is ENABLED\n");
    printf("Ownership qualifiers:\n");
    printf("  - __strong: default, retains objects\n");
    printf("  - __weak: weak reference, prevents cycles\n");
    printf("  - __unsafe_unretained: no retain, not zeroed\n");
    printf("  - __autoreleasing: autorelease on assignment\n");
#else
    printf("ARC is DISABLED (manual memory management)\n");
#endif
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        printf("PCC Generalized ARC Library Test Suite\n");
        printf("=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=\n\n");

        print_arc_statistics();

        test_strong_references();
        test_weak_references();
        test_unsafe_unretained();
        test_autoreleasing();

#if __has_include(<CoreFoundation/CoreFoundation.h>)
        test_bridging();
#else
        printf("\nSkipping CF bridging test (CoreFoundation not available)\n");
#endif

        test_properties();
        test_exceptions();
        test_collections();
        test_optimizations();

        printf("\n" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=\n");
        printf("All ARC tests completed successfully!\n");
    }

    return 0;
}
