/*
 * Simple Objective-C test file for PCC
 * Tests basic Objective-C 1.0 and 2.0 syntax support
 */

#import <objc/Object.h>

/* Objective-C 1.0: Interface declaration */
@interface MyClass : Object
{
    @private
    int value;
    id delegate;
}

/* Objective-C 1.0: Method declarations */
- (id)init;
- (void)setValue:(int)newValue;
- (int)getValue;
+ (id)createWithValue:(int)val;

@end

/* Objective-C 1.0: Protocol declaration */
@protocol MyProtocol
- (void)requiredMethod;
@optional
- (void)optionalMethod;
@required
- (int)anotherRequiredMethod;
@end

/* Objective-C 1.0: Implementation */
@implementation MyClass

- (id)init {
    self = [super init];
    if (self) {
        value = 0;
        delegate = 0;
    }
    return self;
}

- (void)setValue:(int)newValue {
    value = newValue;
}

- (int)getValue {
    return value;
}

+ (id)createWithValue:(int)val {
    id obj = [[self alloc] init];
    [obj setValue:val];
    return obj;
}

@end

/* Objective-C 2.0: Properties */
@interface ModernClass : Object {
    int count;
    id name;
}

@property (nonatomic, assign) int count;
@property (nonatomic, retain) id name;

- (void)doSomething;

@end

@implementation ModernClass

@synthesize count;
@synthesize name;

- (void)doSomething {
    /* Objective-C 2.0: Exception handling */
    @try {
        if (count < 0) {
            @throw [NSException exceptionWithName:@"InvalidCount"
                                          reason:@"Count cannot be negative"
                                        userInfo:0];
        }
        count++;
    }
    @catch (NSException *e) {
        count = 0;
    }
    @finally {
        /* Cleanup code */
    }

    /* Objective-C 2.0: Synchronized blocks */
    @synchronized(self) {
        /* Thread-safe code */
        count = count * 2;
    }
}

@end

/* Test function */
int main(void) {
    id obj = [[MyClass alloc] init];
    SEL selector = @selector(getValue);

    /* Message sends */
    [obj setValue:42];
    int result = [obj getValue];

    /* Class messages */
    id obj2 = [MyClass createWithValue:100];

    /* Objective-C string literals */
    id str = @"Hello, Objective-C!";

    return 0;
}
