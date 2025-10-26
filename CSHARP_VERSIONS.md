# C# Language Version Support

## Overview

The PCC C# compiler supports all major C# language versions from 1.0 through 12.0, allowing you to compile code targeting any C# version. This provides:

- **Full backwards compatibility** - Compile legacy C# 1.0/2.0 code
- **Modern language features** - Use latest C# 12.0 capabilities
- **Version-specific validation** - Features are enforced based on selected version
- **Gradual migration** - Upgrade code incrementally across versions

## Supported Versions

| Version | Year | Key Features |
|---------|------|--------------|
| C# 1.0 | 2002 | Classes, interfaces, delegates, properties, events |
| C# 2.0 | 2005 | **Generics**, nullable types, iterators, anonymous methods, partial types |
| C# 3.0 | 2007 | **LINQ**, lambda expressions, extension methods, anonymous types, auto-properties |
| C# 4.0 | 2010 | **Dynamic binding**, named/optional parameters, generic variance |
| C# 5.0 | 2012 | **async/await**, caller info attributes |
| C# 6.0 | 2015 | Null-conditional operators, string interpolation, expression-bodied members, nameof |
| C# 7.0 | 2017 | **Tuples**, pattern matching, out variables, local functions, ref returns |
| C# 7.1-7.3 | 2017-2018 | Enhancements to 7.0 features |
| C# 8.0 | 2019 | **Nullable reference types**, ranges/indices, switch expressions, default interface methods |
| C# 9.0 | 2020 | **Records**, init-only setters, top-level statements, target-typed new |
| C# 10.0 | 2021 | Global using, file-scoped namespaces, record structs, lambda improvements |
| C# 11.0 | 2022 | Raw string literals, generic attributes, list patterns, required members |
| C# 12.0 | 2023 | **Primary constructors**, collection expressions, inline arrays |

## Usage

### Specifying Language Version

Use the `-langversion:` option to select a specific C# version:

```bash
# Compile as C# 2.0
csharp -langversion:2.0 program.cs

# Compile as C# 7.3
csharp -langversion:7.3 program.cs

# Compile as latest (C# 12.0) - default
csharp -langversion:latest program.cs
csharp program.cs  # Same as above
```

### Checking Available Features

See what features are available for a specific version:

```bash
# Show C# 3.0 features
csharp -langversion:3.0 --features

# Show C# 8.0 features
csharp -langversion:8.0 --features
```

Example output:
```
C# 8.0 Features:
================

✓ Generics (C# 2.0)
✓ Nullable types (C# 2.0)
✓ Iterators with yield (C# 2.0)
✓ LINQ queries (C# 3.0)
✓ Lambda expressions (C# 3.0)
✓ Extension methods (C# 3.0)
✓ Dynamic binding (C# 4.0)
✓ Named/optional parameters (C# 4.0)
✓ async/await (C# 5.0)
✓ Null-conditional operators ?. (C# 6.0)
✓ String interpolation $"" (C# 6.0)
✓ nameof operator (C# 6.0)
✓ Tuples (C# 7.0)
✓ Pattern matching (C# 7.0)
✓ Out variables (C# 7.0)
✓ Local functions (C# 7.0)
✓ Binary literals 0b (C# 7.0)
✓ Digit separators _ (C# 7.0)
✓ Nullable reference types (C# 8.0)
✓ Ranges and indices .. ^ (C# 8.0)
✓ Switch expressions (C# 8.0)
```

### Version Validation

The compiler enforces version-specific features:

```csharp
// With -langversion:2.0
var x = 10;  // ERROR: 'var' keyword not available until C# 3.0

// With -langversion:3.0
var x = 10;  // OK: var keyword available

async Task DoWork() { }  // ERROR: async/await not available until C# 5.0

// With -langversion:5.0
async Task DoWork() { }  // OK: async/await available
```

## Feature Details

### C# 1.0 (2002) - Foundation

**Core Types:**
- Classes, structs, interfaces, enums
- Delegates and events
- Properties and indexers
- Attributes

**Example:**
```csharp
public class Person
{
    private string name;

    public string Name
    {
        get { return name; }
        set { name = value; }
    }

    public event EventHandler NameChanged;
}
```

### C# 2.0 (2005) - Generics Era

**Major Features:**
- **Generics** - Type-safe collections and methods
- **Nullable types** - `int?`, `bool?`, etc.
- **Iterators** - `yield return` and `yield break`
- **Anonymous methods** - `delegate { }` syntax
- **Partial types** - Split class definitions across files
- **Static classes** - Cannot be instantiated

**Examples:**

```csharp
// Generics
public class List<T>
{
    public void Add(T item) { }
}

// Nullable types
int? nullable = null;
if (nullable.HasValue)
    Console.WriteLine(nullable.Value);

// Iterators
public IEnumerable<int> GetNumbers()
{
    yield return 1;
    yield return 2;
    yield return 3;
}

// Anonymous methods
list.ForEach(delegate(int x) {
    Console.WriteLine(x);
});

// Partial classes
partial class Person
{
    public string FirstName { get; set; }
}

partial class Person
{
    public string LastName { get; set; }
}
```

### C# 3.0 (2007) - LINQ Revolution

**Major Features:**
- **LINQ** - Language Integrated Query
- **Lambda expressions** - `=>` syntax
- **Extension methods** - Add methods to existing types
- **Anonymous types** - `new { }` syntax
- **Auto-properties** - `{ get; set; }`
- **Object/collection initializers**
- **Expression trees**

**Examples:**

```csharp
// LINQ
var query = from person in people
            where person.Age > 18
            select person.Name;

// Lambda expressions
var adults = people.Where(p => p.Age > 18);

// Extension methods
public static class StringExtensions
{
    public static string Reverse(this string str) { }
}
"hello".Reverse();

// Anonymous types
var person = new { Name = "Alice", Age = 30 };

// Auto-properties
public string Name { get; set; }

// Object initializers
var person = new Person { Name = "Bob", Age = 25 };

// Collection initializers
var numbers = new List<int> { 1, 2, 3, 4, 5 };
```

### C# 4.0 (2010) - Dynamic Features

**Major Features:**
- **Dynamic binding** - `dynamic` type
- **Named arguments** - Call methods with parameter names
- **Optional parameters** - Default parameter values
- **Generic covariance/contravariance** - `out` and `in` modifiers

**Examples:**

```csharp
// Dynamic
dynamic obj = GetDynamicObject();
obj.SomeMethod();  // Resolved at runtime

// Named arguments
PrintOrder(productName: "Widget", sellerName: "ACME", orderNum: 123);

// Optional parameters
public void DoWork(string name, int timeout = 30) { }
DoWork("Task1");  // Uses default timeout

// Covariance
IEnumerable<string> strings = new List<string>();
IEnumerable<object> objects = strings;  // OK with covariance
```

### C# 5.0 (2012) - Async/Await

**Major Features:**
- **async/await** - Asynchronous programming
- **Caller info attributes** - Compile-time caller information

**Examples:**

```csharp
// async/await
public async Task<string> FetchDataAsync(string url)
{
    var client = new HttpClient();
    string data = await client.GetStringAsync(url);
    return data;
}

// Caller info
public void Log(
    string message,
    [CallerMemberName] string member = "",
    [CallerFilePath] string file = "",
    [CallerLineNumber] int line = 0)
{
    Console.WriteLine($"{member} at {file}:{line} - {message}");
}
```

### C# 6.0 (2015) - Syntactic Sugar

**Major Features:**
- **Null-conditional operators** - `?.` and `?[]`
- **String interpolation** - `$"Hello {name}"`
- **Expression-bodied members** - `=>` for methods/properties
- **nameof operator** - Get member name as string
- **Using static** - Import static members
- **Exception filters** - `catch when`
- **Index initializers** - `dict["key"] = value` in initializer

**Examples:**

```csharp
// Null-conditional
string city = person?.Address?.City;
int? length = array?[0]?.Length;

// String interpolation
string message = $"Hello {name}, you are {age} years old";

// Expression-bodied
public string FullName => $"{FirstName} {LastName}";
public override string ToString() => $"Person: {Name}";

// nameof
string propertyName = nameof(Person.Name);

// Using static
using static System.Math;
double result = Sqrt(16);  // No need for Math.Sqrt

// Exception filters
try { }
catch (Exception ex) when (ex.Message.Contains("timeout"))
{
    // Handle only timeout exceptions
}

// Index initializers
var dict = new Dictionary<string, int>
{
    ["one"] = 1,
    ["two"] = 2
};
```

### C# 7.0 (2017) - Pattern Matching

**Major Features:**
- **Tuples** - `(int, string)` syntax
- **Pattern matching** - `is` and `switch` patterns
- **Out variables** - Declare inline: `int.TryParse(s, out var result)`
- **Local functions** - Functions within functions
- **Ref returns and locals** - Return references
- **Discards** - `_` for unused values
- **Binary literals** - `0b1010`
- **Digit separators** - `1_000_000`

**Examples:**

```csharp
// Tuples
var tuple = (Name: "Alice", Age: 30);
Console.WriteLine(tuple.Name);

// Tuple deconstruction
(string name, int age) = GetPerson();

// Pattern matching
if (obj is string str)
    Console.WriteLine($"String: {str}");

switch (value)
{
    case int i when i > 0:
        Console.WriteLine($"Positive: {i}");
        break;
    case string s:
        Console.WriteLine($"String: {s}");
        break;
}

// Out variables
if (int.TryParse("123", out var result))
    Console.WriteLine(result);

// Local functions
int Factorial(int n)
{
    int Helper(int x)
    {
        return x <= 1 ? 1 : x * Helper(x - 1);
    }
    return Helper(n);
}

// Ref returns
ref int FindMax(int[] array) => ref array[0];

// Discards
if (dict.TryGetValue(key, out _))  // Don't care about value
    Console.WriteLine("Key exists");

// Binary literals and digit separators
int binary = 0b1010_1100;
int large = 1_000_000;
```

### C# 8.0 (2019) - Nullable References

**Major Features:**
- **Nullable reference types** - `string?` vs `string`
- **Async streams** - `await foreach`
- **Ranges and indices** - `array[^1]`, `array[1..5]`
- **Switch expressions** - More concise switch
- **Default interface methods** - Implementations in interfaces
- **Using declarations** - Simplified using

**Examples:**

```csharp
// Nullable reference types
string? nullable = null;  // Can be null
string nonNullable = "";  // Cannot be null

// Ranges and indices
int[] array = { 1, 2, 3, 4, 5 };
int last = array[^1];        // Last element
int[] slice = array[1..4];   // Elements 1, 2, 3

// Switch expressions
var result = value switch
{
    1 => "one",
    2 => "two",
    _ => "other"
};

// Default interface methods
interface ILogger
{
    void Log(string message) => Console.WriteLine(message);
}

// Using declarations
using var file = File.OpenRead("data.txt");
// Disposed at end of scope
```

### C# 9.0 (2020) - Records

**Major Features:**
- **Records** - Immutable reference types with value equality
- **Init-only setters** - `{ get; init; }`
- **Top-level statements** - No `Main()` needed
- **Pattern matching enhancements** - Relational, logical patterns
- **Target-typed new** - `Person p = new(...)`

**Examples:**

```csharp
// Records
public record Person(string FirstName, string LastName, int Age);

var person1 = new Person("John", "Doe", 30);
var person2 = person1 with { Age = 31 };  // Create copy with change

// Value equality (automatic)
var person3 = new Person("John", "Doe", 30);
bool equal = person1 == person3;  // True

// Init-only setters
public class Config
{
    public string Name { get; init; }
}

var config = new Config { Name = "Test" };
// config.Name = "New";  // Error: init-only

// Top-level statements
using System;
Console.WriteLine("Hello World!");  // No Main() needed

// Pattern matching enhancements
var category = age switch
{
    < 18 => "Minor",
    >= 18 and < 65 => "Adult",
    >= 65 => "Senior"
};

// Target-typed new
Person person = new("Jane", "Smith", 28);
```

### C# 10.0 (2021) - Simplifications

**Major Features:**
- **Global using directives** - `global using System;`
- **File-scoped namespaces** - No braces needed
- **Record structs** - Value type records
- **Lambda improvements** - Inferred types, attributes

**Examples:**

```csharp
// Global using (in one file, applies to all)
global using System;
global using System.Collections.Generic;

// File-scoped namespace
namespace MyApp;  // No braces, applies to whole file

class Program { }

// Record structs
public record struct Point(int X, int Y);

// Lambda improvements
var lambda = (string s) => s.Length;  // Inferred return type
var lambda2 = [MyAttribute] (x) => x * 2;  // Attributes on lambdas
```

### C# 11.0 (2022) - Refinements

**Major Features:**
- **Raw string literals** - `""" multiline strings """`
- **Generic attributes** - `[MyAttribute<int>]`
- **List patterns** - Pattern match on arrays/lists
- **Required members** - `required` keyword

**Examples:**

```csharp
// Raw string literals
string json = """
{
    "name": "Alice",
    "age": 30
}
""";

// Generic attributes
[MyAttribute<int>]
public class MyClass { }

// List patterns
if (array is [1, 2, 3])
    Console.WriteLine("Matched [1, 2, 3]");

// Required members
public class Person
{
    required public string Name { get; init; }
}

var person = new Person { Name = "Bob" };  // Must set Name
```

### C# 12.0 (2023) - Latest

**Major Features:**
- **Primary constructors** - Constructor parameters accessible in class
- **Collection expressions** - `int[] arr = [1, 2, 3];`
- **Inline arrays** - Fixed-size arrays as struct fields

**Examples:**

```csharp
// Primary constructors
public class Person(string firstName, string lastName)
{
    public string FullName => $"{firstName} {lastName}";
    // firstName and lastName are accessible throughout class
}

// Collection expressions
int[] numbers = [1, 2, 3, 4, 5];
List<string> names = ["Alice", "Bob", "Charlie"];

// Inline arrays
[InlineArray(10)]
public struct Buffer
{
    private int _element0;
}
```

## Migration Guide

### Upgrading from C# 1.0/2.0 to 3.0

**Use LINQ and lambdas:**
```csharp
// Before (C# 2.0)
List<int> filtered = new List<int>();
foreach (int n in numbers)
{
    if (n > 10)
        filtered.Add(n);
}

// After (C# 3.0)
var filtered = numbers.Where(n => n > 10);
```

**Use auto-properties:**
```csharp
// Before
private string name;
public string Name
{
    get { return name; }
    set { name = value; }
}

// After
public string Name { get; set; }
```

### Upgrading from C# 3.0 to 5.0

**Use async/await:**
```csharp
// Before
public Task<string> FetchDataAsync(string url)
{
    return Task.Run(() => {
        // Blocking call
        return new WebClient().DownloadString(url);
    });
}

// After
public async Task<string> FetchDataAsync(string url)
{
    using var client = new HttpClient();
    return await client.GetStringAsync(url);
}
```

### Upgrading from C# 6.0 to 7.0+

**Use pattern matching:**
```csharp
// Before
if (obj is string)
{
    string str = (string)obj;
    Console.WriteLine(str);
}

// After
if (obj is string str)
{
    Console.WriteLine(str);
}
```

**Use tuples:**
```csharp
// Before
public void GetCoordinates(out int x, out int y)
{
    x = 10;
    y = 20;
}
int x, y;
GetCoordinates(out x, out y);

// After
public (int X, int Y) GetCoordinates()
{
    return (10, 20);
}
var (x, y) = GetCoordinates();
```

### Upgrading to C# 9.0+

**Use records for data classes:**
```csharp
// Before
public class Person
{
    public string FirstName { get; init; }
    public string LastName { get; init; }

    public override bool Equals(object obj) { /* ... */ }
    public override int GetHashCode() { /* ... */ }
}

// After
public record Person(string FirstName, string LastName);
```

## Compatibility Notes

1. **Default Version**: If no version is specified, the compiler uses C# 12.0 (latest)

2. **Forward Compatibility**: Code written for older versions will compile with newer versions

3. **Backward Compatibility**: Code using newer features will fail to compile with older version settings

4. **Module Format**: The compiled module format is version-independent - modules can be generated with any language version

5. **Runtime Requirements**: Some features (like async/await, dynamic) require runtime library support

## Best Practices

1. **Match Your Target**: Use `-langversion:` to match your project's C# version
2. **Explicit Version**: Always specify version in build scripts for reproducibility
3. **Test Migration**: When upgrading, test incrementally with `--features` flag
4. **Document Version**: Include language version in project documentation
5. **Team Alignment**: Ensure all team members use the same language version

## Troubleshooting

### Feature Not Available Error

```
Error: LINQ queries not available in C# 2.0
```

**Solution**: Increase language version or refactor code:
```bash
csharp -langversion:3.0 program.cs
```

### Unknown Version Warning

```
Warning: Unrecognized C# version '13.0', using latest
```

**Solution**: Use a supported version (1.0-12.0) or 'latest'

## See Also

- [C# Compiler Documentation](CSHARP_COMPILER.md)
- [Module Format Specification](CSHARP_COMPILER.md#module-format)
- [ARC Integration](ARC_LIBRARY.md)
