// C# 9.0 Features Test
// Tests records, init-only setters, top-level statements, pattern matching enhancements

using System;

namespace CSharp90Test
{
    // Record types (C# 9.0)
    public record Person(string FirstName, string LastName, int Age)
    {
        // Additional members
        public string FullName => $"{FirstName} {LastName}";
    }

    // Record with init-only properties
    public record Address
    {
        public string Street { get; init; }
        public string City { get; init; }
        public string ZipCode { get; init; }
    }

    // Class with init-only setters (C# 9.0)
    public class Configuration
    {
        public string AppName { get; init; }
        public int Version { get; init; }
        public bool IsProduction { get; init; }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Test records
            var person1 = new Person("John", "Doe", 30);
            Console.WriteLine($"Person: {person1.FullName}, Age: {person1.Age}");

            // With expressions (C# 9.0)
            var person2 = person1 with { Age = 31 };
            Console.WriteLine($"Modified person: {person2.FullName}, Age: {person2.Age}");

            // Value equality (automatic in records)
            var person3 = new Person("John", "Doe", 30);
            Console.WriteLine($"person1 == person3: {person1 == person3}");  // True
            Console.WriteLine($"person1 == person2: {person1 == person2}");  // False

            // Test init-only properties
            var address = new Address
            {
                Street = "123 Main St",
                City = "New York",
                ZipCode = "10001"
            };
            Console.WriteLine($"Address: {address.Street}, {address.City}");

            // address.City = "Boston";  // Error: can't modify init-only property

            var config = new Configuration
            {
                AppName = "MyApp",
                Version = 1,
                IsProduction = true
            };
            Console.WriteLine($"Config: {config.AppName} v{config.Version}");

            // Pattern matching enhancements (C# 9.0)
            TestPatternMatching(person1);
            TestPatternMatching(null);
            TestPatternMatching("text");

            // Relational patterns
            TestNumberPattern(5);
            TestNumberPattern(15);
            TestNumberPattern(25);

            // Logical patterns
            var value = "test";
            var result = value is not null and not "";
            Console.WriteLine($"Value is not null and not empty: {result}");

            // Target-typed new (C# 9.0)
            Person person4 = new("Jane", "Smith", 28);
            Console.WriteLine($"Target-typed new: {person4.FullName}");
        }

        static void TestPatternMatching(object obj)
        {
            // Type patterns with property patterns (C# 9.0)
            var description = obj switch
            {
                Person { Age: > 18 } p => $"Adult: {p.FullName}",
                Person p => $"Minor: {p.FullName}",
                null => "Null value",
                _ => "Unknown type"
            };
            Console.WriteLine(description);
        }

        static void TestNumberPattern(int number)
        {
            var category = number switch
            {
                < 10 => "Single digit",
                >= 10 and < 20 => "10-19",
                >= 20 => "20 or more",
                _ => "Unknown"
            };
            Console.WriteLine($"{number} is: {category}");
        }
    }
}
