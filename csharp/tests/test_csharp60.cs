// C# 6.0 Features Test
// Tests null-conditional, string interpolation, expression-bodied members, etc.

using System;
using System.Collections.Generic;
using static System.Console;
using static System.Math;

namespace CSharp60Test
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public Address Address { get; set; }

        // Expression-bodied property (C# 6.0)
        public string DisplayName => $"{Name} (Age: {Age})";

        // Expression-bodied method (C# 6.0)
        public string GetGreeting() => $"Hello, I'm {Name}!";

        // Auto-property initializer (C# 6.0)
        public DateTime CreatedAt { get; set; } = DateTime.Now;
    }

    public class Address
    {
        public string Street { get; set; }
        public string City { get; set; }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Person person = new Person { Name = "Alice", Age = 30 };

            // String interpolation (C# 6.0)
            WriteLine($"Person: {person.Name}, Age: {person.Age}");
            WriteLine($"Display name: {person.DisplayName}");
            WriteLine($"Created at: {person.CreatedAt:yyyy-MM-dd HH:mm:ss}");

            // Null-conditional operator (C# 6.0)
            string city = person?.Address?.City;
            WriteLine($"City: {city ?? "Unknown"}");

            person.Address = new Address { City = "New York", Street = "5th Ave" };
            city = person?.Address?.City;
            WriteLine($"City: {city}");

            // Null-conditional with method call
            int? length = person?.Name?.Length;
            WriteLine($"Name length: {length}");

            // nameof operator (C# 6.0)
            WriteLine($"Property name: {nameof(Person.Name)}");
            WriteLine($"Type name: {nameof(Person)}");

            // using static (C# 6.0)
            WriteLine($"Square root of 16: {Sqrt(16)}");
            WriteLine($"Pi: {PI}");

            // Index initializers (C# 6.0)
            var dict = new Dictionary<string, int>
            {
                ["one"] = 1,
                ["two"] = 2,
                ["three"] = 3
            };

            WriteLine($"Dictionary['two']: {dict["two"]}");

            // Exception filters (C# 6.0)
            try
            {
                ThrowException(5);
            }
            catch (ArgumentException ex) when (ex.Message.Contains("even"))
            {
                WriteLine($"Caught even exception: {ex.Message}");
            }
            catch (ArgumentException ex)
            {
                WriteLine($"Caught other exception: {ex.Message}");
            }
        }

        static void ThrowException(int value)
        {
            if (value % 2 == 0)
                throw new ArgumentException("Value is even");
            else
                throw new ArgumentException("Value is odd");
        }
    }
}
