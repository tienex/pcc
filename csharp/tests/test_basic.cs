// C# 3.0 Basic Test
// Tests fundamental C# 3.0 features with ARC support

using System;
using System.Collections.Generic;
using System.Linq;

namespace BasicTest
{
    // Class with auto-implemented properties (C# 3.0)
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }

        public Person(string name, int age)
        {
            Name = name;
            Age = age;
        }
    }

    // Extension methods (C# 3.0)
    public static class StringExtensions
    {
        public static string Reverse(this string str)
        {
            char[] chars = str.ToCharArray();
            Array.Reverse(chars);
            return new string(chars);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Object initializer (C# 3.0)
            var person = new Person { Name = "Alice", Age = 30 };

            // Collection initializer (C# 3.0)
            var numbers = new List<int> { 1, 2, 3, 4, 5 };

            // Lambda expression (C# 3.0)
            var evenNumbers = numbers.Where(n => n % 2 == 0);

            // LINQ query expression (C# 3.0)
            var query = from n in numbers
                        where n > 2
                        select n * 2;

            // Anonymous type (C# 3.0)
            var anonymous = new { Name = "Bob", Age = 25 };

            // Extension method call (C# 3.0)
            string reversed = "Hello".Reverse();

            Console.WriteLine("Person: {0}, Age: {1}", person.Name, person.Age);
            Console.WriteLine("Even numbers: {0}", string.Join(", ", evenNumbers));
            Console.WriteLine("Query result: {0}", string.Join(", ", query));
            Console.WriteLine("Anonymous: {0}, {1}", anonymous.Name, anonymous.Age);
            Console.WriteLine("Reversed: {0}", reversed);
        }
    }
}
