// C# 7.0 Features Test
// Tests tuples, pattern matching, out variables, local functions, etc.

using System;

namespace CSharp70Test
{
    class Program
    {
        static void Main(string[] args)
        {
            // Tuples (C# 7.0)
            var tuple = (Name: "Alice", Age: 30);
            Console.WriteLine($"Tuple: {tuple.Name}, {tuple.Age}");

            // Tuple deconstruction
            var (name, age) = GetPerson();
            Console.WriteLine($"Deconstructed: {name}, {age}");

            // Out variables (C# 7.0)
            if (int.TryParse("123", out int result))
            {
                Console.WriteLine($"Parsed: {result}");
            }

            // Out variables with discard
            if (int.TryParse("456", out var parsed))
            {
                Console.WriteLine($"Parsed with var: {parsed}");
            }

            // Pattern matching with 'is' (C# 7.0)
            object obj = "Hello";
            if (obj is string str)
            {
                Console.WriteLine($"String value: {str}");
            }

            // Pattern matching with switch
            TestPattern(42);
            TestPattern("text");
            TestPattern(null);

            // Local functions (C# 7.0)
            int Factorial(int n)
            {
                if (n <= 1) return 1;
                return n * Factorial(n - 1);
            }

            Console.WriteLine($"Factorial of 5: {Factorial(5)}");

            // Binary literals and digit separators (C# 7.0)
            int binary = 0b1010_1100;
            int large = 1_000_000;
            Console.WriteLine($"Binary: {binary}, Large: {large}");

            // Ref returns and locals (C# 7.0)
            int[] numbers = { 1, 2, 3, 4, 5 };
            ref int max = ref FindMax(numbers);
            max = 100;  // Modifies array element
            Console.WriteLine($"Modified array: {string.Join(", ", numbers)}");

            // Expression-bodied everything (C# 7.0)
            var point = new Point(3, 4);
            Console.WriteLine($"Point: {point}");

            // Throw expressions (C# 7.0)
            var notNull = GetValueOrThrow(null) ?? throw new ArgumentException();
        }

        static (string Name, int Age) GetPerson()
        {
            return ("Bob", 25);
        }

        static void TestPattern(object value)
        {
            switch (value)
            {
                case int i when i > 0:
                    Console.WriteLine($"Positive integer: {i}");
                    break;
                case string s:
                    Console.WriteLine($"String: {s}");
                    break;
                case null:
                    Console.WriteLine("Null value");
                    break;
                default:
                    Console.WriteLine("Unknown type");
                    break;
            }
        }

        static ref int FindMax(int[] numbers)
        {
            int maxIndex = 0;
            for (int i = 1; i < numbers.Length; i++)
            {
                if (numbers[i] > numbers[maxIndex])
                    maxIndex = i;
            }
            return ref numbers[maxIndex];
        }

        static string GetValueOrThrow(string value) => value ?? "default";
    }

    // Expression-bodied constructor, destructor, properties (C# 7.0)
    public class Point
    {
        public int X { get; }
        public int Y { get; }

        public Point(int x, int y) => (X, Y) = (x, y);

        ~Point() => Console.WriteLine("Destructor called");

        public override string ToString() => $"({X}, {Y})";
    }
}
