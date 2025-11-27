// C# 2.0 Features Test
// Tests generics, nullable types, iterators, anonymous methods

using System;
using System.Collections.Generic;

namespace CSharp20Test
{
    // Generic class (C# 2.0)
    public class GenericList<T>
    {
        private T[] items;
        private int count;

        public GenericList(int capacity)
        {
            items = new T[capacity];
            count = 0;
        }

        public void Add(T item)
        {
            items[count++] = item;
        }

        public T Get(int index)
        {
            return items[index];
        }

        // Iterator with yield return (C# 2.0)
        public IEnumerable<T> GetItems()
        {
            for (int i = 0; i < count; i++)
            {
                yield return items[i];
            }
        }
    }

    // Generic methods
    public static class Utils
    {
        public static void Swap<T>(ref T a, ref T b)
        {
            T temp = a;
            a = b;
            b = temp;
        }
    }

    // Partial class (C# 2.0)
    public partial class Person
    {
        public string FirstName { get; set; }
    }

    public partial class Person
    {
        public string LastName { get; set; }

        public string FullName
        {
            get { return FirstName + " " + LastName; }
        }
    }

    // Static class (C# 2.0)
    public static class MathHelpers
    {
        public static int Square(int x)
        {
            return x * x;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Test generics
            GenericList<int> numbers = new GenericList<int>(10);
            numbers.Add(1);
            numbers.Add(2);
            numbers.Add(3);

            Console.WriteLine("Numbers:");
            foreach (int num in numbers.GetItems())
            {
                Console.WriteLine(num);
            }

            // Test nullable types (C# 2.0)
            int? nullable = null;
            if (nullable.HasValue)
            {
                Console.WriteLine("Value: " + nullable.Value);
            }
            else
            {
                Console.WriteLine("No value");
            }

            nullable = 42;
            Console.WriteLine("Nullable now has value: " + nullable.Value);

            // Test anonymous methods (C# 2.0)
            List<int> list = new List<int> { 1, 2, 3, 4, 5 };
            list.ForEach(delegate(int x) {
                Console.WriteLine("Item: " + x);
            });

            // Test generic swap
            int a = 10, b = 20;
            Utils.Swap<int>(ref a, ref b);
            Console.WriteLine("After swap: a={0}, b={1}", a, b);

            // Test partial classes
            Person person = new Person();
            person.FirstName = "John";
            person.LastName = "Doe";
            Console.WriteLine("Full name: " + person.FullName);

            // Test static class
            Console.WriteLine("Square of 5: " + MathHelpers.Square(5));
        }
    }
}
