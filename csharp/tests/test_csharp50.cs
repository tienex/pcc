// C# 5.0 Features Test
// Tests async/await and caller info attributes

using System;
using System.Threading.Tasks;
using System.Runtime.CompilerServices;

namespace CSharp50Test
{
    public class DataService
    {
        // Async method (C# 5.0)
        public async Task<string> FetchDataAsync(string url)
        {
            Console.WriteLine("Fetching data from: " + url);

            // Simulate async operation
            await Task.Delay(1000);

            return "Data from " + url;
        }

        // Async method returning void
        public async void ProcessDataAsync()
        {
            Console.WriteLine("Processing started");
            await Task.Delay(500);
            Console.WriteLine("Processing complete");
        }

        // Multiple await expressions
        public async Task<int> CalculateAsync(int a, int b)
        {
            var task1 = Task.Run(() => a * 2);
            var task2 = Task.Run(() => b * 3);

            int result1 = await task1;
            int result2 = await task2;

            return result1 + result2;
        }
    }

    // Caller info attributes (C# 5.0)
    public class Logger
    {
        public void Log(
            string message,
            [CallerMemberName] string memberName = "",
            [CallerFilePath] string filePath = "",
            [CallerLineNumber] int lineNumber = 0)
        {
            Console.WriteLine("Message: {0}", message);
            Console.WriteLine("  Member: {0}", memberName);
            Console.WriteLine("  File: {0}", filePath);
            Console.WriteLine("  Line: {0}", lineNumber);
        }
    }

    class Program
    {
        static async Task Main(string[] args)
        {
            var service = new DataService();

            // Test async/await
            Console.WriteLine("Testing async/await...");
            string result = await service.FetchDataAsync("http://example.com/api");
            Console.WriteLine("Result: " + result);

            // Test multiple awaits
            int calcResult = await service.CalculateAsync(5, 7);
            Console.WriteLine("Calculation result: " + calcResult);

            // Test caller info attributes
            Logger logger = new Logger();
            logger.Log("Test message");

            Console.WriteLine("\nAsync operations completed");
        }
    }
}
