// C# ARC Test
// Tests automatic reference counting for C# reference types

using System;

namespace ARCTest
{
    public class Resource
    {
        private string name;

        public Resource(string name)
        {
            this.name = name;
            Console.WriteLine("Resource '{0}' allocated", name);
        }

        ~Resource()
        {
            Console.WriteLine("Resource '{0}' deallocated", name);
        }

        public void Use()
        {
            Console.WriteLine("Using resource '{0}'", name);
        }
    }

    public class Container
    {
        // Strong reference (default)
        private Resource strongRef;

        // Weak reference to avoid retain cycles
        // In C#, this would use WeakReference<T>
        private WeakReference<Resource> weakRef;

        public Container(Resource resource)
        {
            // ARC: Retain resource
            strongRef = resource;

            // ARC: Create weak reference (no retain)
            weakRef = new WeakReference<Resource>(resource);
        }

        public void UseResource()
        {
            if (strongRef != null)
            {
                strongRef.Use();
            }

            Resource weak;
            if (weakRef != null && weakRef.TryGetTarget(out weak))
            {
                weak.Use();
            }
        }

        // ARC: Release strongRef when container is finalized
    }

    class Program
    {
        static void TestARC()
        {
            {
                // ARC: resource is retained
                Resource resource = new Resource("Test1");
                resource.Use();

                // ARC: container retains resource (strong ref)
                Container container = new Container(resource);
                container.UseResource();

                // ARC: resource and container released at end of scope
            }

            Console.WriteLine("Scope exited - resources should be released");
        }

        static void TestReturnValue()
        {
            // ARC: returned value is retained for caller
            Resource resource = CreateResource("Test2");
            resource.Use();
            // ARC: resource released at end of scope
        }

        static Resource CreateResource(string name)
        {
            Resource r = new Resource(name);
            // ARC: retain before return
            return r;
            // ARC: local 'r' released but returned value is retained
        }

        static void Main(string[] args)
        {
            Console.WriteLine("Testing ARC...");
            TestARC();
            TestReturnValue();
            Console.WriteLine("ARC test completed");
        }
    }
}
