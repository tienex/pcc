// Test ABI-based name mangling

int simple_function(int x) {
    return x * 2;
}

class MyClass {
public:
    int method(int a, int b) {
        return a + b;
    }

    MyClass() { }
    ~MyClass() { }
};

int main() {
    MyClass obj;
    return obj.method(10, 20);
}
