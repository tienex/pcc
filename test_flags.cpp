// Simple C++ test file to verify flag parsing
class TestClass {
public:
    int x;

    TestClass() {
        x = 42;
    }

    ~TestClass() {
    }

    int getValue() {
        return this->x;
    }
};

int main() {
    TestClass obj;
    return obj.getValue();
}
