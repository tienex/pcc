/*
 * Test file for SEH and C++ exception interoperability
 *
 * This file demonstrates that SEH and C++ exceptions can coexist
 * on Unix/Linux/macOS platforms.
 */

#include <iostream>
#include <stdexcept>
#include <string>
#include <seh.h>

using namespace std;

/*
 * Test 1: C++ exception caught by SEH __try/__except
 */
void test_cxx_caught_by_seh() {
    cout << "Test 1: C++ exception caught by SEH" << endl;

    __try {
        cout << "  Inside __try block" << endl;
        cout << "  Throwing C++ exception..." << endl;
        throw runtime_error("C++ exception from __try block");
        cout << "  This won't execute" << endl;
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        cout << "  Caught in __except handler!" << endl;
        cout << "  Exception code: 0x" << hex << _seh_get_exception_code() << dec << endl;

        if (_seh_is_cxx_exception()) {
            cout << "  This is a C++ exception!" << endl;
            void *cxx_obj = _seh_get_cxx_exception();
            if (cxx_obj) {
                try {
                    throw;  /* Re-throw to get proper type */
                } catch (runtime_error &e) {
                    cout << "  Message: " << e.what() << endl;
                } catch (...) {
                    cout << "  Unknown C++ exception type" << endl;
                }
            }
        }
    }

    cout << "  Test 1 completed" << endl << endl;
}

/*
 * Test 2: SEH exception in C++ try/catch
 */
void test_seh_in_cxx_catch() {
    cout << "Test 2: SEH exception in C++ try/catch" << endl;

    try {
        __try {
            cout << "  Inside __try block" << endl;
            cout << "  Raising SEH exception..." << endl;
            _seh_raise_exception(EXCEPTION_ACCESS_VIOLATION, 0, 0, NULL);
            cout << "  This won't execute" << endl;
        }
        __except (EXCEPTION_EXECUTE_HANDLER) {
            cout << "  Caught in __except handler!" << endl;
            cout << "  Exception code: 0x" << hex << _seh_get_exception_code() << dec << endl;
        }
    } catch (...) {
        cout << "  This shouldn't execute (caught in SEH)" << endl;
    }

    cout << "  Test 2 completed" << endl << endl;
}

/*
 * Test 3: Nested SEH in C++ with destructors
 */
class ResourceGuard {
public:
    string name;

    ResourceGuard(const char *n) : name(n) {
        cout << "  Resource acquired: " << name << endl;
    }

    ~ResourceGuard() {
        cout << "  Resource released: " << name << endl;
    }
};

void test_seh_cxx_destructors() {
    cout << "Test 3: SEH with C++ destructors" << endl;

    try {
        ResourceGuard guard1("outer");

        __try {
            ResourceGuard guard2("inner");

            cout << "  Inside __try block" << endl;
            cout << "  Throwing C++ exception..." << endl;
            throw runtime_error("Test exception");
        }
        __except (EXCEPTION_EXECUTE_HANDLER) {
            cout << "  Caught in __except handler" << endl;
        }

    } catch (exception &e) {
        cout << "  Caught in C++ catch: " << e.what() << endl;
    }

    cout << "  Test 3 completed" << endl << endl;
}

/*
 * Test 4: __finally block with C++ exceptions
 */
void test_finally_with_cxx() {
    cout << "Test 4: __finally with C++ exceptions" << endl;

    try {
        __try {
            cout << "  Inside __try block" << endl;
            cout << "  Throwing C++ exception..." << endl;
            throw logic_error("Exception in __try");
        }
        __finally {
            cout << "  __finally block executed (cleanup)" << endl;
        }
    } catch (exception &e) {
        cout << "  Caught in C++ catch: " << e.what() << endl;
    }

    cout << "  Test 4 completed" << endl << endl;
}

/*
 * Test 5: Multiple nested __try blocks with C++ objects
 */
void test_nested_seh_cxx() {
    cout << "Test 5: Nested SEH with C++ objects" << endl;

    ResourceGuard outer_guard("outer_guard");

    __try {
        ResourceGuard guard1("guard1");

        __try {
            ResourceGuard guard2("guard2");

            cout << "  Throwing from inner __try" << endl;
            throw runtime_error("Inner exception");
        }
        __except (EXCEPTION_CONTINUE_SEARCH) {
            cout << "  Inner __except (shouldn't execute)" << endl;
        }

    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        cout << "  Outer __except caught the exception" << endl;
        cout << "  Exception code: 0x" << hex << _seh_get_exception_code() << dec << endl;
    }

    cout << "  Test 5 completed" << endl << endl;
}

/*
 * Test 6: SEH __leave with C++ objects
 */
void test_leave_with_cxx() {
    cout << "Test 6: __leave with C++ objects" << endl;

    __try {
        ResourceGuard guard("guard_in_try");

        cout << "  Before __leave" << endl;
        __leave;
        cout << "  This won't execute" << endl;
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        cout << "  __except handler" << endl;
    }

    cout << "  After __try block" << endl;
    cout << "  Test 6 completed" << endl << endl;
}

/*
 * Main test driver
 */
int main() {
    cout << "=== SEH and C++ Interoperability Tests ===" << endl << endl;

    try {
        test_cxx_caught_by_seh();
        test_seh_in_cxx_catch();
        test_seh_cxx_destructors();
        test_finally_with_cxx();
        test_nested_seh_cxx();
        test_leave_with_cxx();

        cout << "=== All tests completed successfully ===" << endl;
        return 0;

    } catch (exception &e) {
        cerr << "Uncaught exception: " << e.what() << endl;
        return 1;
    } catch (...) {
        cerr << "Uncaught unknown exception" << endl;
        return 2;
    }
}
