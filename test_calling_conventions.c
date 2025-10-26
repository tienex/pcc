/*
 * Test file for calling convention support in PCC
 * Demonstrates custom calling convention definitions and usage
 */

/* Test 1: Using predefined calling conventions */

/* Apply __cdecl convention (caller cleanup) */
#pragma aux (__cdecl) cdecl_function
int cdecl_function(int a, int b, int c);

/* Apply __stdcall convention (callee cleanup) */
#pragma aux (__stdcall) stdcall_function
void stdcall_function(int x);

/* Apply __fastcall convention (first two params in registers) */
#pragma aux (__fastcall) fastcall_function
int fastcall_function(int a, int b, int c);

/* Apply __thiscall convention (C++ this pointer in ECX) */
#pragma aux (__thiscall) method_function
void method_function(void *this, int param);

/* Test 2: Define custom calling conventions */

/* Define a custom register-based calling convention */
#pragma aux __MyFast parm [eax] [ebx] [ecx] value [eax] modify [eax ebx ecx edx] caller
#pragma aux (__MyFast) my_fast_add
int my_fast_add(int a, int b, int c);

/* Define a custom stack-based convention */
#pragma aux __MyStack parm [] value [eax] modify [eax ecx edx] routine
#pragma aux (__MyStack) my_stack_func
void my_stack_func(int a, int b);

/* Test 3: Inline calling convention definitions */

/* Define and apply convention in one pragma */
#pragma aux custom_mul parm [ecx] [edx] value [eax] modify [eax] routine
int custom_mul(int x, int y);

/* Test 4: Complex calling convention with many registers */

#pragma aux __VectorCall parm [ecx] [edx] [r8] [r9] value [eax] modify [eax ecx edx] caller
#pragma aux (__VectorCall) vector_compute
int vector_compute(int a, int b, int c, int d);

/* Test 5: Calling convention for system calls */

#pragma aux __Syscall parm [eax] [ebx] [ecx] [edx] value [eax] modify [eax ebx ecx edx] caller
#pragma aux (__Syscall) system_call
long system_call(int num, void *arg1, void *arg2, void *arg3);

/* Test 6: Define conventions for 16-bit compatibility */

#pragma aux __Far16 loadds parm caller modify [eax ebx ecx edx]
#pragma aux (__Far16) far_function
void far_function(int param);

/* Test 7: Export DLL functions with custom conventions */

#pragma aux __DllExport export loadds parm [eax] value [eax]
#pragma aux (__DllExport) exported_add
int exported_add(int a);

/* Test 8: Multiple functions with same calling convention */

#pragma aux math_conv parm [eax] [ebx] value [eax] modify [eax ebx ecx]
#pragma aux (math_conv) add_integers
#pragma aux (math_conv) sub_integers
#pragma aux (math_conv) mul_integers
int add_integers(int a, int b);
int sub_integers(int a, int b);
int mul_integers(int a, int b);

/* Test 9: Nested/complex calling conventions */

/* High-performance matrix operation convention */
#pragma aux __MatrixOp parm [ecx] [edx] [r8] value [eax] modify [eax ecx edx r8 r9] routine
#pragma aux (__MatrixOp) matrix_multiply
void matrix_multiply(void *a, void *b, void *result);

/* Test 10: Calling convention with minimal modifications */

#pragma aux __Preserve parm [eax] value [eax] modify [eax]
#pragma aux (__Preserve) preserve_function
int preserve_function(int value);

/* Test 11: OS-specific calling conventions */

/* Windows API style */
#pragma aux __WinAPI parm routine modify [eax ecx edx]
#pragma aux (__WinAPI) win_function
int win_function(void *hwnd, int msg, int wparam, int lparam);

/* Linux syscall style */
#pragma aux __LinuxSyscall parm [eax] [ebx] [ecx] [edx] [esi] [edi] value [eax] modify [eax] caller
#pragma aux (__LinuxSyscall) linux_syscall
long linux_syscall(int nr, long a1, long a2, long a3, long a4, long a5);

/* Test 12: Callback conventions */

#pragma aux __Callback parm [eax] [ebx] value [eax] modify [eax ebx ecx edx] caller
#pragma aux (__Callback) callback_handler
int callback_handler(void *ctx, int event);

/* Test 13: Real function implementations */

int cdecl_function(int a, int b, int c) {
    return a + b + c;
}

void stdcall_function(int x) {
    /* Function body */
}

int custom_mul(int x, int y) {
    return x * y;
}

int add_integers(int a, int b) {
    return a + b;
}

int sub_integers(int a, int b) {
    return a - b;
}

int mul_integers(int a, int b) {
    return a * b;
}

/* Main function to test calling conventions */
int main(void) {
    int result;

    /* Test cdecl convention */
    result = cdecl_function(1, 2, 3);

    /* Test custom multiplication */
    result = custom_mul(5, 7);

    /* Test math operations */
    result = add_integers(10, 20);
    result = sub_integers(50, 30);
    result = mul_integers(4, 8);

    return result;
}

/*
 * Expected behavior:
 * - Compiler accepts all calling convention definitions
 * - Functions are marked with appropriate conventions
 * - Code generation uses correct register allocation
 * - Stack cleanup follows specified convention (caller vs routine)
 * - Register modifications are tracked correctly
 */
