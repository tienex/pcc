/*
 * Comprehensive test file for Watcom pragma support in PCC
 * Tests all newly added pragma directives
 */

/* Test #pragma once - include guard replacement */
#pragma once

/* Test #pragma message - print message during compilation */
#pragma message("Compiling test_all_pragmas.c")

/* Test #pragma comment - embed comment in object file */
#pragma comment(compiler, "PCC with Watcom pragma support")
#pragma comment(lib, "mylib")

/* Test #pragma aux - calling convention and register usage */
#pragma aux fast_add parm [eax] [ebx] value [eax]
int fast_add(int a, int b);

#pragma aux multiply parm [ecx] [edx] value [eax] modify [eax ebx]
int multiply(int x, int y);

#pragma aux caller_cleanup parm [eax] caller
void caller_cleanup(int arg);

/* Test #pragma warning - warning control */
#pragma warning(disable : 123 456)
int maybe_problematic_code(void);
#pragma warning(enable : 123)

/* Test #pragma intrinsic - use intrinsic implementations */
#pragma intrinsic(memcpy, memset, strlen)

/* Test #pragma function - disable intrinsic for specific functions */
#pragma function(strcpy, strcmp)

/* Test #pragma code_seg/data_seg - segment control */
#pragma code_seg("INIT_CODE")
void init_function(void) {
    /* Initialization code */
}
#pragma code_seg()

#pragma data_seg("SHARED_DATA")
int shared_variable = 0;
#pragma data_seg()

/* Test #pragma alloc_text - allocate function to segment */
#pragma alloc_text(INIT, early_init)
void early_init(void);

/* Test #pragma inline_depth - control inline expansion */
#pragma inline_depth(8)
inline int compute(int x) { return x * 2; }

/* Test #pragma inline_recursion - control recursive inlining */
#pragma inline_recursion(on)

/* Test #pragma auto_inline - control automatic inlining */
#pragma auto_inline(on)

/* Test #pragma enum - control enum size */
#pragma enum int
enum Status {
    OK,
    ERROR,
    PENDING
};

/* Test #pragma pack - already existing, but worth demonstrating */
#pragma pack(push, 1)
struct PackedStruct {
    char c;
    int i;
    short s;
};
#pragma pack(pop)

/* Test #pragma library - specify library to link */
#pragma library("advapi32")

/* Test #pragma include_alias - create include alias */
#pragma include_alias("myheader.h", "real_header.h")

/* Test #pragma disable_message/enable_message */
#pragma disable_message(201, 202, 203)
int test_function(void) {
    return 42;
}
#pragma enable_message(201)

/* Test #pragma extref - external reference */
#pragma extref external_symbol

/* Test #pragma read_only_file - mark file read-only */
#pragma read_only_file

/* Test #pragma weak - weak symbols (existing) */
#pragma weak weak_function
void weak_function(void);

/* Test #pragma aligned and packed (existing) */
#pragma aligned 16
struct AlignedStruct {
    double d;
    int i;
};

#pragma packed 1
struct PackedStruct2 {
    char c;
    long l;
};

/* Test multiple pragma aux definitions */
#pragma aux __cdecl "_*" parm caller [] modify [eax ecx edx]
#pragma aux __stdcall "_*@*" parm routine [] modify [eax ecx edx]
#pragma aux __fastcall "@_*@*" parm [ecx edx] modify [eax ecx edx]

/* Test #pragma aux with loadds and export */
#pragma aux dll_export loadds export parm [eax] value [eax]
int dll_export(int param);

/* Main function */
int main(void) {
    struct PackedStruct ps = { 'A', 42, 100 };
    struct AlignedStruct as = { 3.14, 99 };

    #pragma message("Inside main function")

    return 0;
}

/* Note: The #pragma error directive would stop compilation, so it's commented out */
/* #pragma error "This is a custom error message" */
