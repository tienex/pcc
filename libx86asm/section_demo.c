/*
 * section_demo.c - Demonstrates comprehensive section support
 *
 * This example shows how to use various section types supported by libx86asm:
 * - Common sections (.text, .data, .bss, .rodata)
 * - ELF sections (init/fini arrays, TLS)
 * - PE/COFF sections (import data, exception handling)
 * - Mach-O sections (Objective-C metadata, literals)
 * - DWARF debug sections
 */

#include "x86asm.h"
#include <stdio.h>
#include <string.h>

/* Generate a simple program with multiple section types */
void
generate_multisection_program(x86asm_format_t fmt, const char *filename)
{
    FILE *fp;
    x86asm_ctx_t *ctx;
    uint32_t init_val = 42;
    const char *msg = "Hello from .rodata!";

    fp = fopen(filename, "w");
    if (!fp) {
        perror("fopen");
        return;
    }

    ctx = x86asm_create(fmt, fp, 64);

    /* Code section - main function */
    x86asm_segment(ctx, SEG_TEXT, NULL);
    x86asm_comment(ctx, "Main code section");
    x86asm_label(ctx, "main", 1);

    /* Load data from .rodata */
    X86ASM_INSN2(ctx, "lea",
                 x86asm_op_reg(REG_RDI, 64),
                 x86asm_op_mem_symbol("message", 0, 64));
    X86ASM_INSN0(ctx, "ret");

    /* Read-only data section */
    x86asm_segment(ctx, SEG_RODATA, NULL);
    x86asm_comment(ctx, "Read-only data section");
    x86asm_label(ctx, "message", 0);
    x86asm_data(ctx, DATA_ASCIZ, msg, strlen(msg));

    /* Initialized data section */
    x86asm_segment(ctx, SEG_DATA, NULL);
    x86asm_comment(ctx, "Initialized data section");
    x86asm_label(ctx, "counter", 1);
    x86asm_data(ctx, DATA_DWORD, &init_val, 1);

    /* BSS (uninitialized data) section */
    x86asm_segment(ctx, SEG_BSS, NULL);
    x86asm_comment(ctx, "Uninitialized data section");
    x86asm_label(ctx, "buffer", 1);
    x86asm_directive(ctx, "skip", "1024");

    /* Constructor function (runs before main) */
    x86asm_segment(ctx, SEG_CTORS, NULL);
    x86asm_comment(ctx, "Global constructor section");
    x86asm_label(ctx, "init_func", 1);
    X86ASM_INSN2(ctx, "mov",
                 x86asm_op_reg(REG_EAX, 32),
                 x86asm_op_imm(0, 32));
    X86ASM_INSN0(ctx, "ret");

    /* Thread-Local Storage */
    x86asm_segment(ctx, SEG_TDATA, NULL);
    x86asm_comment(ctx, "Thread-local storage section");
    x86asm_label(ctx, "thread_counter", 1);
    x86asm_data(ctx, DATA_DWORD, &init_val, 1);

    /* Exception handling frame (for platforms that support it) */
    x86asm_segment(ctx, SEG_EH_FRAME, NULL);
    x86asm_comment(ctx, "Exception handling frame");

    /* Debug info section (for DWARF debugging) */
    x86asm_segment(ctx, SEG_DEBUG_INFO, NULL);
    x86asm_comment(ctx, "DWARF debug information section");

    x86asm_destroy(ctx);
    fclose(fp);
}

/* Generate Mach-O specific example (macOS/Darwin) */
void
generate_macho_example(const char *filename)
{
    FILE *fp;
    x86asm_ctx_t *ctx;
    const char *cstr = "Mach-O string";
    uint32_t literal4 = 0x3f800000; /* 1.0f in IEEE 754 */

    fp = fopen(filename, "w");
    if (!fp) {
        perror("fopen");
        return;
    }

    ctx = x86asm_create(ASM_FMT_APPLE_AS, fp, 64);

    /* Main code */
    x86asm_segment(ctx, SEG_TEXT, NULL);
    x86asm_comment(ctx, "Mach-O example");
    x86asm_label(ctx, "test_func", 1);
    X86ASM_INSN0(ctx, "ret");

    /* C string literals section */
    x86asm_segment(ctx, SEG_CSTRING, NULL);
    x86asm_comment(ctx, "C string literals");
    x86asm_label(ctx, "str1", 0);
    x86asm_data(ctx, DATA_ASCIZ, cstr, strlen(cstr));

    /* 4-byte literals section */
    x86asm_segment(ctx, SEG_LITERAL4, NULL);
    x86asm_comment(ctx, "4-byte literal pool");
    x86asm_label(ctx, "float_one", 0);
    x86asm_data(ctx, DATA_DWORD, &literal4, 1);

    /* Module initialization function */
    x86asm_segment(ctx, SEG_MOD_INIT_FUNC, NULL);
    x86asm_comment(ctx, "Module initializer");
    x86asm_directive(ctx, "quad", "init_func");

    x86asm_destroy(ctx);
    fclose(fp);
}

/* Generate PE/COFF example (Windows) */
void
generate_pecoff_example(const char *filename)
{
    FILE *fp;
    x86asm_ctx_t *ctx;

    fp = fopen(filename, "w");
    if (!fp) {
        perror("fopen");
        return;
    }

    ctx = x86asm_create(ASM_FMT_MASM, fp, 64);

    /* Main code */
    x86asm_segment(ctx, SEG_TEXT, NULL);
    x86asm_comment(ctx, "PE/COFF example");
    x86asm_label(ctx, "WinMain", 1);
    X86ASM_INSN0(ctx, "ret");

    /* Import data section (for DLL imports) */
    x86asm_segment(ctx, SEG_IDATA, NULL);
    x86asm_comment(ctx, "Import data section");

    /* Exception handling data (x64 Windows) */
    x86asm_segment(ctx, SEG_PDATA, NULL);
    x86asm_comment(ctx, "Exception handling procedure data");

    /* Read-only data */
    x86asm_segment(ctx, SEG_RDATA, NULL);
    x86asm_comment(ctx, "Read-only data section");
    x86asm_label(ctx, "const_data", 1);

    /* TLS section for Windows */
    x86asm_segment(ctx, SEG_TLS, NULL);
    x86asm_comment(ctx, "Thread-local storage");
    x86asm_label(ctx, "tls_var", 1);

    x86asm_destroy(ctx);
    fclose(fp);
}

int
main(void)
{
    printf("Section Support Demo\n");
    printf("====================\n\n");

    /* Generate examples for different formats */
    printf("Generating GNU AS (ELF) example with multiple sections...\n");
    generate_multisection_program(ASM_FMT_GNU_AS, "sections_gnu_as.asm");

    printf("Generating NASM example with multiple sections...\n");
    generate_multisection_program(ASM_FMT_NASM, "sections_nasm.asm");

    printf("Generating Apple AS (Mach-O) example...\n");
    generate_macho_example("sections_apple_as.asm");

    printf("Generating MASM (PE/COFF) example...\n");
    generate_pecoff_example("sections_masm.asm");

    printf("\nGenerated assembly files:\n");
    printf("  sections_gnu_as.asm   - ELF sections (GNU AS)\n");
    printf("  sections_nasm.asm     - ELF sections (NASM)\n");
    printf("  sections_apple_as.asm - Mach-O sections (Apple AS)\n");
    printf("  sections_masm.asm     - PE/COFF sections (MASM)\n");
    printf("\nThese examples demonstrate:\n");
    printf("  - Common sections (.text, .data, .bss, .rodata)\n");
    printf("  - Constructor/destructor sections\n");
    printf("  - Thread-Local Storage (TLS)\n");
    printf("  - Exception handling sections\n");
    printf("  - Debug information sections\n");
    printf("  - Platform-specific sections (Mach-O, PE/COFF)\n");

    return 0;
}
