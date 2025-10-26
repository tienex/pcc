/*
 * example.c - Practical example of using libx86asm
 *
 * This example generates a complete assembly program that implements
 * a simple string length function in multiple assembler formats.
 */

#include "x86asm.h"
#include <stdio.h>
#include <string.h>

/*
 * Generate a strlen function in assembly
 */
static void
generate_strlen_function(const char *output_file, x86asm_format_t format, int bits)
{
    FILE *fp;
    x86asm_ctx_t *ctx;

    fp = fopen(output_file, "w");
    if (!fp) {
        fprintf(stderr, "Failed to open %s\n", output_file);
        return;
    }

    ctx = x86asm_create(format, fp, bits);
    if (!ctx) {
        fprintf(stderr, "Failed to create emitter context\n");
        fclose(fp);
        return;
    }

    /* Add header comment */
    x86asm_comment(ctx, "========================================");
    x86asm_comment(ctx, "String length function");
    x86asm_comment(ctx, "size_t my_strlen(const char *str)");
    x86asm_comment(ctx, "========================================");

    /* Code section */
    x86asm_segment(ctx, SEG_TEXT, NULL);

    /* Function: my_strlen */
    x86asm_label(ctx, "my_strlen", 1);

    if (bits == 64) {
        /* 64-bit implementation */
        x86asm_comment(ctx, "String pointer in RDI (System V ABI)");
        x86asm_comment(ctx, "Return length in RAX");

        /* xor rax, rax - Clear counter */
        X86ASM_INSN2(ctx, "xor",
                     x86asm_op_reg(REG_RAX, 64),
                     x86asm_op_reg(REG_RAX, 64));

        /* loop: */
        x86asm_label(ctx, "loop", 0);

        /* cmp byte [rdi + rax], 0 - Check for null terminator */
        X86ASM_INSN2(ctx, "cmp",
                     x86asm_op_mem(REG_RDI, REG_RAX, 1, 0, 8),
                     x86asm_op_imm(0, 8));

        /* je done */
        X86ASM_INSN1(ctx, "je", x86asm_op_label("done"));

        /* inc rax - Increment counter */
        X86ASM_INSN1(ctx, "inc", x86asm_op_reg(REG_RAX, 64));

        /* jmp loop */
        X86ASM_INSN1(ctx, "jmp", x86asm_op_label("loop"));

        /* done: */
        x86asm_label(ctx, "done", 0);

        /* ret */
        X86ASM_INSN0(ctx, "ret");

    } else if (bits == 32) {
        /* 32-bit implementation */
        x86asm_comment(ctx, "String pointer on stack at [esp+4]");
        x86asm_comment(ctx, "Return length in EAX");

        /* push ebp */
        X86ASM_INSN1(ctx, "push", x86asm_op_reg(REG_EBP, 32));

        /* mov ebp, esp */
        X86ASM_INSN2(ctx, "mov",
                     x86asm_op_reg(REG_EBP, 32),
                     x86asm_op_reg(REG_ESP, 32));

        /* mov edx, [ebp+8] - Load string pointer */
        X86ASM_INSN2(ctx, "mov",
                     x86asm_op_reg(REG_EDX, 32),
                     x86asm_op_mem(REG_EBP, REG_NONE, 0, 8, 32));

        /* xor eax, eax - Clear counter */
        X86ASM_INSN2(ctx, "xor",
                     x86asm_op_reg(REG_EAX, 32),
                     x86asm_op_reg(REG_EAX, 32));

        /* loop: */
        x86asm_label(ctx, "loop", 0);

        /* cmp byte [edx + eax], 0 */
        X86ASM_INSN2(ctx, "cmp",
                     x86asm_op_mem(REG_EDX, REG_EAX, 1, 0, 8),
                     x86asm_op_imm(0, 8));

        /* je done */
        X86ASM_INSN1(ctx, "je", x86asm_op_label("done"));

        /* inc eax */
        X86ASM_INSN1(ctx, "inc", x86asm_op_reg(REG_EAX, 32));

        /* jmp loop */
        X86ASM_INSN1(ctx, "jmp", x86asm_op_label("loop"));

        /* done: */
        x86asm_label(ctx, "done", 0);

        /* pop ebp */
        X86ASM_INSN1(ctx, "pop", x86asm_op_reg(REG_EBP, 32));

        /* ret */
        X86ASM_INSN0(ctx, "ret");
    }

    /* Data section with test string */
    x86asm_segment(ctx, SEG_RODATA, NULL);
    x86asm_comment(ctx, "Test string");
    x86asm_label(ctx, "test_string", 1);
    const char *test_str = "Hello, x86asm library!";
    x86asm_data(ctx, DATA_ASCIZ, test_str, strlen(test_str));

    /* Cleanup */
    x86asm_destroy(ctx);
    fclose(fp);

    printf("Generated: %s\n", output_file);
}

/*
 * Generate a more complex function with SIMD
 */
static void
generate_vector_add_function(const char *output_file, x86asm_format_t format)
{
    FILE *fp;
    x86asm_ctx_t *ctx;

    fp = fopen(output_file, "w");
    if (!fp) {
        fprintf(stderr, "Failed to open %s\n", output_file);
        return;
    }

    ctx = x86asm_create(format, fp, 64);
    if (!ctx) {
        fprintf(stderr, "Failed to create emitter context\n");
        fclose(fp);
        return;
    }

    x86asm_comment(ctx, "========================================");
    x86asm_comment(ctx, "Vector addition using SSE");
    x86asm_comment(ctx, "void vec_add(float *dst, float *a, float *b, size_t n)");
    x86asm_comment(ctx, "========================================");

    x86asm_segment(ctx, SEG_TEXT, NULL);
    x86asm_label(ctx, "vec_add", 1);

    x86asm_comment(ctx, "Parameters: RDI=dst, RSI=a, RDX=b, RCX=n");

    /* xor rax, rax - Initialize loop counter */
    X86ASM_INSN2(ctx, "xor",
                 x86asm_op_reg(REG_RAX, 64),
                 x86asm_op_reg(REG_RAX, 64));

    /* loop: */
    x86asm_label(ctx, "loop", 0);

    /* cmp rax, rcx - Check if done */
    X86ASM_INSN2(ctx, "cmp",
                 x86asm_op_reg(REG_RAX, 64),
                 x86asm_op_reg(REG_RCX, 64));

    /* jge done */
    X86ASM_INSN1(ctx, "jge", x86asm_op_label("done"));

    x86asm_comment(ctx, "Load 4 floats from a");
    /* movaps xmm0, [rsi + rax*4] */
    X86ASM_INSN2(ctx, "movaps",
                 x86asm_op_reg(REG_XMM0, 128),
                 x86asm_op_mem(REG_RSI, REG_RAX, 4, 0, 128));

    x86asm_comment(ctx, "Load 4 floats from b");
    /* movaps xmm1, [rdx + rax*4] */
    X86ASM_INSN2(ctx, "movaps",
                 x86asm_op_reg(REG_XMM1, 128),
                 x86asm_op_mem(REG_RDX, REG_RAX, 4, 0, 128));

    x86asm_comment(ctx, "Add vectors");
    /* addps xmm0, xmm1 */
    X86ASM_INSN2(ctx, "addps",
                 x86asm_op_reg(REG_XMM0, 128),
                 x86asm_op_reg(REG_XMM1, 128));

    x86asm_comment(ctx, "Store result");
    /* movaps [rdi + rax*4], xmm0 */
    X86ASM_INSN2(ctx, "movaps",
                 x86asm_op_mem(REG_RDI, REG_RAX, 4, 0, 128),
                 x86asm_op_reg(REG_XMM0, 128));

    /* add rax, 4 - Process 4 elements at a time */
    X86ASM_INSN2(ctx, "add",
                 x86asm_op_reg(REG_RAX, 64),
                 x86asm_op_imm(4, 64));

    /* jmp loop */
    X86ASM_INSN1(ctx, "jmp", x86asm_op_label("loop"));

    /* done: */
    x86asm_label(ctx, "done", 0);

    /* ret */
    X86ASM_INSN0(ctx, "ret");

    x86asm_destroy(ctx);
    fclose(fp);

    printf("Generated: %s\n", output_file);
}

/*
 * Main program
 */
int
main(void)
{
    printf("libx86asm Example Program\n");
    printf("==========================\n\n");

    /* Generate strlen in different formats */
    printf("Generating strlen functions:\n");
    generate_strlen_function("strlen_nasm_64.asm", ASM_FMT_NASM, 64);
    generate_strlen_function("strlen_gas_64.asm", ASM_FMT_GNU_AS, 64);
    generate_strlen_function("strlen_masm_32.asm", ASM_FMT_MASM, 32);
    generate_strlen_function("strlen_yasm_64.asm", ASM_FMT_YASM, 64);
    printf("\n");

    /* Generate vector add in different formats */
    printf("Generating SIMD vector addition:\n");
    generate_vector_add_function("vec_add_nasm.asm", ASM_FMT_NASM);
    generate_vector_add_function("vec_add_gas.asm", ASM_FMT_GNU_AS);
    generate_vector_add_function("vec_add_yasm.asm", ASM_FMT_YASM);
    printf("\n");

    printf("Done! Check the generated .asm files.\n");
    printf("\nTo assemble with NASM:\n");
    printf("  nasm -f elf64 strlen_nasm_64.asm\n");
    printf("\nTo assemble with GAS:\n");
    printf("  as -o strlen_gas_64.o strlen_gas_64.asm\n");

    return 0;
}
