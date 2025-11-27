/*
 * Comprehensive test file for i86 DOS-era extensions
 * Demonstrates memory models, pointer types, calling conventions, and more
 * Compile with: pcc -mcmodel=large -c test_i86_complete.c
 */

#ifdef __i86__

/* ========== Memory Model Tests ========== */

/* In large model, these are all 32-bit far pointers by default */
int *data_ptr;
char *string = "Hello, DOS!";
void (*func_ptr)(void);

/* ========== Pointer Type Modifiers ========== */

/* Near pointers (16-bit, offset only) */
int __near *near_ptr;
char __near *near_string;

/* Far pointers (32-bit, segment:offset) */
int __far *far_ptr;
unsigned char __far *video_memory = (unsigned char __far *)0xB8000000L;

/* Huge pointers (32-bit, normalized) */
long __huge *huge_array;

/* Based pointers (relative to a segment) */
int __based(__segname("_DATA")) *based_data_ptr;

/* Watcom far16 (forward compatibility) */
void __far16 *watcom_far_ptr;

/* ========== Calling Conventions ========== */

/* C calling convention (default) */
int __cdecl add_numbers(int a, int b) {
    return a + b;
}

/* Pascal calling convention (reverse params, callee cleans stack) */
int __pascal PascalFunction(int x, int y) {
    return x - y;
}

/* Fortran calling convention */
void __fortran FortranSubroutine(int *result, int *a, int *b) {
    *result = *a + *b;
}

/* Watcom syscall convention */
int __syscall WatcomSyscall(int service_num) {
    return service_num;
}

/* Watcom register-based calling convention */
int __watcall FastFunction(int a, int b, int c) {
    return a + b + c;
}

/* ========== Function Modifiers ========== */

/* Interrupt service routine */
void __interrupt timer_isr(void) {
    /* This function will save/restore all registers and use IRET */
    /* ... interrupt handling code ... */
}

/* Interrupt with specific vector number */
void __interrupt __attribute__((interrupt(0x1C))) custom_timer(void) {
    /* Custom timer interrupt handler */
}

/* Load DS on function entry */
void __loadds far_function_with_ds(void) {
    /* DS will be loaded from SS at function entry */
}

/* Save/restore all registers */
void __saveregs critical_function(void) {
    /* All registers saved/restored automatically */
}

/* Export for DLL */
void __export DllExportedFunction(void) {
    /* This function is exported from a DLL */
}

/* ========== Segment Register Access ========== */

/* Access via specific segment registers */
unsigned char __ss *stack_ptr;
unsigned char __cs *code_ptr;
unsigned char __ds *data_seg_ptr;
unsigned char __es *extra_seg_ptr;

/* Forward compatibility - i386+ segment registers */
unsigned char __fs *fs_ptr;
unsigned char __gs *gs_ptr;

/* ========== Raw Byte Emission ========== */

void emit_nop_instructions(void) {
    __emit(0x90);              /* Single NOP */
    __emit(0x90, 0x90);        /* Two NOPs */
    __emit(0x0F, 0x31);        /* RDTSC instruction */
}

/* PCC extension: emit with variables */
void emit_dynamic_opcode(unsigned char opcode) {
    __emit(opcode);            /* Variable opcode */
}

/* ========== MSVC-style Inline Assembly ========== */

void enable_interrupts(void) {
    __asm sti
}

void disable_interrupts(void) {
    __asm cli
}

int get_flags(void) {
    int flags;
    __asm {
        pushf
        pop ax
        mov flags, ax
    }
    return flags;
}

/* ========== Complex Example: Video Memory Access ========== */

struct video_cell {
    char character;
    char attribute;
};

void write_string_to_screen(const char *str, int row, int col, char attr) {
    /* Video memory at B800:0000 in text mode */
    struct video_cell __far *video =
        (struct video_cell __far *)0xB8000000L;

    /* Calculate position: row * 80 + col */
    video += (row * 80 + col);

    while (*str) {
        video->character = *str++;
        video->attribute = attr;
        video++;
    }
}

/* ========== Complex Example: DOS Interrupt Call ========== */

struct dos_regs {
    unsigned int ax, bx, cx, dx;
    unsigned int si, di, bp, ds, es;
};

void __interrupt dos_int21_handler(void) {
    /* DOS INT 21h interrupt handler */
    __asm {
        push ds
        push es
        push bp

        ; Handle DOS interrupt

        pop bp
        pop es
        pop ds
    }
}

/* ========== Complex Example: Memory Model Interaction ========== */

/* Tiny model function (code and data in same 64K segment) */
void __attribute__((tiny)) tiny_function(void) {
    /* All pointers are near in this function */
}

/* Large model function with mixed pointer types */
void process_large_data(void) {
    /* Default pointers are far in large model */
    char *far_string = "Far string";

    /* Can use near pointers for local data */
    char __near local_buffer[100];

    /* Far pointer for accessing distant memory */
    long __far *remote_data = (long __far *)0x80000000L;

    /* Huge pointer for arrays larger than 64K */
    char __huge *huge_buffer;
}

/* ========== Segment Manipulation ========== */

unsigned int get_data_segment(void) {
    unsigned int seg;
    __asm {
        mov ax, ds
        mov seg, ax
    }
    return seg;
}

void set_extra_segment(unsigned int seg) {
    __asm {
        mov ax, seg
        mov es, ax
    }
}

/* ========== Combined Example: Protected Mode DOS Extender ========== */

struct descriptor {
    unsigned short limit_low;
    unsigned short base_low;
    unsigned char base_mid;
    unsigned char access;
    unsigned char granularity;
    unsigned char base_high;
};

void __loadds setup_descriptor(
    struct descriptor __far *gdt,
    unsigned short selector,
    unsigned long base,
    unsigned long limit
) {
    struct descriptor __far *desc = &gdt[selector >> 3];

    desc->limit_low = (unsigned short)(limit & 0xFFFF);
    desc->base_low = (unsigned short)(base & 0xFFFF);
    desc->base_mid = (unsigned char)((base >> 16) & 0xFF);
    desc->base_high = (unsigned char)((base >> 24) & 0xFF);
    desc->access = 0x92;  /* Data segment, read/write */
    desc->granularity = (unsigned char)(((limit >> 16) & 0x0F) | 0x40);
}

/* ========== Watcom Extensions ========== */

/* Segment type */
__segment data_seg;
__segment code_seg;

/* Self-relative addressing */
int __self *self_ptr;

/* ========== All Features Combined ========== */

void __interrupt __loadds __saveregs combined_isr(void) {
    /* Interrupt handler that:
     * - Saves all registers (__saveregs)
     * - Loads DS register (__loadds)
     * - Uses IRET return (__interrupt)
     */

    unsigned char __far *video = (unsigned char __far *)0xB8000000L;
    video[0] = 'I';
    video[1] = 0x0F;  /* White on black */
}

#endif /* __i86__ */
