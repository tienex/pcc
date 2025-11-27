/*
 * Test file for i386 memory models and extensions
 * Compile with: pcc -mcmodel=large -c test_i386_extensions.c
 */

/* Test __far16 - Watcom 16-bit far pointer */
#ifdef __i386__
unsigned char __far16 *video_mem = (unsigned char __far16 *)0xB8000000L;

void write_to_16bit_far(char __far16 *dest, const char *src, int len) {
    while (len--) {
        *dest++ = *src++;
    }
}

/* Test __fs segment register access */
struct thread_local_data {
    int thread_id;
    void *stack_base;
};

int get_thread_id(void) {
    struct thread_local_data __fs *tls = (struct thread_local_data __fs *)0;
    return tls->thread_id;
}

/* Test __gs segment register access */
void *read_gs_pointer(unsigned int offset) {
    void __gs **ptr = (void __gs **)offset;
    return *ptr;
}

/* Mixed usage */
struct segment_descriptor {
    unsigned short selector;
    void __far16 *legacy_ptr;    /* 16-bit far pointer */
    void __fs *fs_ptr;            /* FS-based pointer */
    void __gs *gs_ptr;            /* GS-based pointer */
};

/* Function using attribute form */
void __attribute__((far16)) legacy_callback(int event) {
    /* This function uses 16-bit far pointer conventions */
}

/* Memory model test - in large model, pointers are 48-bit by default */
void test_large_model(void) {
    int *normal_ptr;              /* 48-bit in large model */
    char *string = "test";        /* 48-bit far pointer to constant */
    void __far16 *compat_ptr;     /* Explicitly 16-bit far */
}

#endif /* __i386__ */
