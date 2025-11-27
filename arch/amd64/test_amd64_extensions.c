/*
 * Test file for x86-64 (amd64) extensions
 * Compile with: pcc -c test_amd64_extensions.c
 */

#ifdef __amd64__

/* Test __fs - Thread-Local Storage access (Linux/BSD convention) */
struct thread_info {
    int thread_id;
    void *stack_base;
    unsigned long stack_canary;
};

/* Read thread-local data using FS */
int get_current_thread_id(void) {
    struct thread_info __fs *ti = (struct thread_info __fs *)0;
    return ti->thread_id;
}

/* Get stack canary for security checks */
unsigned long get_stack_canary(void) {
    /* GS:0x28 contains stack canary on Linux x86-64 */
    unsigned long __gs *canary_ptr = (unsigned long __gs *)0x28;
    return *canary_ptr;
}

/* Test __gs - Windows TEB access */
void *get_windows_teb(void) {
    /* Windows x64 uses GS for TEB */
    void __gs **teb_ptr = (void __gs **)0x30;
    return *teb_ptr;
}

/* Test __near32 - macOS 32-bit routine export */
int __near32 legacy_api_function(int x, int y) {
    /* Function uses 32-bit calling convention */
    /* Pointers are 32-bit within this function */
    return x + y;
}

/* 32-bit callback type for legacy APIs */
typedef void (__near32 *legacy_callback_t)(int event, void *data);

void register_32bit_callback(legacy_callback_t callback) {
    /* Callback will be called with 32-bit pointers */
    if (callback) {
        callback(1, (void *)0);
    }
}

/* Wine/CrossOver thunk layer example */
void __near32 *wine_get_dos_path(const char *unix_path) {
    /* Returns 32-bit pointer for 32-bit Windows application */
    static char dos_path[260];
    /* ... conversion logic ... */
    return (void __near32 *)dos_path;
}

/* Attribute form for __near32 */
int __attribute__((near32)) legacy_syscall(int syscall_num) {
    return syscall_num;
}

/* Mixed segment register usage */
struct kernel_percpu_data {
    int cpu_id;
    void *current_task;
    unsigned long kernel_stack;
};

void kernel_setup_gs_base(void *percpu_area) {
    /* Kernel uses GS for per-CPU data */
    struct kernel_percpu_data __gs *data =
        (struct kernel_percpu_data __gs *)percpu_area;
    data->cpu_id = 0;
}

/* TLS access pattern */
__thread int thread_local_var;  /* Traditional TLS variable */

int *get_tls_var_address(void) {
    /* Access TLS variable address via FS */
    /* This demonstrates low-level TLS access */
    return &thread_local_var;
}

#endif /* __amd64__ */
