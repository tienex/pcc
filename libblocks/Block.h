/*
 * Block.h - Apple Blocks Runtime Support for PCC
 *
 * This header provides runtime support for Apple's Blocks C language extension.
 * Blocks are closures (anonymous functions with state capture) introduced by
 * Apple for C, C++, and Objective-C.
 *
 * Standard Proposal: WG14 N1451 (submitted 2010, not adopted)
 * Reference Implementation: LLVM compiler-rt BlocksRuntime
 *
 * USAGE:
 *   With compiler support (Clang with -fblocks):
 *     int (^myBlock)(int) = ^(int x) { return x * 2; };
 *     int result = myBlock(5);
 *
 *   With this runtime library (PCC without -fblocks):
 *     See Block_macros.h for macro-based workarounds
 *
 * LICENSE: Dual licensed under UIUC or MIT (your choice)
 */

#ifndef _BLOCK_H_
#define _BLOCK_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ================================================================
 * Block Descriptor Structures
 * ================================================================ */

/**
 * Block flags (bitfield in Block_layout->flags)
 */
enum {
    BLOCK_DEALLOCATING      = (0x0001),  /* Block is being deallocated */
    BLOCK_REFCOUNT_MASK     = (0xfffe),  /* Reference count mask */
    BLOCK_NEEDS_FREE        = (1 << 24), /* Should free() on dealloc */
    BLOCK_HAS_COPY_DISPOSE  = (1 << 25), /* Has copy/dispose helpers */
    BLOCK_HAS_CTOR          = (1 << 26), /* Has C++ constructors */
    BLOCK_IS_GC             = (1 << 27), /* Allocated with GC */
    BLOCK_IS_GLOBAL         = (1 << 28), /* Global block (never freed) */
    BLOCK_USE_STRET         = (1 << 29), /* Uses stret for return */
    BLOCK_HAS_SIGNATURE     = (1 << 30), /* Has type encoding */
    BLOCK_HAS_EXTENDED_LAYOUT=(1U << 31) /* Has extended layout */
};

/**
 * Block descriptor (version 1)
 * Contains metadata about the block
 */
struct Block_descriptor_1 {
    uintptr_t reserved;  /* NULL for now */
    uintptr_t size;      /* Size of Block_layout structure */
};

/**
 * Block descriptor with copy/dispose helpers (version 2)
 */
struct Block_descriptor_2 {
    /* Copy helper function (called when block is copied to heap) */
    void (*copy)(void *dst, const void *src);

    /* Dispose helper function (called when block is deallocated) */
    void (*dispose)(const void *);
};

/**
 * Block descriptor with signature (version 3)
 */
struct Block_descriptor_3 {
    const char *signature;  /* Type encoding string */
    const char *layout;     /* Extended layout info */
};

/**
 * Complete block layout structure
 * This is what a block actually looks like in memory
 */
struct Block_layout {
    void *isa;                           /* Pointer to class (for Obj-C) */
    volatile int32_t flags;              /* Block flags */
    int32_t reserved;                    /* Reserved for future use */
    void (*invoke)(void *, ...);         /* Function pointer to invoke */
    struct Block_descriptor_1 *descriptor;
    /* Followed by captured variables */
};

/* Convenience typedef */
typedef struct Block_layout *Block;

/* ================================================================
 * Block Runtime Functions
 * ================================================================ */

/**
 * Copy a block to the heap (if not already there)
 * Increments reference count for heap blocks
 * Returns the same block for global blocks
 *
 * @param aBlock Block to copy
 * @return Heap-allocated copy of the block
 */
void *_Block_copy(const void *aBlock);

/**
 * Release a block (decrement reference count)
 * Frees the block if reference count reaches zero
 * No-op for global blocks
 *
 * @param aBlock Block to release
 */
void _Block_release(const void *aBlock);

/**
 * Get the current reference count of a block
 * Returns 0 for global blocks and NULL
 *
 * @param aBlock Block to query
 * @return Current reference count
 */
int _Block_refcount(const void *aBlock);

/**
 * Check if a block is on the heap
 *
 * @param aBlock Block to check
 * @return 1 if heap-allocated, 0 otherwise
 */
int _Block_is_heap(const void *aBlock);

/**
 * Check if a block is global
 *
 * @param aBlock Block to check
 * @return 1 if global, 0 otherwise
 */
int _Block_is_global(const void *aBlock);

/* ================================================================
 * Type-Safe Wrapper Macros
 * ================================================================ */

/**
 * Type-safe Block_copy
 * Casts result to the appropriate type
 */
#define Block_copy(...) \
    ((__typeof__(__VA_ARGS__))_Block_copy((const void *)(__VA_ARGS__)))

/**
 * Type-safe Block_release
 */
#define Block_release(...) \
    _Block_release((const void *)(__VA_ARGS__))

/* ================================================================
 * Block Object Class (for Objective-C compatibility)
 * ================================================================ */

/* These are defined in the runtime */
extern void * _NSConcreteStackBlock[32];  /* Stack block class */
extern void * _NSConcreteGlobalBlock[32]; /* Global block class */
extern void * _NSConcreteMallocBlock[32]; /* Heap block class */

/* ================================================================
 * Helper Macros for Block Declaration (Without Compiler Support)
 * ================================================================ */

#ifndef __BLOCKS__
/*
 * When compiler doesn't support blocks natively, provide helper macros
 * See Block_macros.h for full macro-based simulation
 */

/* Indicate we're using the runtime-only version */
#define BLOCKS_RUNTIME_ONLY 1

/* Function pointer typedef for blocks */
#define BLOCK_TYPE(return_type, ...) \
    return_type (^)(__VA_ARGS__)

#endif /* !__BLOCKS__ */

/* ================================================================
 * Utility Functions
 * ================================================================ */

/**
 * Get the size of a block
 *
 * @param aBlock Block to query
 * @return Size in bytes
 */
size_t Block_size(void *aBlock);

/**
 * Get the signature (type encoding) of a block
 * Returns NULL if block doesn't have signature
 *
 * @param aBlock Block to query
 * @return Type encoding string or NULL
 */
const char *Block_signature(void *aBlock);

/**
 * Get the extended layout of a block
 * Returns NULL if block doesn't have extended layout
 *
 * @param aBlock Block to query
 * @return Layout string or NULL
 */
const char *Block_layout(void *aBlock);

/* ================================================================
 * Internal Functions (For Runtime Use Only)
 * ================================================================ */

#ifdef BLOCK_IMPL

/* Initialize the block runtime */
void _Block_runtime_init(void);

/* Object assignment helpers */
void _Block_object_assign(void *dest, const void *src, const int flags);
void _Block_object_dispose(const void *object, const int flags);

/* Byref structure support */
void _Block_byref_assign_copy(void *dest, const void *src);
void _Block_byref_release(const void *src);

#endif /* BLOCK_IMPL */

#ifdef __cplusplus
}
#endif

#endif /* _BLOCK_H_ */
