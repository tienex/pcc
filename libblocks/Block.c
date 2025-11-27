/*
 * Block.c - Apple Blocks Runtime Implementation
 *
 * Provides runtime support for Apple's Blocks C language extension.
 * Based on LLVM compiler-rt BlocksRuntime implementation.
 *
 * LICENSE: Dual licensed under UIUC or MIT (your choice)
 */

#define BLOCK_IMPL
#include "Block.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __linux__
#include <unistd.h>
#endif

/* ================================================================
 * Block Class Objects
 * ================================================================ */

/* Placeholder class structures for Objective-C compatibility */
static void *_NSConcreteStackBlock_storage[32] = { NULL };
static void *_NSConcreteGlobalBlock_storage[32] = { NULL };
static void *_NSConcreteMallocBlock_storage[32] = { NULL };

void *_NSConcreteStackBlock[32] = { _NSConcreteStackBlock_storage };
void *_NSConcreteGlobalBlock[32] = { _NSConcreteGlobalBlock_storage };
void *_NSConcreteMallocBlock[32] = { _NSConcreteMallocBlock_storage };

/* ================================================================
 * Internal Helper Functions
 * ================================================================ */

static inline struct Block_descriptor_2 *
_Block_descriptor_2(struct Block_layout *aBlock)
{
    if (!(aBlock->flags & BLOCK_HAS_COPY_DISPOSE))
        return NULL;

    uint8_t *desc = (uint8_t *)aBlock->descriptor;
    desc += sizeof(struct Block_descriptor_1);
    return (struct Block_descriptor_2 *)desc;
}

static inline struct Block_descriptor_3 *
_Block_descriptor_3(struct Block_layout *aBlock)
{
    if (!(aBlock->flags & BLOCK_HAS_SIGNATURE))
        return NULL;

    uint8_t *desc = (uint8_t *)aBlock->descriptor;
    desc += sizeof(struct Block_descriptor_1);

    if (aBlock->flags & BLOCK_HAS_COPY_DISPOSE)
        desc += sizeof(struct Block_descriptor_2);

    return (struct Block_descriptor_3 *)desc;
}

static inline int
_Block_get_refcount(struct Block_layout *aBlock)
{
    return (aBlock->flags & BLOCK_REFCOUNT_MASK) >> 1;
}

static inline void
_Block_set_refcount(struct Block_layout *aBlock, int refcount)
{
    aBlock->flags = (aBlock->flags & ~BLOCK_REFCOUNT_MASK) |
                    ((refcount << 1) & BLOCK_REFCOUNT_MASK);
}

/* ================================================================
 * Block Copy Implementation
 * ================================================================ */

void *
_Block_copy(const void *arg)
{
    struct Block_layout *aBlock;

    if (!arg)
        return NULL;

    aBlock = (struct Block_layout *)arg;

    /* If it's already a heap block, just increment refcount */
    if (aBlock->flags & BLOCK_NEEDS_FREE) {
        /* Atomic increment would be here in production code */
        int refcount = _Block_get_refcount(aBlock);
        _Block_set_refcount(aBlock, refcount + 1);  /* Increment refcount by 1 */
        return aBlock;
    }

    /* Global blocks are never copied */
    if (aBlock->flags & BLOCK_IS_GLOBAL)
        return aBlock;

    /* Copy stack block to heap */
    size_t size = aBlock->descriptor->size;
    struct Block_layout *result = (struct Block_layout *)malloc(size);

    if (!result)
        return NULL;

    /* Copy the block structure */
    memcpy(result, aBlock, size);

    /* Update flags */
    result->flags &= ~(BLOCK_REFCOUNT_MASK | BLOCK_DEALLOCATING);
    result->flags |= BLOCK_NEEDS_FREE;
    _Block_set_refcount(result, 1);  /* Initial refcount of 1 */

    /* Update isa pointer to heap block class */
    result->isa = _NSConcreteMallocBlock;

    /* Call copy helper if present */
    struct Block_descriptor_2 *desc2 = _Block_descriptor_2(result);
    if (desc2 && desc2->copy) {
        desc2->copy(result, aBlock);
    }

    return result;
}

/* ================================================================
 * Block Release Implementation
 * ================================================================ */

void
_Block_release(const void *arg)
{
    struct Block_layout *aBlock;
    int refcount;

    if (!arg)
        return;

    aBlock = (struct Block_layout *)arg;

    /* Global blocks are never released */
    if (aBlock->flags & BLOCK_IS_GLOBAL)
        return;

    /* Stack blocks shouldn't be released, but be lenient */
    if (!(aBlock->flags & BLOCK_NEEDS_FREE))
        return;

    /* Atomic decrement would be here in production code */
    refcount = _Block_get_refcount(aBlock);

    if (refcount > 1) {
        /* Still has references */
        _Block_set_refcount(aBlock, refcount - 1);
        return;
    }

    /* Refcount hit zero, deallocate */

    /* Call dispose helper if present */
    struct Block_descriptor_2 *desc2 = _Block_descriptor_2(aBlock);
    if (desc2 && desc2->dispose) {
        desc2->dispose(aBlock);
    }

    /* Free the block */
    free(aBlock);
}

/* ================================================================
 * Block Query Functions
 * ================================================================ */

int
_Block_refcount(const void *arg)
{
    struct Block_layout *aBlock;

    if (!arg)
        return 0;

    aBlock = (struct Block_layout *)arg;

    if (aBlock->flags & BLOCK_IS_GLOBAL)
        return 0;

    if (!(aBlock->flags & BLOCK_NEEDS_FREE))
        return 1;  /* Stack block */

    return _Block_get_refcount(aBlock);
}

int
_Block_is_heap(const void *arg)
{
    struct Block_layout *aBlock;

    if (!arg)
        return 0;

    aBlock = (struct Block_layout *)arg;
    return (aBlock->flags & BLOCK_NEEDS_FREE) ? 1 : 0;
}

int
_Block_is_global(const void *arg)
{
    struct Block_layout *aBlock;

    if (!arg)
        return 0;

    aBlock = (struct Block_layout *)arg;
    return (aBlock->flags & BLOCK_IS_GLOBAL) ? 1 : 0;
}

/* ================================================================
 * Utility Functions
 * ================================================================ */

size_t
Block_size(void *arg)
{
    struct Block_layout *aBlock;

    if (!arg)
        return 0;

    aBlock = (struct Block_layout *)arg;
    return aBlock->descriptor->size;
}

const char *
Block_signature(void *arg)
{
    struct Block_layout *aBlock;
    struct Block_descriptor_3 *desc3;

    if (!arg)
        return NULL;

    aBlock = (struct Block_layout *)arg;
    desc3 = _Block_descriptor_3(aBlock);

    return desc3 ? desc3->signature : NULL;
}

const char *
Block_layout(void *arg)
{
    struct Block_layout *aBlock;
    struct Block_descriptor_3 *desc3;

    if (!arg)
        return NULL;

    aBlock = (struct Block_layout *)arg;
    desc3 = _Block_descriptor_3(aBlock);

    return desc3 ? desc3->layout : NULL;
}

/* ================================================================
 * Object Assignment Helpers (for Objective-C compatibility)
 * ================================================================ */

enum {
    BLOCK_FIELD_IS_OBJECT   =  3,  /* id, NSObject, __attribute__((NSObject)), block */
    BLOCK_FIELD_IS_BLOCK    =  7,  /* a block variable */
    BLOCK_FIELD_IS_BYREF    =  8,  /* the on stack structure holding __block variable */
    BLOCK_FIELD_IS_WEAK     = 16,  /* declared __weak */
    BLOCK_FIELD_IS_ARC      = 128  /* called under ARC */
};

void
_Block_object_assign(void *dest, const void *src, const int flags)
{
    /* Simplified implementation - just copy the pointer */
    /* In a full implementation, this would handle retain/release */

    if (flags & BLOCK_FIELD_IS_BLOCK) {
        *(void **)dest = _Block_copy(src);
    } else {
        *(void **)dest = (void *)src;
    }
}

void
_Block_object_dispose(const void *object, const int flags)
{
    /* Simplified implementation */

    if (flags & BLOCK_FIELD_IS_BLOCK) {
        _Block_release(object);
    }
}

/* ================================================================
 * Byref Support (__block variables)
 * ================================================================ */

struct Block_byref {
    void *isa;
    struct Block_byref *forwarding;
    int flags;
    int size;
    /* Followed by variable */
};

void
_Block_byref_assign_copy(void *dest, const void *src)
{
    struct Block_byref *src_byref = *(struct Block_byref **)src;
    struct Block_byref **dst_byref = (struct Block_byref **)dest;

    if (!src_byref) {
        *dst_byref = NULL;
        return;
    }

    /* If already on heap, just copy the pointer */
    if (src_byref->flags & BLOCK_NEEDS_FREE) {
        *dst_byref = src_byref;
        return;
    }

    /* Copy to heap */
    size_t size = src_byref->size;
    struct Block_byref *copy = (struct Block_byref *)malloc(size);

    if (copy) {
        memcpy(copy, src_byref, size);
        copy->flags |= BLOCK_NEEDS_FREE;
        copy->forwarding = copy;
        *dst_byref = copy;
    }
}

void
_Block_byref_release(const void *arg)
{
    struct Block_byref *byref = *(struct Block_byref **)arg;

    if (!byref)
        return;

    if (byref->flags & BLOCK_NEEDS_FREE) {
        /* In a full implementation, would check refcount */
        free(byref);
    }
}

/* ================================================================
 * Runtime Initialization
 * ================================================================ */

void
_Block_runtime_init(void)
{
    /* Initialize block classes if needed */
    /* In this minimal implementation, already initialized statically */
}

/* ================================================================
 * Debug/Info Functions
 * ================================================================ */

#ifdef BLOCK_DEBUG

void
_Block_dump(void *arg)
{
    struct Block_layout *aBlock;

    if (!arg) {
        printf("Block: (null)\n");
        return;
    }

    aBlock = (struct Block_layout *)arg;

    printf("Block %p:\n", aBlock);
    printf("  isa:        %p\n", aBlock->isa);
    printf("  flags:      0x%08x", aBlock->flags);

    if (aBlock->flags & BLOCK_IS_GLOBAL)
        printf(" GLOBAL");
    if (aBlock->flags & BLOCK_NEEDS_FREE)
        printf(" HEAP");
    if (aBlock->flags & BLOCK_HAS_COPY_DISPOSE)
        printf(" COPY_DISPOSE");
    if (aBlock->flags & BLOCK_HAS_SIGNATURE)
        printf(" SIGNATURE");

    printf("\n");
    printf("  refcount:   %d\n", _Block_refcount(aBlock));
    printf("  invoke:     %p\n", aBlock->invoke);
    printf("  descriptor: %p\n", aBlock->descriptor);
    printf("  size:       %zu bytes\n", (size_t)aBlock->descriptor->size);

    const char *sig = Block_signature(aBlock);
    if (sig)
        printf("  signature:  %s\n", sig);
}

#endif /* BLOCK_DEBUG */
