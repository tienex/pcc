/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Microsoft DOS Compiler Virtual Memory Compatibility Layer
 *
 * Compatible with Microsoft C 7.0, Visual C++ 1.x DOS virtual memory API
 */

#ifndef VMEM_COMPAT_H
#define VMEM_COMPAT_H

#include "dosmem.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Microsoft-compatible virtual memory handle */
typedef dosmem_handle_t* _vmhnd_t;

/* Microsoft-compatible error codes */
#define _VM_SUCCESS		0
#define _VM_NOMEM		1
#define _VM_BADSIZE		2
#define _VM_BADHANDLE		3
#define _VM_NOTLOCKED		4

/* Microsoft-compatible allocation flags */
#define _VM_CLEAN		0x0001	/* Zero-initialize */
#define _VM_ZEROFILL		0x0001	/* Same as _VM_CLEAN */
#define _VM_ALWAYSSWAP		0x0002	/* Allow swapping to disk */
#define _VM_NOSWAP		0x0004	/* Never swap to disk */

/*
 * Microsoft Virtual Memory API - DOS Compiler Compatible
 */

/* Initialize virtual memory manager */
int _vm_init(void);

/* Shutdown virtual memory manager */
void _vm_term(void);

/* Allocate virtual memory */
_vmhnd_t _vm_alloc(unsigned long size);

/* Allocate virtual memory with flags */
_vmhnd_t _vm_allocx(unsigned long size, unsigned int flags);

/* Free virtual memory */
int _vm_free(_vmhnd_t handle);

/* Lock virtual memory (prevent swapping) */
void * _vm_lock(_vmhnd_t handle);

/* Unlock virtual memory */
int _vm_unlock(_vmhnd_t handle);

/* Reallocate virtual memory */
_vmhnd_t _vm_realloc(_vmhnd_t handle, unsigned long new_size);

/* Get size of allocated block */
unsigned long _vm_size(_vmhnd_t handle);

/* Query available virtual memory */
unsigned long _vm_avail(void);

/* Set virtual memory swap file */
int _vm_setswap(const char *filename, unsigned long max_size);

/*
 * Compatibility macros for different naming conventions
 */
#define vmhnd_t			_vmhnd_t
#define vm_init			_vm_init
#define vm_term			_vm_term
#define vm_alloc		_vm_alloc
#define vm_allocx		_vm_allocx
#define vm_free			_vm_free
#define vm_lock			_vm_lock
#define vm_unlock		_vm_unlock
#define vm_realloc		_vm_realloc
#define vm_size			_vm_size
#define vm_avail		_vm_avail
#define vm_setswap		_vm_setswap

/*
 * Enhanced API - PCC Extensions
 * These provide more control than the original Microsoft API
 */

/* Allocate from specific memory type */
_vmhnd_t _vm_alloc_typed(unsigned long size, unsigned int type, unsigned int flags);

/* Get information about virtual memory subsystem */
int _vm_info(dosmem_info_t *info);

/* Copy data to/from virtual memory */
int _vm_copy_to(_vmhnd_t dest, unsigned long offset, const void *src, unsigned long size);
int _vm_copy_from(void *dest, _vmhnd_t src, unsigned long offset, unsigned long size);

#ifdef __cplusplus
}
#endif

#endif /* VMEM_COMPAT_H */
