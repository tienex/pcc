/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Microsoft DOS Compiler Virtual Memory Compatibility Implementation
 */

#include "vmem_compat.h"
#include <stdlib.h>
#include <string.h>

/* Swap file configuration */
static struct {
	char filename[128];
	unsigned long max_size;
	int enabled;
} swap_config = {{0}, 0, 0};

/* Initialize virtual memory */
int
_vm_init(void)
{
	return (dosmem_init() == DOSMEM_SUCCESS) ? _VM_SUCCESS : _VM_NOMEM;
}

/* Shutdown virtual memory */
void
_vm_term(void)
{
	dosmem_shutdown();
}

/* Allocate virtual memory */
_vmhnd_t
_vm_alloc(unsigned long size)
{
	return _vm_allocx(size, 0);
}

/* Allocate virtual memory with flags */
_vmhnd_t
_vm_allocx(unsigned long size, unsigned int flags)
{
	dosmem_handle_t *handle;
	unsigned int dosmem_flags = 0;
	unsigned int mem_type = DOSMEM_ANY;
	int result;

	if (size == 0)
		return NULL;

	handle = (dosmem_handle_t *)malloc(sizeof(dosmem_handle_t));
	if (!handle)
		return NULL;

	/* Convert Microsoft flags to dosmem flags */
	if (flags & _VM_CLEAN)
		dosmem_flags |= DOSMEM_ZERO;

	/* Allocate memory */
	result = dosmem_alloc(size, mem_type, dosmem_flags, handle);
	if (result != DOSMEM_SUCCESS) {
		free(handle);
		return NULL;
	}

	return handle;
}

/* Allocate from specific memory type */
_vmhnd_t
_vm_alloc_typed(unsigned long size, unsigned int type, unsigned int flags)
{
	dosmem_handle_t *handle;
	int result;

	if (size == 0)
		return NULL;

	handle = (dosmem_handle_t *)malloc(sizeof(dosmem_handle_t));
	if (!handle)
		return NULL;

	result = dosmem_alloc(size, type, flags, handle);
	if (result != DOSMEM_SUCCESS) {
		free(handle);
		return NULL;
	}

	return handle;
}

/* Free virtual memory */
int
_vm_free(_vmhnd_t handle)
{
	int result;

	if (!handle)
		return _VM_BADHANDLE;

	result = dosmem_free(handle);
	free(handle);

	return (result == DOSMEM_SUCCESS) ? _VM_SUCCESS : _VM_BADHANDLE;
}

/* Lock virtual memory */
void *
_vm_lock(_vmhnd_t handle)
{
	if (!handle)
		return NULL;

	/* If already locked or if we can lock it */
	if (handle->locked || dosmem_lock(handle) == DOSMEM_SUCCESS) {
		return dosmem_get_pointer(handle);
	}

	return NULL;
}

/* Unlock virtual memory */
int
_vm_unlock(_vmhnd_t handle)
{
	int result;

	if (!handle)
		return _VM_BADHANDLE;

	if (!handle->locked)
		return _VM_NOTLOCKED;

	result = dosmem_unlock(handle);
	return (result == DOSMEM_SUCCESS) ? _VM_SUCCESS : _VM_BADHANDLE;
}

/* Reallocate virtual memory */
_vmhnd_t
_vm_realloc(_vmhnd_t handle, unsigned long new_size)
{
	int result;

	if (!handle)
		return NULL;

	if (new_size == 0) {
		_vm_free(handle);
		return NULL;
	}

	result = dosmem_resize(handle, new_size);
	if (result == DOSMEM_SUCCESS)
		return handle;

	/* Resize failed - allocate new block and copy */
	{
		_vmhnd_t new_handle;
		unsigned long copy_size;
		void *old_ptr, *new_ptr;

		new_handle = _vm_alloc(new_size);
		if (!new_handle)
			return NULL;

		/* Copy data */
		copy_size = (handle->size < new_size) ? handle->size : new_size;

		old_ptr = dosmem_get_pointer(handle);
		new_ptr = dosmem_get_pointer(new_handle);

		if (old_ptr && new_ptr) {
			memcpy(new_ptr, old_ptr, copy_size);
		} else {
			/* Use copy functions for non-directly-accessible memory */
			dosmem_copy_from(new_ptr, handle, 0, copy_size);
		}

		/* Free old handle */
		dosmem_free(handle);
		free(handle);

		return new_handle;
	}
}

/* Get size of allocated block */
unsigned long
_vm_size(_vmhnd_t handle)
{
	if (!handle)
		return 0;

	return handle->size;
}

/* Query available virtual memory */
unsigned long
_vm_avail(void)
{
	dosmem_info_t info;

	if (dosmem_get_info(&info) != DOSMEM_SUCCESS)
		return 0;

	/* Return total free memory in bytes */
	return info.total_free * 1024;
}

/* Set virtual memory swap file */
int
_vm_setswap(const char *filename, unsigned long max_size)
{
	if (!filename)
		return _VM_BADSIZE;

	strncpy(swap_config.filename, filename, sizeof(swap_config.filename) - 1);
	swap_config.filename[sizeof(swap_config.filename) - 1] = '\0';
	swap_config.max_size = max_size;
	swap_config.enabled = 1;

	/* Note: Actual swap file implementation would go here */
	/* This is a placeholder for future disk swapping support */

	return _VM_SUCCESS;
}

/* Get information about virtual memory subsystem */
int
_vm_info(dosmem_info_t *info)
{
	if (!info)
		return _VM_BADHANDLE;

	return (dosmem_get_info(info) == DOSMEM_SUCCESS) ? _VM_SUCCESS : _VM_BADHANDLE;
}

/* Copy data to virtual memory */
int
_vm_copy_to(_vmhnd_t dest, unsigned long offset, const void *src, unsigned long size)
{
	int result;

	if (!dest || !src)
		return _VM_BADHANDLE;

	result = dosmem_copy_to(dest, offset, src, size);
	return (result == DOSMEM_SUCCESS) ? _VM_SUCCESS : _VM_BADHANDLE;
}

/* Copy data from virtual memory */
int
_vm_copy_from(void *dest, _vmhnd_t src, unsigned long offset, unsigned long size)
{
	int result;

	if (!dest || !src)
		return _VM_BADHANDLE;

	result = dosmem_copy_from(dest, src, offset, size);
	return (result == DOSMEM_SUCCESS) ? _VM_SUCCESS : _VM_BADHANDLE;
}
