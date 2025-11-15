/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * DOS Memory Manager - Main Implementation
 */

#include "dosmem.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Internal state */
static struct {
	int initialized;
	dosmem_info_t info;
} dosmem_state = {0};

/* Error messages */
static const char *error_messages[] = {
	"Success",
	"General error",
	"Out of memory",
	"Memory manager not available",
	"Invalid parameter",
	"Memory is locked"
};

int
dosmem_init(void)
{
	if (dosmem_state.initialized)
		return DOSMEM_SUCCESS;

	memset(&dosmem_state.info, 0, sizeof(dosmem_state.info));

	/* Detect DPMI */
	dosmem_state.info.dpmi_available = dosmem_dpmi_available();
	if (dosmem_state.info.dpmi_available) {
		int major, minor;
		dosmem_dpmi_get_version(&major, &minor);
		dosmem_state.info.dpmi_version_major = major;
		dosmem_state.info.dpmi_version_minor = minor;
		dosmem_state.info.cpu_mode = 1; /* Protected mode */
	}

	/* Detect VCPI (only in real mode or V86 mode) */
	if (!dosmem_state.info.dpmi_available) {
		dosmem_state.info.vcpi_available = dosmem_vcpi_available();
	}

	/* Detect XMS */
	dosmem_state.info.xms_available = dosmem_xms_available();

	/* Detect EMS */
	dosmem_state.info.ems_available = dosmem_ems_available();
	if (dosmem_state.info.ems_available) {
		int version = dosmem_ems_get_version();
		dosmem_state.info.ems_version = version;
	}

	/* Get free memory info */
	dosmem_get_info(&dosmem_state.info);

	dosmem_state.initialized = 1;
	return DOSMEM_SUCCESS;
}

void
dosmem_shutdown(void)
{
	/* Future: track all allocations and free them here */
	dosmem_state.initialized = 0;
}

int
dosmem_get_info(dosmem_info_t *info)
{
	if (!info)
		return DOSMEM_INVALID;

	if (!dosmem_state.initialized) {
		if (dosmem_init() != DOSMEM_SUCCESS)
			return DOSMEM_ERROR;
	}

	*info = dosmem_state.info;

	/* Update free memory counts */
	/* TODO: Implement actual free memory queries for each manager */

	return DOSMEM_SUCCESS;
}

int
dosmem_alloc(unsigned long size, unsigned int type, unsigned int flags,
             dosmem_handle_t *handle)
{
	int result;

	if (!handle || size == 0)
		return DOSMEM_INVALID;

	if (!dosmem_state.initialized) {
		if (dosmem_init() != DOSMEM_SUCCESS)
			return DOSMEM_ERROR;
	}

	memset(handle, 0, sizeof(dosmem_handle_t));

	/* Try memory managers in order of preference */

	/* Try DPMI first for extended memory (most capable) */
	if ((type & DOSMEM_EXTENDED) && dosmem_state.info.dpmi_available) {
		result = dosmem_dpmi_alloc(size, handle);
		if (result == DOSMEM_SUCCESS) {
			if (flags & DOSMEM_LOCKED)
				dosmem_lock(handle);
			if (flags & DOSMEM_ZERO)
				memset(handle->linear, 0, size);
			return DOSMEM_SUCCESS;
		}
	}

	/* Try XMS for extended memory */
	if ((type & DOSMEM_EXTENDED) && dosmem_state.info.xms_available) {
		result = dosmem_xms_alloc((size + 1023) / 1024, handle);
		if (result == DOSMEM_SUCCESS) {
			if (flags & DOSMEM_ZERO) {
				/* Zero memory via copy operations */
				static char zero_buf[1024];
				unsigned long offset;
				for (offset = 0; offset < size; offset += 1024) {
					unsigned long chunk = (size - offset > 1024) ? 1024 : (size - offset);
					dosmem_copy_to(handle, offset, zero_buf, chunk);
				}
			}
			return DOSMEM_SUCCESS;
		}
	}

	/* Try EMS for expanded memory */
	if ((type & DOSMEM_EXPANDED) && dosmem_state.info.ems_available) {
		unsigned int pages = (size + 16383) / 16384; /* 16KB pages */
		result = dosmem_ems_alloc(pages, handle);
		if (result == DOSMEM_SUCCESS) {
			if (flags & DOSMEM_ZERO) {
				/* Zero memory by mapping and clearing each page */
				unsigned int page;
				unsigned int frame_seg;
				void *frame_ptr;

				dosmem_ems_get_page_frame(&frame_seg);
				frame_ptr = (void *)((unsigned long)frame_seg << 4);

				for (page = 0; page < pages; page++) {
					dosmem_ems_map(handle, 0, page);
					memset(frame_ptr, 0, 16384);
				}
			}
			return DOSMEM_SUCCESS;
		}
	}

	/* Try VCPI */
	if ((type & DOSMEM_VCPI) && dosmem_state.info.vcpi_available) {
		unsigned int pages = (size + 4095) / 4096; /* 4KB pages */
		result = dosmem_vcpi_alloc_pages(pages, handle);
		if (result == DOSMEM_SUCCESS)
			return DOSMEM_SUCCESS;
	}

	return DOSMEM_NOMEM;
}

int
dosmem_free(dosmem_handle_t *handle)
{
	if (!handle)
		return DOSMEM_INVALID;

	switch (handle->type) {
	case DOSMEM_EXTENDED:
		if (dosmem_state.info.dpmi_available && handle->linear)
			return dosmem_dpmi_free(handle);
		if (dosmem_state.info.xms_available && handle->handle)
			return dosmem_xms_free(handle);
		break;

	case DOSMEM_EXPANDED:
		if (dosmem_state.info.ems_available && handle->handle)
			return dosmem_ems_free(handle);
		break;

	case DOSMEM_VCPI:
		if (dosmem_state.info.vcpi_available)
			return dosmem_vcpi_free_pages(handle);
		break;
	}

	return DOSMEM_ERROR;
}

int
dosmem_resize(dosmem_handle_t *handle, unsigned long new_size)
{
	/* TODO: Implement resize for each memory manager */
	return DOSMEM_ERROR;
}

int
dosmem_lock(dosmem_handle_t *handle)
{
	/* Only DPMI supports locking */
	if (!handle || !dosmem_state.info.dpmi_available)
		return DOSMEM_NOTAVAIL;

	if (handle->locked)
		return DOSMEM_SUCCESS;

	/* TODO: Implement DPMI memory locking */
	handle->locked = 1;
	return DOSMEM_SUCCESS;
}

int
dosmem_unlock(dosmem_handle_t *handle)
{
	if (!handle || !handle->locked)
		return DOSMEM_INVALID;

	/* TODO: Implement DPMI memory unlocking */
	handle->locked = 0;
	return DOSMEM_SUCCESS;
}

int
dosmem_ems_map(dosmem_handle_t *handle, int physical_page, int logical_page)
{
	if (!handle || handle->type != DOSMEM_EXPANDED)
		return DOSMEM_INVALID;

	/* Implemented in ems/ems.c */
	return DOSMEM_ERROR;
}

void *
dosmem_get_pointer(dosmem_handle_t *handle)
{
	if (!handle)
		return NULL;

	/* DPMI allocations have linear pointers */
	if (handle->linear)
		return handle->linear;

	/* Real mode segment:offset */
	if (handle->segment)
		return (void *)((unsigned long)handle->segment << 4 | handle->offset);

	/* EMS requires explicit mapping */
	return NULL;
}

int
dosmem_copy_to(dosmem_handle_t *dest, unsigned long dest_offset,
               const void *src, unsigned long size)
{
	void *dest_ptr = dosmem_get_pointer(dest);
	if (dest_ptr) {
		memcpy((char *)dest_ptr + dest_offset, src, size);
		return DOSMEM_SUCCESS;
	}

	/* TODO: Handle EMS and XMS block moves */
	return DOSMEM_ERROR;
}

int
dosmem_copy_from(void *dest, dosmem_handle_t *src,
                 unsigned long src_offset, unsigned long size)
{
	void *src_ptr = dosmem_get_pointer(src);
	if (src_ptr) {
		memcpy(dest, (char *)src_ptr + src_offset, size);
		return DOSMEM_SUCCESS;
	}

	/* TODO: Handle EMS and XMS block moves */
	return DOSMEM_ERROR;
}

const char *
dosmem_error_string(int error_code)
{
	int index = -error_code;
	if (index >= 0 && index < sizeof(error_messages) / sizeof(error_messages[0]))
		return error_messages[index];
	return "Unknown error";
}
