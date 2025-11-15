/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * EMS (Expanded Memory Specification) Support
 *
 * Supports EMS 3.2 and EMS 4.0 (LIM EMS)
 */

#include "../dosmem.h"
#include <dos.h>
#include <string.h>

/* EMS Interrupt */
#define EMS_INT				0x67

/* EMS Function Numbers */
#define EMS_GET_STATUS			0x40
#define EMS_GET_PAGE_FRAME		0x41
#define EMS_GET_PAGES			0x42	/* Get unallocated page count */
#define EMS_ALLOC_PAGES			0x43
#define EMS_MAP_PAGE			0x44
#define EMS_FREE_PAGES			0x45
#define EMS_GET_VERSION			0x46
#define EMS_SAVE_MAP			0x47
#define EMS_RESTORE_MAP			0x48
#define EMS_GET_HANDLE_COUNT		0x4B
#define EMS_GET_HANDLE_PAGES		0x4C
#define EMS_GET_ALL_HANDLE_PAGES	0x4D

/* EMS 4.0 functions */
#define EMS_MAP_MULTIPLE		0x50	/* Map multiple pages */
#define EMS_REALLOC_PAGES		0x51	/* Reallocate pages */
#define EMS_GET_HANDLE_ATTR		0x52
#define EMS_SET_HANDLE_ATTR		0x53
#define EMS_GET_HANDLE_NAME		0x53
#define EMS_SET_HANDLE_NAME		0x54

/* EMS Status Codes */
#define EMS_SUCCESS			0x00
#define EMS_ERROR_INVALID_FUNC		0x84
#define EMS_ERROR_NO_HANDLES		0x85
#define EMS_ERROR_ALLOC_FAILED		0x88
#define EMS_ERROR_INVALID_HANDLE	0x83
#define EMS_ERROR_INVALID_PAGE		0x8A

/* Check if EMS driver is installed */
int
dosmem_ems_available(void)
{
	union REGS regs;
	struct SREGS sregs;
	char far *ems_name;
	const char *expected = "EMMXXXX0";
	int i;

	/* Check for EMM device driver name at interrupt vector */
	/* Get interrupt vector 0x67 */
	memset(&sregs, 0, sizeof(sregs));
	regs.h.ah = 0x35;
	regs.h.al = EMS_INT;
	int86x(0x21, &regs, &regs, &sregs);

	/* ES:BX points to interrupt handler */
	/* Device name should be at offset 0x0A */
	ems_name = (char far *)((unsigned long)sregs.es << 16 | (regs.x.bx + 0x0A));

	/* Check for "EMMXXXX0" */
	for (i = 0; i < 8; i++) {
		if (ems_name[i] != expected[i])
			return 0;
	}

	/* Verify EMS is actually functional */
	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_GET_STATUS;
	int86(EMS_INT, &regs, &regs);

	return (regs.h.ah == EMS_SUCCESS);
}

/* Get EMS version */
int
dosmem_ems_get_version(void)
{
	union REGS regs;

	if (!dosmem_ems_available())
		return 0;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_GET_VERSION;
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS)
		return 0;

	/* AL = version (BCD: 0x32 = 3.2, 0x40 = 4.0) */
	return regs.h.al;
}

/* Get EMS page frame segment */
int
dosmem_ems_get_page_frame(unsigned int *segment)
{
	union REGS regs;

	if (!segment)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_GET_PAGE_FRAME;
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS)
		return DOSMEM_ERROR;

	/* BX = segment of page frame */
	*segment = regs.x.bx;
	return DOSMEM_SUCCESS;
}

/* Allocate EMS pages */
int
dosmem_ems_alloc(unsigned int pages, dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle || pages == 0)
		return DOSMEM_INVALID;

	if (!dosmem_ems_available())
		return DOSMEM_NOTAVAIL;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_ALLOC_PAGES;
	regs.x.bx = pages; /* Number of 16KB pages */
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS) {
		if (regs.h.ah == EMS_ERROR_ALLOC_FAILED)
			return DOSMEM_NOMEM;
		return DOSMEM_ERROR;
	}

	/* DX = EMS handle */
	memset(handle, 0, sizeof(dosmem_handle_t));
	handle->handle = regs.x.dx;
	handle->size = (unsigned long)pages * 16384; /* 16KB per page */
	handle->type = DOSMEM_EXPANDED;

	/* Get page frame segment */
	dosmem_ems_get_page_frame(&handle->segment);

	return DOSMEM_SUCCESS;
}

/* Free EMS pages */
int
dosmem_ems_free(dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle || !handle->handle)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_FREE_PAGES;
	regs.x.dx = handle->handle;
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS)
		return DOSMEM_ERROR;

	memset(handle, 0, sizeof(dosmem_handle_t));
	return DOSMEM_SUCCESS;
}

/* Map EMS page */
int
dosmem_ems_map_page(unsigned short ems_handle, int physical_page, int logical_page)
{
	union REGS regs;

	if (physical_page < 0 || physical_page > 3)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_MAP_PAGE;
	regs.h.al = (unsigned char)physical_page;	/* Physical page (0-3) */
	regs.x.bx = (unsigned short)logical_page;	/* Logical page */
	regs.x.dx = ems_handle;				/* EMS handle */
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS)
		return DOSMEM_ERROR;

	return DOSMEM_SUCCESS;
}

/* Map EMS page (wrapper for handle) */
int
dosmem_ems_map(dosmem_handle_t *handle, int physical_page, int logical_page)
{
	if (!handle || handle->type != DOSMEM_EXPANDED)
		return DOSMEM_INVALID;

	return dosmem_ems_map_page(handle->handle, physical_page, logical_page);
}

/* Get free EMS pages */
int
dosmem_ems_get_free_pages(unsigned int *total_pages, unsigned int *free_pages)
{
	union REGS regs;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_GET_PAGES;
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS)
		return DOSMEM_ERROR;

	/* BX = unallocated pages, DX = total pages */
	if (free_pages)
		*free_pages = regs.x.bx;
	if (total_pages)
		*total_pages = regs.x.dx;

	return DOSMEM_SUCCESS;
}

/* Reallocate EMS pages (EMS 4.0 only) */
int
dosmem_ems_realloc(dosmem_handle_t *handle, unsigned int new_pages)
{
	union REGS regs;
	int version;

	if (!handle || !handle->handle)
		return DOSMEM_INVALID;

	version = dosmem_ems_get_version();
	if (version < 0x40)
		return DOSMEM_NOTAVAIL; /* EMS 4.0 required */

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_REALLOC_PAGES;
	regs.x.bx = new_pages;
	regs.x.dx = handle->handle;
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS) {
		if (regs.h.ah == EMS_ERROR_ALLOC_FAILED)
			return DOSMEM_NOMEM;
		return DOSMEM_ERROR;
	}

	handle->size = (unsigned long)new_pages * 16384;
	return DOSMEM_SUCCESS;
}

/* Save EMS page map */
int
dosmem_ems_save_map(unsigned short ems_handle)
{
	union REGS regs;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_SAVE_MAP;
	regs.x.dx = ems_handle;
	int86(EMS_INT, &regs, &regs);

	return (regs.h.ah == EMS_SUCCESS) ? DOSMEM_SUCCESS : DOSMEM_ERROR;
}

/* Restore EMS page map */
int
dosmem_ems_restore_map(unsigned short ems_handle)
{
	union REGS regs;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_RESTORE_MAP;
	regs.x.dx = ems_handle;
	int86(EMS_INT, &regs, &regs);

	return (regs.h.ah == EMS_SUCCESS) ? DOSMEM_SUCCESS : DOSMEM_ERROR;
}

/* Get handle page count */
int
dosmem_ems_get_handle_pages(unsigned short ems_handle, unsigned int *pages)
{
	union REGS regs;

	if (!pages)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = EMS_GET_HANDLE_PAGES;
	regs.x.dx = ems_handle;
	int86(EMS_INT, &regs, &regs);

	if (regs.h.ah != EMS_SUCCESS)
		return DOSMEM_ERROR;

	/* BX = number of pages allocated to handle */
	*pages = regs.x.bx;
	return DOSMEM_SUCCESS;
}
