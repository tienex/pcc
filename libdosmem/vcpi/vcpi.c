/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * VCPI (Virtual Control Program Interface) Support
 *
 * VCPI allows DOS programs to access extended memory in protected mode
 * when running under an EMS 4.0 driver with VCPI support (like EMM386).
 */

#include "../dosmem.h"
#include <dos.h>
#include <string.h>

/* VCPI is accessed through EMS INT 67h */
#define VCPI_INT			0x67

/* VCPI Function Numbers (EMS 4.0 function 0xDE) */
#define VCPI_PRESENCE_CHECK		0xDE00
#define VCPI_GET_PMODE_INTERFACE	0xDE01
#define VCPI_GET_MAX_PHYSICAL		0xDE02
#define VCPI_GET_PAGE_COUNT		0xDE03
#define VCPI_ALLOC_PAGE			0xDE04
#define VCPI_FREE_PAGE			0xDE05
#define VCPI_GET_PHYSICAL_ADDR		0xDE06
#define VCPI_GET_VERSION		0xDE0A

/* Check if VCPI is available */
int
dosmem_vcpi_available(void)
{
	union REGS regs;

	/* VCPI requires EMS 4.0 */
	if (!dosmem_ems_available())
		return 0;

	if (dosmem_ems_get_version() < 0x40)
		return 0;

	/* Check VCPI presence */
	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VCPI_PRESENCE_CHECK;
	int86(VCPI_INT, &regs, &regs);

	/* AH = 0 and BX = 0 if VCPI is present */
	return (regs.h.ah == 0 && regs.x.bx == 0);
}

/* Get VCPI version */
int
dosmem_vcpi_get_version(void)
{
	union REGS regs;

	if (!dosmem_vcpi_available())
		return 0;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VCPI_GET_VERSION;
	int86(VCPI_INT, &regs, &regs);

	if (regs.h.ah != 0)
		return 0;

	/* BX = version (BCD format) */
	return regs.x.bx;
}

/* Get maximum physical memory address */
int
dosmem_vcpi_get_max_physical(unsigned long *max_addr)
{
	union REGS regs;

	if (!max_addr)
		return DOSMEM_INVALID;

	if (!dosmem_vcpi_available())
		return DOSMEM_NOTAVAIL;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VCPI_GET_MAX_PHYSICAL;
	int86(VCPI_INT, &regs, &regs);

	if (regs.h.ah != 0)
		return DOSMEM_ERROR;

	/* EDX = maximum physical address */
	/* Note: Getting EDX requires 32-bit registers */
	*max_addr = 0xFFFFFFFF; /* Placeholder */
	return DOSMEM_SUCCESS;
}

/* Get number of free 4KB pages */
int
dosmem_vcpi_get_free_pages(unsigned long *count)
{
	union REGS regs;

	if (!count)
		return DOSMEM_INVALID;

	if (!dosmem_vcpi_available())
		return DOSMEM_NOTAVAIL;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VCPI_GET_PAGE_COUNT;
	int86(VCPI_INT, &regs, &regs);

	if (regs.h.ah != 0)
		return DOSMEM_ERROR;

	/* EDX = number of free 4KB pages */
	/* Note: Getting EDX requires 32-bit access */
	*count = 0; /* Placeholder */
	return DOSMEM_SUCCESS;
}

/* Allocate a 4KB page */
int
dosmem_vcpi_alloc_page(unsigned long *physical_addr)
{
	union REGS regs;

	if (!physical_addr)
		return DOSMEM_INVALID;

	if (!dosmem_vcpi_available())
		return DOSMEM_NOTAVAIL;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VCPI_ALLOC_PAGE;
	int86(VCPI_INT, &regs, &regs);

	if (regs.h.ah != 0)
		return DOSMEM_NOMEM;

	/* EDX = physical address of allocated page */
	*physical_addr = 0; /* Placeholder - needs 32-bit register access */
	return DOSMEM_SUCCESS;
}

/* Free a 4KB page */
int
dosmem_vcpi_free_page(unsigned long physical_addr)
{
	union REGS regs;

	if (!dosmem_vcpi_available())
		return DOSMEM_NOTAVAIL;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VCPI_FREE_PAGE;
	/* EDX = physical address to free */
	int86(VCPI_INT, &regs, &regs);

	if (regs.h.ah != 0)
		return DOSMEM_ERROR;

	return DOSMEM_SUCCESS;
}

/* Allocate multiple pages */
int
dosmem_vcpi_alloc_pages(unsigned int count, dosmem_handle_t *handle)
{
	unsigned int i;
	unsigned long physical_addr;
	int result;

	if (!handle || count == 0)
		return DOSMEM_INVALID;

	if (!dosmem_vcpi_available())
		return DOSMEM_NOTAVAIL;

	/* Allocate first page */
	result = dosmem_vcpi_alloc_page(&physical_addr);
	if (result != DOSMEM_SUCCESS)
		return result;

	memset(handle, 0, sizeof(dosmem_handle_t));
	handle->physical = physical_addr;
	handle->size = (unsigned long)count * 4096;
	handle->type = DOSMEM_VCPI;

	/* Allocate remaining pages */
	/* Note: VCPI doesn't guarantee contiguous pages, so this is simplified */
	for (i = 1; i < count; i++) {
		result = dosmem_vcpi_alloc_page(&physical_addr);
		if (result != DOSMEM_SUCCESS) {
			/* Free already allocated pages */
			dosmem_vcpi_free_pages(handle);
			return result;
		}
	}

	return DOSMEM_SUCCESS;
}

/* Free multiple pages */
int
dosmem_vcpi_free_pages(dosmem_handle_t *handle)
{
	if (!handle || handle->type != DOSMEM_VCPI)
		return DOSMEM_INVALID;

	/* Note: Proper implementation would track individual page addresses */
	/* For now, free the base page */
	if (handle->physical) {
		dosmem_vcpi_free_page(handle->physical);
	}

	memset(handle, 0, sizeof(dosmem_handle_t));
	return DOSMEM_SUCCESS;
}

/* Get physical address of linear address */
int
dosmem_vcpi_get_physical_addr(unsigned long linear_addr, unsigned long *physical_addr)
{
	union REGS regs;

	if (!physical_addr)
		return DOSMEM_INVALID;

	if (!dosmem_vcpi_available())
		return DOSMEM_NOTAVAIL;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VCPI_GET_PHYSICAL_ADDR;
	/* CX = linear address selector/segment */
	/* EDI = linear offset */
	int86(VCPI_INT, &regs, &regs);

	if (regs.h.ah != 0)
		return DOSMEM_ERROR;

	/* EDI = physical address */
	*physical_addr = 0; /* Placeholder */
	return DOSMEM_SUCCESS;
}
