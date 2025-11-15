/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * HMA (High Memory Area) Support
 *
 * The HMA is the first 64KB - 16 bytes of extended memory (addresses 1MB to 1MB+64KB-16)
 * Accessible in real mode using segment 0xFFFF
 */

#include "../dosmem.h"
#include <dos.h>
#include <string.h>

/* HMA is controlled through XMS */
#define XMS_ALLOC_HMA		0x01
#define XMS_FREE_HMA		0x02
#define XMS_GLOBAL_ENABLE_A20	0x03
#define XMS_GLOBAL_DISABLE_A20	0x04
#define XMS_LOCAL_ENABLE_A20	0x05
#define XMS_LOCAL_DISABLE_A20	0x06
#define XMS_QUERY_A20		0x07
#define XMS_QUERY_FREE_HMA	0x08

/* XMS driver entry point (defined in xms.c) */
extern void (far *xms_entry)(void);

/* HMA state */
static struct {
	int allocated;
	unsigned long size;
	void far *hma_ptr;
} hma_state = {0};

/* Enable A20 line (required to access HMA) */
static int
hma_enable_a20(void)
{
	union REGS regs;

	if (!xms_entry)
		return DOSMEM_NOTAVAIL;

#ifdef __WATCOMC__
	{
		_asm {
			mov ah, XMS_GLOBAL_ENABLE_A20
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
		}

		return (regs.x.ax == 1) ? DOSMEM_SUCCESS : DOSMEM_ERROR;
	}
#else
	return DOSMEM_NOTAVAIL;
#endif
}

/* Disable A20 line */
static int
hma_disable_a20(void)
{
	union REGS regs;

	if (!xms_entry)
		return DOSMEM_NOTAVAIL;

#ifdef __WATCOMC__
	{
		_asm {
			mov ah, XMS_GLOBAL_DISABLE_A20
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
		}

		return (regs.x.ax == 1) ? DOSMEM_SUCCESS : DOSMEM_ERROR;
	}
#else
	return DOSMEM_NOTAVAIL;
#endif
}

/* Check if HMA is available */
int
dosmem_hma_available(void)
{
	union REGS regs;

	if (!xms_entry)
		return 0;

#ifdef __WATCOMC__
	{
		unsigned short size_avail;

		_asm {
			mov ah, XMS_QUERY_FREE_HMA
			call dword ptr [xms_entry]
			mov word ptr size_avail, ax
			mov word ptr regs.x.bx, bx
		}

		/* AX=1 if HMA exists, BX = largest free HMA block in bytes */
		return (size_avail == 1 && regs.x.bx > 0);
	}
#else
	return 0;
#endif
}

/* Allocate HMA */
int
dosmem_hma_alloc(unsigned int bytes_needed, dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle || bytes_needed == 0)
		return DOSMEM_INVALID;

	if (!xms_entry)
		return DOSMEM_NOTAVAIL;

	if (hma_state.allocated)
		return DOSMEM_ERROR; /* HMA already allocated */

	/* HMA can only be allocated once by one program */
	/* bytes_needed is typically 0xFFFF (all HMA) */
	if (bytes_needed > 65520)
		bytes_needed = 65520; /* Max HMA size: 64KB - 16 */

#ifdef __WATCOMC__
	{
		_asm {
			mov ah, XMS_ALLOC_HMA
			mov dx, bytes_needed
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
		}

		if (regs.x.ax == 1) {
			/* HMA allocated successfully */
			/* HMA is accessed via segment 0xFFFF, offset 0x0010 */
			memset(handle, 0, sizeof(dosmem_handle_t));
			handle->segment = 0xFFFF;
			handle->offset = 0x0010;
			handle->size = bytes_needed;
			handle->type = DOSMEM_EXTENDED; /* HMA is extended memory */
			handle->physical = 0x100000 + 0x10; /* Physical address: 1MB + 16 bytes */

			hma_state.allocated = 1;
			hma_state.size = bytes_needed;
			hma_state.hma_ptr = MK_FP(0xFFFF, 0x0010);

			/* Enable A20 line */
			hma_enable_a20();

			return DOSMEM_SUCCESS;
		}
	}
#else
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_ERROR;
}

/* Free HMA */
int
dosmem_hma_free(dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle)
		return DOSMEM_INVALID;

	if (!hma_state.allocated)
		return DOSMEM_ERROR;

	if (!xms_entry)
		return DOSMEM_NOTAVAIL;

#ifdef __WATCOMC__
	{
		_asm {
			mov ah, XMS_FREE_HMA
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
		}

		if (regs.x.ax == 1) {
			hma_state.allocated = 0;
			hma_state.size = 0;
			hma_state.hma_ptr = NULL;

			/* Disable A20 line */
			hma_disable_a20();

			memset(handle, 0, sizeof(dosmem_handle_t));
			return DOSMEM_SUCCESS;
		}
	}
#else
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_ERROR;
}

/* Get HMA pointer */
void far *
dosmem_hma_get_pointer(void)
{
	if (!hma_state.allocated)
		return NULL;

	return hma_state.hma_ptr;
}

/* Get HMA size */
unsigned long
dosmem_hma_get_size(void)
{
	return hma_state.size;
}
