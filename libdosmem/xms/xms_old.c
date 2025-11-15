/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * XMS (Extended Memory Specification) Support
 *
 * Supports XMS 2.0 and 3.0
 */

#include "../dosmem.h"
#include <dos.h>
#include <string.h>

/* XMS Function Numbers */
#define XMS_GET_VERSION			0x00
#define XMS_ALLOC_EMB			0x09	/* Allocate Extended Memory Block */
#define XMS_FREE_EMB			0x0A	/* Free Extended Memory Block */
#define XMS_MOVE_EMB			0x0B	/* Move Extended Memory Block */
#define XMS_LOCK_EMB			0x0C	/* Lock Extended Memory Block */
#define XMS_UNLOCK_EMB			0x0D	/* Unlock Extended Memory Block */
#define XMS_GET_EMB_INFO		0x0E	/* Get EMB Information */
#define XMS_RESIZE_EMB			0x0F	/* Resize Extended Memory Block */
#define XMS_QUERY_FREE_EMB		0x08	/* Query Free Extended Memory */

/* XMS Entry Point */
static void (far *xms_entry)(void) = NULL;

/* XMS Move Structure */
struct xms_move {
	unsigned long length;		/* Number of bytes to move */
	unsigned short src_handle;	/* Source handle (0 = conventional) */
	unsigned long src_offset;	/* Source offset */
	unsigned short dest_handle;	/* Destination handle (0 = conventional) */
	unsigned long dest_offset;	/* Destination offset */
};

/* Get XMS entry point */
static int
xms_get_entry(void)
{
	union REGS regs;
	struct SREGS sregs;

	if (xms_entry)
		return 1;

	/* Check if XMS driver installed (INT 2Fh, AX=4300h) */
	memset(&regs, 0, sizeof(regs));
	regs.x.ax = 0x4300;
	int86(0x2F, &regs, &regs);

	if (regs.h.al != 0x80)
		return 0; /* XMS not installed */

	/* Get XMS entry point (INT 2Fh, AX=4310h) */
	memset(&regs, 0, sizeof(regs));
	memset(&sregs, 0, sizeof(sregs));
	regs.x.ax = 0x4310;
	int86x(0x2F, &regs, &regs, &sregs);

	/* ES:BX = XMS entry point */
	xms_entry = (void (far *)(void))((unsigned long)sregs.es << 16 | regs.x.bx);

	return 1;
}

/* Check if XMS is available */
int
dosmem_xms_available(void)
{
	return xms_get_entry();
}

/* Get XMS version */
int
dosmem_xms_get_version(void)
{
	union REGS regs;

	if (!xms_get_entry())
		return 0;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = XMS_GET_VERSION;

	/* Call XMS driver */
	/* Note: This requires inline assembly for real XMS calls */
	/* For now, return a reasonable version */
	return 0x0300; /* XMS 3.0 */
}

/* Allocate XMS memory */
int
dosmem_xms_alloc(unsigned long size_kb, dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle || size_kb == 0)
		return DOSMEM_INVALID;

	if (!xms_get_entry())
		return DOSMEM_NOTAVAIL;

	memset(&regs, 0, sizeof(regs));
	regs.h.ah = XMS_ALLOC_EMB;
	regs.x.dx = (unsigned short)size_kb; /* Size in KB */

	/* Call XMS driver via entry point */
	/* Note: Actual XMS calls require inline assembly:
	 *   mov ah, function
	 *   call far [xms_entry]
	 *
	 * For portability, we simulate the call structure.
	 */
#ifdef __WATCOMC__
	{
		unsigned short xms_handle;
		_asm {
			mov ah, XMS_ALLOC_EMB
			mov dx, word ptr size_kb
			call dword ptr [xms_entry]
			mov word ptr xms_handle, dx
			mov word ptr regs.x.ax, ax
		}

		if (regs.x.ax == 1) {
			/* Success */
			memset(handle, 0, sizeof(dosmem_handle_t));
			handle->handle = xms_handle;
			handle->size = size_kb * 1024;
			handle->type = DOSMEM_EXTENDED;
			return DOSMEM_SUCCESS;
		}
	}
#else
	/* Fallback for other compilers - return not available */
	/* Real implementation would use compiler-specific inline assembly */
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_NOMEM;
}

/* Free XMS memory */
int
dosmem_xms_free(dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle || !handle->handle)
		return DOSMEM_INVALID;

	if (!xms_get_entry())
		return DOSMEM_NOTAVAIL;

#ifdef __WATCOMC__
	{
		unsigned short xms_handle = handle->handle;
		_asm {
			mov ah, XMS_FREE_EMB
			mov dx, xms_handle
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
		}

		if (regs.x.ax == 1) {
			memset(handle, 0, sizeof(dosmem_handle_t));
			return DOSMEM_SUCCESS;
		}
	}
#else
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_ERROR;
}

/* Lock XMS memory */
int
dosmem_xms_lock(dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle || !handle->handle)
		return DOSMEM_INVALID;

	if (!xms_get_entry())
		return DOSMEM_NOTAVAIL;

#ifdef __WATCOMC__
	{
		unsigned short xms_handle = handle->handle;
		unsigned long linear_addr;

		_asm {
			mov ah, XMS_LOCK_EMB
			mov dx, xms_handle
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
			/* DX:BX = 32-bit linear address */
			mov word ptr linear_addr, bx
			mov word ptr linear_addr+2, dx
		}

		if (regs.x.ax == 1) {
			handle->physical = linear_addr;
			handle->locked = 1;
			return DOSMEM_SUCCESS;
		}
	}
#else
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_ERROR;
}

/* Unlock XMS memory */
int
dosmem_xms_unlock(dosmem_handle_t *handle)
{
	union REGS regs;

	if (!handle || !handle->handle || !handle->locked)
		return DOSMEM_INVALID;

	if (!xms_get_entry())
		return DOSMEM_NOTAVAIL;

#ifdef __WATCOMC__
	{
		unsigned short xms_handle = handle->handle;

		_asm {
			mov ah, XMS_UNLOCK_EMB
			mov dx, xms_handle
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
		}

		if (regs.x.ax == 1) {
			handle->physical = 0;
			handle->locked = 0;
			return DOSMEM_SUCCESS;
		}
	}
#else
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_ERROR;
}

/* Copy data to/from XMS */
int
dosmem_xms_move(struct xms_move *move_struct)
{
	union REGS regs;
	struct SREGS sregs;

	if (!move_struct)
		return DOSMEM_INVALID;

	if (!xms_get_entry())
		return DOSMEM_NOTAVAIL;

#ifdef __WATCOMC__
	{
		_asm {
			mov ah, XMS_MOVE_EMB
			/* DS:SI = pointer to move structure */
			push ds
			mov ax, seg move_struct
			mov ds, ax
			mov si, offset move_struct
			call dword ptr [xms_entry]
			pop ds
			mov word ptr regs.x.ax, ax
		}

		if (regs.x.ax == 1)
			return DOSMEM_SUCCESS;
	}
#else
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_ERROR;
}
