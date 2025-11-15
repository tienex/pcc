/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * UMB (Upper Memory Blocks) Support
 *
 * UMBs are memory blocks in the 640KB-1MB region (Upper Memory Area)
 * Used for TSRs, device drivers, and freeing conventional memory
 * Controlled through XMS and DOS
 */

#include "../dosmem.h"
#include <dos.h>
#include <string.h>

/* XMS UMB functions */
#define XMS_REQUEST_UMB		0x10
#define XMS_RELEASE_UMB		0x11

/* DOS UMB functions */
#define DOS_ALLOC_MEMORY	0x48
#define DOS_FREE_MEMORY		0x49
#define DOS_RESIZE_MEMORY	0x4A
#define DOS_GET_UMB_LINK	0x5802
#define DOS_SET_UMB_LINK	0x5803

/* XMS driver entry point */
extern void (far *xms_entry)(void);

/* UMB tracking */
#define MAX_UMB_BLOCKS	16

static struct {
	int in_use;
	unsigned int segment;
	unsigned int size_para;	/* Size in paragraphs (16-byte units) */
} umb_blocks[MAX_UMB_BLOCKS];

/* Enable UMB linking with DOS memory chain */
static int
umb_enable_dos_link(void)
{
	union REGS regs;

	/* Get current UMB link state */
	memset(&regs, 0, sizeof(regs));
	regs.x.ax = DOS_GET_UMB_LINK;
	int86(0x21, &regs, &regs);

	/* AL = 0: UMBs not linked, AL = 1: UMBs linked */
	if (regs.h.al == 0) {
		/* Enable UMB linking */
		memset(&regs, 0, sizeof(regs));
		regs.x.ax = DOS_SET_UMB_LINK;
		regs.x.bx = 1; /* Link UMBs */
		int86(0x21, &regs, &regs);

		if (regs.x.cflag)
			return DOSMEM_ERROR;
	}

	return DOSMEM_SUCCESS;
}

/* Check if UMBs are available */
int
dosmem_umb_available(void)
{
	union REGS regs;

	if (!xms_entry)
		return 0;

	/* Try to allocate a tiny UMB to test availability */
#ifdef __WATCOMC__
	{
		_asm {
			mov ah, XMS_REQUEST_UMB
			mov dx, 1		/* Request 1 paragraph (16 bytes) */
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
			mov word ptr regs.x.bx, bx
			mov word ptr regs.x.dx, dx
		}

		if (regs.x.ax == 1) {
			/* UMB available - free it */
			unsigned short seg = regs.x.bx;
			_asm {
				mov ah, XMS_RELEASE_UMB
				mov dx, seg
				call dword ptr [xms_entry]
			}
			return 1;
		}
	}
#endif

	return 0;
}

/* Allocate UMB via XMS */
int
dosmem_umb_alloc(unsigned long size, dosmem_handle_t *handle)
{
	union REGS regs;
	unsigned int para_needed;
	int i;

	if (!handle || size == 0)
		return DOSMEM_INVALID;

	if (!xms_entry)
		return DOSMEM_NOTAVAIL;

	/* Convert bytes to paragraphs (round up) */
	para_needed = (unsigned int)((size + 15) / 16);

#ifdef __WATCOMC__
	{
		unsigned short seg, size_allocated;

		_asm {
			mov ah, XMS_REQUEST_UMB
			mov dx, para_needed
			call dword ptr [xms_entry]
			mov word ptr regs.x.ax, ax
			mov word ptr seg, bx		/* BX = segment of UMB */
			mov word ptr size_allocated, dx	/* DX = size allocated in paragraphs */
		}

		if (regs.x.ax == 1) {
			/* UMB allocated successfully */
			/* Find free slot to track this UMB */
			for (i = 0; i < MAX_UMB_BLOCKS; i++) {
				if (!umb_blocks[i].in_use) {
					umb_blocks[i].in_use = 1;
					umb_blocks[i].segment = seg;
					umb_blocks[i].size_para = size_allocated;

					/* Fill handle */
					memset(handle, 0, sizeof(dosmem_handle_t));
					handle->segment = seg;
					handle->offset = 0;
					handle->size = (unsigned long)size_allocated * 16;
					handle->type = DOSMEM_CONVENTIONAL; /* UMBs are in conventional address space */
					handle->physical = (unsigned long)seg << 4;

					return DOSMEM_SUCCESS;
				}
			}

			/* No free tracking slot - free the UMB */
			_asm {
				mov ah, XMS_RELEASE_UMB
				mov dx, seg
				call dword ptr [xms_entry]
			}

			return DOSMEM_NOMEM;
		}
	}
#else
	return DOSMEM_NOTAVAIL;
#endif

	return DOSMEM_NOMEM;
}

/* Free UMB */
int
dosmem_umb_free(dosmem_handle_t *handle)
{
	union REGS regs;
	int i;
	unsigned short seg;

	if (!handle)
		return DOSMEM_INVALID;

	if (!xms_entry)
		return DOSMEM_NOTAVAIL;

	seg = handle->segment;

	/* Find and clear tracking entry */
	for (i = 0; i < MAX_UMB_BLOCKS; i++) {
		if (umb_blocks[i].in_use && umb_blocks[i].segment == seg) {
			umb_blocks[i].in_use = 0;
			break;
		}
	}

#ifdef __WATCOMC__
	{
		_asm {
			mov ah, XMS_RELEASE_UMB
			mov dx, seg
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

/* Allocate UMB via DOS (alternative method when UMBs are linked) */
int
dosmem_umb_alloc_dos(unsigned long size, dosmem_handle_t *handle)
{
	union REGS regs;
	struct SREGS sregs;
	unsigned int para_needed;

	if (!handle || size == 0)
		return DOSMEM_INVALID;

	/* Enable UMB linking */
	if (umb_enable_dos_link() != DOSMEM_SUCCESS)
		return DOSMEM_ERROR;

	/* Convert bytes to paragraphs (round up) */
	para_needed = (unsigned int)((size + 15) / 16);

	/* Allocate memory via DOS */
	memset(&regs, 0, sizeof(regs));
	memset(&sregs, 0, sizeof(sregs));
	regs.h.ah = DOS_ALLOC_MEMORY;
	regs.x.bx = para_needed;

	int86x(0x21, &regs, &regs, &sregs);

	if (!regs.x.cflag) {
		/* AX = segment of allocated block */
		memset(handle, 0, sizeof(dosmem_handle_t));
		handle->segment = regs.x.ax;
		handle->offset = 0;
		handle->size = (unsigned long)para_needed * 16;
		handle->type = DOSMEM_CONVENTIONAL;
		handle->physical = (unsigned long)regs.x.ax << 4;

		return DOSMEM_SUCCESS;
	}

	return DOSMEM_NOMEM;
}

/* Free DOS-allocated UMB */
int
dosmem_umb_free_dos(dosmem_handle_t *handle)
{
	union REGS regs;
	struct SREGS sregs;

	if (!handle)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	memset(&sregs, 0, sizeof(sregs));
	regs.h.ah = DOS_FREE_MEMORY;
	sregs.es = handle->segment;

	int86x(0x21, &regs, &regs, &sregs);

	if (!regs.x.cflag) {
		memset(handle, 0, sizeof(dosmem_handle_t));
		return DOSMEM_SUCCESS;
	}

	return DOSMEM_ERROR;
}

/* Get total free UMB memory */
unsigned long
dosmem_umb_get_free(void)
{
	/* Try to get free UMB info from XMS */
	/* This is a simplified implementation */
	return 0; /* Would need to query each UMB block */
}
