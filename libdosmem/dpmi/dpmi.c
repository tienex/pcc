/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * DPMI (DOS Protected Mode Interface) Support
 *
 * Supports DPMI 0.9 and 1.0 specifications
 */

#include "../dosmem.h"
#include <dos.h>
#include <string.h>

/* DPMI Function Numbers */
#define DPMI_INT			0x31

#define DPMI_GET_VERSION		0x0400
#define DPMI_ALLOC_DOS_MEMORY		0x0100
#define DPMI_FREE_DOS_MEMORY		0x0101
#define DPMI_ALLOC_MEMORY		0x0501
#define DPMI_FREE_MEMORY		0x0502
#define DPMI_RESIZE_MEMORY		0x0503
#define DPMI_LOCK_MEMORY		0x0600
#define DPMI_UNLOCK_MEMORY		0x0601
#define DPMI_GET_PAGE_SIZE		0x0604

/* Check if DPMI is available */
int
dosmem_dpmi_available(void)
{
#if defined(__DPMI__) || defined(__DJGPP__) || defined(__WATCOMC__)
	/* Compiler indicates DPMI environment */
	return 1;
#elif defined(MSDOS) || defined(__MSDOS__)
	union REGS regs;
	struct SREGS sregs;

	/* Try to detect DPMI host using INT 2Fh AX=1687h */
	memset(&regs, 0, sizeof(regs));
	memset(&sregs, 0, sizeof(sregs));
	regs.x.ax = 0x1687;
	int86x(0x2F, &regs, &regs, &sregs);

	/* AX = 0 if DPMI available */
	return (regs.x.ax == 0);
#else
	return 0;
#endif
}

/* Get DPMI version */
int
dosmem_dpmi_get_version(int *major, int *minor)
{
#if defined(__DPMI__) || defined(__DJGPP__) || defined(__WATCOMC__)
	union REGS regs;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = DPMI_GET_VERSION;
	int86(DPMI_INT, &regs, &regs);

	if (major)
		*major = regs.h.ah;
	if (minor)
		*minor = regs.h.al;

	return DOSMEM_SUCCESS;
#else
	if (major)
		*major = 0;
	if (minor)
		*minor = 0;
	return DOSMEM_NOTAVAIL;
#endif
}

/* Allocate DPMI memory */
int
dosmem_dpmi_alloc(unsigned long size, dosmem_handle_t *handle)
{
#if defined(__DPMI__) || defined(__DJGPP__) || defined(__WATCOMC__)
	union REGS regs;

	if (!handle || size == 0)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = DPMI_ALLOC_MEMORY;
	/* BX:CX = size in bytes */
	regs.x.bx = (unsigned short)(size >> 16);
	regs.x.cx = (unsigned short)(size & 0xFFFF);

	int86(DPMI_INT, &regs, &regs);

	if (regs.x.cflag) {
		/* Allocation failed */
		return DOSMEM_NOMEM;
	}

	/* BX:CX = linear address, SI:DI = handle */
	handle->linear = (void *)((unsigned long)regs.x.bx << 16 | regs.x.cx);
	handle->handle = regs.x.si; /* Store DPMI handle in lower 16 bits */
	handle->size = size;
	handle->type = DOSMEM_EXTENDED;

	return DOSMEM_SUCCESS;
#else
	return DOSMEM_NOTAVAIL;
#endif
}

/* Free DPMI memory */
int
dosmem_dpmi_free(dosmem_handle_t *handle)
{
#if defined(__DPMI__) || defined(__DJGPP__) || defined(__WATCOMC__)
	union REGS regs;

	if (!handle || !handle->handle)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = DPMI_FREE_MEMORY;
	/* SI:DI = handle */
	regs.x.si = (unsigned short)handle->handle;
	regs.x.di = (unsigned short)(handle->handle >> 16);

	int86(DPMI_INT, &regs, &regs);

	if (regs.x.cflag)
		return DOSMEM_ERROR;

	memset(handle, 0, sizeof(dosmem_handle_t));
	return DOSMEM_SUCCESS;
#else
	return DOSMEM_NOTAVAIL;
#endif
}

/* Resize DPMI memory */
int
dosmem_dpmi_resize(dosmem_handle_t *handle, unsigned long new_size)
{
#if defined(__DPMI__) || defined(__DJGPP__) || defined(__WATCOMC__)
	union REGS regs;

	if (!handle || !handle->handle || new_size == 0)
		return DOSMEM_INVALID;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = DPMI_RESIZE_MEMORY;
	/* SI:DI = handle, BX:CX = new size */
	regs.x.si = (unsigned short)handle->handle;
	regs.x.di = (unsigned short)(handle->handle >> 16);
	regs.x.bx = (unsigned short)(new_size >> 16);
	regs.x.cx = (unsigned short)(new_size & 0xFFFF);

	int86(DPMI_INT, &regs, &regs);

	if (regs.x.cflag)
		return DOSMEM_NOMEM;

	/* BX:CX = new linear address */
	handle->linear = (void *)((unsigned long)regs.x.bx << 16 | regs.x.cx);
	handle->size = new_size;

	return DOSMEM_SUCCESS;
#else
	return DOSMEM_NOTAVAIL;
#endif
}

/* Lock memory in physical RAM */
int
dosmem_dpmi_lock(dosmem_handle_t *handle)
{
#if defined(__DPMI__) || defined(__DJGPP__) || defined(__WATCOMC__)
	union REGS regs;
	unsigned long linear_addr;

	if (!handle || !handle->linear)
		return DOSMEM_INVALID;

	linear_addr = (unsigned long)handle->linear;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = DPMI_LOCK_MEMORY;
	/* BX:CX = linear address, SI:DI = size */
	regs.x.bx = (unsigned short)(linear_addr >> 16);
	regs.x.cx = (unsigned short)(linear_addr & 0xFFFF);
	regs.x.si = (unsigned short)(handle->size >> 16);
	regs.x.di = (unsigned short)(handle->size & 0xFFFF);

	int86(DPMI_INT, &regs, &regs);

	if (regs.x.cflag)
		return DOSMEM_ERROR;

	handle->locked = 1;
	return DOSMEM_SUCCESS;
#else
	return DOSMEM_NOTAVAIL;
#endif
}

/* Unlock memory */
int
dosmem_dpmi_unlock(dosmem_handle_t *handle)
{
#if defined(__DPMI__) || defined(__DJGPP__) || defined(__WATCOMC__)
	union REGS regs;
	unsigned long linear_addr;

	if (!handle || !handle->linear || !handle->locked)
		return DOSMEM_INVALID;

	linear_addr = (unsigned long)handle->linear;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = DPMI_UNLOCK_MEMORY;
	/* BX:CX = linear address, SI:DI = size */
	regs.x.bx = (unsigned short)(linear_addr >> 16);
	regs.x.cx = (unsigned short)(linear_addr & 0xFFFF);
	regs.x.si = (unsigned short)(handle->size >> 16);
	regs.x.di = (unsigned short)(handle->size & 0xFFFF);

	int86(DPMI_INT, &regs, &regs);

	if (regs.x.cflag)
		return DOSMEM_ERROR;

	handle->locked = 0;
	return DOSMEM_SUCCESS;
#else
	return DOSMEM_NOTAVAIL;
#endif
}
