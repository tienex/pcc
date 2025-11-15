/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * DOS Memory Manager - Unified API for DPMI, VCPI, XMS, EMS
 *
 * Simple, easy-to-use interface for DOS memory management supporting:
 * - DPMI (DOS Protected Mode Interface)
 * - VCPI (Virtual Control Program Interface)
 * - XMS (Extended Memory Specification)
 * - EMS 3.2 and 4.0 (Expanded Memory Specification)
 *
 * Works with DOS16 real mode, DOS16 protected mode, and DOS32.
 * Automatically detects available memory managers and uses the best option.
 */

#ifndef DOSMEM_H
#define DOSMEM_H

#ifdef __cplusplus
extern "C" {
#endif

/* Memory types - bitmask */
#define DOSMEM_CONVENTIONAL	0x01	/* Conventional (< 640KB) */
#define DOSMEM_EXTENDED		0x02	/* Extended memory (XMS/DPMI) */
#define DOSMEM_EXPANDED		0x04	/* Expanded memory (EMS) */
#define DOSMEM_VCPI		0x08	/* VCPI-managed memory */
#define DOSMEM_ANY		0xFF	/* Any available type */

/* Memory allocation flags */
#define DOSMEM_ZERO		0x0100	/* Zero-initialize memory */
#define DOSMEM_LOCKED		0x0200	/* Lock in physical memory (DPMI) */
#define DOSMEM_BELOW_1MB	0x0400	/* Allocate below 1MB (for DMA) */

/* Error codes */
#define DOSMEM_SUCCESS		0
#define DOSMEM_ERROR		-1
#define DOSMEM_NOMEM		-2
#define DOSMEM_NOTAVAIL		-3
#define DOSMEM_INVALID		-4
#define DOSMEM_LOCKED		-5

/* Memory handle - opaque type */
typedef struct dosmem_handle {
	unsigned long physical;	/* Physical address (if available) */
	void *linear;		/* Linear/flat address (protected mode) */
	unsigned int segment;	/* Segment (real mode) */
	unsigned int offset;	/* Offset within segment */
	unsigned long size;	/* Size in bytes */
	unsigned char type;	/* Memory type (DOSMEM_xxx) */
	unsigned short handle;	/* Manager-specific handle (XMS/EMS) */
	unsigned char locked;	/* Is memory locked? */
	unsigned char _reserved[7];
} dosmem_handle_t;

/* Memory manager information */
typedef struct dosmem_info {
	unsigned char dpmi_available;	/* DPMI detected */
	unsigned char vcpi_available;	/* VCPI detected */
	unsigned char xms_available;	/* XMS detected */
	unsigned char ems_available;	/* EMS detected */
	unsigned char ems_version;	/* EMS version (32=3.2, 40=4.0) */
	unsigned char dpmi_version_major;
	unsigned char dpmi_version_minor;
	unsigned char cpu_mode;		/* 0=real, 1=protected */

	unsigned long conv_free;	/* Free conventional memory (bytes) */
	unsigned long ext_free;		/* Free extended memory (KB) */
	unsigned long exp_free;		/* Free expanded memory (KB) */
	unsigned long total_free;	/* Total free memory (KB) */
} dosmem_info_t;

/*
 * Initialize the DOS memory manager
 * Detects available memory managers and initializes the best options.
 * Returns DOSMEM_SUCCESS on success, DOSMEM_ERROR on failure.
 */
int dosmem_init(void);

/*
 * Shutdown the DOS memory manager
 * Frees all allocated memory and releases resources.
 */
void dosmem_shutdown(void);

/*
 * Get information about available memory managers and free memory
 */
int dosmem_get_info(dosmem_info_t *info);

/*
 * Allocate memory
 * size: size in bytes
 * type: memory type(s) to try (DOSMEM_xxx bitmask)
 * flags: allocation flags (DOSMEM_ZERO, etc.)
 * handle: output parameter for memory handle
 *
 * Returns DOSMEM_SUCCESS on success, error code on failure.
 * The library automatically selects the best available memory manager.
 */
int dosmem_alloc(unsigned long size, unsigned int type, unsigned int flags,
                 dosmem_handle_t *handle);

/*
 * Free previously allocated memory
 */
int dosmem_free(dosmem_handle_t *handle);

/*
 * Resize allocated memory block
 */
int dosmem_resize(dosmem_handle_t *handle, unsigned long new_size);

/*
 * Lock memory in physical RAM (DPMI only)
 * Useful for DMA buffers and real-mode callbacks
 */
int dosmem_lock(dosmem_handle_t *handle);

/*
 * Unlock previously locked memory
 */
int dosmem_unlock(dosmem_handle_t *handle);

/*
 * Map EMS pages into page frame (EMS only)
 * physical_page: page number in page frame (0-3 typically)
 * logical_page: logical page number to map
 */
int dosmem_ems_map(dosmem_handle_t *handle, int physical_page, int logical_page);

/*
 * Get pointer to memory (for direct access)
 * Returns NULL if memory cannot be directly accessed.
 * For EMS, you must map pages first using dosmem_ems_map().
 */
void *dosmem_get_pointer(dosmem_handle_t *handle);

/*
 * Copy data to/from DOS memory
 * Handles all memory types transparently.
 */
int dosmem_copy_to(dosmem_handle_t *dest, unsigned long dest_offset,
                   const void *src, unsigned long size);
int dosmem_copy_from(void *dest, dosmem_handle_t *src,
                     unsigned long src_offset, unsigned long size);

/*
 * Get last error message (for debugging)
 */
const char *dosmem_error_string(int error_code);

/*
 * Low-level interface - for advanced users
 * Direct access to specific memory managers
 */

/* DPMI functions */
int dosmem_dpmi_available(void);
int dosmem_dpmi_get_version(int *major, int *minor);
int dosmem_dpmi_alloc(unsigned long size, dosmem_handle_t *handle);
int dosmem_dpmi_free(dosmem_handle_t *handle);

/* VCPI functions */
int dosmem_vcpi_available(void);
int dosmem_vcpi_get_version(void);
int dosmem_vcpi_alloc_pages(unsigned int count, dosmem_handle_t *handle);
int dosmem_vcpi_free_pages(dosmem_handle_t *handle);

/* XMS functions */
int dosmem_xms_available(void);
int dosmem_xms_get_version(void);
int dosmem_xms_alloc(unsigned long size_kb, dosmem_handle_t *handle);
int dosmem_xms_free(dosmem_handle_t *handle);

/* EMS functions */
int dosmem_ems_available(void);
int dosmem_ems_get_version(void);
int dosmem_ems_alloc(unsigned int pages, dosmem_handle_t *handle);
int dosmem_ems_free(dosmem_handle_t *handle);
int dosmem_ems_get_page_frame(unsigned int *segment);

/* HMA (High Memory Area) functions */
int dosmem_hma_available(void);
int dosmem_hma_alloc(unsigned int bytes_needed, dosmem_handle_t *handle);
int dosmem_hma_free(dosmem_handle_t *handle);
void far *dosmem_hma_get_pointer(void);
unsigned long dosmem_hma_get_size(void);

/* UMB (Upper Memory Blocks) functions */
int dosmem_umb_available(void);
int dosmem_umb_alloc(unsigned long size, dosmem_handle_t *handle);
int dosmem_umb_free(dosmem_handle_t *handle);
int dosmem_umb_alloc_dos(unsigned long size, dosmem_handle_t *handle);
int dosmem_umb_free_dos(dosmem_handle_t *handle);
unsigned long dosmem_umb_get_free(void);

/* Swapping functions */
int dosmem_swap_init(const char *swap_file, unsigned long max_swap_size);
void dosmem_swap_shutdown(void);
int dosmem_swap_alloc(unsigned long size, dosmem_handle_t *handle);
int dosmem_swap_free(dosmem_handle_t *handle);
int dosmem_swap_read(dosmem_handle_t *handle, unsigned long offset,
                     void *buffer, unsigned long size);
int dosmem_swap_write(dosmem_handle_t *handle, unsigned long offset,
                      const void *buffer, unsigned long size);
int dosmem_swap_lock(dosmem_handle_t *handle);
int dosmem_swap_unlock(dosmem_handle_t *handle);

#ifdef __cplusplus
}
#endif

#endif /* DOSMEM_H */
