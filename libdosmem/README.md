# libdosmem - DOS Memory Manager Library

A comprehensive, easy-to-use DOS memory management library for PCC (Portable C Compiler) supporting multiple memory managers and providing Microsoft DOS compiler compatibility.

## Features

- **Unified API** - Single, simple interface for all DOS memory types
- **Microsoft Compatible** - Drop-in replacement for Microsoft C 7.0+ virtual memory API
- **Multiple Memory Managers**:
  - **DPMI** (DOS Protected Mode Interface) - 0.9 and 1.0
  - **VCPI** (Virtual Control Program Interface)
  - **XMS** (Extended Memory Specification) - 2.0 and 3.0
  - **EMS** (Expanded Memory Specification) - 3.2 and 4.0
- **Automatic Detection** - Detects available memory managers automatically
- **Works Everywhere** - DOS 16-bit real mode, DOS 16-bit protected mode, DOS 32-bit
- **Hardware Agnostic** - Works with any hardware configuration

## Supported Memory Types

| Memory Type | Description | Access Method |
|-------------|-------------|---------------|
| Conventional | < 640KB DOS memory | Direct pointer |
| Extended (XMS) | > 1MB memory via XMS | Copy operations or locked pointer |
| Extended (DPMI) | > 1MB memory via DPMI | Direct linear pointer |
| Expanded (EMS) | Bank-switched memory | Page mapping + pointer |
| VCPI | Protected mode pages | Page allocation |

## Quick Start

### Simple API

```c
#include <dosmem.h>

int main(void)
{
    dosmem_handle_t handle;
    void *ptr;

    /* Initialize */
    dosmem_init();

    /* Allocate 64KB of any available memory */
    dosmem_alloc(65536, DOSMEM_ANY, DOSMEM_ZERO, &handle);

    /* Get pointer and use it */
    ptr = dosmem_get_pointer(&handle);
    strcpy(ptr, "Hello, World!");

    /* Free memory */
    dosmem_free(&handle);

    dosmem_shutdown();
    return 0;
}
```

### Microsoft-Compatible API

```c
#include <vmem_compat.h>

int main(void)
{
    _vmhnd_t handle;
    void *ptr;

    /* Initialize */
    _vm_init();

    /* Allocate memory (automatically zero-filled) */
    handle = _vm_allocx(32768, _VM_CLEAN);

    /* Lock memory and get pointer */
    ptr = _vm_lock(handle);
    strcpy(ptr, "Microsoft Compatible!");
    _vm_unlock(handle);

    /* Free memory */
    _vm_free(handle);

    _vm_term();
    return 0;
}
```

## API Reference

### Initialization

```c
int dosmem_init(void);
void dosmem_shutdown(void);
int dosmem_get_info(dosmem_info_t *info);
```

### Memory Allocation

```c
int dosmem_alloc(unsigned long size, unsigned int type,
                 unsigned int flags, dosmem_handle_t *handle);
int dosmem_free(dosmem_handle_t *handle);
int dosmem_resize(dosmem_handle_t *handle, unsigned long new_size);
```

**Memory Types:**
- `DOSMEM_CONVENTIONAL` - Conventional memory (< 640KB)
- `DOSMEM_EXTENDED` - Extended memory (XMS/DPMI)
- `DOSMEM_EXPANDED` - Expanded memory (EMS)
- `DOSMEM_VCPI` - VCPI memory
- `DOSMEM_ANY` - Any available type

**Flags:**
- `DOSMEM_ZERO` - Zero-initialize memory
- `DOSMEM_LOCKED` - Lock in physical RAM (DPMI only)
- `DOSMEM_BELOW_1MB` - Allocate below 1MB for DMA

### Memory Access

```c
void *dosmem_get_pointer(dosmem_handle_t *handle);
int dosmem_lock(dosmem_handle_t *handle);
int dosmem_unlock(dosmem_handle_t *handle);
int dosmem_copy_to(dosmem_handle_t *dest, unsigned long dest_offset,
                   const void *src, unsigned long size);
int dosmem_copy_from(void *dest, dosmem_handle_t *src,
                     unsigned long src_offset, unsigned long size);
```

### EMS-Specific Functions

```c
int dosmem_ems_map(dosmem_handle_t *handle, int physical_page, int logical_page);
int dosmem_ems_get_page_frame(unsigned int *segment);
```

### Low-Level Access

Direct access to specific memory managers:

```c
/* DPMI */
int dosmem_dpmi_available(void);
int dosmem_dpmi_alloc(unsigned long size, dosmem_handle_t *handle);

/* XMS */
int dosmem_xms_available(void);
int dosmem_xms_alloc(unsigned long size_kb, dosmem_handle_t *handle);

/* EMS */
int dosmem_ems_available(void);
int dosmem_ems_alloc(unsigned int pages, dosmem_handle_t *handle);

/* VCPI */
int dosmem_vcpi_available(void);
int dosmem_vcpi_alloc_pages(unsigned int count, dosmem_handle_t *handle);
```

## Microsoft Virtual Memory API

Compatible with Microsoft C 7.0, Visual C++ 1.x for DOS:

```c
int _vm_init(void);
void _vm_term(void);
_vmhnd_t _vm_alloc(unsigned long size);
_vmhnd_t _vm_allocx(unsigned long size, unsigned int flags);
int _vm_free(_vmhnd_t handle);
void *_vm_lock(_vmhnd_t handle);
int _vm_unlock(_vmhnd_t handle);
_vmhnd_t _vm_realloc(_vmhnd_t handle, unsigned long new_size);
unsigned long _vm_size(_vmhnd_t handle);
unsigned long _vm_avail(void);
int _vm_setswap(const char *filename, unsigned long max_size);
```

## Examples

### Allocating Specific Memory Types

```c
/* Allocate extended memory only (XMS or DPMI) */
dosmem_alloc(1048576, DOSMEM_EXTENDED, 0, &handle);

/* Allocate expanded memory only (EMS) */
dosmem_alloc(65536, DOSMEM_EXPANDED, DOSMEM_ZERO, &handle);
```

### Using EMS with Page Mapping

```c
dosmem_handle_t handle;
unsigned int frame_seg;
char far *page_frame;

/* Allocate 4 pages (64KB) */
dosmem_ems_alloc(4, &handle);

/* Get page frame segment */
dosmem_ems_get_page_frame(&frame_seg);
page_frame = MK_FP(frame_seg, 0);

/* Map logical page 0 to physical page 0 */
dosmem_ems_map(&handle, 0, 0);
strcpy(page_frame, "Page 0 data");

/* Map logical page 1 to physical page 0 */
dosmem_ems_map(&handle, 0, 1);
strcpy(page_frame, "Page 1 data");
```

### Querying Available Memory

```c
dosmem_info_t info;

dosmem_get_info(&info);
printf("DPMI: %s\n", info.dpmi_available ? "Yes" : "No");
printf("XMS:  %s\n", info.xms_available ? "Yes" : "No");
printf("EMS:  %s (v%d.%d)\n",
       info.ems_available ? "Yes" : "No",
       info.ems_version >> 4, info.ems_version & 0x0F);
printf("Free extended memory: %lu KB\n", info.ext_free);
```

## Building

```bash
make
make install
```

For DOS cross-compilation:
```bash
CC=i86-msdos-pcc make
```

## Testing

```bash
make test
```

Or run the test program directly:
```bash
./test_dosmem
```

## Compatibility Notes

### DOS 16-bit Real Mode
- Uses XMS, EMS, or VCPI
- Direct memory access when possible
- Copy operations for XMS/EMS when needed

### DOS 16-bit Protected Mode (DPMI)
- Full DPMI support
- Linear memory addressing
- Memory locking for DMA buffers

### DOS 32-bit (DOS/4GW, DJGPP, etc.)
- DPMI 0.9/1.0 support
- Flat memory model
- Direct pointer access to all memory

## Error Handling

All functions return error codes:
- `DOSMEM_SUCCESS` (0) - Success
- `DOSMEM_ERROR` (-1) - General error
- `DOSMEM_NOMEM` (-2) - Out of memory
- `DOSMEM_NOTAVAIL` (-3) - Memory manager not available
- `DOSMEM_INVALID` (-4) - Invalid parameter
- `DOSMEM_LOCKED` (-5) - Memory is locked

Use `dosmem_error_string()` to get human-readable error messages.

## License

Copyright (c) 2025 PCC Project.

## See Also

- DPMI Specification 0.9 and 1.0
- XMS Specification 2.0 and 3.0
- EMS/LIM Specification 3.2 and 4.0
- VCPI Specification 1.0
- Microsoft C 7.0 Programmer's Guide
