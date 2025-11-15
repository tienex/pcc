/*	$Id$	*/
/*
 * Test/Example program for libdosmem
 */

#include "dosmem.h"
#include "vmem_compat.h"
#include <stdio.h>
#include <string.h>

static void
print_info(void)
{
	dosmem_info_t info;

	printf("DOS Memory Manager Test\n");
	printf("=======================\n\n");

	if (dosmem_get_info(&info) == DOSMEM_SUCCESS) {
		printf("Memory Managers Available:\n");
		printf("  DPMI:  %s", info.dpmi_available ? "Yes" : "No");
		if (info.dpmi_available)
			printf(" (v%d.%d)", info.dpmi_version_major, info.dpmi_version_minor);
		printf("\n");

		printf("  VCPI:  %s\n", info.vcpi_available ? "Yes" : "No");
		printf("  XMS:   %s\n", info.xms_available ? "Yes" : "No");

		printf("  EMS:   %s", info.ems_available ? "Yes" : "No");
		if (info.ems_available)
			printf(" (v%d.%d)", info.ems_version >> 4, info.ems_version & 0x0F);
		printf("\n\n");

		printf("Free Memory:\n");
		printf("  Conventional: %lu bytes\n", info.conv_free);
		printf("  Extended:     %lu KB\n", info.ext_free);
		printf("  Expanded:     %lu KB\n", info.exp_free);
		printf("  Total:        %lu KB\n\n", info.total_free);
	}
}

static void
test_simple_api(void)
{
	dosmem_handle_t handle;
	void *ptr;
	char test_data[] = "Hello, DOS Memory!";
	char read_buffer[32];
	int result;

	printf("Testing Simple API:\n");

	/* Allocate 64KB of any available memory */
	result = dosmem_alloc(65536, DOSMEM_ANY, DOSMEM_ZERO, &handle);
	if (result == DOSMEM_SUCCESS) {
		printf("  Allocated 64KB successfully\n");

		/* Try to get a pointer */
		ptr = dosmem_get_pointer(&handle);
		if (ptr) {
			printf("  Got direct pointer: %p\n", ptr);
			strcpy(ptr, test_data);
			printf("  Wrote: %s\n", test_data);
		} else {
			printf("  No direct pointer (using copy operations)\n");
			dosmem_copy_to(&handle, 0, test_data, strlen(test_data) + 1);
			printf("  Wrote: %s\n", test_data);
		}

		/* Read back */
		if (ptr) {
			printf("  Read:  %s\n", (char *)ptr);
		} else {
			dosmem_copy_from(read_buffer, &handle, 0, sizeof(read_buffer));
			printf("  Read:  %s\n", read_buffer);
		}

		/* Free memory */
		dosmem_free(&handle);
		printf("  Freed memory\n\n");
	} else {
		printf("  Allocation failed: %s\n\n",
		       dosmem_error_string(result));
	}
}

static void
test_microsoft_api(void)
{
	_vmhnd_t handle;
	void *ptr;
	char test_data[] = "Microsoft Compatible API";
	unsigned long size;

	printf("Testing Microsoft-Compatible API:\n");

	/* Initialize */
	if (_vm_init() != _VM_SUCCESS) {
		printf("  Initialization failed\n\n");
		return;
	}

	/* Allocate memory */
	handle = _vm_allocx(32768, _VM_CLEAN);
	if (handle) {
		printf("  Allocated 32KB successfully\n");

		size = _vm_size(handle);
		printf("  Block size: %lu bytes\n", size);

		/* Lock and access */
		ptr = _vm_lock(handle);
		if (ptr) {
			printf("  Locked memory at: %p\n", ptr);
			strcpy(ptr, test_data);
			printf("  Wrote: %s\n", test_data);
			printf("  Read:  %s\n", (char *)ptr);

			_vm_unlock(handle);
			printf("  Unlocked memory\n");
		} else {
			printf("  Could not lock memory\n");
		}

		/* Free memory */
		_vm_free(handle);
		printf("  Freed memory\n\n");
	} else {
		printf("  Allocation failed\n\n");
	}

	_vm_term();
}

static void
test_specific_managers(void)
{
	printf("Testing Specific Memory Managers:\n");

	/* Test XMS */
	if (dosmem_xms_available()) {
		dosmem_handle_t handle;
		int version = dosmem_xms_get_version();
		printf("  XMS v%d.%d available\n", version >> 8, version & 0xFF);

		if (dosmem_xms_alloc(64, &handle) == DOSMEM_SUCCESS) {
			printf("  Allocated 64KB via XMS\n");
			dosmem_xms_free(&handle);
			printf("  Freed XMS memory\n");
		}
	}

	/* Test EMS */
	if (dosmem_ems_available()) {
		dosmem_handle_t handle;
		unsigned int frame_seg;
		int version = dosmem_ems_get_version();
		printf("  EMS v%d.%d available\n", version >> 4, version & 0x0F);

		if (dosmem_ems_get_page_frame(&frame_seg) == DOSMEM_SUCCESS) {
			printf("  Page frame at segment: 0x%04X\n", frame_seg);
		}

		if (dosmem_ems_alloc(4, &handle) == DOSMEM_SUCCESS) {
			printf("  Allocated 4 pages (64KB) via EMS\n");

			/* Map first page */
			if (dosmem_ems_map(&handle, 0, 0) == DOSMEM_SUCCESS) {
				printf("  Mapped logical page 0 to physical page 0\n");
			}

			dosmem_ems_free(&handle);
			printf("  Freed EMS memory\n");
		}
	}

	/* Test DPMI */
	if (dosmem_dpmi_available()) {
		int major, minor;
		dosmem_dpmi_get_version(&major, &minor);
		printf("  DPMI v%d.%d available\n", major, minor);
	}

	/* Test VCPI */
	if (dosmem_vcpi_available()) {
		int version = dosmem_vcpi_get_version();
		printf("  VCPI v%d.%d available\n", version >> 8, version & 0xFF);
	}

	printf("\n");
}

int
main(void)
{
	/* Initialize library */
	if (dosmem_init() != DOSMEM_SUCCESS) {
		printf("Failed to initialize DOS memory manager\n");
		return 1;
	}

	/* Print information */
	print_info();

	/* Run tests */
	test_simple_api();
	test_microsoft_api();
	test_specific_managers();

	/* Shutdown */
	dosmem_shutdown();

	printf("All tests completed.\n");
	return 0;
}
