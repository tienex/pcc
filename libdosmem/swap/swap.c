/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Disk Swapping Support for DOS Memory Manager
 *
 * Provides virtual memory via disk swapping when physical memory is exhausted
 */

#include "../dosmem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <io.h>
#include <sys/stat.h>

/* Swap file configuration */
#define SWAP_PAGE_SIZE		4096
#define MAX_SWAP_PAGES		1024
#define DEFAULT_SWAP_FILE	"DOSMEM.SWP"

/* Swap page descriptor */
typedef struct swap_page {
	int in_use;
	unsigned long page_id;
	unsigned long file_offset;
	void *buffer;		/* In-memory buffer (if loaded) */
	int dirty;		/* Modified since last write */
	int locked;		/* Cannot be swapped out */
	struct swap_page *next;
} swap_page_t;

/* Swap file state */
static struct {
	int initialized;
	int fd;			/* Swap file descriptor */
	char filename[260];	/* Swap file path */
	unsigned long max_size;	/* Maximum swap file size */
	unsigned long current_size;
	unsigned long next_page_id;
	swap_page_t *page_list;
	int page_count;
	int pages_in_memory;
	int max_pages_in_memory;
} swap_state = {0};

/* Initialize swapping system */
int
dosmem_swap_init(const char *swap_file, unsigned long max_swap_size)
{
	if (swap_state.initialized)
		return DOSMEM_SUCCESS;

	/* Set swap file name */
	if (swap_file && swap_file[0]) {
		strncpy(swap_state.filename, swap_file, sizeof(swap_state.filename) - 1);
		swap_state.filename[sizeof(swap_state.filename) - 1] = '\0';
	} else {
		strcpy(swap_state.filename, DEFAULT_SWAP_FILE);
	}

	/* Set maximum swap size (default: 16MB) */
	swap_state.max_size = max_swap_size ? max_swap_size : (16 * 1024 * 1024);

	/* Create or open swap file */
	swap_state.fd = open(swap_state.filename, O_RDWR | O_BINARY | O_CREAT, S_IREAD | S_IWRITE);
	if (swap_state.fd < 0)
		return DOSMEM_ERROR;

	swap_state.current_size = 0;
	swap_state.next_page_id = 1;
	swap_state.page_list = NULL;
	swap_state.page_count = 0;
	swap_state.pages_in_memory = 0;
	swap_state.max_pages_in_memory = 64; /* Keep up to 64 pages in memory */

	swap_state.initialized = 1;
	return DOSMEM_SUCCESS;
}

/* Shutdown swapping system */
void
dosmem_swap_shutdown(void)
{
	swap_page_t *page, *next;

	if (!swap_state.initialized)
		return;

	/* Free all pages */
	page = swap_state.page_list;
	while (page) {
		next = page->next;
		if (page->buffer)
			free(page->buffer);
		free(page);
		page = next;
	}

	/* Close and delete swap file */
	if (swap_state.fd >= 0) {
		close(swap_state.fd);
		remove(swap_state.filename);
	}

	swap_state.initialized = 0;
}

/* Find least recently used page to swap out */
static swap_page_t *
find_lru_page(void)
{
	swap_page_t *page, *lru_page = NULL;

	/* Find first unlocked page with a buffer */
	for (page = swap_state.page_list; page; page = page->next) {
		if (page->buffer && !page->locked) {
			lru_page = page;
			break;
		}
	}

	return lru_page;
}

/* Swap page to disk */
static int
swap_page_out(swap_page_t *page)
{
	long file_pos;

	if (!page || !page->buffer)
		return DOSMEM_ERROR;

	/* Write page to disk if dirty */
	if (page->dirty) {
		file_pos = lseek(swap_state.fd, page->file_offset, SEEK_SET);
		if (file_pos < 0)
			return DOSMEM_ERROR;

		if (write(swap_state.fd, page->buffer, SWAP_PAGE_SIZE) != SWAP_PAGE_SIZE)
			return DOSMEM_ERROR;

		page->dirty = 0;
	}

	/* Free memory buffer */
	free(page->buffer);
	page->buffer = NULL;
	swap_state.pages_in_memory--;

	return DOSMEM_SUCCESS;
}

/* Swap page into memory */
static int
swap_page_in(swap_page_t *page)
{
	long file_pos;

	if (!page)
		return DOSMEM_ERROR;

	/* Check if we need to swap out another page first */
	while (swap_state.pages_in_memory >= swap_state.max_pages_in_memory) {
		swap_page_t *lru = find_lru_page();
		if (!lru)
			break;
		swap_page_out(lru);
	}

	/* Allocate memory buffer */
	page->buffer = malloc(SWAP_PAGE_SIZE);
	if (!page->buffer)
		return DOSMEM_NOMEM;

	/* Read page from disk */
	file_pos = lseek(swap_state.fd, page->file_offset, SEEK_SET);
	if (file_pos < 0) {
		free(page->buffer);
		page->buffer = NULL;
		return DOSMEM_ERROR;
	}

	if (read(swap_state.fd, page->buffer, SWAP_PAGE_SIZE) != SWAP_PAGE_SIZE) {
		free(page->buffer);
		page->buffer = NULL;
		return DOSMEM_ERROR;
	}

	swap_state.pages_in_memory++;
	return DOSMEM_SUCCESS;
}

/* Allocate swapped memory */
int
dosmem_swap_alloc(unsigned long size, dosmem_handle_t *handle)
{
	swap_page_t *page;
	unsigned long pages_needed;
	unsigned long i;

	if (!swap_state.initialized) {
		if (dosmem_swap_init(NULL, 0) != DOSMEM_SUCCESS)
			return DOSMEM_ERROR;
	}

	if (!handle || size == 0)
		return DOSMEM_INVALID;

	/* Calculate pages needed */
	pages_needed = (size + SWAP_PAGE_SIZE - 1) / SWAP_PAGE_SIZE;

	/* Check if we have space in swap file */
	if (swap_state.current_size + (pages_needed * SWAP_PAGE_SIZE) > swap_state.max_size)
		return DOSMEM_NOMEM;

	/* Allocate pages */
	for (i = 0; i < pages_needed; i++) {
		page = (swap_page_t *)malloc(sizeof(swap_page_t));
		if (!page)
			return DOSMEM_NOMEM;

		memset(page, 0, sizeof(swap_page_t));
		page->in_use = 1;
		page->page_id = swap_state.next_page_id++;
		page->file_offset = swap_state.current_size;
		page->buffer = NULL;
		page->dirty = 0;
		page->locked = 0;

		/* Add to list */
		page->next = swap_state.page_list;
		swap_state.page_list = page;
		swap_state.page_count++;

		swap_state.current_size += SWAP_PAGE_SIZE;
	}

	/* Fill handle */
	memset(handle, 0, sizeof(dosmem_handle_t));
	handle->handle = (unsigned short)swap_state.page_list->page_id;
	handle->size = size;
	handle->type = DOSMEM_EXTENDED; /* Virtual extended memory */

	return DOSMEM_SUCCESS;
}

/* Free swapped memory */
int
dosmem_swap_free(dosmem_handle_t *handle)
{
	swap_page_t *page, *prev = NULL;
	unsigned long page_id;

	if (!swap_state.initialized || !handle)
		return DOSMEM_INVALID;

	page_id = handle->handle;

	/* Find and free page */
	for (page = swap_state.page_list; page; prev = page, page = page->next) {
		if (page->page_id == page_id) {
			/* Remove from list */
			if (prev)
				prev->next = page->next;
			else
				swap_state.page_list = page->next;

			/* Free buffer if loaded */
			if (page->buffer) {
				free(page->buffer);
				swap_state.pages_in_memory--;
			}

			free(page);
			swap_state.page_count--;

			memset(handle, 0, sizeof(dosmem_handle_t));
			return DOSMEM_SUCCESS;
		}
	}

	return DOSMEM_ERROR;
}

/* Read from swapped memory */
int
dosmem_swap_read(dosmem_handle_t *handle, unsigned long offset,
                 void *buffer, unsigned long size)
{
	swap_page_t *page;
	unsigned long page_id, page_offset;

	if (!swap_state.initialized || !handle || !buffer)
		return DOSMEM_INVALID;

	page_id = handle->handle;

	/* Find page */
	for (page = swap_state.page_list; page; page = page->next) {
		if (page->page_id == page_id) {
			/* Ensure page is in memory */
			if (!page->buffer) {
				if (swap_page_in(page) != DOSMEM_SUCCESS)
					return DOSMEM_ERROR;
			}

			/* Copy data */
			page_offset = offset % SWAP_PAGE_SIZE;
			if (page_offset + size > SWAP_PAGE_SIZE)
				size = SWAP_PAGE_SIZE - page_offset;

			memcpy(buffer, (char *)page->buffer + page_offset, size);
			return DOSMEM_SUCCESS;
		}
	}

	return DOSMEM_ERROR;
}

/* Write to swapped memory */
int
dosmem_swap_write(dosmem_handle_t *handle, unsigned long offset,
                  const void *buffer, unsigned long size)
{
	swap_page_t *page;
	unsigned long page_id, page_offset;

	if (!swap_state.initialized || !handle || !buffer)
		return DOSMEM_INVALID;

	page_id = handle->handle;

	/* Find page */
	for (page = swap_state.page_list; page; page = page->next) {
		if (page->page_id == page_id) {
			/* Ensure page is in memory */
			if (!page->buffer) {
				if (swap_page_in(page) != DOSMEM_SUCCESS)
					return DOSMEM_ERROR;
			}

			/* Copy data */
			page_offset = offset % SWAP_PAGE_SIZE;
			if (page_offset + size > SWAP_PAGE_SIZE)
				size = SWAP_PAGE_SIZE - page_offset;

			memcpy((char *)page->buffer + page_offset, buffer, size);
			page->dirty = 1;
			return DOSMEM_SUCCESS;
		}
	}

	return DOSMEM_ERROR;
}

/* Lock swapped page in memory */
int
dosmem_swap_lock(dosmem_handle_t *handle)
{
	swap_page_t *page;
	unsigned long page_id;

	if (!swap_state.initialized || !handle)
		return DOSMEM_INVALID;

	page_id = handle->handle;

	for (page = swap_state.page_list; page; page = page->next) {
		if (page->page_id == page_id) {
			/* Ensure page is in memory */
			if (!page->buffer) {
				if (swap_page_in(page) != DOSMEM_SUCCESS)
					return DOSMEM_ERROR;
			}

			page->locked = 1;
			return DOSMEM_SUCCESS;
		}
	}

	return DOSMEM_ERROR;
}

/* Unlock swapped page */
int
dosmem_swap_unlock(dosmem_handle_t *handle)
{
	swap_page_t *page;
	unsigned long page_id;

	if (!swap_state.initialized || !handle)
		return DOSMEM_INVALID;

	page_id = handle->handle;

	for (page = swap_state.page_list; page; page = page->next) {
		if (page->page_id == page_id) {
			page->locked = 0;
			return DOSMEM_SUCCESS;
		}
	}

	return DOSMEM_ERROR;
}
