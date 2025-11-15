/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using VESA BIOS Extension (VBE)
 *
 * Provides high-resolution BGI graphics on DOS
 * Works on: DOS (protected mode), VESA-compatible BIOSes
 * Requires: VESA VBE 1.2+ support
 * Supports: 640x480, 800x600, 1024x768, 1280x1024
 */

#include "../graphics.h"
#include <dos.h>
#include <dpmi.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* VESA function numbers */
#define VESA_GET_INFO		0x4F00
#define VESA_GET_MODE_INFO	0x4F01
#define VESA_SET_MODE		0x4F02
#define VESA_GET_MODE		0x4F03
#define VESA_WINDOW_CONTROL	0x4F05
#define VESA_SCANLINE_LENGTH	0x4F06
#define VESA_DISPLAY_START	0x4F07

/* VESA mode numbers */
#define VESA_640x480x8		0x101
#define VESA_800x600x8		0x103
#define VESA_1024x768x8		0x105
#define VESA_1280x1024x8	0x107
#define VESA_640x480x15		0x110
#define VESA_640x480x16		0x111
#define VESA_640x480x24		0x112
#define VESA_LINEAR_BIT		0x4000

/* VESA info structures */
#pragma pack(1)
typedef struct {
	char signature[4];		/* "VESA" */
	unsigned short version;		/* VBE version */
	unsigned long oem_string_ptr;
	unsigned long capabilities;
	unsigned long video_modes_ptr;
	unsigned short total_memory;	/* In 64KB blocks */
	unsigned char reserved[236];
} vesa_info_t;

typedef struct {
	unsigned short mode_attributes;
	unsigned char win_a_attributes;
	unsigned char win_b_attributes;
	unsigned short win_granularity;
	unsigned short win_size;
	unsigned short win_a_segment;
	unsigned short win_b_segment;
	unsigned long win_func_ptr;
	unsigned short bytes_per_scanline;
	unsigned short x_resolution;
	unsigned short y_resolution;
	unsigned char x_char_size;
	unsigned char y_char_size;
	unsigned char number_of_planes;
	unsigned char bits_per_pixel;
	unsigned char number_of_banks;
	unsigned char memory_model;
	unsigned char bank_size;
	unsigned char number_of_image_pages;
	unsigned char reserved1;
	unsigned char red_mask_size;
	unsigned char red_field_position;
	unsigned char green_mask_size;
	unsigned char green_field_position;
	unsigned char blue_mask_size;
	unsigned char blue_field_position;
	unsigned char reserved_mask_size;
	unsigned char reserved_field_position;
	unsigned char direct_color_mode_info;
	unsigned long phys_base_ptr;		/* VBE 2.0+ */
	unsigned long reserved2;
	unsigned short reserved3;
	unsigned char reserved[206];
} vesa_mode_info_t;
#pragma pack()

/* Internal state */
static struct {
	unsigned char FAR *vesa_mem;	/* Video memory pointer */
	int width;
	int height;
	int bpp;
	int bytes_per_scanline;
	unsigned short current_mode;
	int current_color;
	int current_bkcolor;
	int current_x;
	int current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style;
	int fill_color;
	int text_justify_h;
	int text_justify_v;
	int initialized;
	int windowed;			/* Using bank switching */
	int current_bank;
	unsigned short win_granularity;
} vesa_state;

/* EGA palette (first 16 colors) */
static const unsigned char ega_palette[16] = {
	0, 1, 2, 3, 4, 5, 20, 7, 56, 57, 58, 59, 60, 61, 62, 63
};

static int
vesa_get_mode_info(unsigned short mode, vesa_mode_info_t *info)
{
	__dpmi_regs regs;
	unsigned long dos_addr;
	int selector;

	/* Allocate DOS memory for mode info */
	dos_addr = __dpmi_allocate_dos_memory(sizeof(vesa_mode_info_t) / 16 + 1, &selector);
	if (dos_addr == 0)
		return 0;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VESA_GET_MODE_INFO;
	regs.x.cx = mode;
	regs.x.es = dos_addr >> 4;
	regs.x.di = dos_addr & 0x0F;

	__dpmi_int(0x10, &regs);

	if (regs.x.ax != 0x004F) {
		__dpmi_free_dos_memory(selector);
		return 0;
	}

	/* Copy mode info from DOS memory */
	dosmemget(dos_addr, sizeof(vesa_mode_info_t), info);
	__dpmi_free_dos_memory(selector);

	return 1;
}

static void
vesa_set_mode(unsigned short mode)
{
	__dpmi_regs regs;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VESA_SET_MODE;
	regs.x.bx = mode | VESA_LINEAR_BIT;	/* Try linear framebuffer */
	__dpmi_int(0x10, &regs);
}

static void
vesa_set_bank(int bank)
{
	__dpmi_regs regs;

	if (bank == vesa_state.current_bank)
		return;

	memset(&regs, 0, sizeof(regs));
	regs.x.ax = VESA_WINDOW_CONTROL;
	regs.x.bx = 0;		/* Set window */
	regs.x.dx = bank;
	__dpmi_int(0x10, &regs);

	vesa_state.current_bank = bank;
}

void
initgraph(int *driver, int *mode, const char *path)
{
	vesa_mode_info_t mode_info;
	unsigned short vesa_mode;
	int width = 640;
	int height = 480;

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGAHI;
	}

	/* Determine VESA mode based on BGI mode */
	switch (*mode) {
	case VGAHI:
		vesa_mode = VESA_640x480x8;
		width = 640;
		height = 480;
		break;
	case SVGA_800_600:
		vesa_mode = VESA_800x600x8;
		width = 800;
		height = 600;
		break;
	case SVGA_1024_768:
		vesa_mode = VESA_1024x768x8;
		width = 1024;
		height = 768;
		break;
	case SVGA_1280_1024:
		vesa_mode = VESA_1280x1024x8;
		width = 1280;
		height = 1024;
		break;
	default:
		vesa_mode = VESA_640x480x8;
		width = 640;
		height = 480;
	}

	/* Get mode information */
	if (!vesa_get_mode_info(vesa_mode, &mode_info)) {
		*driver = grNotDetected;
		return;
	}

	/* Check if mode is supported */
	if (!(mode_info.mode_attributes & 0x01)) {
		*driver = grNotDetected;
		return;
	}

	/* Set VESA mode */
	vesa_set_mode(vesa_mode);

	vesa_state.width = mode_info.x_resolution;
	vesa_state.height = mode_info.y_resolution;
	vesa_state.bpp = mode_info.bits_per_pixel;
	vesa_state.bytes_per_scanline = mode_info.bytes_per_scanline;
	vesa_state.current_mode = vesa_mode;

	/* Try to get linear framebuffer */
	if (mode_info.phys_base_ptr != 0 && mode_info.mode_attributes & 0x80) {
		/* Linear framebuffer available */
		__dpmi_meminfo mem_info;
		mem_info.address = mode_info.phys_base_ptr;
		mem_info.size = (unsigned long)vesa_state.height * vesa_state.bytes_per_scanline;

		if (__dpmi_physical_address_mapping(&mem_info) == 0) {
			vesa_state.vesa_mem = (unsigned char FAR *)mem_info.address;
			vesa_state.windowed = 0;
		} else {
			/* Fall back to bank switching */
			vesa_state.vesa_mem = (unsigned char FAR *)MK_FP(mode_info.win_a_segment, 0);
			vesa_state.windowed = 1;
			vesa_state.win_granularity = mode_info.win_granularity;
			vesa_state.current_bank = -1;
		}
	} else {
		/* Use bank switching */
		vesa_state.vesa_mem = (unsigned char FAR *)MK_FP(mode_info.win_a_segment, 0);
		vesa_state.windowed = 1;
		vesa_state.win_granularity = mode_info.win_granularity;
		vesa_state.current_bank = -1;
	}

	vesa_state.current_color = WHITE;
	vesa_state.current_bkcolor = BLACK;
	vesa_state.current_x = 0;
	vesa_state.current_y = 0;
	vesa_state.line_style = SOLID_LINE;
	vesa_state.line_pattern = 0xFFFF;
	vesa_state.fill_style = SOLID_FILL;
	vesa_state.fill_color = WHITE;
	vesa_state.text_justify_h = LEFT_TEXT;
	vesa_state.text_justify_v = TOP_TEXT;
	vesa_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!vesa_state.initialized)
		return;

	/* Return to text mode */
	vesa_set_mode(0x03);

	memset(&vesa_state, 0, sizeof(vesa_state));
}

int
getmaxx(void)
{
	return vesa_state.width - 1;
}

int
getmaxy(void)
{
	return vesa_state.height - 1;
}

void
setcolor(int color)
{
	vesa_state.current_color = color & 0xFF;
}

int
getcolor(void)
{
	return vesa_state.current_color;
}

void
setbkcolor(int color)
{
	vesa_state.current_bkcolor = color & 0xFF;
}

int
getbkcolor(void)
{
	return vesa_state.current_bkcolor;
}

void
cleardevice(void)
{
	unsigned long offset;
	unsigned long total_size = (unsigned long)vesa_state.height * vesa_state.bytes_per_scanline;
	unsigned char color = (unsigned char)vesa_state.current_bkcolor;

	if (vesa_state.windowed) {
		/* Use bank switching */
		for (offset = 0; offset < total_size; offset++) {
			int bank = offset / (vesa_state.win_granularity * 1024);
			int bank_offset = offset % (vesa_state.win_granularity * 1024);
			vesa_set_bank(bank);
			vesa_state.vesa_mem[bank_offset] = color;
		}
		vesa_state.current_bank = -1;
	} else {
		/* Linear framebuffer */
		for (offset = 0; offset < total_size; offset++)
			vesa_state.vesa_mem[offset] = color;
	}
}

void
putpixel(int x, int y, int color)
{
	unsigned long offset;

	if (x < 0 || x >= vesa_state.width || y < 0 || y >= vesa_state.height)
		return;

	offset = (unsigned long)y * vesa_state.bytes_per_scanline + x;

	if (vesa_state.windowed) {
		int bank = offset / (vesa_state.win_granularity * 1024);
		int bank_offset = offset % (vesa_state.win_granularity * 1024);
		vesa_set_bank(bank);
		vesa_state.vesa_mem[bank_offset] = (unsigned char)color;
	} else {
		vesa_state.vesa_mem[offset] = (unsigned char)color;
	}
}

unsigned int
getpixel(int x, int y)
{
	unsigned long offset;

	if (x < 0 || x >= vesa_state.width || y < 0 || y >= vesa_state.height)
		return 0;

	offset = (unsigned long)y * vesa_state.bytes_per_scanline + x;

	if (vesa_state.windowed) {
		int bank = offset / (vesa_state.win_granularity * 1024);
		int bank_offset = offset % (vesa_state.win_granularity * 1024);
		vesa_set_bank(bank);
		return vesa_state.vesa_mem[bank_offset];
	} else {
		return vesa_state.vesa_mem[offset];
	}
}

void
moveto(int x, int y)
{
	vesa_state.current_x = x;
	vesa_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	vesa_state.current_x += dx;
	vesa_state.current_y += dy;
}

int
getx(void)
{
	return vesa_state.current_x;
}

int
gety(void)
{
	return vesa_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	/* Bresenham's line algorithm */
	int dx = abs(x2 - x1);
	int dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1;
	int sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy;
	int e2;
	int pattern_pos = 0;

	while (1) {
		/* Check line pattern for styled lines */
		if (vesa_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, vesa_state.current_color);

		if (x1 == x2 && y1 == y2)
			break;

		e2 = 2 * err;
		if (e2 > -dy) {
			err -= dy;
			x1 += sx;
		}
		if (e2 < dx) {
			err += dx;
			y1 += sy;
		}

		pattern_pos++;
	}
}

void
lineto(int x, int y)
{
	line(vesa_state.current_x, vesa_state.current_y, x, y);
	vesa_state.current_x = x;
	vesa_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = vesa_state.current_x + dx;
	int y2 = vesa_state.current_y + dy;
	line(vesa_state.current_x, vesa_state.current_y, x2, y2);
	vesa_state.current_x = x2;
	vesa_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void
circle(int x, int y, int radius)
{
	/* Midpoint circle algorithm */
	int dx = 0;
	int dy = radius;
	int d = 1 - radius;

	while (dx <= dy) {
		putpixel(x + dx, y + dy, vesa_state.current_color);
		putpixel(x - dx, y + dy, vesa_state.current_color);
		putpixel(x + dx, y - dy, vesa_state.current_color);
		putpixel(x - dx, y - dy, vesa_state.current_color);
		putpixel(x + dy, y + dx, vesa_state.current_color);
		putpixel(x - dy, y + dx, vesa_state.current_color);
		putpixel(x + dy, y - dx, vesa_state.current_color);
		putpixel(x - dy, y - dx, vesa_state.current_color);

		if (d < 0) {
			d += 2 * dx + 3;
		} else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	vesa_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		vesa_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		vesa_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		vesa_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		vesa_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		vesa_state.line_pattern = pattern;
		break;
	default:
		vesa_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	vesa_state.fill_style = pattern;
	vesa_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, vesa_state.fill_color);
		}
	}
}

void
settextjustify(int horiz, int vert)
{
	vesa_state.text_justify_h = horiz;
	vesa_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 8;
}

int
textwidth(const char *textstring)
{
	return strlen(textstring) * 8;
}

const char *
grapherrormsg(int errorcode)
{
	switch (errorcode) {
	case grOk:
		return "No error";
	case grNoInitGraph:
		return "Graphics not initialized";
	case grNotDetected:
		return "Graphics hardware not detected";
	case grFileNotFound:
		return "Driver file not found";
	case grInvalidDriver:
		return "Invalid graphics driver";
	case grNoLoadMem:
		return "Insufficient memory to load driver";
	default:
		return "Unknown error";
	}
}

int
graphresult(void)
{
	return vesa_state.initialized ? grOk : grNoInitGraph;
}
