/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using UEFI GOP (Graphics Output Protocol)
 *
 * UEFI firmware graphics for bootloaders and OS kernels
 * Works on: UEFI firmware (modern PC/server boot environment)
 * Requires: UEFI GOP protocol
 * Features: Framebuffer access, multiple resolutions
 * Note: For UEFI applications, bootloaders, and bare-metal code
 */

#include "../graphics.h"
#include <efi.h>
#include <efilib.h>
#include <stddef.h>

static struct {
	EFI_GRAPHICS_OUTPUT_PROTOCOL *gop;
	EFI_GRAPHICS_OUTPUT_BLT_PIXEL *framebuffer;
	UINT32 width, height;
	UINT32 pixels_per_scanline;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	EFI_GRAPHICS_OUTPUT_BLT_PIXEL colors[16];
	int initialized;
} gop_state;

/* EGA palette in UEFI GOP format (BGRA) */
static const EFI_GRAPHICS_OUTPUT_BLT_PIXEL ega_palette[16] = {
	{0x00, 0x00, 0x00, 0x00},  /* BLACK */
	{0xAA, 0x00, 0x00, 0x00},  /* BLUE */
	{0x00, 0xAA, 0x00, 0x00},  /* GREEN */
	{0xAA, 0xAA, 0x00, 0x00},  /* CYAN */
	{0x00, 0x00, 0xAA, 0x00},  /* RED */
	{0xAA, 0x00, 0xAA, 0x00},  /* MAGENTA */
	{0x00, 0x55, 0xAA, 0x00},  /* BROWN */
	{0xAA, 0xAA, 0xAA, 0x00},  /* LIGHTGRAY */
	{0x55, 0x55, 0x55, 0x00},  /* DARKGRAY */
	{0xFF, 0x55, 0x55, 0x00},  /* LIGHTBLUE */
	{0x55, 0xFF, 0x55, 0x00},  /* LIGHTGREEN */
	{0xFF, 0xFF, 0x55, 0x00},  /* LIGHTCYAN */
	{0x55, 0x55, 0xFF, 0x00},  /* LIGHTRED */
	{0xFF, 0x55, 0xFF, 0x00},  /* LIGHTMAGENTA */
	{0x55, 0xFF, 0xFF, 0x00},  /* YELLOW */
	{0xFF, 0xFF, 0xFF, 0x00}   /* WHITE */
};

extern EFI_SYSTEM_TABLE *ST;
extern EFI_BOOT_SERVICES *BS;

void initgraph(int *driver, int *mode, const char *path) {
	EFI_STATUS status;
	EFI_GUID gop_guid = EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;
	UINTN mode_num = 0;
	UINT32 target_width = 640, target_height = 480;
	UINTN i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* Map BGI mode to resolution */
	switch (*mode) {
	case VGALO: target_width = 640; target_height = 200; break;
	case VGAMED: target_width = 640; target_height = 350; break;
	case VGAHI: target_width = 640; target_height = 480; break;
	case SVGA_800_600: target_width = 800; target_height = 600; break;
	case SVGA_1024_768: target_width = 1024; target_height = 768; break;
	default: target_width = 640; target_height = 480;
	}

	/* Locate GOP protocol */
	status = BS->LocateProtocol(&gop_guid, NULL, (void **)&gop_state.gop);
	if (EFI_ERROR(status)) {
		*driver = grNotDetected;
		return;
	}

	/* Find matching mode */
	for (i = 0; i < gop_state.gop->Mode->MaxMode; i++) {
		EFI_GRAPHICS_OUTPUT_MODE_INFORMATION *info;
		UINTN size_of_info;

		status = gop_state.gop->QueryMode(gop_state.gop, i, &size_of_info, &info);
		if (EFI_ERROR(status))
			continue;

		if (info->HorizontalResolution == target_width &&
		    info->VerticalResolution == target_height) {
			mode_num = i;
			break;
		}
	}

	/* Set graphics mode */
	status = gop_state.gop->SetMode(gop_state.gop, mode_num);
	if (EFI_ERROR(status)) {
		*driver = grNotDetected;
		return;
	}

	/* Get framebuffer info */
	gop_state.framebuffer = (EFI_GRAPHICS_OUTPUT_BLT_PIXEL *)
		gop_state.gop->Mode->FrameBufferBase;
	gop_state.width = gop_state.gop->Mode->Info->HorizontalResolution;
	gop_state.height = gop_state.gop->Mode->Info->VerticalResolution;
	gop_state.pixels_per_scanline = gop_state.gop->Mode->Info->PixelsPerScanLine;

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		gop_state.colors[i] = ega_palette[i];

	gop_state.current_color = WHITE;
	gop_state.current_bkcolor = BLACK;
	gop_state.current_x = 0;
	gop_state.current_y = 0;
	gop_state.line_style = SOLID_LINE;
	gop_state.line_pattern = 0xFFFF;
	gop_state.fill_style = SOLID_FILL;
	gop_state.fill_color = WHITE;
	gop_state.text_justify_h = LEFT_TEXT;
	gop_state.text_justify_v = TOP_TEXT;
	gop_state.initialized = 1;

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!gop_state.initialized)
		return;

	/* Reset to mode 0 (text mode) */
	if (gop_state.gop)
		gop_state.gop->SetMode(gop_state.gop, 0);

	for (int i = 0; i < sizeof(gop_state); i++)
		((char *)&gop_state)[i] = 0;
}

int getmaxx(void) { return gop_state.width - 1; }
int getmaxy(void) { return gop_state.height - 1; }

void setcolor(int color) {
	gop_state.current_color = color & 0x0F;
}

int getcolor(void) { return gop_state.current_color; }
void setbkcolor(int color) { gop_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return gop_state.current_bkcolor; }

void cleardevice(void) {
	EFI_GRAPHICS_OUTPUT_BLT_PIXEL *pixel = gop_state.framebuffer;
	UINT32 x, y;

	for (y = 0; y < gop_state.height; y++) {
		for (x = 0; x < gop_state.width; x++) {
			pixel[y * gop_state.pixels_per_scanline + x] =
				gop_state.colors[gop_state.current_bkcolor];
		}
	}
}

void putpixel(int x, int y, int color) {
	if (x < 0 || x >= (int)gop_state.width || y < 0 || y >= (int)gop_state.height)
		return;

	gop_state.framebuffer[y * gop_state.pixels_per_scanline + x] =
		gop_state.colors[color & 0x0F];
}

unsigned int getpixel(int x, int y) {
	EFI_GRAPHICS_OUTPUT_BLT_PIXEL pixel;
	int i;

	if (x < 0 || x >= (int)gop_state.width || y < 0 || y >= (int)gop_state.height)
		return 0;

	pixel = gop_state.framebuffer[y * gop_state.pixels_per_scanline + x];

	/* Find matching EGA color */
	for (i = 0; i < 16; i++) {
		if (pixel.Blue == gop_state.colors[i].Blue &&
		    pixel.Green == gop_state.colors[i].Green &&
		    pixel.Red == gop_state.colors[i].Red)
			return i;
	}

	return 0;
}

void moveto(int x, int y) {
	gop_state.current_x = x;
	gop_state.current_y = y;
}

void moverel(int dx, int dy) {
	gop_state.current_x += dx;
	gop_state.current_y += dy;
}

int getx(void) { return gop_state.current_x; }
int gety(void) { return gop_state.current_y; }

static int abs(int x) { return x < 0 ? -x : x; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;

	/* Bresenham's line algorithm */
	while (1) {
		if (gop_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, gop_state.current_color);
		if (x1 == x2 && y1 == y2)
			break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(gop_state.current_x, gop_state.current_y, x, y);
	gop_state.current_x = x;
	gop_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = gop_state.current_x + dx;
	int y2 = gop_state.current_y + dy;
	line(gop_state.current_x, gop_state.current_y, x2, y2);
	gop_state.current_x = x2;
	gop_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;

	/* Bresenham's circle algorithm */
	while (dx <= dy) {
		putpixel(x + dx, y + dy, gop_state.current_color);
		putpixel(x - dx, y + dy, gop_state.current_color);
		putpixel(x + dx, y - dy, gop_state.current_color);
		putpixel(x - dx, y - dy, gop_state.current_color);
		putpixel(x + dy, y + dx, gop_state.current_color);
		putpixel(x - dy, y + dx, gop_state.current_color);
		putpixel(x + dy, y - dx, gop_state.current_color);
		putpixel(x - dy, y - dx, gop_state.current_color);

		if (d < 0)
			d += 2 * dx + 3;
		else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	gop_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: gop_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: gop_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: gop_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: gop_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: gop_state.line_pattern = pattern; break;
	default: gop_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	gop_state.fill_style = pattern;
	gop_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, gop_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	gop_state.text_justify_h = horiz;
	gop_state.text_justify_v = vert;
}

static UINTN strlen(const char *s) {
	UINTN len = 0;
	while (*s++) len++;
	return len;
}

int textheight(const char *textstring) { return 16; }
int textwidth(const char *textstring) { return strlen(textstring) * 8; }

const char *grapherrormsg(int errorcode) {
	switch (errorcode) {
	case grOk: return "No error";
	case grNoInitGraph: return "Graphics not initialized";
	case grNotDetected: return "Graphics hardware not detected";
	case grFileNotFound: return "Driver file not found";
	case grInvalidDriver: return "Invalid graphics driver";
	case grNoLoadMem: return "Insufficient memory to load driver";
	default: return "Unknown error";
	}
}

int graphresult(void) {
	return gop_state.initialized ? grOk : grNoInitGraph;
}
