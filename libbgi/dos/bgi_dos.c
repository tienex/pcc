/*
 * Copyright (c) 2025 PCC Project
 * Borland Graphics Interface (BGI) - DOS Implementation
 * Direct VGA/VESA framebuffer access
 */

#if defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)

#include "../bgi.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <dos.h>

#define VGA_SEGMENT 0xA000
#define VGA_WIDTH 320
#define VGA_HEIGHT 200

/* VGA Mode 13h (320x200x256) framebuffer */
static struct {
	int initialized;
	int width;
	int height;
	int color;
	int fillcolor;
	int linestyle;
	int fillstyle;
	int textjustify_h;
	int textjustify_v;
	unsigned char far *framebuffer;
	int x, y;  /* Current position */
	int old_mode;  /* Original video mode */
} bgi_state = {0};

static const unsigned char bgi_palette_vga[] = {
	0,   /* BLACK */
	1,   /* BLUE */
	2,   /* GREEN */
	3,   /* CYAN */
	4,   /* RED */
	5,   /* MAGENTA */
	20,  /* BROWN */
	7,   /* LIGHTGRAY */
	56,  /* DARKGRAY */
	57,  /* LIGHTBLUE */
	58,  /* LIGHTGREEN */
	59,  /* LIGHTCYAN */
	60,  /* LIGHTRED */
	61,  /* LIGHTMAGENTA */
	62,  /* YELLOW */
	63   /* WHITE */
};

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;
	(void)driver; (void)mode; (void)path;

	/* Save old video mode */
	regs.h.ah = 0x0F;
	int86(0x10, &regs, &regs);
	bgi_state.old_mode = regs.h.al;

	/* Set VGA mode 13h (320x200x256) */
	regs.h.ah = 0x00;
	regs.h.al = 0x13;
	int86(0x10, &regs, &regs);

	bgi_state.width = VGA_WIDTH;
	bgi_state.height = VGA_HEIGHT;
	bgi_state.framebuffer = (unsigned char far *)MK_FP(VGA_SEGMENT, 0);
	bgi_state.color = WHITE;
	bgi_state.fillcolor = WHITE;
	bgi_state.x = bgi_state.y = 0;
	bgi_state.initialized = 1;

	/* Set BGI palette colors */
	for (int i = 0; i < 16; i++) {
		outp(0x3C8, bgi_palette_vga[i]);
		outp(0x3C9, (i & 4) ? 63 : 0);  /* R */
		outp(0x3C9, (i & 2) ? 63 : 0);  /* G */
		outp(0x3C9, (i & 1) ? 63 : 0);  /* B */
	}
}

void closegraph(void) {
	union REGS regs;

	if (bgi_state.initialized) {
		/* Restore old video mode */
		regs.h.ah = 0x00;
		regs.h.al = bgi_state.old_mode;
		int86(0x10, &regs, &regs);

		bgi_state.initialized = 0;
	}
}

void cleardevice(void) {
	if (!bgi_state.initialized) return;
	_fmemset(bgi_state.framebuffer, 0, VGA_WIDTH * VGA_HEIGHT);
}

void setcolor(int color) {
	bgi_state.color = color;
}

void setfillstyle(int pattern, int color) {
	bgi_state.fillstyle = pattern;
	bgi_state.fillcolor = color;
}

static inline void putpixel_direct(int x, int y, int color) {
	if (x >= 0 && x < bgi_state.width && y >= 0 && y < bgi_state.height) {
		bgi_state.framebuffer[y * bgi_state.width + x] = bgi_palette_vga[color & 15];
	}
}

void putpixel(int x, int y, int color) {
	putpixel_direct(x, y, color);
}

int getpixel(int x, int y) {
	if (x >= 0 && x < bgi_state.width && y >= 0 && y < bgi_state.height) {
		unsigned char c = bgi_state.framebuffer[y * bgi_state.width + x];
		/* Reverse lookup in palette */
		for (int i = 0; i < 16; i++) {
			if (bgi_palette_vga[i] == c) return i;
		}
	}
	return 0;
}

void line(int x1, int y1, int x2, int y2) {
	if (!bgi_state.initialized) return;

	/* Bresenham's line algorithm */
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy;

	while (1) {
		putpixel_direct(x1, y1, bgi_state.color);

		if (x1 == x2 && y1 == y2) break;

		int e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
	}

	bgi_state.x = x2;
	bgi_state.y = y2;
}

void lineto(int x, int y) {
	line(bgi_state.x, bgi_state.y, x, y);
}

void moveto(int x, int y) {
	bgi_state.x = x;
	bgi_state.y = y;
}

void circle(int x, int y, int radius) {
	if (!bgi_state.initialized) return;

	/* Midpoint circle algorithm */
	int d = 3 - 2 * radius;
	int cx = 0, cy = radius;

	while (cy >= cx) {
		putpixel_direct(x + cx, y + cy, bgi_state.color);
		putpixel_direct(x - cx, y + cy, bgi_state.color);
		putpixel_direct(x + cx, y - cy, bgi_state.color);
		putpixel_direct(x - cx, y - cy, bgi_state.color);
		putpixel_direct(x + cy, y + cx, bgi_state.color);
		putpixel_direct(x - cy, y + cx, bgi_state.color);
		putpixel_direct(x + cy, y - cx, bgi_state.color);
		putpixel_direct(x - cy, y - cx, bgi_state.color);

		cx++;
		if (d > 0) {
			cy--;
			d = d + 4 * (cx - cy) + 10;
		} else {
			d = d + 4 * cx + 6;
		}
	}
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void bar(int left, int top, int right, int bottom) {
	if (!bgi_state.initialized) return;

	for (int y = top; y <= bottom; y++) {
		for (int x = left; x <= right; x++) {
			putpixel_direct(x, y, bgi_state.fillcolor);
		}
	}
}

void outtextxy(int x, int y, const char *text) {
	/* Simple 8x8 text rendering using BIOS */
	(void)x; (void)y; (void)text;
	/* Would implement bitmap font rendering here */
}

void outtext(const char *text) {
	outtextxy(bgi_state.x, bgi_state.y, text);
}

void setbkcolor(int color) {
	/* Set background color via VGA DAC */
	outp(0x3C8, 0);
	outp(0x3C9, (color & 4) ? 63 : 0);
	outp(0x3C9, (color & 2) ? 63 : 0);
	outp(0x3C9, (color & 1) ? 63 : 0);
}

int getbkcolor(void) {
	return 0;  /* Would need to read from VGA DAC */
}

int getcolor(void) {
	return bgi_state.color;
}

int getmaxx(void) {
	return bgi_state.width - 1;
}

int getmaxy(void) {
	return bgi_state.height - 1;
}

int getx(void) {
	return bgi_state.x;
}

int gety(void) {
	return bgi_state.y;
}

#endif /* __DOS__ */
