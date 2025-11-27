/*
 * Copyright (c) 2025 PCC Project
 * Borland Graphics Interface (BGI) - Modern Implementation
 */

#include "bgi.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/* Simple framebuffer-based implementation */
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
	unsigned int *framebuffer;
	int x, y;  /* Current position */
} bgi_state = {0};

static const unsigned int bgi_palette[] = {
	0x000000, 0x0000AA, 0x00AA00, 0x00AAAA,
	0xAA0000, 0xAA00AA, 0xAA5500, 0xAAAAAA,
	0x555555, 0x5555FF, 0x55FF55, 0x55FFFF,
	0xFF5555, 0xFF55FF, 0xFFFF55, 0xFFFFFF
};

void initgraph(int *driver, int *mode, const char *path) {
	(void)driver; (void)mode; (void)path;
	
	bgi_state.width = 640;
	bgi_state.height = 480;
	bgi_state.framebuffer = calloc(bgi_state.width * bgi_state.height, sizeof(unsigned int));
	bgi_state.color = WHITE;
	bgi_state.fillcolor = WHITE;
	bgi_state.x = bgi_state.y = 0;
	bgi_state.initialized = 1;
}

void closegraph(void) {
	if (bgi_state.framebuffer) {
		free(bgi_state.framebuffer);
		bgi_state.framebuffer = NULL;
	}
	bgi_state.initialized = 0;
}

void cleardevice(void) {
	if (!bgi_state.initialized || !bgi_state.framebuffer) return;
	memset(bgi_state.framebuffer, 0, bgi_state.width * bgi_state.height * sizeof(unsigned int));
}

void setcolor(int color) {
	bgi_state.color = color;
}

void setfillstyle(int pattern, int color) {
	bgi_state.fillstyle = pattern;
	bgi_state.fillcolor = color;
}

void line(int x1, int y1, int x2, int y2) {
	if (!bgi_state.initialized || !bgi_state.framebuffer) return;
	
	/* Bresenham's line algorithm */
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy;
	
	unsigned int col = bgi_palette[bgi_state.color & 15];
	
	while (1) {
		if (x1 >= 0 && x1 < bgi_state.width && y1 >= 0 && y1 < bgi_state.height) {
			bgi_state.framebuffer[y1 * bgi_state.width + x1] = col;
		}
		
		if (x1 == x2 && y1 == y2) break;
		
		int e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
	}
}

void circle(int x, int y, int radius) {
	if (!bgi_state.initialized || !bgi_state.framebuffer) return;
	
	/* Midpoint circle algorithm */
	int d = 3 - 2 * radius;
	int cx = 0, cy = radius;
	
	unsigned int col = bgi_palette[bgi_state.color & 15];
	
	while (cy >= cx) {
		/* Plot 8 points */
		int points[8][2] = {
			{x + cx, y + cy}, {x - cx, y + cy}, {x + cx, y - cy}, {x - cx, y - cy},
			{x + cy, y + cx}, {x - cy, y + cx}, {x + cy, y - cx}, {x - cy, y - cx}
		};
		
		for (int i = 0; i < 8; i++) {
			int px = points[i][0], py = points[i][1];
			if (px >= 0 && px < bgi_state.width && py >= 0 && py < bgi_state.height) {
				bgi_state.framebuffer[py * bgi_state.width + px] = col;
			}
		}
		
		cx++;
		if (d > 0) { cy--; d = d + 4 * (cx - cy) + 10; }
		else d = d + 4 * cx + 6;
	}
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void bar(int left, int top, int right, int bottom) {
	if (!bgi_state.initialized || !bgi_state.framebuffer) return;
	
	unsigned int col = bgi_palette[bgi_state.fillcolor & 15];
	
	for (int y = top; y <= bottom && y < bgi_state.height; y++) {
		if (y < 0) continue;
		for (int x = left; x <= right && x < bgi_state.width; x++) {
			if (x >= 0) {
				bgi_state.framebuffer[y * bgi_state.width + x] = col;
			}
		}
	}
}

void outtextxy(int x, int y, const char *text) {
	/* Simple text rendering stub */
	(void)x; (void)y; (void)text;
}

int getmaxx(void) { return bgi_state.width - 1; }
int getmaxy(void) { return bgi_state.height - 1; }
int getx(void) { return bgi_state.x; }
int gety(void) { return bgi_state.y; }
void moveto(int x, int y) { bgi_state.x = x; bgi_state.y = y; }
void lineto(int x, int y) { line(bgi_state.x, bgi_state.y, x, y); moveto(x, y); }

int kbhit(void) { return 0; }
int getch(void) { return 0; }
void delay(unsigned int milliseconds) {
	/* Platform-specific delay implementation */
	(void)milliseconds;
}
