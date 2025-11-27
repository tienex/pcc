/*	$Id$	*/
/* BGI for NetBSD wscons framebuffer - Similar to OpenBSD */
#include "../../graphics.h"
#include <dev/wscons/wsconsio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>

static struct {
	int fb_fd;
	struct wsdisplay_fbinfo fb_info;
	unsigned char *framebuffer;
	int width, height, stride;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	uint32_t colors[16];
	int initialized;
} nbsd_state;

static const uint32_t ega_palette[16] = {
	0xFF000000, 0xFF0000AA, 0xFF00AA00, 0xFF00AAAA, 0xFFAA0000, 0xFFAA00AA,
	0xFFAA5500, 0xFFAAAAAA, 0xFF555555, 0xFF5555FF, 0xFF55FF55, 0xFF55FFFF,
	0xFFFF5555, 0xFFFF55FF, 0xFFFFFF55, 0xFFFFFFFF
};

void initgraph(int *driver, int *mode, const char *path) {
	struct wsdisplay_fbinfo info;
	int i; size_t fb_size;
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }
	nbsd_state.fb_fd = open("/dev/ttyE0", O_RDWR);
	if (nbsd_state.fb_fd < 0) { *driver = grNotDetected; return; }
	if (ioctl(nbsd_state.fb_fd, WSDISPLAYIO_GINFO, &info) < 0) {
		close(nbsd_state.fb_fd); *driver = grNotDetected; return;
	}
	nbsd_state.fb_info = info;
	nbsd_state.width = info.width; nbsd_state.height = info.height;
	nbsd_state.stride = info.width * (info.depth / 8);
	fb_size = nbsd_state.stride * nbsd_state.height;
	nbsd_state.framebuffer = (unsigned char *)mmap(NULL, fb_size,
	                          PROT_READ | PROT_WRITE, MAP_SHARED, nbsd_state.fb_fd, 0);
	if (nbsd_state.framebuffer == MAP_FAILED) {
		close(nbsd_state.fb_fd); *driver = grNotDetected; return;
	}
	for (i = 0; i < 16; i++) nbsd_state.colors[i] = ega_palette[i];
	nbsd_state.current_color = WHITE; nbsd_state.current_bkcolor = BLACK;
	nbsd_state.current_x = 0; nbsd_state.current_y = 0;
	nbsd_state.line_style = SOLID_LINE; nbsd_state.line_pattern = 0xFFFF;
	nbsd_state.fill_style = SOLID_FILL; nbsd_state.fill_color = WHITE;
	nbsd_state.text_justify_h = LEFT_TEXT; nbsd_state.text_justify_v = TOP_TEXT;
	nbsd_state.initialized = 1;
	cleardevice(); *driver = 0; *mode = 0;
}

void closegraph(void) {
	if (!nbsd_state.initialized) return;
	if (nbsd_state.framebuffer != MAP_FAILED)
		munmap(nbsd_state.framebuffer, nbsd_state.stride * nbsd_state.height);
	if (nbsd_state.fb_fd >= 0) close(nbsd_state.fb_fd);
	memset(&nbsd_state, 0, sizeof(nbsd_state)); nbsd_state.fb_fd = -1;
}

int getmaxx(void) { return nbsd_state.width - 1; }
int getmaxy(void) { return nbsd_state.height - 1; }
void setcolor(int color) { nbsd_state.current_color = color & 0x0F; }
int getcolor(void) { return nbsd_state.current_color; }
void setbkcolor(int color) { nbsd_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return nbsd_state.current_bkcolor; }

void cleardevice(void) {
	uint32_t *fb = (uint32_t *)nbsd_state.framebuffer;
	uint32_t bg = nbsd_state.colors[nbsd_state.current_bkcolor];
	int i; for (i = 0; i < nbsd_state.width * nbsd_state.height; i++) fb[i] = bg;
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < nbsd_state.width && y >= 0 && y < nbsd_state.height) {
		uint32_t *fb = (uint32_t *)nbsd_state.framebuffer;
		fb[y * nbsd_state.width + x] = nbsd_state.colors[color & 0x0F];
	}
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= nbsd_state.width || y < 0 || y >= nbsd_state.height) return 0;
	uint32_t *fb = (uint32_t *)nbsd_state.framebuffer;
	uint32_t pixel = fb[y * nbsd_state.width + x];
	int i; for (i = 0; i < 16; i++) if (nbsd_state.colors[i] == pixel) return i;
	return 0;
}

void moveto(int x, int y) { nbsd_state.current_x = x; nbsd_state.current_y = y; }
void moverel(int dx, int dy) { nbsd_state.current_x += dx; nbsd_state.current_y += dy; }
int getx(void) { return nbsd_state.current_x; }
int gety(void) { return nbsd_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (nbsd_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, nbsd_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(nbsd_state.current_x, nbsd_state.current_y, x, y);
	nbsd_state.current_x = x; nbsd_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = nbsd_state.current_x + dx, y2 = nbsd_state.current_y + dy;
	line(nbsd_state.current_x, nbsd_state.current_y, x2, y2);
	nbsd_state.current_x = x2; nbsd_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top); line(right, top, right, bottom);
	line(right, bottom, left, bottom); line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, nbsd_state.current_color);
		putpixel(x - dx, y + dy, nbsd_state.current_color);
		putpixel(x + dx, y - dy, nbsd_state.current_color);
		putpixel(x - dx, y - dy, nbsd_state.current_color);
		putpixel(x + dy, y + dx, nbsd_state.current_color);
		putpixel(x - dy, y + dx, nbsd_state.current_color);
		putpixel(x + dy, y - dx, nbsd_state.current_color);
		putpixel(x - dy, y - dx, nbsd_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	nbsd_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: nbsd_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: nbsd_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: nbsd_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: nbsd_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: nbsd_state.line_pattern = pattern; break;
	default: nbsd_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	nbsd_state.fill_style = pattern; nbsd_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, nbsd_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	nbsd_state.text_justify_h = horiz; nbsd_state.text_justify_v = vert;
}

int textheight(const char *textstring) { return 12; }
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
	return nbsd_state.initialized ? grOk : grNoInitGraph;
}
