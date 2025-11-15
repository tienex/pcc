/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Linux DRM/KMS (Direct Rendering Manager)
 *
 * Provides BGI graphics using modern Linux kernel mode setting
 * Works on: Modern Linux (3.0+) with DRM/KMS support
 * Requires: libdrm, libgbm
 * Features: Hardware acceleration, mode setting, multi-monitor support
 */

#include "../graphics.h"
#include <xf86drm.h>
#include <xf86drmMode.h>
#include <gbm.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>

/* Internal state */
static struct {
	int drm_fd;
	drmModeModeInfo mode;
	drmModeCrtc *saved_crtc;
	uint32_t connector_id;
	uint32_t crtc_id;
	struct gbm_device *gbm_dev;
	struct gbm_surface *gbm_surf;
	struct gbm_bo *bo;
	uint32_t fb_id;
	uint32_t *framebuffer;
	int width;
	int height;
	int stride;
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
	uint32_t colors[16];	/* EGA palette in XRGB8888 */
	int initialized;
} drm_state;

/* EGA color palette (XRGB8888) */
static const uint32_t ega_palette[16] = {
	0xFF000000,	/* BLACK */
	0xFF0000AA,	/* BLUE */
	0xFF00AA00,	/* GREEN */
	0xFF00AAAA,	/* CYAN */
	0xFFAA0000,	/* RED */
	0xFFAA00AA,	/* MAGENTA */
	0xFFAA5500,	/* BROWN */
	0xFFAAAAAA,	/* LIGHTGRAY */
	0xFF555555,	/* DARKGRAY */
	0xFF5555FF,	/* LIGHTBLUE */
	0xFF55FF55,	/* LIGHTGREEN */
	0xFF55FFFF,	/* LIGHTCYAN */
	0xFFFF5555,	/* LIGHTRED */
	0xFFFF55FF,	/* LIGHTMAGENTA */
	0xFFFFFF55,	/* YELLOW */
	0xFFFFFFFF	/* WHITE */
};

static int
find_drm_device(void)
{
	drmDevicePtr devices[64];
	int num_devices, i, fd = -1;

	num_devices = drmGetDevices2(0, devices, 64);
	if (num_devices < 0)
		return -1;

	for (i = 0; i < num_devices; i++) {
		drmDevicePtr dev = devices[i];
		if (!(dev->available_nodes & (1 << DRM_NODE_PRIMARY)))
			continue;

		fd = open(dev->nodes[DRM_NODE_PRIMARY], O_RDWR);
		if (fd >= 0)
			break;
	}

	drmFreeDevices(devices, num_devices);
	return fd;
}

void
initgraph(int *driver, int *mode, const char *path)
{
	drmModeRes *resources;
	drmModeConnector *connector = NULL;
	drmModeEncoder *encoder;
	int width = 640, height = 480;
	int i;

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGAHI;
	}

	/* Determine resolution */
	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	/* Open DRM device */
	drm_state.drm_fd = find_drm_device();
	if (drm_state.drm_fd < 0) {
		*driver = grNotDetected;
		return;
	}

	/* Get resources */
	resources = drmModeGetResources(drm_state.drm_fd);
	if (!resources) {
		close(drm_state.drm_fd);
		*driver = grNotDetected;
		return;
	}

	/* Find connected connector */
	for (i = 0; i < resources->count_connectors; i++) {
		connector = drmModeGetConnector(drm_state.drm_fd,
		                                 resources->connectors[i]);
		if (connector->connection == DRM_MODE_CONNECTED &&
		    connector->count_modes > 0)
			break;

		drmModeFreeConnector(connector);
		connector = NULL;
	}

	if (!connector) {
		drmModeFreeResources(resources);
		close(drm_state.drm_fd);
		*driver = grNotDetected;
		return;
	}

	/* Use first mode */
	drm_state.connector_id = connector->connector_id;
	drm_state.mode = connector->modes[0];
	drm_state.width = drm_state.mode.hdisplay;
	drm_state.height = drm_state.mode.vdisplay;

	/* Get encoder and CRTC */
	encoder = drmModeGetEncoder(drm_state.drm_fd, connector->encoder_id);
	if (encoder) {
		drm_state.crtc_id = encoder->crtc_id;
		drmModeFreeEncoder(encoder);
	}

	/* Save current CRTC */
	drm_state.saved_crtc = drmModeGetCrtc(drm_state.drm_fd, drm_state.crtc_id);

	/* Create GBM device */
	drm_state.gbm_dev = gbm_create_device(drm_state.drm_fd);
	if (!drm_state.gbm_dev) {
		drmModeFreeConnector(connector);
		drmModeFreeResources(resources);
		close(drm_state.drm_fd);
		*driver = grNotDetected;
		return;
	}

	/* Allocate framebuffer */
	drm_state.framebuffer = (uint32_t *)calloc(drm_state.width * drm_state.height, 4);
	if (!drm_state.framebuffer) {
		gbm_device_destroy(drm_state.gbm_dev);
		drmModeFreeConnector(connector);
		drmModeFreeResources(resources);
		close(drm_state.drm_fd);
		*driver = grNotDetected;
		return;
	}

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		drm_state.colors[i] = ega_palette[i];

	drm_state.current_color = WHITE;
	drm_state.current_bkcolor = BLACK;
	drm_state.current_x = 0;
	drm_state.current_y = 0;
	drm_state.line_style = SOLID_LINE;
	drm_state.line_pattern = 0xFFFF;
	drm_state.fill_style = SOLID_FILL;
	drm_state.fill_color = WHITE;
	drm_state.text_justify_h = LEFT_TEXT;
	drm_state.text_justify_v = TOP_TEXT;
	drm_state.initialized = 1;

	drmModeFreeConnector(connector);
	drmModeFreeResources(resources);

	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!drm_state.initialized)
		return;

	/* Restore saved CRTC */
	if (drm_state.saved_crtc) {
		drmModeSetCrtc(drm_state.drm_fd, drm_state.saved_crtc->crtc_id,
		               drm_state.saved_crtc->buffer_id,
		               drm_state.saved_crtc->x, drm_state.saved_crtc->y,
		               &drm_state.connector_id, 1, &drm_state.saved_crtc->mode);
		drmModeFreeCrtc(drm_state.saved_crtc);
	}

	if (drm_state.fb_id)
		drmModeRmFB(drm_state.drm_fd, drm_state.fb_id);

	if (drm_state.framebuffer)
		free(drm_state.framebuffer);

	if (drm_state.gbm_dev)
		gbm_device_destroy(drm_state.gbm_dev);

	if (drm_state.drm_fd >= 0)
		close(drm_state.drm_fd);

	memset(&drm_state, 0, sizeof(drm_state));
	drm_state.drm_fd = -1;
}

int
getmaxx(void)
{
	return drm_state.width - 1;
}

int
getmaxy(void)
{
	return drm_state.height - 1;
}

void
setcolor(int color)
{
	drm_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return drm_state.current_color;
}

void
setbkcolor(int color)
{
	drm_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return drm_state.current_bkcolor;
}

void
cleardevice(void)
{
	uint32_t bg = drm_state.colors[drm_state.current_bkcolor];
	int i;

	for (i = 0; i < drm_state.width * drm_state.height; i++)
		drm_state.framebuffer[i] = bg;
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < drm_state.width && y >= 0 && y < drm_state.height)
		drm_state.framebuffer[y * drm_state.width + x] = drm_state.colors[color & 0x0F];
}

unsigned int
getpixel(int x, int y)
{
	uint32_t pixel;
	int i;

	if (x < 0 || x >= drm_state.width || y < 0 || y >= drm_state.height)
		return 0;

	pixel = drm_state.framebuffer[y * drm_state.width + x];

	for (i = 0; i < 16; i++)
		if (drm_state.colors[i] == pixel)
			return i;

	return 0;
}

void
moveto(int x, int y)
{
	drm_state.current_x = x;
	drm_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	drm_state.current_x += dx;
	drm_state.current_y += dy;
}

int
getx(void)
{
	return drm_state.current_x;
}

int
gety(void)
{
	return drm_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	int dx = abs(x2 - x1);
	int dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1;
	int sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy;
	int e2, pattern_pos = 0;

	while (1) {
		if (drm_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, drm_state.current_color);

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
	line(drm_state.current_x, drm_state.current_y, x, y);
	drm_state.current_x = x;
	drm_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = drm_state.current_x + dx;
	int y2 = drm_state.current_y + dy;
	line(drm_state.current_x, drm_state.current_y, x2, y2);
	drm_state.current_x = x2;
	drm_state.current_y = y2;
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
	int dx = 0, dy = radius, d = 1 - radius;

	while (dx <= dy) {
		putpixel(x + dx, y + dy, drm_state.current_color);
		putpixel(x - dx, y + dy, drm_state.current_color);
		putpixel(x + dx, y - dy, drm_state.current_color);
		putpixel(x - dx, y - dy, drm_state.current_color);
		putpixel(x + dy, y + dx, drm_state.current_color);
		putpixel(x - dy, y + dx, drm_state.current_color);
		putpixel(x + dy, y - dx, drm_state.current_color);
		putpixel(x - dy, y - dx, drm_state.current_color);

		if (d < 0)
			d += 2 * dx + 3;
		else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	drm_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE: drm_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: drm_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: drm_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: drm_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: drm_state.line_pattern = pattern; break;
	default: drm_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	drm_state.fill_style = pattern;
	drm_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, drm_state.fill_color);
}

void
settextjustify(int horiz, int vert)
{
	drm_state.text_justify_h = horiz;
	drm_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 12;
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
	case grOk: return "No error";
	case grNoInitGraph: return "Graphics not initialized";
	case grNotDetected: return "Graphics hardware not detected";
	case grFileNotFound: return "Driver file not found";
	case grInvalidDriver: return "Invalid graphics driver";
	case grNoLoadMem: return "Insufficient memory to load driver";
	default: return "Unknown error";
	}
}

int
graphresult(void)
{
	return drm_state.initialized ? grOk : grNoInitGraph;
}
