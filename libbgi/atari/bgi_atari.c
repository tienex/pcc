/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Atari TOS/GEM using VDI
 *
 * Provides BGI graphics on Atari ST/STE/TT/Falcon using GEM VDI
 * Works on: Atari TOS, FreeMiNT
 * Requires: GEM VDI (Virtual Device Interface)
 */

#include "../graphics.h"
#include <vdi.h>
#include <aes.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	short vdi_handle;
	short work_in[11];
	short work_out[57];
	short width;
	short height;
	short current_color;
	short current_bkcolor;
	short current_x;
	short current_y;
	short line_style;
	unsigned short line_pattern;
	short fill_style;
	short fill_color;
	short text_justify_h;
	short text_justify_v;
	short initialized;
} atari_state;

/* EGA to VDI color mapping */
static const short ega_to_vdi[16] = {
	0,	/* BLACK -> WHITE (VDI uses 0 for white) */
	9,	/* BLUE */
	13,	/* GREEN */
	14,	/* CYAN */
	12,	/* RED */
	6,	/* MAGENTA */
	4,	/* BROWN */
	8,	/* LIGHTGRAY */
	15,	/* DARKGRAY */
	11,	/* LIGHTBLUE */
	3,	/* LIGHTGREEN */
	7,	/* LIGHTCYAN */
	2,	/* LIGHTRED */
	5,	/* LIGHTMAGENTA */
	10,	/* YELLOW */
	1	/* WHITE -> BLACK (VDI uses 1 for black) */
};

void
initgraph(int *driver, int *mode, const char *path)
{
	short i;

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGAHI;
	}

	/* Initialize AES */
	appl_init();

	/* Open virtual workstation */
	atari_state.vdi_handle = graf_handle(&i, &i, &i, &i);

	/* Set up work_in array */
	for (i = 0; i < 10; i++)
		atari_state.work_in[i] = 1;
	atari_state.work_in[10] = 2;	/* Raster coordinates */

	v_opnvwk(atari_state.work_in, &atari_state.vdi_handle, atari_state.work_out);

	if (atari_state.vdi_handle == 0) {
		appl_exit();
		*driver = grNotDetected;
		return;
	}

	/* Get screen dimensions */
	atari_state.width = atari_state.work_out[0] + 1;
	atari_state.height = atari_state.work_out[1] + 1;

	/* Initialize state */
	atari_state.current_color = 1;		/* VDI black (BGI white) */
	atari_state.current_bkcolor = 0;	/* VDI white (BGI black) */
	atari_state.current_x = 0;
	atari_state.current_y = 0;
	atari_state.line_style = SOLID_LINE;
	atari_state.line_pattern = 0xFFFF;
	atari_state.fill_style = SOLID_FILL;
	atari_state.fill_color = 1;
	atari_state.text_justify_h = LEFT_TEXT;
	atari_state.text_justify_v = TOP_TEXT;
	atari_state.initialized = 1;

	/* Set drawing mode */
	vswr_mode(atari_state.vdi_handle, MD_REPLACE);

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!atari_state.initialized)
		return;

	if (atari_state.vdi_handle > 0) {
		v_clsvwk(atari_state.vdi_handle);
		appl_exit();
	}

	memset(&atari_state, 0, sizeof(atari_state));
}

int
getmaxx(void)
{
	return atari_state.width - 1;
}

int
getmaxy(void)
{
	return atari_state.height - 1;
}

void
setcolor(int color)
{
	atari_state.current_color = ega_to_vdi[color & 0x0F];
	vsl_color(atari_state.vdi_handle, atari_state.current_color);
	vsm_color(atari_state.vdi_handle, atari_state.current_color);
}

int
getcolor(void)
{
	int i;
	/* Convert VDI color back to BGI color */
	for (i = 0; i < 16; i++) {
		if (ega_to_vdi[i] == atari_state.current_color)
			return i;
	}
	return 0;
}

void
setbkcolor(int color)
{
	atari_state.current_bkcolor = ega_to_vdi[color & 0x0F];
}

int
getbkcolor(void)
{
	int i;
	for (i = 0; i < 16; i++) {
		if (ega_to_vdi[i] == atari_state.current_bkcolor)
			return i;
	}
	return 0;
}

void
cleardevice(void)
{
	short pxy[4];

	/* Fill screen with background color */
	vsf_color(atari_state.vdi_handle, atari_state.current_bkcolor);
	vsf_interior(atari_state.vdi_handle, FIS_SOLID);

	pxy[0] = 0;
	pxy[1] = 0;
	pxy[2] = atari_state.width - 1;
	pxy[3] = atari_state.height - 1;

	vr_recfl(atari_state.vdi_handle, pxy);

	/* Restore foreground color */
	vsf_color(atari_state.vdi_handle, atari_state.current_color);
}

void
putpixel(int x, int y, int color)
{
	short pxy[2];

	if (x >= 0 && x < atari_state.width && y >= 0 && y < atari_state.height) {
		pxy[0] = x;
		pxy[1] = y;
		vsm_color(atari_state.vdi_handle, ega_to_vdi[color & 0x0F]);
		v_pmarker(atari_state.vdi_handle, 1, pxy);
		vsm_color(atari_state.vdi_handle, atari_state.current_color);
	}
}

unsigned int
getpixel(int x, int y)
{
	short color;
	short pxy[2];
	int i;

	if (x < 0 || x >= atari_state.width || y < 0 || y >= atari_state.height)
		return 0;

	pxy[0] = x;
	pxy[1] = y;
	vq_color(atari_state.vdi_handle, 0, 0, pxy);
	color = pxy[0];

	/* Convert VDI color to BGI color */
	for (i = 0; i < 16; i++) {
		if (ega_to_vdi[i] == color)
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	atari_state.current_x = x;
	atari_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	atari_state.current_x += dx;
	atari_state.current_y += dy;
}

int
getx(void)
{
	return atari_state.current_x;
}

int
gety(void)
{
	return atari_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	short pxy[4];

	if (atari_state.line_style == SOLID_LINE) {
		pxy[0] = x1;
		pxy[1] = y1;
		pxy[2] = x2;
		pxy[3] = y2;
		v_pline(atari_state.vdi_handle, 2, pxy);
	} else {
		/* Bresenham's line algorithm for styled lines */
		int dx = abs(x2 - x1);
		int dy = abs(y2 - y1);
		int sx = (x1 < x2) ? 1 : -1;
		int sy = (y1 < y2) ? 1 : -1;
		int err = dx - dy;
		int e2;
		int pattern_pos = 0;

		while (1) {
			if (atari_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, atari_state.current_color);

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
}

void
lineto(int x, int y)
{
	line(atari_state.current_x, atari_state.current_y, x, y);
	atari_state.current_x = x;
	atari_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = atari_state.current_x + dx;
	int y2 = atari_state.current_y + dy;
	line(atari_state.current_x, atari_state.current_y, x2, y2);
	atari_state.current_x = x2;
	atari_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	short pxy[10];

	pxy[0] = left;
	pxy[1] = top;
	pxy[2] = right;
	pxy[3] = top;
	pxy[4] = right;
	pxy[5] = bottom;
	pxy[6] = left;
	pxy[7] = bottom;
	pxy[8] = left;
	pxy[9] = top;

	v_pline(atari_state.vdi_handle, 5, pxy);
}

void
circle(int x, int y, int radius)
{
	v_circle(atari_state.vdi_handle, x, y, radius);
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	atari_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		atari_state.line_pattern = 0xFFFF;
		vsl_type(atari_state.vdi_handle, LT_SOLID);
		break;
	case DOTTED_LINE:
		atari_state.line_pattern = 0xCCCC;
		vsl_type(atari_state.vdi_handle, LT_DOT);
		break;
	case CENTER_LINE:
		atari_state.line_pattern = 0xF8F8;
		vsl_type(atari_state.vdi_handle, LT_DASHDOT);
		break;
	case DASHED_LINE:
		atari_state.line_pattern = 0xF0F0;
		vsl_type(atari_state.vdi_handle, LT_DASH);
		break;
	case USERBIT_LINE:
		atari_state.line_pattern = pattern;
		vsl_udsty(atari_state.vdi_handle, pattern);
		vsl_type(atari_state.vdi_handle, LT_USERDEF);
		break;
	default:
		atari_state.line_pattern = 0xFFFF;
		vsl_type(atari_state.vdi_handle, LT_SOLID);
	}

	if (thickness > 0)
		vsl_width(atari_state.vdi_handle, thickness);
}

void
setfillstyle(int pattern, int color)
{
	atari_state.fill_style = pattern;
	atari_state.fill_color = ega_to_vdi[color & 0x0F];

	vsf_color(atari_state.vdi_handle, atari_state.fill_color);

	if (pattern == SOLID_FILL)
		vsf_interior(atari_state.vdi_handle, FIS_SOLID);
	else
		vsf_interior(atari_state.vdi_handle, FIS_PATTERN);
}

void
bar(int left, int top, int right, int bottom)
{
	short pxy[4];

	pxy[0] = left;
	pxy[1] = top;
	pxy[2] = right;
	pxy[3] = bottom;

	vr_recfl(atari_state.vdi_handle, pxy);
}

void
settextjustify(int horiz, int vert)
{
	atari_state.text_justify_h = horiz;
	atari_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	short dummy, charh;
	vqt_attributes(atari_state.vdi_handle, &dummy);
	return charh;
}

int
textwidth(const char *textstring)
{
	short extent[8];
	vqt_extent(atari_state.vdi_handle, (char *)textstring, extent);
	return extent[2] - extent[0];
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
	return atari_state.initialized ? grOk : grNoInitGraph;
}
