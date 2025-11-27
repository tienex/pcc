/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for PC GEM (Digital Research GEM for IBM PC)
 *
 * GEM Desktop Environment for IBM PC compatibles
 * Works on: DOS with Digital Research GEM/1, GEM/2, GEM/3
 * Features: VDI graphics, PC-specific GEM implementation
 */

#include "../graphics.h"
#include <gem.h>
#include <vdi.h>
#include <stdlib.h>
#include <string.h>

static struct {
	int vdi_handle;
	int work_in[11];
	int work_out[57];
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	int initialized;
} pcgem_state;

/* EGA palette for PC GEM */
static const int ega_vdi_colors[16] = {
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
};

void initgraph(int *driver, int *mode, const char *path) {
	int i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* Initialize GEM Application */
	appl_init();

	/* Open VDI workstation */
	for (i = 0; i < 10; i++)
		pcgem_state.work_in[i] = 1;
	pcgem_state.work_in[10] = 2;  /* Raster coordinates */

	pcgem_state.vdi_handle = graf_handle(&i, &i, &i, &i);
	v_opnvwk(pcgem_state.work_in, &pcgem_state.vdi_handle, pcgem_state.work_out);

	if (pcgem_state.vdi_handle == 0) {
		appl_exit();
		*driver = grNotDetected;
		return;
	}

	/* Get screen dimensions */
	pcgem_state.width = pcgem_state.work_out[0] + 1;
	pcgem_state.height = pcgem_state.work_out[1] + 1;

	pcgem_state.current_color = WHITE;
	pcgem_state.current_bkcolor = BLACK;
	pcgem_state.current_x = 0;
	pcgem_state.current_y = 0;
	pcgem_state.line_style = SOLID_LINE;
	pcgem_state.line_pattern = 0xFFFF;
	pcgem_state.fill_style = SOLID_FILL;
	pcgem_state.fill_color = WHITE;
	pcgem_state.text_justify_h = LEFT_TEXT;
	pcgem_state.text_justify_v = TOP_TEXT;
	pcgem_state.initialized = 1;

	/* Set initial colors */
	vsf_color(pcgem_state.vdi_handle, ega_vdi_colors[pcgem_state.current_color]);
	vsl_color(pcgem_state.vdi_handle, ega_vdi_colors[pcgem_state.current_color]);

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!pcgem_state.initialized)
		return;

	v_clsvwk(pcgem_state.vdi_handle);
	appl_exit();
	memset(&pcgem_state, 0, sizeof(pcgem_state));
}

int getmaxx(void) { return pcgem_state.width - 1; }
int getmaxy(void) { return pcgem_state.height - 1; }

void setcolor(int color) {
	pcgem_state.current_color = color & 0x0F;
	vsl_color(pcgem_state.vdi_handle, ega_vdi_colors[pcgem_state.current_color]);
	vsm_color(pcgem_state.vdi_handle, ega_vdi_colors[pcgem_state.current_color]);
}

int getcolor(void) { return pcgem_state.current_color; }
void setbkcolor(int color) { pcgem_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return pcgem_state.current_bkcolor; }

void cleardevice(void) {
	v_clrwk(pcgem_state.vdi_handle);
}

void putpixel(int x, int y, int color) {
	int pxy[2];
	pxy[0] = x;
	pxy[1] = y;
	vsm_color(pcgem_state.vdi_handle, ega_vdi_colors[color & 0x0F]);
	v_pmarker(pcgem_state.vdi_handle, 1, pxy);
}

unsigned int getpixel(int x, int y) {
	/* VDI doesn't provide pixel reading, return 0 */
	return 0;
}

void moveto(int x, int y) {
	pcgem_state.current_x = x;
	pcgem_state.current_y = y;
}

void moverel(int dx, int dy) {
	pcgem_state.current_x += dx;
	pcgem_state.current_y += dy;
}

int getx(void) { return pcgem_state.current_x; }
int gety(void) { return pcgem_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int pxy[4];
	pxy[0] = x1;
	pxy[1] = y1;
	pxy[2] = x2;
	pxy[3] = y2;
	v_pline(pcgem_state.vdi_handle, 2, pxy);
}

void lineto(int x, int y) {
	line(pcgem_state.current_x, pcgem_state.current_y, x, y);
	pcgem_state.current_x = x;
	pcgem_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = pcgem_state.current_x + dx;
	int y2 = pcgem_state.current_y + dy;
	line(pcgem_state.current_x, pcgem_state.current_y, x2, y2);
	pcgem_state.current_x = x2;
	pcgem_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	int pxy[4];
	pxy[0] = left;
	pxy[1] = top;
	pxy[2] = right;
	pxy[3] = bottom;
	v_bar(pcgem_state.vdi_handle, pxy);
}

void circle(int x, int y, int radius) {
	v_circle(pcgem_state.vdi_handle, x, y, radius);
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	int vdi_style;

	pcgem_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		vdi_style = 1;
		pcgem_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		vdi_style = 3;
		pcgem_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		vdi_style = 4;
		pcgem_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		vdi_style = 2;
		pcgem_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		vdi_style = 7;
		pcgem_state.line_pattern = pattern;
		break;
	default:
		vdi_style = 1;
		pcgem_state.line_pattern = 0xFFFF;
	}

	vsl_type(pcgem_state.vdi_handle, vdi_style);
	if (thickness > 0)
		vsl_width(pcgem_state.vdi_handle, thickness);
}

void setfillstyle(int pattern, int color) {
	int vdi_pattern;

	pcgem_state.fill_style = pattern;
	pcgem_state.fill_color = color;

	vsf_color(pcgem_state.vdi_handle, ega_vdi_colors[color & 0x0F]);

	switch (pattern) {
	case EMPTY_FILL: vdi_pattern = 0; break;
	case SOLID_FILL: vdi_pattern = 1; break;
	case LINE_FILL: vdi_pattern = 2; break;
	case LTSLASH_FILL: vdi_pattern = 3; break;
	case SLASH_FILL: vdi_pattern = 4; break;
	case BKSLASH_FILL: vdi_pattern = 5; break;
	case LTBKSLASH_FILL: vdi_pattern = 6; break;
	case HATCH_FILL: vdi_pattern = 7; break;
	case XHATCH_FILL: vdi_pattern = 8; break;
	case INTERLEAVE_FILL: vdi_pattern = 9; break;
	case WIDE_DOT_FILL: vdi_pattern = 10; break;
	case CLOSE_DOT_FILL: vdi_pattern = 11; break;
	default: vdi_pattern = 1;
	}

	vsf_interior(pcgem_state.vdi_handle, 2);  /* Pattern fill */
	vsf_style(pcgem_state.vdi_handle, vdi_pattern);
}

void bar(int left, int top, int right, int bottom) {
	int pxy[4];
	pxy[0] = left;
	pxy[1] = top;
	pxy[2] = right;
	pxy[3] = bottom;
	vsf_color(pcgem_state.vdi_handle, ega_vdi_colors[pcgem_state.fill_color & 0x0F]);
	v_bar(pcgem_state.vdi_handle, pxy);
}

void settextjustify(int horiz, int vert) {
	pcgem_state.text_justify_h = horiz;
	pcgem_state.text_justify_v = vert;
}

int textheight(const char *textstring) {
	int char_width, char_height, cell_width, cell_height;
	vqt_attributes(pcgem_state.vdi_handle, &char_width, &char_height, &cell_width, &cell_height);
	return char_height;
}

int textwidth(const char *textstring) {
	return strlen(textstring) * 8;  /* Approximate */
}

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
	return pcgem_state.initialized ? grOk : grNoInitGraph;
}
