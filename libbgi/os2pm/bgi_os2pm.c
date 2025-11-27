/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for OS/2 Presentation Manager (16-bit)
 *
 * OS/2 1.x/2.x graphics using PM
 * Works on: OS/2 1.x, 2.x, Warp
 * Features: GPI graphics, native PM integration
 */

#include "../graphics.h"
#define INCL_GPI
#define INCL_WIN
#include <os2.h>
#include <stdlib.h>
#include <string.h>

static struct {
	HAB hab;
	HPS hps;
	HDC hdc;
	int width, height;
	int current_color, current_bkcolor;
	LONG colors[16];
	int initialized;
} os2_state;

static const LONG ega_rgb[16] = {
	0x00000000, 0x00AA0000, 0x0000AA00, 0x00AAAA00,
	0x000000AA, 0x00AA00AA, 0x0055AA00, 0x00AAAAAA,
	0x00555555, 0x00FF5555, 0x0055FF55, 0x00FFFF55,
	0x005555FF, 0x00FF55FF, 0x0055FFFF, 0x00FFFFFF
};

void initgraph(int *driver, int *mode, const char *path) {
	SIZEL sizl = {0, 0};
	int width = 640, height = 480, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }
	switch (*mode) {
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	}

	os2_state.hab = WinInitialize(0);
	if (!os2_state.hab) { *driver = grNotDetected; return; }

	os2_state.hdc = DevOpenDC(os2_state.hab, OD_MEMORY, "*", 0L, NULL, NULL);
	if (!os2_state.hdc) { WinTerminate(os2_state.hab); *driver = grNotDetected; return; }

	os2_state.hps = GpiCreatePS(os2_state.hab, os2_state.hdc, &sizl,
	                             PU_PELS | GPIT_MICRO | GPIA_ASSOC);
	if (!os2_state.hps) {
		DevCloseDC(os2_state.hdc);
		WinTerminate(os2_state.hab);
		*driver = grNotDetected;
		return;
	}

	for (i = 0; i < 16; i++) os2_state.colors[i] = ega_rgb[i];
	os2_state.width = width;
	os2_state.height = height;
	os2_state.current_color = WHITE;
	os2_state.current_bkcolor = BLACK;
	os2_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	if (!os2_state.initialized) return;
	if (os2_state.hps) GpiDestroyPS(os2_state.hps);
	if (os2_state.hdc) DevCloseDC(os2_state.hdc);
	if (os2_state.hab) WinTerminate(os2_state.hab);
	memset(&os2_state, 0, sizeof(os2_state));
}

int getmaxx(void) { return os2_state.width - 1; }
int getmaxy(void) { return os2_state.height - 1; }
void setcolor(int color) { os2_state.current_color = color & 0x0F; }

void cleardevice(void) {
	RECTL rcl = {0, 0, os2_state.width, os2_state.height};
	GpiSetColor(os2_state.hps, os2_state.colors[os2_state.current_bkcolor]);
	GpiBox(os2_state.hps, DRO_FILL, (PPOINTL)&rcl, 0L, 0L);
}

void line(int x1, int y1, int x2, int y2) {
	POINTL pt;
	GpiSetColor(os2_state.hps, os2_state.colors[os2_state.current_color]);
	pt.x = x1; pt.y = y1;
	GpiMove(os2_state.hps, &pt);
	pt.x = x2; pt.y = y2;
	GpiLine(os2_state.hps, &pt);
}

const char *grapherrormsg(int errorcode) {
	return (errorcode == grOk) ? "No error" : "Error";
}

int graphresult(void) { return os2_state.initialized ? grOk : grNoInitGraph; }
