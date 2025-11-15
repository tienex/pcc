/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for PC/GEOS (GeoWorks Ensemble)
 *
 * GeoWorks GEOS graphics
 * Works on: PC/GEOS 1.x, 2.x, Ensemble
 * Features: GEOS graphics kernel
 */

#include "../graphics.h"
#include <geos.h>
#include <graphics.h>

static struct {
	GStateHandle gstate;
	int width, height;
	int current_color;
	int initialized;
} geos_state;

void initgraph(int *driver, int *mode, const char *path) {
	geos_state.gstate = GrCreateState(0);
	if (!geos_state.gstate) { *driver = grNotDetected; return; }

	geos_state.width = 640;
	geos_state.height = 480;
	geos_state.current_color = C_WHITE;
	geos_state.initialized = 1;
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	if (geos_state.gstate) GrDestroyState(geos_state.gstate);
	geos_state.initialized = 0;
}

int getmaxx(void) { return geos_state.width - 1; }
void setcolor(int color) { geos_state.current_color = color; }

void line(int x1, int y1, int x2, int y2) {
	GrDrawLine(geos_state.gstate, x1, y1, x2, y2);
}

int graphresult(void) { return geos_state.initialized ? grOk : grNoInitGraph; }
