/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Symbian OS
 *
 * Symbian OS graphics
 * Works on: Symbian S60, UIQ
 * Features: CFbsBitmap, CWindowGc
 */

#include "../graphics.h"
#include <e32std.h>
#include <fbs.h>
#include <w32std.h>

static struct {
	CFbsBitmap *bitmap;
	CFbsBitmapDevice *device;
	CFbsBitGc *gc;
	int width, height;
	int initialized;
} symbian_state;

extern "C" void initgraph(int *driver, int *mode, const char *path) {
	symbian_state.bitmap = new CFbsBitmap();
	symbian_state.bitmap->Create(TSize(640, 480), EColor16M);
	symbian_state.device = CFbsBitmapDevice::NewL(symbian_state.bitmap);
	symbian_state.device->CreateContext(symbian_state.gc);
	symbian_state.width = 640;
	symbian_state.height = 480;
	symbian_state.initialized = 1;
	*driver = 0; *mode = 0;
}

extern "C" void closegraph(void) {
	delete symbian_state.gc;
	delete symbian_state.device;
	delete symbian_state.bitmap;
	symbian_state.initialized = 0;
}

extern "C" int getmaxx(void) { return symbian_state.width - 1; }

extern "C" void line(int x1, int y1, int x2, int y2) {
	symbian_state.gc->DrawLine(TPoint(x1, y1), TPoint(x2, y2));
}

extern "C" int graphresult(void) { return symbian_state.initialized ? grOk : grNoInitGraph; }
