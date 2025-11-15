/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for RISC OS
 *
 * Acorn RISC OS graphics
 * Works on: RISC OS 3.x, 4.x, 5.x
 * Features: VDU graphics, WIMP
 */

#include "../graphics.h"
#include <kernel.h>
#include <swis.h>

static struct {
	int width, height;
	int current_color;
	int initialized;
} riscos_state;

void initgraph(int *driver, int *mode, const char *path) {
	_kernel_oswrch(22);  /* MODE */
	_kernel_oswrch(13);  /* 640x480 */
	riscos_state.width = 640;
	riscos_state.height = 480;
	riscos_state.current_color = 15;
	riscos_state.initialized = 1;
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	_kernel_oswrch(22);
	_kernel_oswrch(7);  /* Text mode */
	riscos_state.initialized = 0;
}

int getmaxx(void) { return riscos_state.width - 1; }

void line(int x1, int y1, int x2, int y2) {
	_kernel_oswrch(25);  /* PLOT */
	_kernel_oswrch(4);   /* Move */
	_kernel_oswrch(x1 & 0xFF);
	_kernel_oswrch((x1 >> 8) & 0xFF);
	_kernel_oswrch(y1 & 0xFF);
	_kernel_oswrch((y1 >> 8) & 0xFF);
	_kernel_oswrch(25);
	_kernel_oswrch(5);   /* Line */
	_kernel_oswrch(x2 & 0xFF);
	_kernel_oswrch((x2 >> 8) & 0xFF);
	_kernel_oswrch(y2 & 0xFF);
	_kernel_oswrch((y2 >> 8) & 0xFF);
}

int graphresult(void) { return riscos_state.initialized ? grOk : grNoInitGraph; }
