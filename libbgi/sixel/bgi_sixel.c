/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using SIXEL graphics (DEC VT340)
 *
 * DEC SIXEL raster graphics protocol
 * Works on: DEC VT340, xterm +sixel, mintty, mlterm
 * Features: 16-color palette, raster graphics
 * Output: SIXEL escape sequences to stdout
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIXEL_INTRODUCER "\033Pq"
#define SIXEL_TERMINATOR "\033\\"

static struct {
	unsigned char *framebuffer;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	int initialized;
} sixel_state;

/* EGA palette in RGB (0-100 range for SIXEL) */
static const int ega_sixel_rgb[16][3] = {
	{0, 0, 0},       /* BLACK */
	{0, 0, 67},      /* BLUE */
	{0, 67, 0},      /* GREEN */
	{0, 67, 67},     /* CYAN */
	{67, 0, 0},      /* RED */
	{67, 0, 67},     /* MAGENTA */
	{67, 33, 0},     /* BROWN */
	{67, 67, 67},    /* LIGHTGRAY */
	{33, 33, 33},    /* DARKGRAY */
	{33, 33, 100},   /* LIGHTBLUE */
	{33, 100, 33},   /* LIGHTGREEN */
	{33, 100, 100},  /* LIGHTCYAN */
	{100, 33, 33},   /* LIGHTRED */
	{100, 33, 100},  /* LIGHTMAGENTA */
	{100, 100, 33},  /* YELLOW */
	{100, 100, 100}  /* WHITE */
};

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
	return sixel_state.initialized ? grOk : grNoInitGraph;
}
