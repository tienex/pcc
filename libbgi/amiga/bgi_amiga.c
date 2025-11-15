/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Amiga using Intuition and Graphics Library
 *
 * Provides BGI graphics on AmigaOS (68k, PowerPC)
 * Works on: AmigaOS 1.x, 2.x, 3.x, 4.x (OS4)
 * Requires: graphics.library, intuition.library
 */

#include "../graphics.h"
#include <exec/types.h>
#include <exec/memory.h>
#include <graphics/gfx.h>
#include <graphics/gfxbase.h>
#include <graphics/rastport.h>
#include <graphics/view.h>
#include <intuition/intuition.h>
#include <intuition/screens.h>
#include <proto/exec.h>
#include <proto/graphics.h>
#include <proto/intuition.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	struct Screen *screen;
	struct Window *window;
	struct RastPort *rastport;
	int width;
	int height;
	int depth;
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
	int initialized;
} amiga_state;

/* EGA color palette (RGB4) */
static const UWORD ega_palette[16] = {
	0x0000,	/* BLACK */
	0x000A,	/* BLUE */
	0x00A0,	/* GREEN */
	0x00AA,	/* CYAN */
	0x0A00,	/* RED */
	0x0A0A,	/* MAGENTA */
	0x0A50,	/* BROWN */
	0x0AAA,	/* LIGHTGRAY */
	0x0555,	/* DARKGRAY */
	0x055F,	/* LIGHTBLUE */
	0x05F5,	/* LIGHTGREEN */
	0x05FF,	/* LIGHTCYAN */
	0x0F55,	/* LIGHTRED */
	0x0F5F,	/* LIGHTMAGENTA */
	0x0FF5,	/* YELLOW */
	0x0FFF	/* WHITE */
};

void
initgraph(int *driver, int *mode, const char *path)
{
	struct NewScreen new_screen;
	struct NewWindow new_window;
	int width = 640;
	int height = 480;
	int depth = 4;	/* 16 colors */
	int i;

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGAHI;
	}

	/* Determine resolution based on mode */
	switch (*mode) {
	case VGALO:
		width = 640;
		height = 200;
		break;
	case VGAMED:
		width = 640;
		height = 350;
		break;
	case VGAHI:
		width = 640;
		height = 480;
		break;
	case SVGA_800_600:
		width = 800;
		height = 600;
		break;
	case SVGA_1024_768:
		width = 1024;
		height = 768;
		break;
	default:
		width = 640;
		height = 480;
	}

	/* Open screen */
	memset(&new_screen, 0, sizeof(new_screen));
	new_screen.LeftEdge = 0;
	new_screen.TopEdge = 0;
	new_screen.Width = width;
	new_screen.Height = height;
	new_screen.Depth = depth;
	new_screen.DetailPen = 0;
	new_screen.BlockPen = 1;
	new_screen.ViewModes = HIRES;
	new_screen.Type = CUSTOMSCREEN;
	new_screen.Font = NULL;
	new_screen.DefaultTitle = (UBYTE *)"BGI Graphics";
	new_screen.Gadgets = NULL;
	new_screen.CustomBitMap = NULL;

	amiga_state.screen = OpenScreen(&new_screen);
	if (!amiga_state.screen) {
		*driver = grNotDetected;
		return;
	}

	/* Set up EGA palette */
	for (i = 0; i < 16; i++) {
		SetRGB4(&amiga_state.screen->ViewPort, i,
		        (ega_palette[i] >> 8) & 0x0F,
		        (ega_palette[i] >> 4) & 0x0F,
		        ega_palette[i] & 0x0F);
	}

	/* Open window */
	memset(&new_window, 0, sizeof(new_window));
	new_window.LeftEdge = 0;
	new_window.TopEdge = 0;
	new_window.Width = width;
	new_window.Height = height;
	new_window.DetailPen = 0;
	new_window.BlockPen = 1;
	new_window.Title = (UBYTE *)"BGI Graphics";
	new_window.Flags = WFLG_ACTIVATE | WFLG_BACKDROP | WFLG_BORDERLESS;
	new_window.IDCMPFlags = 0;
	new_window.Type = CUSTOMSCREEN;
	new_window.FirstGadget = NULL;
	new_window.CheckMark = NULL;
	new_window.Screen = amiga_state.screen;
	new_window.BitMap = NULL;
	new_window.MinWidth = width;
	new_window.MinHeight = height;
	new_window.MaxWidth = width;
	new_window.MaxHeight = height;

	amiga_state.window = OpenWindow(&new_window);
	if (!amiga_state.window) {
		CloseScreen(amiga_state.screen);
		*driver = grNotDetected;
		return;
	}

	amiga_state.rastport = amiga_state.window->RPort;
	amiga_state.width = width;
	amiga_state.height = height;
	amiga_state.depth = depth;
	amiga_state.current_color = WHITE;
	amiga_state.current_bkcolor = BLACK;
	amiga_state.current_x = 0;
	amiga_state.current_y = 0;
	amiga_state.line_style = SOLID_LINE;
	amiga_state.line_pattern = 0xFFFF;
	amiga_state.fill_style = SOLID_FILL;
	amiga_state.fill_color = WHITE;
	amiga_state.text_justify_h = LEFT_TEXT;
	amiga_state.text_justify_v = TOP_TEXT;
	amiga_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!amiga_state.initialized)
		return;

	if (amiga_state.window)
		CloseWindow(amiga_state.window);

	if (amiga_state.screen)
		CloseScreen(amiga_state.screen);

	memset(&amiga_state, 0, sizeof(amiga_state));
}

int
getmaxx(void)
{
	return amiga_state.width - 1;
}

int
getmaxy(void)
{
	return amiga_state.height - 1;
}

void
setcolor(int color)
{
	amiga_state.current_color = color & 0x0F;
	SetAPen(amiga_state.rastport, amiga_state.current_color);
}

int
getcolor(void)
{
	return amiga_state.current_color;
}

void
setbkcolor(int color)
{
	amiga_state.current_bkcolor = color & 0x0F;
	SetBPen(amiga_state.rastport, amiga_state.current_bkcolor);
}

int
getbkcolor(void)
{
	return amiga_state.current_bkcolor;
}

void
cleardevice(void)
{
	SetAPen(amiga_state.rastport, amiga_state.current_bkcolor);
	RectFill(amiga_state.rastport, 0, 0, amiga_state.width - 1, amiga_state.height - 1);
	SetAPen(amiga_state.rastport, amiga_state.current_color);
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < amiga_state.width && y >= 0 && y < amiga_state.height) {
		SetAPen(amiga_state.rastport, color & 0x0F);
		WritePixel(amiga_state.rastport, x, y);
		SetAPen(amiga_state.rastport, amiga_state.current_color);
	}
}

unsigned int
getpixel(int x, int y)
{
	LONG color;

	if (x < 0 || x >= amiga_state.width || y < 0 || y >= amiga_state.height)
		return 0;

	color = ReadPixel(amiga_state.rastport, x, y);
	return (unsigned int)color;
}

void
moveto(int x, int y)
{
	amiga_state.current_x = x;
	amiga_state.current_y = y;
	Move(amiga_state.rastport, x, y);
}

void
moverel(int dx, int dy)
{
	amiga_state.current_x += dx;
	amiga_state.current_y += dy;
	Move(amiga_state.rastport, amiga_state.current_x, amiga_state.current_y);
}

int
getx(void)
{
	return amiga_state.current_x;
}

int
gety(void)
{
	return amiga_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	if (amiga_state.line_style == SOLID_LINE) {
		Move(amiga_state.rastport, x1, y1);
		Draw(amiga_state.rastport, x2, y2);
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
			if (amiga_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, amiga_state.current_color);

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
	line(amiga_state.current_x, amiga_state.current_y, x, y);
	amiga_state.current_x = x;
	amiga_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = amiga_state.current_x + dx;
	int y2 = amiga_state.current_y + dy;
	line(amiga_state.current_x, amiga_state.current_y, x2, y2);
	amiga_state.current_x = x2;
	amiga_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	Move(amiga_state.rastport, left, top);
	Draw(amiga_state.rastport, right, top);
	Draw(amiga_state.rastport, right, bottom);
	Draw(amiga_state.rastport, left, bottom);
	Draw(amiga_state.rastport, left, top);
}

void
circle(int x, int y, int radius)
{
	DrawCircle(amiga_state.rastport, x, y, radius);
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	amiga_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		amiga_state.line_pattern = 0xFFFF;
		SetDrMd(amiga_state.rastport, JAM1);
		break;
	case DOTTED_LINE:
		amiga_state.line_pattern = 0xCCCC;
		SetDrPt(amiga_state.rastport, 0xCCCC);
		break;
	case CENTER_LINE:
		amiga_state.line_pattern = 0xF8F8;
		SetDrPt(amiga_state.rastport, 0xF8F8);
		break;
	case DASHED_LINE:
		amiga_state.line_pattern = 0xF0F0;
		SetDrPt(amiga_state.rastport, 0xF0F0);
		break;
	case USERBIT_LINE:
		amiga_state.line_pattern = pattern;
		SetDrPt(amiga_state.rastport, pattern);
		break;
	default:
		amiga_state.line_pattern = 0xFFFF;
		SetDrMd(amiga_state.rastport, JAM1);
	}
}

void
setfillstyle(int pattern, int color)
{
	amiga_state.fill_style = pattern;
	amiga_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	SetAPen(amiga_state.rastport, amiga_state.fill_color & 0x0F);
	RectFill(amiga_state.rastport, left, top, right, bottom);
	SetAPen(amiga_state.rastport, amiga_state.current_color);
}

void
settextjustify(int horiz, int vert)
{
	amiga_state.text_justify_h = horiz;
	amiga_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return amiga_state.rastport->TxHeight;
}

int
textwidth(const char *textstring)
{
	return TextLength(amiga_state.rastport, (STRPTR)textstring, strlen(textstring));
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
	return amiga_state.initialized ? grOk : grNoInitGraph;
}
