/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Microsoft Graphics Library
 *
 * Provides BGI compatibility on top of Microsoft _graph.h functions
 */

#include "../graphics.h"
#include "graph_compat.h"
#include <stdlib.h>
#include <string.h>
#include <dos.h>

/* Internal state */
static struct {
	int initialized;
	int current_color;
	int current_bkcolor;
	struct _xycoord current_pos;
	int line_style;
	int fill_style;
	int fill_color;
	struct _videoconfig vconfig;
	int text_justify_h;
	int text_justify_v;
} msgraph_state;

/* BGI to Microsoft Graphics mode mapping */
static short bgi_to_ms_mode(int driver, int mode)
{
	/* Map BGI drivers and modes to Microsoft modes */
	switch (driver) {
	case CGA:
		if (mode == CGAC0 || mode == CGAC1 || mode == CGAC2 || mode == CGAC3)
			return _MRES4COLOR;
		return _HRESBW;

	case MCGA:
		if (mode == MCGAC0 || mode == MCGAC1)
			return _MRES16COLOR;
		return _MRES256COLOR;

	case EGA:
	case EGA64:
		if (mode == EGALO || mode == EGAHI)
			return _ERESCOLOR;
		return _HRES16COLOR;

	case VGA:
		if (mode == VGAMED)
			return _MRES256COLOR;
		return _VRES16COLOR;

	case HERCULES:
		return _HERCMONO;

	case ATT400:
		return _MRES16COLOR;

	case PC3270:
		return _TEXTC80;

	default:
		return _VRES16COLOR; /* Default to VGA 640x480x16 */
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	short ms_mode;

	if (*driver == DETECT) {
		/* Auto-detect best available mode */
		*driver = VGA;
		*mode = VGAHI;
	}

	ms_mode = bgi_to_ms_mode(*driver, *mode);

	if (_setvideomode(ms_mode) == 0) {
		/* Mode setting failed */
		*driver = grNotDetected;
		return;
	}

	/* Get video configuration */
	_getvideoconfig(&msgraph_state.vconfig);

	/* Initialize state */
	msgraph_state.initialized = 1;
	msgraph_state.current_color = _WHITE;
	msgraph_state.current_bkcolor = _BLACK;
	msgraph_state.line_style = SOLID_LINE;
	msgraph_state.fill_style = SOLID_FILL;
	msgraph_state.fill_color = _WHITE;
	msgraph_state.text_justify_h = LEFT_TEXT;
	msgraph_state.text_justify_v = TOP_TEXT;

	_setcolor(_WHITE);
	_setbkcolor(_BLACK);
	_clearscreen(_GCLEARSCREEN);

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!msgraph_state.initialized)
		return;

	_setvideomode(_DEFAULTMODE); /* Return to text mode */
	msgraph_state.initialized = 0;
}

int
getmaxx(void)
{
	return msgraph_state.vconfig.numxpixels - 1;
}

int
getmaxy(void)
{
	return msgraph_state.vconfig.numypixels - 1;
}

void
setcolor(int color)
{
	msgraph_state.current_color = color;
	_setcolor((short)color);
}

int
getcolor(void)
{
	return msgraph_state.current_color;
}

void
setbkcolor(int color)
{
	msgraph_state.current_bkcolor = color;
	_setbkcolor((long)color);
}

int
getbkcolor(void)
{
	return msgraph_state.current_bkcolor;
}

void
cleardevice(void)
{
	_clearscreen(_GCLEARSCREEN);
}

void
moveto(int x, int y)
{
	_moveto((short)x, (short)y);
	msgraph_state.current_pos.xcoord = x;
	msgraph_state.current_pos.ycoord = y;
}

void
moverel(int dx, int dy)
{
	struct _xycoord pos = _getcurrentposition();
	_moveto(pos.xcoord + dx, pos.ycoord + dy);
}

int
getx(void)
{
	struct _xycoord pos = _getcurrentposition();
	return pos.xcoord;
}

int
gety(void)
{
	struct _xycoord pos = _getcurrentposition();
	return pos.ycoord;
}

void
lineto(int x, int y)
{
	_lineto((short)x, (short)y);
}

void
linerel(int dx, int dy)
{
	struct _xycoord pos = _getcurrentposition();
	_lineto(pos.xcoord + dx, pos.ycoord + dy);
}

void
line(int x1, int y1, int x2, int y2)
{
	_moveto((short)x1, (short)y1);
	_lineto((short)x2, (short)y2);
}

void
rectangle(int left, int top, int right, int bottom)
{
	_rectangle(_GBORDER, (short)left, (short)top, (short)right, (short)bottom);
}

void
bar(int left, int top, int right, int bottom)
{
	short old_color = _getcolor();
	_setcolor((short)msgraph_state.fill_color);
	_rectangle(_GFILLINTERIOR, (short)left, (short)top, (short)right, (short)bottom);
	_setcolor(old_color);
}

void
bar3d(int left, int top, int right, int bottom, int depth, int topflag)
{
	int d = depth;

	/* Draw main bar */
	bar(left, top, right, bottom);
	rectangle(left, top, right, bottom);

	/* Draw 3D effect */
	if (topflag) {
		/* Top face */
		line(left, top, left + d, top - d);
		line(right, top, right + d, top - d);
		line(left + d, top - d, right + d, top - d);
		line(right + d, top - d, right + d, bottom - d);
	}

	/* Right face */
	line(right, top, right + d, top - d);
	line(right, bottom, right + d, bottom - d);
	line(right + d, top - d, right + d, bottom - d);
}

void
circle(int x, int y, int radius)
{
	_ellipse(_GBORDER, (short)(x - radius), (short)(y - radius),
	         (short)(x + radius), (short)(y + radius));
}

void
ellipse(int x, int y, int stangle, int endangle, int xradius, int yradius)
{
	/* Microsoft Graphics doesn't have arc angles, use full ellipse */
	_ellipse(_GBORDER, (short)(x - xradius), (short)(y - yradius),
	         (short)(x + xradius), (short)(y + yradius));
}

void
fillellipse(int x, int y, int xradius, int yradius)
{
	short old_color = _getcolor();
	_setcolor((short)msgraph_state.fill_color);
	_ellipse(_GFILLINTERIOR, (short)(x - xradius), (short)(y - yradius),
	         (short)(x + xradius), (short)(y + yradius));
	_setcolor(old_color);
}

void
arc(int x, int y, int stangle, int endangle, int radius)
{
	/* Simplified - full circle for now */
	circle(x, y, radius);
}

void
putpixel(int x, int y, int color)
{
	short old_color = _getcolor();
	_setcolor((short)color);
	_setpixel((short)x, (short)y);
	_setcolor(old_color);
}

unsigned int
getpixel(int x, int y)
{
	return (unsigned int)_getpixel((short)x, (short)y);
}

void
floodfill(int x, int y, int border)
{
	_floodfill((short)x, (short)y, (short)border);
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	unsigned short mask;

	msgraph_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		mask = 0xFFFF;
		break;
	case DOTTED_LINE:
		mask = 0xCCCC;
		break;
	case CENTER_LINE:
		mask = 0xF8F8;
		break;
	case DASHED_LINE:
		mask = 0xF0F0;
		break;
	case USERBIT_LINE:
		mask = pattern;
		break;
	default:
		mask = 0xFFFF;
	}

	_setlinestyle(mask);
}

void
setfillstyle(int pattern, int color)
{
	msgraph_state.fill_style = pattern;
	msgraph_state.fill_color = color;
}

void
outtextxy(int x, int y, const char *textstring)
{
	_moveto((short)x, (short)y);
	_outtext((char *)textstring);
}

void
outtext(const char *textstring)
{
	_outtext((char *)textstring);
}

void
settextjustify(int horiz, int vert)
{
	msgraph_state.text_justify_h = horiz;
	msgraph_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	/* Assume 8x8 font for simplicity */
	return 8;
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
	case grNoScanMem:
		return "Insufficient memory to scan";
	case grNoFloodMem:
		return "Insufficient memory for flood fill";
	case grFontNotFound:
		return "Font file not found";
	case grNoFontMem:
		return "Insufficient memory to load font";
	case grInvalidMode:
		return "Invalid graphics mode";
	default:
		return "Unknown error";
	}
}

int
graphresult(void)
{
	return msgraph_state.initialized ? grOk : grNoInitGraph;
}
