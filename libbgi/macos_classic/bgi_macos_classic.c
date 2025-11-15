/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Mac OS Classic (7-9) using QuickDraw
 *
 * Provides BGI graphics on classic Macintosh systems
 * Works on: Mac OS 7, 8, 9 (68k and PowerPC)
 * Requires: QuickDraw, Toolbox
 */

#include "../graphics.h"
#include <MacTypes.h>
#include <Quickdraw.h>
#include <Windows.h>
#include <Events.h>
#include <Memory.h>
#include <OSUtils.h>
#include <ToolUtils.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	WindowPtr window;
	GrafPtr grafport;
	PixMapHandle pixmap;
	int width;
	int height;
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
	RGBColor colors[16];	/* EGA palette */
	int initialized;
} mac_state;

/* EGA color palette */
static const RGBColor ega_palette[16] = {
	{0x0000, 0x0000, 0x0000},	/* BLACK */
	{0x0000, 0x0000, 0xAAAA},	/* BLUE */
	{0x0000, 0xAAAA, 0x0000},	/* GREEN */
	{0x0000, 0xAAAA, 0xAAAA},	/* CYAN */
	{0xAAAA, 0x0000, 0x0000},	/* RED */
	{0xAAAA, 0x0000, 0xAAAA},	/* MAGENTA */
	{0xAAAA, 0x5555, 0x0000},	/* BROWN */
	{0xAAAA, 0xAAAA, 0xAAAA},	/* LIGHTGRAY */
	{0x5555, 0x5555, 0x5555},	/* DARKGRAY */
	{0x5555, 0x5555, 0xFFFF},	/* LIGHTBLUE */
	{0x5555, 0xFFFF, 0x5555},	/* LIGHTGREEN */
	{0x5555, 0xFFFF, 0xFFFF},	/* LIGHTCYAN */
	{0xFFFF, 0x5555, 0x5555},	/* LIGHTRED */
	{0xFFFF, 0x5555, 0xFFFF},	/* LIGHTMAGENTA */
	{0xFFFF, 0xFFFF, 0x5555},	/* YELLOW */
	{0xFFFF, 0xFFFF, 0xFFFF}	/* WHITE */
};

void
initgraph(int *driver, int *mode, const char *path)
{
	Rect bounds;
	int width = 640;
	int height = 480;
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

	/* Initialize Toolbox */
	InitGraf(&qd.thePort);
	InitFonts();
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(nil);
	InitCursor();

	/* Create window */
	SetRect(&bounds, 0, 0, width, height);
	OffsetRect(&bounds, 50, 50);	/* Position on screen */

	mac_state.window = NewCWindow(nil, &bounds, "\pBGI Graphics", true,
	                               documentProc, (WindowPtr)-1L, true, 0);

	if (!mac_state.window) {
		*driver = grNotDetected;
		return;
	}

	SetPort(mac_state.window);
	mac_state.grafport = mac_state.window;

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		mac_state.colors[i] = ega_palette[i];

	mac_state.width = width;
	mac_state.height = height;
	mac_state.current_color = WHITE;
	mac_state.current_bkcolor = BLACK;
	mac_state.current_x = 0;
	mac_state.current_y = 0;
	mac_state.line_style = SOLID_LINE;
	mac_state.line_pattern = 0xFFFF;
	mac_state.fill_style = SOLID_FILL;
	mac_state.fill_color = WHITE;
	mac_state.text_justify_h = LEFT_TEXT;
	mac_state.text_justify_v = TOP_TEXT;
	mac_state.initialized = 1;

	/* Set drawing colors */
	RGBForeColor(&mac_state.colors[WHITE]);
	RGBBackColor(&mac_state.colors[BLACK]);

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!mac_state.initialized)
		return;

	if (mac_state.window)
		DisposeWindow(mac_state.window);

	memset(&mac_state, 0, sizeof(mac_state));
}

int
getmaxx(void)
{
	return mac_state.width - 1;
}

int
getmaxy(void)
{
	return mac_state.height - 1;
}

void
setcolor(int color)
{
	mac_state.current_color = color & 0x0F;
	RGBForeColor(&mac_state.colors[mac_state.current_color]);
}

int
getcolor(void)
{
	return mac_state.current_color;
}

void
setbkcolor(int color)
{
	mac_state.current_bkcolor = color & 0x0F;
	RGBBackColor(&mac_state.colors[mac_state.current_bkcolor]);
}

int
getbkcolor(void)
{
	return mac_state.current_bkcolor;
}

void
cleardevice(void)
{
	Rect r;

	RGBForeColor(&mac_state.colors[mac_state.current_bkcolor]);
	SetRect(&r, 0, 0, mac_state.width, mac_state.height);
	PaintRect(&r);
	RGBForeColor(&mac_state.colors[mac_state.current_color]);
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < mac_state.width && y >= 0 && y < mac_state.height) {
		RGBForeColor(&mac_state.colors[color & 0x0F]);
		MoveTo(x, y);
		LineTo(x, y);
		RGBForeColor(&mac_state.colors[mac_state.current_color]);
	}
}

unsigned int
getpixel(int x, int y)
{
	RGBColor pixel;
	int i;

	if (x < 0 || x >= mac_state.width || y < 0 || y >= mac_state.height)
		return 0;

	GetCPixel(x, y, &pixel);

	/* Find matching EGA color */
	for (i = 0; i < 16; i++) {
		if (pixel.red == mac_state.colors[i].red &&
		    pixel.green == mac_state.colors[i].green &&
		    pixel.blue == mac_state.colors[i].blue)
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	mac_state.current_x = x;
	mac_state.current_y = y;
	MoveTo(x, y);
}

void
moverel(int dx, int dy)
{
	mac_state.current_x += dx;
	mac_state.current_y += dy;
	Move(dx, dy);
}

int
getx(void)
{
	return mac_state.current_x;
}

int
gety(void)
{
	return mac_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	if (mac_state.line_style == SOLID_LINE) {
		MoveTo(x1, y1);
		LineTo(x2, y2);
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
			if (mac_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, mac_state.current_color);

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
	line(mac_state.current_x, mac_state.current_y, x, y);
	mac_state.current_x = x;
	mac_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = mac_state.current_x + dx;
	int y2 = mac_state.current_y + dy;
	line(mac_state.current_x, mac_state.current_y, x2, y2);
	mac_state.current_x = x2;
	mac_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	Rect r;

	SetRect(&r, left, top, right + 1, bottom + 1);
	FrameRect(&r);
}

void
circle(int x, int y, int radius)
{
	Rect r;

	SetRect(&r, x - radius, y - radius, x + radius, y + radius);
	FrameOval(&r);
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	Pattern qd_pattern;
	int i;

	mac_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		mac_state.line_pattern = 0xFFFF;
		PenPat(&qd.black);
		break;
	case DOTTED_LINE:
		mac_state.line_pattern = 0xCCCC;
		/* Create dotted pattern */
		for (i = 0; i < 8; i++)
			qd_pattern[i] = (i & 1) ? 0xCC : 0xCC;
		PenPat(&qd_pattern);
		break;
	case CENTER_LINE:
		mac_state.line_pattern = 0xF8F8;
		for (i = 0; i < 8; i++)
			qd_pattern[i] = 0xF8;
		PenPat(&qd_pattern);
		break;
	case DASHED_LINE:
		mac_state.line_pattern = 0xF0F0;
		for (i = 0; i < 8; i++)
			qd_pattern[i] = 0xF0;
		PenPat(&qd_pattern);
		break;
	case USERBIT_LINE:
		mac_state.line_pattern = pattern;
		for (i = 0; i < 8; i++)
			qd_pattern[i] = (pattern >> 8) & 0xFF;
		PenPat(&qd_pattern);
		break;
	default:
		mac_state.line_pattern = 0xFFFF;
		PenPat(&qd.black);
	}

	if (thickness > 0)
		PenSize(thickness, thickness);
}

void
setfillstyle(int pattern, int color)
{
	mac_state.fill_style = pattern;
	mac_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	Rect r;

	SetRect(&r, left, top, right + 1, bottom + 1);
	RGBForeColor(&mac_state.colors[mac_state.fill_color & 0x0F]);
	PaintRect(&r);
	RGBForeColor(&mac_state.colors[mac_state.current_color]);
}

void
settextjustify(int horiz, int vert)
{
	mac_state.text_justify_h = horiz;
	mac_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	FontInfo info;

	GetFontInfo(&info);
	return info.ascent + info.descent;
}

int
textwidth(const char *textstring)
{
	return TextWidth(textstring, 0, strlen(textstring));
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
	return mac_state.initialized ? grOk : grNoInitGraph;
}
