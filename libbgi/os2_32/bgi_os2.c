/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for OS/2 32-bit using Presentation Manager and DIVE
 *
 * Provides BGI graphics on OS/2
 * Works on: OS/2 2.x, Warp 3/4, eComStation, ArcaOS
 * Requires: Presentation Manager, DIVE library (optional for acceleration)
 */

#include "../graphics.h"
#define INCL_WIN
#define INCL_GPI
#define INCL_DOS
#include <os2.h>
#ifdef USE_DIVE
#include <dive.h>
#include <fourcc.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	HAB hab;
	HMQ hmq;
	HWND hwnd_frame;
	HWND hwnd_client;
	HPS hps;
	HDC hdc;
#ifdef USE_DIVE
	HDIVE hdive;
	PBYTE pbuffer;
	ULONG scan_line_bytes;
	ULONG buffer_number;
#endif
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
	LONG colors[16];	/* EGA palette */
	int initialized;
	int use_dive;
} os2_state;

/* EGA color palette (RGB) */
static const RGB2 ega_palette[16] = {
	{0x00, 0x00, 0x00},	/* BLACK */
	{0x00, 0x00, 0xAA},	/* BLUE */
	{0x00, 0xAA, 0x00},	/* GREEN */
	{0x00, 0xAA, 0xAA},	/* CYAN */
	{0xAA, 0x00, 0x00},	/* RED */
	{0xAA, 0x00, 0xAA},	/* MAGENTA */
	{0xAA, 0x55, 0x00},	/* BROWN */
	{0xAA, 0xAA, 0xAA},	/* LIGHTGRAY */
	{0x55, 0x55, 0x55},	/* DARKGRAY */
	{0x55, 0x55, 0xFF},	/* LIGHTBLUE */
	{0x55, 0xFF, 0x55},	/* LIGHTGREEN */
	{0x55, 0xFF, 0xFF},	/* LIGHTCYAN */
	{0xFF, 0x55, 0x55},	/* LIGHTRED */
	{0xFF, 0x55, 0xFF},	/* LIGHTMAGENTA */
	{0xFF, 0xFF, 0x55},	/* YELLOW */
	{0xFF, 0xFF, 0xFF}	/* WHITE */
};

/* Window procedure */
static MRESULT EXPENTRY
ClientWndProc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
	switch (msg) {
	case WM_PAINT:
		{
			HPS hps;
			RECTL rcl;

			hps = WinBeginPaint(hwnd, NULLHANDLE, &rcl);
#ifdef USE_DIVE
			if (os2_state.use_dive && os2_state.hdive) {
				SETUP_BLITTER setup;
				POINTL ptl[4];

				/* Set up blit */
				setup.ulStructLen = sizeof(SETUP_BLITTER);
				setup.fInvert = FALSE;
				setup.fccSrcColorFormat = FOURCC_BGR3;
				setup.ulSrcWidth = os2_state.width;
				setup.ulSrcHeight = os2_state.height;
				setup.ulSrcPosX = 0;
				setup.ulSrcPosY = 0;
				setup.ulDitherType = 0;
				setup.fccDitherFormat = 0;

				DiveSetupBlitter(os2_state.hdive, &setup);

				/* Blit to screen */
				ptl[0].x = rcl.xLeft;
				ptl[0].y = rcl.yBottom;
				ptl[1].x = rcl.xRight;
				ptl[1].y = rcl.yTop;
				ptl[2].x = 0;
				ptl[2].y = 0;
				ptl[3].x = os2_state.width;
				ptl[3].y = os2_state.height;

				DiveBlitImage(os2_state.hdive, os2_state.buffer_number,
				              DIVE_BUFFER_SCREEN, &ptl[0], &ptl[2], 2);
			}
#endif
			WinEndPaint(hps);
		}
		return 0;

	case WM_CLOSE:
		WinPostMsg(hwnd, WM_QUIT, 0, 0);
		return 0;

	default:
		return WinDefWindowProc(hwnd, msg, mp1, mp2);
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	ULONG frame_flags;
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

	/* Initialize PM */
	os2_state.hab = WinInitialize(0);
	if (!os2_state.hab) {
		*driver = grNotDetected;
		return;
	}

	os2_state.hmq = WinCreateMsgQueue(os2_state.hab, 0);
	if (!os2_state.hmq) {
		WinTerminate(os2_state.hab);
		*driver = grNotDetected;
		return;
	}

	/* Register window class */
	WinRegisterClass(os2_state.hab, "BGIWindow", ClientWndProc,
	                 CS_SIZEREDRAW, 0);

	/* Create frame window */
	frame_flags = FCF_TITLEBAR | FCF_SYSMENU | FCF_SIZEBORDER |
	              FCF_MINMAX | FCF_SHELLPOSITION | FCF_TASKLIST;

	os2_state.hwnd_frame = WinCreateStdWindow(HWND_DESKTOP, 0, &frame_flags,
	                                           "BGIWindow", "BGI Graphics",
	                                           0, NULLHANDLE, 1, &os2_state.hwnd_client);

	if (!os2_state.hwnd_frame) {
		WinDestroyMsgQueue(os2_state.hmq);
		WinTerminate(os2_state.hab);
		*driver = grNotDetected;
		return;
	}

	/* Get device context and presentation space */
	os2_state.hdc = WinOpenWindowDC(os2_state.hwnd_client);
	os2_state.hps = GpiCreatePS(os2_state.hab, os2_state.hdc,
	                             &(SIZEL){width, height},
	                             PU_PELS | GPIA_ASSOC);

	if (!os2_state.hps) {
		WinDestroyWindow(os2_state.hwnd_frame);
		WinDestroyMsgQueue(os2_state.hmq);
		WinTerminate(os2_state.hab);
		*driver = grNotDetected;
		return;
	}

	/* Try to initialize DIVE for hardware acceleration */
	os2_state.use_dive = 0;
#ifdef USE_DIVE
	if (DiveQueryCaps(&caps, DIVE_BUFFER_SCREEN) == DIVE_SUCCESS) {
		if (DiveOpen(&os2_state.hdive, FALSE, NULL) == DIVE_SUCCESS) {
			/* Allocate DIVE buffer */
			os2_state.scan_line_bytes = width * 3;	/* 24-bit RGB */
			DiveAllocImageBuffer(os2_state.hdive, &os2_state.buffer_number,
			                     FOURCC_BGR3, width, height,
			                     os2_state.scan_line_bytes, NULL);

			DiveBeginImageBufferAccess(os2_state.hdive, os2_state.buffer_number,
			                           &os2_state.pbuffer, &os2_state.scan_line_bytes,
			                           NULL);
			os2_state.use_dive = 1;
		}
	}
#endif

	/* Create color table */
	for (i = 0; i < 16; i++) {
		os2_state.colors[i] = GpiCreateLogColorTable(os2_state.hps, 0,
		                                               LCOLF_RGB, i, 1,
		                                               (PLONG)&ega_palette[i]);
	}

	os2_state.width = width;
	os2_state.height = height;
	os2_state.current_color = WHITE;
	os2_state.current_bkcolor = BLACK;
	os2_state.current_x = 0;
	os2_state.current_y = 0;
	os2_state.line_style = SOLID_LINE;
	os2_state.line_pattern = 0xFFFF;
	os2_state.fill_style = SOLID_FILL;
	os2_state.fill_color = WHITE;
	os2_state.text_justify_h = LEFT_TEXT;
	os2_state.text_justify_v = TOP_TEXT;
	os2_state.initialized = 1;

	/* Show window */
	WinSetWindowPos(os2_state.hwnd_frame, HWND_TOP, 100, 100,
	                width, height, SWP_SIZE | SWP_MOVE | SWP_ACTIVATE | SWP_SHOW);

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!os2_state.initialized)
		return;

#ifdef USE_DIVE
	if (os2_state.use_dive) {
		DiveEndImageBufferAccess(os2_state.hdive, os2_state.buffer_number);
		DiveFreeImageBuffer(os2_state.hdive, os2_state.buffer_number);
		DiveClose(os2_state.hdive);
	}
#endif

	if (os2_state.hps)
		GpiDestroyPS(os2_state.hps);

	if (os2_state.hwnd_frame)
		WinDestroyWindow(os2_state.hwnd_frame);

	if (os2_state.hmq)
		WinDestroyMsgQueue(os2_state.hmq);

	if (os2_state.hab)
		WinTerminate(os2_state.hab);

	memset(&os2_state, 0, sizeof(os2_state));
}

int
getmaxx(void)
{
	return os2_state.width - 1;
}

int
getmaxy(void)
{
	return os2_state.height - 1;
}

void
setcolor(int color)
{
	os2_state.current_color = color & 0x0F;
	GpiSetColor(os2_state.hps, os2_state.colors[os2_state.current_color]);
}

int
getcolor(void)
{
	return os2_state.current_color;
}

void
setbkcolor(int color)
{
	os2_state.current_bkcolor = color & 0x0F;
	GpiSetBackColor(os2_state.hps, os2_state.colors[os2_state.current_bkcolor]);
}

int
getbkcolor(void)
{
	return os2_state.current_bkcolor;
}

void
cleardevice(void)
{
	RECTL rcl;

	rcl.xLeft = 0;
	rcl.yBottom = 0;
	rcl.xRight = os2_state.width;
	rcl.yTop = os2_state.height;

	WinFillRect(os2_state.hps, &rcl, os2_state.colors[os2_state.current_bkcolor]);
	WinInvalidateRect(os2_state.hwnd_client, NULL, FALSE);
}

void
putpixel(int x, int y, int color)
{
	POINTL ptl;

	if (x >= 0 && x < os2_state.width && y >= 0 && y < os2_state.height) {
		ptl.x = x;
		ptl.y = os2_state.height - 1 - y;	/* Flip Y coordinate */
		GpiSetColor(os2_state.hps, os2_state.colors[color & 0x0F]);
		GpiSetPel(os2_state.hps, &ptl);
		GpiSetColor(os2_state.hps, os2_state.colors[os2_state.current_color]);
	}
}

unsigned int
getpixel(int x, int y)
{
	POINTL ptl;
	LONG color;
	int i;

	if (x < 0 || x >= os2_state.width || y < 0 || y >= os2_state.height)
		return 0;

	ptl.x = x;
	ptl.y = os2_state.height - 1 - y;
	color = GpiQueryPel(os2_state.hps, &ptl);

	/* Find matching EGA color */
	for (i = 0; i < 16; i++) {
		if (os2_state.colors[i] == color)
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	POINTL ptl;

	os2_state.current_x = x;
	os2_state.current_y = y;

	ptl.x = x;
	ptl.y = os2_state.height - 1 - y;
	GpiMove(os2_state.hps, &ptl);
}

void
moverel(int dx, int dy)
{
	os2_state.current_x += dx;
	os2_state.current_y += dy;
	moveto(os2_state.current_x, os2_state.current_y);
}

int
getx(void)
{
	return os2_state.current_x;
}

int
gety(void)
{
	return os2_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	POINTL aptl[2];

	/* Flip Y coordinates */
	y1 = os2_state.height - 1 - y1;
	y2 = os2_state.height - 1 - y2;

	if (os2_state.line_style == SOLID_LINE) {
		aptl[0].x = x1;
		aptl[0].y = y1;
		aptl[1].x = x2;
		aptl[1].y = y2;

		GpiMove(os2_state.hps, &aptl[0]);
		GpiLine(os2_state.hps, &aptl[1]);
	} else {
		/* Bresenham's line algorithm for styled lines */
		int dx = abs(x2 - x1);
		int dy = abs(y2 - y1);
		int sx = (x1 < x2) ? 1 : -1;
		int sy = (y1 < y2) ? 1 : -1;
		int err = dx - dy;
		int e2;
		int pattern_pos = 0;
		int orig_y1 = os2_state.height - 1 - y1;
		int orig_y2 = os2_state.height - 1 - y2;

		/* Restore for putpixel */
		y1 = orig_y1;
		y2 = orig_y2;

		while (1) {
			if (os2_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, os2_state.current_color);

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

	WinInvalidateRect(os2_state.hwnd_client, NULL, FALSE);
}

void
lineto(int x, int y)
{
	line(os2_state.current_x, os2_state.current_y, x, y);
	os2_state.current_x = x;
	os2_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = os2_state.current_x + dx;
	int y2 = os2_state.current_y + dy;
	line(os2_state.current_x, os2_state.current_y, x2, y2);
	os2_state.current_x = x2;
	os2_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	POINTL aptl[5];

	/* Flip Y coordinates */
	int temp = top;
	top = os2_state.height - 1 - bottom;
	bottom = os2_state.height - 1 - temp;

	aptl[0].x = left;
	aptl[0].y = top;
	aptl[1].x = right;
	aptl[1].y = top;
	aptl[2].x = right;
	aptl[2].y = bottom;
	aptl[3].x = left;
	aptl[3].y = bottom;
	aptl[4].x = left;
	aptl[4].y = top;

	GpiMove(os2_state.hps, &aptl[0]);
	GpiPolyLine(os2_state.hps, 4, &aptl[1]);
	WinInvalidateRect(os2_state.hwnd_client, NULL, FALSE);
}

void
circle(int x, int y, int radius)
{
	POINTL ptl;
	ARCPARAMS arc;

	/* Flip Y coordinate */
	y = os2_state.height - 1 - y;

	ptl.x = x;
	ptl.y = y;

	arc.lP = radius;
	arc.lQ = radius;
	arc.lR = 0;
	arc.lS = 0;

	GpiSetArcParams(os2_state.hps, &arc);
	GpiMove(os2_state.hps, &ptl);
	GpiFullArc(os2_state.hps, DRO_OUTLINE, MAKEFIXED(1, 0));
	WinInvalidateRect(os2_state.hwnd_client, NULL, FALSE);
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	os2_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		os2_state.line_pattern = 0xFFFF;
		GpiSetLineType(os2_state.hps, LINETYPE_SOLID);
		break;
	case DOTTED_LINE:
		os2_state.line_pattern = 0xCCCC;
		GpiSetLineType(os2_state.hps, LINETYPE_DOT);
		break;
	case CENTER_LINE:
		os2_state.line_pattern = 0xF8F8;
		GpiSetLineType(os2_state.hps, LINETYPE_DASHDOT);
		break;
	case DASHED_LINE:
		os2_state.line_pattern = 0xF0F0;
		GpiSetLineType(os2_state.hps, LINETYPE_SHORTDASH);
		break;
	case USERBIT_LINE:
		os2_state.line_pattern = pattern;
		GpiSetLineType(os2_state.hps, LINETYPE_ALTERNATE);
		break;
	default:
		os2_state.line_pattern = 0xFFFF;
		GpiSetLineType(os2_state.hps, LINETYPE_SOLID);
	}

	if (thickness > 0) {
		LINEWIDTH width;
		width.fxWidth = MAKEFIXED(thickness, 0);
		GpiSetLineWidth(os2_state.hps, &width);
	}
}

void
setfillstyle(int pattern, int color)
{
	os2_state.fill_style = pattern;
	os2_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	RECTL rcl;

	/* Flip Y coordinates */
	int temp = top;
	top = os2_state.height - 1 - bottom;
	bottom = os2_state.height - 1 - temp;

	rcl.xLeft = left;
	rcl.yBottom = top;
	rcl.xRight = right + 1;
	rcl.yTop = bottom + 1;

	WinFillRect(os2_state.hps, &rcl, os2_state.colors[os2_state.fill_color & 0x0F]);
	WinInvalidateRect(os2_state.hwnd_client, NULL, FALSE);
}

void
settextjustify(int horiz, int vert)
{
	os2_state.text_justify_h = horiz;
	os2_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	FONTMETRICS fm;
	GpiQueryFontMetrics(os2_state.hps, sizeof(fm), &fm);
	return fm.lMaxBaselineExt;
}

int
textwidth(const char *textstring)
{
	POINTL aptl[TXTBOX_COUNT];
	GpiQueryTextBox(os2_state.hps, strlen(textstring), (PCH)textstring,
	                TXTBOX_COUNT, aptl);
	return aptl[TXTBOX_CONCAT].x - aptl[TXTBOX_BOTTOMLEFT].x;
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
	return os2_state.initialized ? grOk : grNoInitGraph;
}
