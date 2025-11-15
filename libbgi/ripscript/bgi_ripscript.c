/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using RIPscript (Remote Imaging Protocol)
 *
 * BBS graphics protocol for remote terminal graphics
 * Works on: Any terminal with RIPscript support
 * Features: Vector graphics over serial/telnet connections
 * Output: RIPscrip 1.54 commands to stdout
 * Note: Requires RIP-compatible terminal emulator (TeleGrafix, RIPterm, etc.)
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define RIP_HEADER "\x1B!1"  /* RIPscrip escape sequence */

static struct {
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	int initialized;
} rip_state;

/* RIPscrip meganum encoding */
static void rip_encode_meganum(char *buf, int value) {
	buf[0] = (value / 36) + '!';
	buf[1] = (value % 36) + '!';
	buf[2] = '\0';
}

/* Output RIPscrip command */
static void rip_command(const char *cmd) {
	printf("%s%s\r\n", RIP_HEADER, cmd);
	fflush(stdout);
}

void initgraph(int *driver, int *mode, const char *path) {
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* RIPscrip standard resolution is 640x350 (EGA) */
	rip_state.width = 640;
	rip_state.height = 350;

	rip_state.current_color = WHITE;
	rip_state.current_bkcolor = BLACK;
	rip_state.current_x = 0;
	rip_state.current_y = 0;
	rip_state.line_style = SOLID_LINE;
	rip_state.line_pattern = 0xFFFF;
	rip_state.fill_style = SOLID_FILL;
	rip_state.fill_color = WHITE;
	rip_state.text_justify_h = LEFT_TEXT;
	rip_state.text_justify_v = TOP_TEXT;
	rip_state.initialized = 1;

	/* Initialize RIPscrip: reset and enter graphics mode */
	rip_command("R");  /* Reset */
	rip_command("#");  /* End of graphics section */

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!rip_state.initialized)
		return;

	/* Exit RIPscrip graphics mode */
	rip_command("E");  /* End graphics */
	printf("\x1B[0m");  /* Reset ANSI attributes */
	fflush(stdout);

	memset(&rip_state, 0, sizeof(rip_state));
}

int getmaxx(void) { return rip_state.width - 1; }
int getmaxy(void) { return rip_state.height - 1; }

void setcolor(int color) {
	char cmd[32];
	rip_state.current_color = color & 0x0F;
	sprintf(cmd, "c%02X", rip_state.current_color);
	rip_command(cmd);
}

int getcolor(void) { return rip_state.current_color; }

void setbkcolor(int color) {
	rip_state.current_bkcolor = color & 0x0F;
}

int getbkcolor(void) { return rip_state.current_bkcolor; }

void cleardevice(void) {
	char cmd[32];
	sprintf(cmd, "E%02X", rip_state.current_bkcolor);
	rip_command(cmd);  /* Erase window with background color */
}

void putpixel(int x, int y, int color) {
	char cmd[64], x1[3], y1[3];

	if (x < 0 || x >= rip_state.width || y < 0 || y >= rip_state.height)
		return;

	rip_encode_meganum(x1, x);
	rip_encode_meganum(y1, y);
	sprintf(cmd, "P%s%s%02X", x1, y1, color & 0x0F);
	rip_command(cmd);
}

unsigned int getpixel(int x, int y) {
	/* RIPscrip doesn't support pixel reading */
	return 0;
}

void moveto(int x, int y) {
	char cmd[64], x1[3], y1[3];

	rip_state.current_x = x;
	rip_state.current_y = y;

	rip_encode_meganum(x1, x);
	rip_encode_meganum(y1, y);
	sprintf(cmd, "m%s%s", x1, y1);
	rip_command(cmd);
}

void moverel(int dx, int dy) {
	moveto(rip_state.current_x + dx, rip_state.current_y + dy);
}

int getx(void) { return rip_state.current_x; }
int gety(void) { return rip_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	char cmd[64], sx1[3], sy1[3], sx2[3], sy2[3];

	rip_encode_meganum(sx1, x1);
	rip_encode_meganum(sy1, y1);
	rip_encode_meganum(sx2, x2);
	rip_encode_meganum(sy2, y2);

	sprintf(cmd, "L%s%s%s%s%02X", sx1, sy1, sx2, sy2, rip_state.current_color);
	rip_command(cmd);
}

void lineto(int x, int y) {
	line(rip_state.current_x, rip_state.current_y, x, y);
	rip_state.current_x = x;
	rip_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = rip_state.current_x + dx;
	int y2 = rip_state.current_y + dy;
	line(rip_state.current_x, rip_state.current_y, x2, y2);
	rip_state.current_x = x2;
	rip_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	char cmd[64], x1[3], y1[3], x2[3], y2[3];

	rip_encode_meganum(x1, left);
	rip_encode_meganum(y1, top);
	rip_encode_meganum(x2, right);
	rip_encode_meganum(y2, bottom);

	sprintf(cmd, "R%s%s%s%s%02X", x1, y1, x2, y2, rip_state.current_color);
	rip_command(cmd);
}

void circle(int x, int y, int radius) {
	char cmd[64], sx[3], sy[3], sr[3];

	rip_encode_meganum(sx, x);
	rip_encode_meganum(sy, y);
	rip_encode_meganum(sr, radius);

	sprintf(cmd, "C%s%s%s%02X", sx, sy, sr, rip_state.current_color);
	rip_command(cmd);
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	char cmd[32];

	rip_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		rip_state.line_pattern = 0xFFFF;
		rip_command("=0");  /* Solid line */
		break;
	case DOTTED_LINE:
		rip_state.line_pattern = 0xCCCC;
		rip_command("=1");  /* Dotted line */
		break;
	case DASHED_LINE:
		rip_state.line_pattern = 0xF0F0;
		rip_command("=2");  /* Dashed line */
		break;
	case USERBIT_LINE:
		rip_state.line_pattern = pattern;
		sprintf(cmd, "=FFFF%04X", pattern);
		rip_command(cmd);
		break;
	default:
		rip_state.line_pattern = 0xFFFF;
		rip_command("=0");
	}

	/* Set line thickness */
	if (thickness > 0) {
		sprintf(cmd, "=%d", thickness);
		rip_command(cmd);
	}
}

void setfillstyle(int pattern, int color) {
	char cmd[32];

	rip_state.fill_style = pattern;
	rip_state.fill_color = color;

	/* RIPscrip fill patterns */
	switch (pattern) {
	case EMPTY_FILL:
		rip_command("f00");
		break;
	case SOLID_FILL:
		sprintf(cmd, "f%02X", color & 0x0F);
		rip_command(cmd);
		break;
	case LINE_FILL:
		rip_command("f01");
		break;
	case LTSLASH_FILL:
		rip_command("f02");
		break;
	case SLASH_FILL:
		rip_command("f03");
		break;
	case BKSLASH_FILL:
		rip_command("f04");
		break;
	case HATCH_FILL:
		rip_command("f05");
		break;
	case XHATCH_FILL:
		rip_command("f06");
		break;
	default:
		sprintf(cmd, "f%02X", color & 0x0F);
		rip_command(cmd);
	}
}

void bar(int left, int top, int right, int bottom) {
	char cmd[64], x1[3], y1[3], x2[3], y2[3];

	rip_encode_meganum(x1, left);
	rip_encode_meganum(y1, top);
	rip_encode_meganum(x2, right);
	rip_encode_meganum(y2, bottom);

	sprintf(cmd, "B%s%s%s%s%02X", x1, y1, x2, y2, rip_state.fill_color & 0x0F);
	rip_command(cmd);
}

void settextjustify(int horiz, int vert) {
	rip_state.text_justify_h = horiz;
	rip_state.text_justify_v = vert;
}

int textheight(const char *textstring) {
	return 14;  /* RIPscrip default font height */
}

int textwidth(const char *textstring) {
	return strlen(textstring) * 8;  /* RIPscrip default font width */
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
	return rip_state.initialized ? grOk : grNoInitGraph;
}
