/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Microsoft Graphics Library Compatibility Header
 *
 * Compatible with Microsoft C 6.0+, Visual C++ 1.x _graph.h
 * Provides low-level graphics functions for DOS/Windows
 */

#ifndef GRAPH_COMPAT_H
#define GRAPH_COMPAT_H

#ifdef __cplusplus
extern "C" {
#endif

/* Video modes - Microsoft Graphics Library */
#define _DEFAULTMODE	-1
#define _TEXTBW40	0
#define _TEXTC40	1
#define _TEXTBW80	2
#define _TEXTC80	3
#define _MRES4COLOR	4		/* CGA 320x200, 4 color */
#define _MRESNOCOLOR	5		/* CGA 320x200, BW */
#define _HRESBW		6		/* CGA 640x200, BW */
#define _TEXTMONO	7		/* MDA/Hercules 80x25 text */
#define _HERCMONO	8		/* Hercules 720x348, BW */
#define _MRES16COLOR	13		/* EGA 320x200, 16 color */
#define _HRES16COLOR	14		/* EGA 640x200, 16 color */
#define _ERESNOCOLOR	15		/* EGA 640x350, BW */
#define _ERESCOLOR	16		/* EGA 640x350, 4/16 color */
#define _VRES2COLOR	17		/* VGA 640x480, BW */
#define _VRES16COLOR	18		/* VGA 640x480, 16 color */
#define _MRES256COLOR	19		/* VGA 320x200, 256 color */
#define _URES256COLOR	256		/* SVGA 640x400, 256 color */
#define _VRES256COLOR	257		/* SVGA 640x480, 256 color */
#define _SVRES16COLOR	258		/* SVGA 800x600, 16 color */
#define _SVRES256COLOR	259		/* SVGA 800x600, 256 color */
#define _XRES16COLOR	260		/* SVGA 1024x768, 16 color */
#define _XRES256COLOR	261		/* SVGA 1024x768, 256 color */

/* Color values */
#define _BLACK		0
#define _BLUE		1
#define _GREEN		2
#define _CYAN		3
#define _RED		4
#define _MAGENTA	5
#define _BROWN		6
#define _WHITE		7
#define _GRAY		8
#define _LIGHTBLUE	9
#define _LIGHTGREEN	10
#define _LIGHTCYAN	11
#define _LIGHTRED	12
#define _LIGHTMAGENTA	13
#define _YELLOW		14
#define _BRIGHTWHITE	15

/* Line styles */
#define _SOLID		0
#define _DASHED		1
#define _DOTTED		2
#define _DASHDOT	3

/* Fill masks */
#define _GFILLINTERIOR	2

/* Text positioning */
#define _GCENTERTEXT	1
#define _GRIGHTTEXT	2

/* Video configuration structure */
struct _videoconfig {
	short numxpixels;	/* Width in pixels */
	short numypixels;	/* Height in pixels */
	short numtextcols;	/* Text columns */
	short numtextrows;	/* Text rows */
	short numcolors;	/* Number of colors */
	short bitsperpixel;	/* Bits per pixel */
	short numvideopages;	/* Number of video pages */
	short mode;		/* Current video mode */
	short adapter;		/* Adapter type */
	short monitor;		/* Monitor type */
	short memory;		/* Video memory in KB */
};

/* Coordinate structure */
struct _xycoord {
	short xcoord;
	short ycoord;
};

/* Window coordinate structure */
struct _wxycoord {
	double wx;
	double wy;
};

/* Rectangle structure */
struct _rccoord {
	short row;
	short col;
};

/*
 * Video mode functions
 */
short _setvideomode(short mode);
short _getvideomode(void);
struct _videoconfig * _getvideoconfig(struct _videoconfig *config);
short _setvideomode(short mode);
short _setvideomoderows(short mode, short rows);

/*
 * Color and palette functions
 */
short _setcolor(short color);
short _getcolor(void);
long _setbkcolor(long color);
long _getbkcolor(void);
short _remappalette(short index, long color);
short _remapallpalette(long *colors);

/*
 * Drawing functions
 */
short _moveto(short x, short y);
struct _xycoord _getcurrentposition(void);
short _lineto(short x, short y);
short _line(short x1, short y1, short x2, short y2);
short _rectangle(short control, short x1, short y1, short x2, short y2);
short _ellipse(short control, short x1, short y1, short x2, short y2);
short _arc(short x1, short y1, short x2, short y2, short x3, short y3, short x4, short y4);
short _pie(short control, short x1, short y1, short x2, short y2, short x3, short y3, short x4, short y4);
short _polygon(short control, short numpoints, struct _xycoord *points);
short _floodfill(short x, short y, short boundary);

/*
 * Pixel functions
 */
short _setpixel(short x, short y);
short _getpixel(short x, short y);

/*
 * Line style functions
 */
void _setlinestyle(unsigned short mask);
unsigned short _getlinestyle(void);

/*
 * Fill functions
 */
short _setfillmask(unsigned char *mask);
unsigned char * _getfillmask(unsigned char *mask);

/*
 * Text functions
 */
short _outtext(char *text);
short _outmem(char *text, short length);
short _settextposition(short row, short col);
struct _rccoord _gettextposition(void);
void _settextcolor(short color);
short _gettextcolor(void);
void _settextwindow(short r1, short c1, short r2, short c2);
void _scrolltextwindow(short lines);
void _clearscreen(short area);

/* Clear screen areas */
#define _GCLEARSCREEN	0
#define _GVIEWPORT	1
#define _GWINDOW	2

/*
 * Viewport functions
 */
void _setviewport(short x1, short y1, short x2, short y2);
void _getviewport(short *x1, short *y1, short *x2, short *y2);
void _setcliprgn(short x1, short y1, short x2, short y2);

/*
 * Window coordinate functions
 */
short _setwindow(short invert, double x1, double y1, double x2, double y2);
short _getwindow(struct _wxycoord *min, struct _wxycoord *max);

/*
 * Image functions
 */
long _imagesize(short x1, short y1, short x2, short y2);
void _getimage(short x1, short y1, short x2, short y2, char *image);
void _putimage(short x, short y, char *image, short action);

/* Image actions */
#define _GPSET		3
#define _GPRESET	2
#define _GAND		1
#define _GOR		0
#define _GXOR		4

/*
 * Font functions
 */
short _registerfonts(char *path);
short _unregisterfonts(void);
short _setfont(char *options);
short _getfontinfo(void *info);
short _outgtext(char *text);

/*
 * Status and error functions
 */
short _grstatus(void);
void _setgtextvector(short x, short y);

/* Control codes for drawing */
#define _GBORDER	2
#define _GFILLINTERIOR	2

#ifdef __cplusplus
}
#endif

#endif /* GRAPH_COMPAT_H */
