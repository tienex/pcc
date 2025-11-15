/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Microsoft Presentation Graphics Library Implementation
 *
 * High-level charting functions for business graphics
 */

#include "pgchart.h"
#include "graph_compat.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

/* Default palette */
static paletteentry default_palette = {
	{_BLUE, _GREEN, _RED, _CYAN, _MAGENTA, _BROWN, _LIGHTBLUE,
	 _LIGHTGREEN, _LIGHTRED, _LIGHTCYAN, _LIGHTMAGENTA, _YELLOW,
	 _WHITE, _GRAY, _LIGHTGRAY, _DARKGRAY},
	{0xFFFF, 0xEEEE, 0xDDDD, 0xCCCC, 0xBBBB, 0xAAAA, 0x9999,
	 0x8888, 0x7777, 0x6666, 0x5555, 0x4444, 0x3333, 0x2222,
	 0x1111, 0x0F0F},
	{'*', '+', 'x', 'o', '.', '^', 'v', '<', '>', 's', 'd', 'p',
	 'h', '|', '-', '#'}
};

static paletteentry current_palette;
static int pg_initialized = 0;

/* Helper function: Draw text centered */
static void
draw_centered_text(short x, short y, short color, const char *text)
{
	short old_color = _getcolor();
	short text_len = strlen(text) * 8; /* Assume 8x8 font */

	_setcolor(color);
	_moveto(x - text_len / 2, y);
	_outtext((char *)text);
	_setcolor(old_color);
}

/* Helper function: Draw vertical text */
static void
draw_vertical_text(short x, short y, short color, const char *text)
{
	short old_color = _getcolor();
	int i;
	short ypos = y;

	_setcolor(color);
	for (i = 0; text[i]; i++) {
		char ch[2] = {text[i], 0};
		_moveto(x, ypos);
		_outtext(ch);
		ypos += 10; /* Character height + spacing */
	}
	_setcolor(old_color);
}

/* Initialize presentation graphics */
short
_pg_initchart(void)
{
	struct _videoconfig config;

	if (pg_initialized)
		return 0;

	_getvideoconfig(&config);
	if (config.mode == _DEFAULTMODE)
		return -1; /* Graphics mode not set */

	/* Initialize palette */
	memcpy(&current_palette, &default_palette, sizeof(paletteentry));

	pg_initialized = 1;
	return 0;
}

/* Set default chart environment */
short
_pg_defaultchart(chartenv *env, short type, short style)
{
	struct _videoconfig config;

	if (!env)
		return -1;

	_getvideoconfig(&config);

	memset(env, 0, sizeof(chartenv));

	/* Chart type and style */
	env->charttype = type;
	env->chartstyle = style;

	/* Chart window (full screen with margins) */
	env->chartwindow.x1 = 10;
	env->chartwindow.y1 = 10;
	env->chartwindow.x2 = config.numxpixels - 10;
	env->chartwindow.y2 = config.numypixels - 10;
	env->chartwindow.border = 1;
	env->chartwindow.background = _BLACK;
	env->chartwindow.borderstyle = _SOLID;
	env->chartwindow.bordercolor = _WHITE;

	/* Data window (inside chart window) */
	env->datawindow.x1 = env->chartwindow.x1 + 60;
	env->datawindow.y1 = env->chartwindow.y1 + 40;
	env->datawindow.x2 = env->chartwindow.x2 - 60;
	env->datawindow.y2 = env->chartwindow.y2 - 40;
	env->datawindow.border = 1;
	env->datawindow.background = _BLACK;
	env->datawindow.borderstyle = _SOLID;
	env->datawindow.bordercolor = _WHITE;

	/* Main title */
	strcpy(env->maintitle.title, "Chart Title");
	env->maintitle.titlecolor = _WHITE;
	env->maintitle.justify = _GCENTERTEXT;

	/* Subtitle */
	env->subtitle.titlecolor = _LIGHTGRAY;
	env->subtitle.justify = _GCENTERTEXT;

	/* X-axis */
	env->xaxis.grid = 1;
	env->xaxis.gridstyle = _DOTTED;
	env->xaxis.axiscolor = _WHITE;
	env->xaxis.labeled = 1;
	env->xaxis.autoscale = 1;

	/* Y-axis */
	env->yaxis.grid = 1;
	env->yaxis.gridstyle = _DOTTED;
	env->yaxis.axiscolor = _WHITE;
	env->yaxis.labeled = 1;
	env->yaxis.autoscale = 1;

	/* Legend */
	env->legend.legend = 1;
	env->legend.place = _PG_RIGHT;
	env->legend.textcolor = _WHITE;
	env->legend.autosize = 1;

	return 0;
}

/* Draw chart window and borders */
static void
draw_chart_frame(chartenv *env)
{
	/* Draw chart window border */
	if (env->chartwindow.border) {
		_setcolor(env->chartwindow.bordercolor);
		_rectangle(_GBORDER, env->chartwindow.x1, env->chartwindow.y1,
		          env->chartwindow.x2, env->chartwindow.y2);
	}

	/* Draw data window border */
	if (env->datawindow.border) {
		_setcolor(env->datawindow.bordercolor);
		_rectangle(_GBORDER, env->datawindow.x1, env->datawindow.y1,
		          env->datawindow.x2, env->datawindow.y2);
	}

	/* Draw main title */
	if (env->maintitle.title[0]) {
		short title_x = (env->chartwindow.x1 + env->chartwindow.x2) / 2;
		short title_y = env->chartwindow.y1 + 5;
		draw_centered_text(title_x, title_y, env->maintitle.titlecolor,
		                  env->maintitle.title);
	}

	/* Draw subtitle */
	if (env->subtitle.title[0]) {
		short subtitle_x = (env->chartwindow.x1 + env->chartwindow.x2) / 2;
		short subtitle_y = env->chartwindow.y1 + 20;
		draw_centered_text(subtitle_x, subtitle_y, env->subtitle.titlecolor,
		                  env->subtitle.title);
	}
}

/* Single-series bar chart */
short
_pg_chart(chartenv *env, char **categories, float *values, short n)
{
	short i;
	short bar_width, bar_spacing;
	short x, y, height;
	float max_value, scale;
	short data_height, data_width;
	char label[64];

	if (!pg_initialized || !env || !values || n <= 0)
		return -1;

	/* Clear screen */
	_clearscreen(_GCLEARSCREEN);

	/* Draw chart frame */
	draw_chart_frame(env);

	/* Calculate scaling */
	max_value = values[0];
	for (i = 1; i < n; i++) {
		if (values[i] > max_value)
			max_value = values[i];
	}

	data_width = env->datawindow.x2 - env->datawindow.x1;
	data_height = env->datawindow.y2 - env->datawindow.y1;

	bar_width = data_width / (n * 2);
	bar_spacing = bar_width;

	if (max_value > 0)
		scale = (float)data_height / max_value;
	else
		scale = 1.0f;

	/* Draw bars */
	x = env->datawindow.x1 + bar_spacing;
	for (i = 0; i < n; i++) {
		height = (short)(values[i] * scale);
		y = env->datawindow.y2 - height;

		/* Draw bar */
		_setcolor(current_palette.color[i % _PG_COLORS]);
		_rectangle(_GFILLINTERIOR, x, y, x + bar_width, env->datawindow.y2);

		/* Draw bar outline */
		_setcolor(_WHITE);
		_rectangle(_GBORDER, x, y, x + bar_width, env->datawindow.y2);

		/* Draw category label */
		if (categories && categories[i] && env->xaxis.labeled) {
			draw_centered_text(x + bar_width / 2, env->datawindow.y2 + 5,
			                  env->xaxis.axiscolor, categories[i]);
		}

		/* Draw value label */
		snprintf(label, sizeof(label), "%.1f", values[i]);
		draw_centered_text(x + bar_width / 2, y - 10, _WHITE, label);

		x += bar_width + bar_spacing;
	}

	/* Draw axes */
	_setcolor(env->xaxis.axiscolor);
	_line(env->datawindow.x1, env->datawindow.y2,
	      env->datawindow.x2, env->datawindow.y2);

	_setcolor(env->yaxis.axiscolor);
	_line(env->datawindow.x1, env->datawindow.y1,
	      env->datawindow.x1, env->datawindow.y2);

	return 0;
}

/* Multi-series bar chart */
short
_pg_chartms(chartenv *env, char **categories, float **values,
            short nseries, short n, short arraydim, char **seriesnames)
{
	short i, j;
	short bar_width, group_spacing;
	short x, y, height;
	float max_value, scale;
	short data_height, data_width;
	short group_width;

	if (!pg_initialized || !env || !values || n <= 0 || nseries <= 0)
		return -1;

	/* Clear screen */
	_clearscreen(_GCLEARSCREEN);

	/* Draw chart frame */
	draw_chart_frame(env);

	/* Calculate scaling */
	max_value = values[0][0];
	for (i = 0; i < nseries; i++) {
		for (j = 0; j < n; j++) {
			if (values[i][j] > max_value)
				max_value = values[i][j];
		}
	}

	data_width = env->datawindow.x2 - env->datawindow.x1;
	data_height = env->datawindow.y2 - env->datawindow.y1;

	group_width = data_width / n;
	bar_width = group_width / (nseries + 1);
	group_spacing = bar_width / 2;

	if (max_value > 0)
		scale = (float)data_height / max_value;
	else
		scale = 1.0f;

	/* Draw bars */
	for (i = 0; i < n; i++) {
		x = env->datawindow.x1 + (i * group_width) + group_spacing;

		for (j = 0; j < nseries; j++) {
			height = (short)(values[j][i] * scale);
			y = env->datawindow.y2 - height;

			/* Draw bar */
			_setcolor(current_palette.color[j % _PG_COLORS]);
			_rectangle(_GFILLINTERIOR, x, y, x + bar_width, env->datawindow.y2);

			/* Draw bar outline */
			_setcolor(_WHITE);
			_rectangle(_GBORDER, x, y, x + bar_width, env->datawindow.y2);

			x += bar_width;
		}

		/* Draw category label */
		if (categories && categories[i] && env->xaxis.labeled) {
			short label_x = env->datawindow.x1 + (i * group_width) + group_width / 2;
			draw_centered_text(label_x, env->datawindow.y2 + 5,
			                  env->xaxis.axiscolor, categories[i]);
		}
	}

	/* Draw legend */
	if (env->legend.legend && seriesnames) {
		short legend_x = env->datawindow.x2 + 10;
		short legend_y = env->datawindow.y1;

		for (i = 0; i < nseries; i++) {
			/* Draw color box */
			_setcolor(current_palette.color[i % _PG_COLORS]);
			_rectangle(_GFILLINTERIOR, legend_x, legend_y,
			          legend_x + 10, legend_y + 10);

			/* Draw series name */
			_setcolor(env->legend.textcolor);
			_moveto(legend_x + 15, legend_y);
			_outtext(seriesnames[i]);

			legend_y += 15;
		}
	}

	/* Draw axes */
	_setcolor(env->xaxis.axiscolor);
	_line(env->datawindow.x1, env->datawindow.y2,
	      env->datawindow.x2, env->datawindow.y2);

	_setcolor(env->yaxis.axiscolor);
	_line(env->datawindow.x1, env->datawindow.y1,
	      env->datawindow.x1, env->datawindow.y2);

	return 0;
}

/* Pie chart */
short
_pg_chartpie(chartenv *env, char **categories, float *values,
             short *explode, short n)
{
	short i;
	float total = 0.0f;
	float start_angle = 0.0f;
	short cx, cy, radius;
	char label[64];

	if (!pg_initialized || !env || !values || n <= 0)
		return -1;

	/* Clear screen */
	_clearscreen(_GCLEARSCREEN);

	/* Draw chart frame */
	draw_chart_frame(env);

	/* Calculate total */
	for (i = 0; i < n; i++)
		total += values[i];

	if (total <= 0.0f)
		return -1;

	/* Calculate pie position */
	cx = (env->datawindow.x1 + env->datawindow.x2) / 2;
	cy = (env->datawindow.y1 + env->datawindow.y2) / 2;
	radius = ((env->datawindow.x2 - env->datawindow.x1) <
	          (env->datawindow.y2 - env->datawindow.y1))
	         ? (env->datawindow.x2 - env->datawindow.x1) / 3
	         : (env->datawindow.y2 - env->datawindow.y1) / 3;

	/* Draw pie slices (simplified - full pie for now) */
	_setcolor(_WHITE);
	_ellipse(_GBORDER, cx - radius, cy - radius, cx + radius, cy + radius);

	/* Draw legend with percentages */
	{
		short legend_x = env->datawindow.x2 + 10;
		short legend_y = env->datawindow.y1;

		for (i = 0; i < n; i++) {
			float percent = (values[i] / total) * 100.0f;

			/* Draw color box */
			_setcolor(current_palette.color[i % _PG_COLORS]);
			_rectangle(_GFILLINTERIOR, legend_x, legend_y,
			          legend_x + 10, legend_y + 10);

			/* Draw label with percentage */
			_setcolor(env->legend.textcolor);
			_moveto(legend_x + 15, legend_y);
			if (categories && categories[i]) {
				snprintf(label, sizeof(label), "%s: %.1f%%",
				        categories[i], percent);
			} else {
				snprintf(label, sizeof(label), "%.1f%%", percent);
			}
			_outtext(label);

			legend_y += 15;
		}
	}

	return 0;
}

/* Scatter chart */
short
_pg_chartscatter(chartenv *env, float *xvalues, float *yvalues, short n)
{
	short i;
	float xmin, xmax, ymin, ymax;
	float xscale, yscale;
	short x, y;
	short data_width, data_height;

	if (!pg_initialized || !env || !xvalues || !yvalues || n <= 0)
		return -1;

	/* Clear screen */
	_clearscreen(_GCLEARSCREEN);

	/* Draw chart frame */
	draw_chart_frame(env);

	/* Find min/max */
	xmin = xmax = xvalues[0];
	ymin = ymax = yvalues[0];
	for (i = 1; i < n; i++) {
		if (xvalues[i] < xmin) xmin = xvalues[i];
		if (xvalues[i] > xmax) xmax = xvalues[i];
		if (yvalues[i] < ymin) ymin = yvalues[i];
		if (yvalues[i] > ymax) ymax = yvalues[i];
	}

	/* Calculate scaling */
	data_width = env->datawindow.x2 - env->datawindow.x1;
	data_height = env->datawindow.y2 - env->datawindow.y1;

	xscale = (xmax > xmin) ? (float)data_width / (xmax - xmin) : 1.0f;
	yscale = (ymax > ymin) ? (float)data_height / (ymax - ymin) : 1.0f;

	/* Draw points */
	_setcolor(_WHITE);
	for (i = 0; i < n; i++) {
		x = env->datawindow.x1 + (short)((xvalues[i] - xmin) * xscale);
		y = env->datawindow.y2 - (short)((yvalues[i] - ymin) * yscale);

		/* Draw point as small circle */
		_ellipse(_GFILLINTERIOR, x - 2, y - 2, x + 2, y + 2);
	}

	/* Draw axes */
	_setcolor(env->xaxis.axiscolor);
	_line(env->datawindow.x1, env->datawindow.y2,
	      env->datawindow.x2, env->datawindow.y2);

	_setcolor(env->yaxis.axiscolor);
	_line(env->datawindow.x1, env->datawindow.y1,
	      env->datawindow.x1, env->datawindow.y2);

	return 0;
}

/* Get palette */
short
_pg_getpalette(paletteentry *palette)
{
	if (!palette)
		return -1;

	memcpy(palette, &current_palette, sizeof(paletteentry));
	return 0;
}

/* Set palette */
short
_pg_setpalette(paletteentry *palette)
{
	if (!palette)
		return -1;

	memcpy(&current_palette, palette, sizeof(paletteentry));
	return 0;
}

/* Reset palette */
short
_pg_resetpalette(void)
{
	memcpy(&current_palette, &default_palette, sizeof(paletteentry));
	return 0;
}

/* Placeholder stubs for other functions */
short _pg_chartscatterms(chartenv *env, float **xvalues, float **yvalues,
                         short nseries, short n, short arraydim, char **seriesnames)
{
	return -1; /* Not implemented */
}

short _pg_analyzechart(chartenv *env, char **categories, float *values, short n)
{
	return 0; /* No-op for now */
}

short _pg_analyzechartms(chartenv *env, char **categories, float **values,
                         short nseries, short n, short arraydim)
{
	return 0; /* No-op for now */
}

short _pg_analyzepie(chartenv *env, char **categories, float *values, short n)
{
	return 0; /* No-op for now */
}

short _pg_analyzescatter(chartenv *env, float *xvalues, float *yvalues, short n)
{
	return 0; /* No-op for now */
}

short _pg_analyzescatterms(chartenv *env, float **xvalues, float **yvalues,
                           short nseries, short n, short arraydim)
{
	return 0; /* No-op for now */
}

short _pg_hlabelchart(chartenv *env, short x, short y, short color, char *label)
{
	_setcolor(color);
	_moveto(x, y);
	_outtext(label);
	return 0;
}

short _pg_vlabelchart(chartenv *env, short x, short y, short color, char *label)
{
	draw_vertical_text(x, y, color, label);
	return 0;
}

short _pg_getstyleset(unsigned short *styleset)
{
	return -1; /* Not implemented */
}

short _pg_setstyleset(unsigned short *styleset)
{
	return -1; /* Not implemented */
}

short _pg_resetstyleset(void)
{
	return 0;
}

short _pg_getchardef(short ch, unsigned char *def)
{
	return -1; /* Not implemented */
}

short _pg_setchardef(short ch, unsigned char *def)
{
	return -1; /* Not implemented */
}
