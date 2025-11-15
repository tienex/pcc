# BGI Architecture and Implementation Guide

## Overview

The PCC BGI library provides a complete implementation of the Borland Graphics Interface across 65+ backends. This document describes the architecture and how to implement or update backends.

## Architecture

### Three-Layer Architecture

```
┌─────────────────────────────────────┐
│   Application Code (User)           │
│   Uses: initgraph(), line(), etc.   │
└───────────────┬─────────────────────┘
                │
┌───────────────▼─────────────────────┐
│   BGI API Layer (graphics.h)        │
│   - Complete BGI function set       │
│   - Standard types and constants    │
└───────────────┬─────────────────────┘
                │
         ┌──────┴──────┐
         │             │
┌────────▼─────┐ ┌────▼──────────────┐
│ Common Lib   │ │ Backend-Specific  │
│ (bgi_common) │ │ Implementation    │
│              │ │                   │
│ - Bresenham  │ │ - Hardware access │
│ - Flood fill │ │ - Native APIs     │
│ - Patterns   │ │ - Acceleration    │
│ - Text       │ │                   │
└──────────────┘ └───────────────────┘
```

### Core Files

1. **graphics.h** - Complete BGI API declaration
2. **bgi_common.h** - Common functionality interface
3. **bgi_common.c** - Shared algorithm implementations
4. **backend/*.c** - Backend-specific implementations

## Complete BGI API

All backends must implement the complete BGI API:

### Initialization & Control
- `initgraph()` - Initialize graphics mode
- `closegraph()` - Close graphics mode
- `graphresult()` - Get last error code
- `grapherrormsg()` - Get error message

### Screen Control
- `cleardevice()` - Clear entire screen
- `clearviewport()` - Clear viewport
- `setactivepage()` - Set drawing page
- `setvisualpage()` - Set visible page

### Coordinates
- `getmaxx()`, `getmaxy()` - Get screen dimensions
- `getx()`, `gety()` - Get current position
- `moveto()`, `moverel()` - Move cursor
- `setviewport()` - Set clipping region
- `getviewsettings()` - Get viewport info

### Drawing Primitives
- `putpixel()`, `getpixel()` - Pixel operations
- `line()`, `lineto()`, `linerel()` - Line drawing
- `rectangle()` - Rectangle outline
- `circle()` - Circle outline
- `arc()` - Arc drawing
- `ellipse()` - Ellipse drawing
- `drawpoly()` - Polygon outline
- `pieslice()` - Filled pie slice
- `sector()` - Elliptical sector

### Filled Shapes
- `bar()` - Filled rectangle
- `bar3d()` - 3D bar
- `fillpoly()` - Filled polygon
- `fillellipse()` - Filled ellipse
- `floodfill()` - Boundary fill

### Line Styles
- `setlinestyle()` - Set line pattern
- `getlinesettings()` - Get line settings

### Fill Patterns
- `setfillstyle()` - Set fill pattern
- `setfillpattern()` - Set custom pattern
- `getfillsettings()` - Get fill settings
- `getfillpattern()` - Get pattern data

### Colors
- `setcolor()`, `getcolor()` - Drawing color
- `setbkcolor()`, `getbkcolor()` - Background color
- `setpalette()` - Change palette entry
- `setallpalette()` - Set all colors
- `getpalette()` - Get palette
- `getpalettesize()` - Get color count

### Text
- `outtext()`, `outtextxy()` - Output text
- `settextjustify()` - Set text alignment
- `settextstyle()` - Set font and size
- `setusercharsize()` - Set char scaling
- `gettextsettings()` - Get text settings
- `textheight()`, `textwidth()` - Text metrics

### Images
- `getimage()` - Capture bitmap
- `putimage()` - Display bitmap
- `imagesize()` - Get bitmap size

### Arc Coordinates
- `getarccoords()` - Get last arc endpoints

## Common Library (bgi_common)

The common library provides software implementations of all graphics algorithms. Backends should use these when hardware acceleration is not available.

### Available Functions

#### Line Drawing
```c
void bgi_draw_line_bresenham(int x1, int y1, int x2, int y2,
                              void (*putpixel)(int, int, int),
                              int color, unsigned short pattern);
```
Bresenham line algorithm with pattern support.

#### Circle Drawing
```c
void bgi_draw_circle_bresenham(int x, int y, int radius,
                                void (*putpixel)(int, int, int),
                                int color);
void bgi_fill_circle_bresenham(int x, int y, int radius,
                                void (*putpixel)(int, int, int),
                                int color);
```

#### Ellipse Drawing
```c
void bgi_draw_ellipse(int x, int y, int stangle, int endangle,
                      int xradius, int yradius,
                      void (*putpixel)(int, int, int),
                      int color);
void bgi_fill_ellipse(int x, int y, int xradius, int yradius,
                      void (*putpixel)(int, int, int),
                      int color);
```

#### Arc Drawing
```c
void bgi_draw_arc(int x, int y, int stangle, int endangle, int radius,
                  void (*putpixel)(int, int, int),
                  int color, struct arccoordstype *coords);
```

#### Polygon Operations
```c
void bgi_draw_polygon(int numpoints, const int *polypoints,
                      void (*line_func)(int, int, int, int));
void bgi_fill_polygon(int numpoints, const int *polypoints,
                      void (*putpixel)(int, int, int),
                      int color, int width, int height);
```

#### Flood Fill
```c
void bgi_flood_fill(int x, int y, int border,
                    unsigned int (*getpixel)(int, int),
                    void (*putpixel)(int, int, int),
                    int fill_color, int width, int height);
```

#### Pattern Fill
```c
void bgi_pattern_fill_rect(int left, int top, int right, int bottom,
                            void (*putpixel)(int, int, int),
                            const unsigned char *pattern, int color);
```

#### Text Rendering
```c
void bgi_draw_text_simple(int x, int y, const char *text,
                          void (*putpixel)(int, int, int),
                          int color, int horiz, int vert);
int bgi_text_width_simple(const char *text);
int bgi_text_height_simple(void);
```

### Standard Fill Patterns

```c
extern const unsigned char bgi_fill_patterns[12][8];
```

Available patterns:
- `EMPTY_FILL`, `SOLID_FILL`, `LINE_FILL`
- `LTSLASH_FILL`, `SLASH_FILL`, `BKSLASH_FILL`
- `LTBKSLASH_FILL`, `HATCH_FILL`, `XHATCH_FILL`
- `INTERLEAVE_FILL`, `WIDE_DOT_FILL`, `CLOSE_DOT_FILL`

## Backend Implementation Guide

### Required State Structure

Each backend should maintain:

```c
static struct {
	struct bgi_common_state common;  /* Use common state */
	/* Backend-specific fields... */
	int initialized;
} backend_state;
```

### Using Common State

The `bgi_common_state` structure includes:
- Screen dimensions
- Current position
- Current colors
- Line style and pattern
- Fill style and pattern
- Text settings
- Viewport settings
- Arc coordinates
- Write mode
- Page numbers

### Hardware Acceleration

When possible, use native graphics APIs:

```c
void line(int x1, int y1, int x2, int y2) {
	if (backend_has_hw_line()) {
		/* Use hardware */
		backend_hw_line(x1, y1, x2, y2);
	} else {
		/* Fall back to common library */
		bgi_draw_line_bresenham(x1, y1, x2, y2, putpixel,
		                        state.common.current_color,
		                        state.common.line_pattern);
	}
}
```

### Backend Templates

#### DOS/Hardware Backend
For direct hardware access (VGA, CGA, EGA, etc.):
1. Direct framebuffer writes for `putpixel()`
2. Use common library for complex shapes
3. Implement hardware-specific palette control

#### Modern API Backend
For Cairo, Skia, Direct2D, etc.:
1. Use native line/rect/circle primitives
2. Use common library only when native API lacks features
3. Leverage anti-aliasing, transforms, etc.

#### Terminal Backend
For SIXEL, iTerm2, Kitty, etc.:
1. Maintain software framebuffer
2. Use common library for all drawing
3. Output framebuffer on `closegraph()` or key events

### Example: Complete Backend Implementation

See `libbgi/vga/bgi_vga_complete.c` for a complete reference implementation showing:
- Proper use of common library
- Complete API implementation
- Hardware acceleration (direct VGA memory)
- All BGI functions
- Proper state management

## Testing

Each backend should support:
1. All drawing primitives
2. All fill patterns
3. Line patterns
4. Text rendering
5. Viewport clipping
6. Image capture/restore
7. Palette manipulation (if supported)

## Performance Guidelines

1. **Always provide putpixel/getpixel** - These are required
2. **Use hardware when available** - Native APIs are faster
3. **Common library is optimized** - Bresenham is fast
4. **Batch operations** - Update screen once per frame
5. **Respect viewport clipping** - Check bounds efficiently

## Future Enhancements

Potential improvements:
- Hardware-accelerated pattern fills
- Bitmap font support
- Anti-aliasing support
- Alpha blending
- Transformation matrices
- Gradient fills
- Texture mapping

---

**Note**: All backends should be self-contained and not depend on other backends. The only shared dependency is the common library.
