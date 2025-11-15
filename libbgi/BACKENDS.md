# BGI (Borland Graphics Interface) Backend Implementations

This directory contains BGI implementations for various platforms and graphics systems.

## Modern Cross-Platform Backends

### SDL2 (sdl2/)
- **Platform**: Linux, BSD, Windows, macOS, Android, iOS
- **Status**: ✅ Implemented
- **Features**: Hardware-accelerated rendering, window management, event handling
- **Dependencies**: SDL2 library
- **Notes**: Best choice for modern cross-platform applications

### SDL1 (sdl1/)
- **Platform**: Legacy systems, older Linux distributions
- **Status**: ✅ Implemented
- **Features**: Software rendering, legacy compatibility
- **Dependencies**: SDL 1.2 library
- **Notes**: Works on systems where SDL2 is not available

### EGL/OpenGL ES (egl/)
- **Platform**: Embedded Linux, Android, modern graphics systems
- **Status**: ⏳ Planned
- **Features**: GPU-accelerated rendering, shader support
- **Dependencies**: EGL, OpenGL ES 2.0+

## Linux-Specific Backends

### Linux Framebuffer (fbdev/)
- **Platform**: Linux console (/dev/fb0)
- **Status**: ✅ Implemented
- **Features**: Direct framebuffer access, no X11 required, 16/32-bit color
- **Dependencies**: Linux kernel framebuffer device
- **Notes**: Works in console mode, embedded systems

### DRM/KMS (drm/)
- **Platform**: Modern Linux (Direct Rendering Manager)
- **Status**: ⏳ Planned
- **Features**: Mode setting, hardware acceleration, multi-monitor
- **Dependencies**: libdrm, libgbm
- **Notes**: Modern replacement for fbdev

### SVGAlib (svgalib/)
- **Platform**: Legacy Linux (x86 only)
- **Status**: ⏳ Planned
- **Features**: Direct VGA hardware access from user space
- **Dependencies**: svgalib library
- **Notes**: Deprecated, security concerns, root access required

## BSD-Specific Backends

### FreeBSD Framebuffer (bsd_fb/freebsd/)
- **Platform**: FreeBSD console
- **Status**: ⏳ Planned
- **Features**: VESA framebuffer, syscons/vt
- **Dependencies**: FreeBSD kernel framebuffer

### OpenBSD Framebuffer (bsd_fb/openbsd/)
- **Platform**: OpenBSD wscons
- **Status**: ⏳ Planned
- **Features**: wscons framebuffer device
- **Dependencies**: OpenBSD wsdisplay

### NetBSD Framebuffer (bsd_fb/netbsd/)
- **Platform**: NetBSD wscons
- **Status**: ⏳ Planned
- **Features**: wscons framebuffer device
- **Dependencies**: NetBSD wsdisplay

## DOS/Windows Legacy Backends

### VGA Direct (vga/)
- **Platform**: DOS, real mode
- **Status**: ✅ Implemented
- **Features**: Direct VGA register access, Mode 13h (320x200x256), EGA palette
- **Dependencies**: Direct hardware access
- **Notes**: Requires DOS or DOSEMU, works with PCC, Watcom, MSC, Borland C

### SVGA/VESA (vesa/)
- **Platform**: DOS, protected mode
- **Status**: ✅ Implemented
- **Features**: VESA BIOS Extension (VBE), high resolutions, bank switching, linear framebuffer
- **Dependencies**: VESA-compatible BIOS, DPMI
- **Modes**: 640x480, 800x600, 1024x768, 1280x1024, 8-bit color
- **Notes**: Supports both banked and linear framebuffer modes

### EGA (ega/)
- **Platform**: DOS, IBM PC/XT/AT
- **Status**: ✅ Implemented
- **Features**: 16-color graphics, 640x350, planar mode
- **Dependencies**: EGA card
- **Notes**: Historical compatibility, direct register access

### CGA (cga/)
- **Platform**: DOS, IBM PC
- **Status**: ✅ Implemented
- **Features**: 4-color graphics, 320x200, palette switching
- **Dependencies**: CGA card
- **Notes**: Historical compatibility, supports both palettes

### Hercules (hercules/)
- **Platform**: DOS, Hercules Graphics Card
- **Status**: ✅ Implemented
- **Features**: Monochrome graphics, 720x348, 4-way interleaved memory
- **Dependencies**: Hercules card
- **Notes**: Historical compatibility, highest resolution monochrome

### XGA (xga/)
- **Platform**: DOS/OS2, IBM PS/2
- **Status**: ⏳ Planned
- **Features**: 1024x768, hardware acceleration
- **Dependencies**: XGA adapter

### Paradise PVGA (paradise/)
- **Platform**: DOS
- **Status**: ⏳ Planned
- **Features**: Paradise/Western Digital PVGA1A, 800x600x256
- **Dependencies**: Paradise VGA card

### Tseng ET3000/ET4000 (tseng/)
- **Platform**: DOS
- **Status**: ⏳ Planned
- **Features**: High-resolution SVGA, up to 1280x1024
- **Dependencies**: Tseng Labs ET3000 or ET4000 chipset

### S3 (s3/)
- **Platform**: DOS
- **Status**: ⏳ Planned
- **Features**: S3 86C911/924, Trio32/64, ViRGE, hardware acceleration
- **Dependencies**: S3 graphics card

### 8514/A (8514a/)
- **Platform**: DOS/OS2
- **Status**: ⏳ Planned
- **Features**: 1024x768, hardware acceleration, IBM 8514/A compatibility
- **Dependencies**: 8514/A or compatible adapter

### ATI (ati/)
- **Platform**: DOS
- **Status**: ⏳ Planned
- **Features**: VGA Wonder, Mach8, Mach32, Mach64, hardware acceleration
- **Dependencies**: ATI graphics card

### PCjr (pcjr/)
- **Platform**: IBM PCjr
- **Status**: ⏳ Planned
- **Features**: 320x200x16, PCjr video gate array
- **Dependencies**: IBM PCjr

### Tandy (tandy/)
- **Platform**: Tandy 1000 series
- **Status**: ⏳ Planned
- **Features**: Tandy Graphics Adapter, 320x200x16
- **Dependencies**: Tandy 1000

## Windows Backends

### Win16 (win16/)
- **Platform**: Windows 3.x
- **Status**: ⏳ Planned
- **Features**: GDI graphics, WinG acceleration
- **Dependencies**: Windows 3.1+, WinG library (optional)
- **Notes**: 16-bit Windows applications

### Win32 GDI (win32/)
- **Platform**: Windows 95/98/ME/NT/2000/XP/Vista/7/8/10/11
- **Status**: ✅ Implemented
- **Features**: GDI graphics, double buffering, window management
- **Dependencies**: Windows API
- **Notes**: Compatible with all modern Windows versions

### Direct2D (d2d/)
- **Platform**: Windows 7+
- **Status**: ⏳ Planned
- **Features**: Hardware-accelerated 2D graphics
- **Dependencies**: Direct2D, Direct3D 11+
- **Notes**: Modern Windows graphics API

## OS/2 Backends

### OS/2 16-bit (os2_16/)
- **Platform**: OS/2 1.x
- **Status**: ⏳ Planned
- **Features**: Presentation Manager graphics
- **Dependencies**: OS/2 1.x PM
- **Notes**: 16-bit OS/2 applications

### OS/2 32-bit (os2_32/)
- **Platform**: OS/2 2.x, Warp 3/4, eComStation, ArcaOS
- **Status**: ✅ Implemented
- **Features**: Presentation Manager graphics, DIVE hardware acceleration, double buffering
- **Dependencies**: OS/2 2.0+ PM, DIVE library (optional for acceleration)
- **Notes**: Hardware acceleration via DIVE when available, falls back to GPI

## X11-Based Backends

### X11 (x11/)
- **Platform**: Unix/Linux with X Window System
- **Status**: ✅ Implemented
- **Features**: Xlib-based rendering, double buffering, EGA palette
- **Dependencies**: libX11
- **Notes**: Works on all Unix systems with X11

### X11 + Motif (x11_motif/)
- **Platform**: Unix/Linux/commercial Unix
- **Status**: ✅ Implemented
- **Features**: Motif widgets, professional appearance
- **Dependencies**: libX11, libXm (Motif)
- **Notes**: Best for commercial Unix systems (Solaris, AIX, HP-UX)

### GLX (glx/)
- **Platform**: Unix/Linux with OpenGL and X11
- **Status**: ⏳ Planned
- **Features**: Hardware-accelerated OpenGL rendering
- **Dependencies**: libX11, libGL, GLX extension
- **Notes**: Best performance on Unix/Linux

## OpenGL Backends

### WGL (wgl/)
- **Platform**: Windows with OpenGL
- **Status**: ⏳ Planned
- **Features**: Hardware-accelerated OpenGL rendering
- **Dependencies**: Windows, OpenGL
- **Notes**: Best performance on Windows

### CGL (cgl/)
- **Platform**: macOS with OpenGL
- **Status**: ⏳ Planned
- **Features**: Hardware-accelerated OpenGL rendering
- **Dependencies**: macOS, OpenGL
- **Notes**: macOS native OpenGL (deprecated on modern macOS)

## Platform-Specific Backends

### Atari GEM (atari/)
- **Platform**: Atari TOS/GEM
- **Status**: ✅ Implemented
- **Features**: GEM VDI (Virtual Device Interface), native line/circle drawing
- **Dependencies**: TOS/GEM
- **Notes**: ST, STE, TT, Falcon030, uses VDI functions

### Amiga Intuition (amiga/)
- **Platform**: AmigaOS (68k, PowerPC)
- **Status**: ✅ Implemented
- **Features**: Intuition graphics library, screens, hardware sprites
- **Dependencies**: AmigaOS graphics.library, intuition.library
- **Notes**: Supports all Amigas (500, 1200, 2000, 3000, 4000, OS4)

### macOS Classic (macos_classic/)
- **Platform**: Mac OS 7-9 (pre-OSX)
- **Status**: ✅ Implemented
- **Features**: QuickDraw, Toolbox, Color QuickDraw
- **Dependencies**: Mac Toolbox
- **Notes**: 68k and PowerPC Macs, System 7+

### macOS Cocoa (macos/)
- **Platform**: macOS 10.0+ (OS X, macOS)
- **Status**: ✅ Implemented
- **Features**: Quartz 2D, Cocoa, Core Graphics, CGLayer for off-screen rendering
- **Dependencies**: Cocoa framework, Core Graphics
- **Notes**: Intel and Apple Silicon, modern macOS, uses Objective-C

### macOS Catalyst (catalyst/)
- **Platform**: macOS 10.15+ (Catalyst apps)
- **Status**: ⏳ Planned
- **Features**: UIKit on macOS, cross-platform with iOS
- **Dependencies**: Catalyst framework
- **Notes**: Shared codebase with iOS

### Plan 9 (plan9/)
- **Platform**: Plan 9 from Bell Labs
- **Status**: ✅ Implemented
- **Features**: /dev/draw interface, native graphics, double buffering with Image
- **Dependencies**: Plan 9 graphics system, libdraw
- **Notes**: Rio window system, 9front, plan9port

### RISC OS (riscos/)
- **Platform**: Acorn RISC OS
- **Status**: ⏳ Planned
- **Features**: RISC OS graphics system, Wimp
- **Dependencies**: RISC OS
- **Notes**: Acorn Archimedes, RiscPC, modern ARM systems

### PC/GEOS (geos/)
- **Platform**: PC/GEOS (GeoWorks)
- **Status**: ⏳ Planned
- **Features**: GEOS graphics kernel
- **Dependencies**: PC/GEOS
- **Notes**: GeoWorks Ensemble, Breadbox Ensemble

## Mobile Backends

### Android (android/)
- **Platform**: Android
- **Status**: ⏳ Planned
- **Features**: Android Canvas, SurfaceView
- **Dependencies**: Android SDK
- **Notes**: Works via SDL2 or native Android graphics

### iOS (ios/)
- **Platform**: iOS, iPadOS
- **Status**: ⏳ Planned
- **Features**: Core Graphics, UIKit
- **Dependencies**: iOS SDK
- **Notes**: iPhone, iPad, Apple Silicon Macs via Catalyst

### Symbian (symbian/)
- **Platform**: Symbian OS
- **Status**: ⏳ Planned
- **Features**: Symbian graphics system
- **Dependencies**: Symbian SDK
- **Notes**: Nokia, Sony Ericsson, historical

## Graphics Library Backends

### OpenGL (opengl/)
- **Platform**: Cross-platform
- **Status**: ⏳ Planned
- **Features**: OpenGL 1.x/2.x/3.x/4.x, hardware-accelerated
- **Dependencies**: OpenGL library
- **Notes**: Works on any platform with OpenGL support

### OpenGL ES (opengles/)
- **Platform**: Embedded, mobile
- **Status**: ⏳ Planned
- **Features**: OpenGL ES 1.x/2.x/3.x, embedded systems
- **Dependencies**: OpenGL ES library
- **Notes**: For embedded Linux, Android, iOS

### Cairo (cairo/)
- **Platform**: Cross-platform
- **Status**: ⏳ Planned
- **Features**: 2D vector graphics, anti-aliasing
- **Dependencies**: Cairo library
- **Notes**: High-quality 2D rendering

### Skia (skia/)
- **Platform**: Cross-platform
- **Status**: ⏳ Planned
- **Features**: 2D graphics library, GPU acceleration
- **Dependencies**: Skia library
- **Notes**: Used by Chrome, Android

### GGI (ggi/)
- **Platform**: Linux, BSD
- **Status**: ⏳ Planned
- **Features**: General Graphics Interface, modular architecture
- **Dependencies**: libggi
- **Notes**: Alternative to X11 on Linux

### xf86vm (xf86vm/)
- **Platform**: X11 with XFree86/Xorg
- **Status**: ⏳ Planned
- **Features**: Video mode switching, fullscreen
- **Dependencies**: X11, libXxf86vm
- **Notes**: Direct video mode control

## UEFI Backends

### UEFI GOP (uefi_gop/)
- **Platform**: UEFI firmware
- **Status**: ⏳ Planned
- **Features**: Graphics Output Protocol, framebuffer
- **Dependencies**: UEFI
- **Notes**: Modern firmware graphics

### UEFI UGA (uefi_uga/)
- **Platform**: UEFI firmware (legacy)
- **Status**: ⏳ Planned
- **Features**: Universal Graphics Adapter (pre-GOP)
- **Dependencies**: UEFI
- **Notes**: Older UEFI firmware

## Terminal Graphics Backends

### SIXEL (sixel/)
- **Platform**: SIXEL-capable terminals
- **Status**: ✅ Implemented
- **Features**: Raster graphics in text terminals, 16-color EGA palette, pixel buffer
- **Dependencies**: VT340, xterm, mintty, mlterm, WezTerm, foot
- **Notes**: DEC VT340 compatibility, widely supported, outputs graphics directly to terminal

### iTerm2 (iterm2/)
- **Platform**: iTerm2 terminal emulator
- **Status**: ⏳ Planned
- **Features**: Inline images protocol
- **Dependencies**: iTerm2 on macOS
- **Notes**: macOS-specific terminal

### kitty (kitty/)
- **Platform**: kitty terminal emulator
- **Status**: ⏳ Planned
- **Features**: Graphics protocol
- **Dependencies**: kitty terminal
- **Notes**: Cross-platform terminal emulator

### Tektronix (tek/)
- **Platform**: Tektronix 4010/4014 compatible terminals
- **Status**: ⏳ Planned
- **Features**: Vector graphics terminal
- **Dependencies**: Tektronix 4014 emulation (xterm)
- **Notes**: Historical vector terminal graphics

## Specialized Backends

### CGA Composite (cga_composite/)
- **Platform**: DOS, CGA with composite monitor
- **Status**: ⏳ Planned
- **Features**: NTSC artifact colors, 160x200x16 composite mode
- **Dependencies**: CGA card, composite video output
- **Notes**: Uses NTSC artifacts to create additional colors

### RIPscript (ripscript/)
- **Platform**: BBS terminals, ANSI art
- **Status**: ⏳ Planned
- **Features**: Remote Imaging Protocol, vector graphics for BBS
- **Dependencies**: RIPscrip-compatible terminal
- **Notes**: 1990s BBS graphics standard

## Microsoft Graphics Compatibility

### Microsoft Graphics (msgraph/)
- **Platform**: DOS with Microsoft C compiler
- **Status**: ✅ Implemented
- **Features**: Microsoft _graph.h compatibility, pgchart
- **Dependencies**: None (uses native MS graphics or BGI backends)
- **Notes**: Compatible with Microsoft C 6.0+, Visual C++ 1.x

## Selection Priority

When multiple backends are available, use this priority order:

1. **Modern Cross-Platform**: SDL2 > SDL1 > EGL
2. **Linux**: DRM > fbdev > SVGAlib > GLX > X11
3. **BSD**: Native fbdev > X11
4. **DOS**: VESA > VGA > EGA > CGA > Hercules
5. **Windows**: Direct2D > WGL > GDI > WinG
6. **macOS**: Cocoa/Quartz > CGL > SDL2
7. **OS/2**: DIVE > PM
8. **Amiga**: Intuition > SDL
9. **Atari**: GEM VDI > fbdev
10. **Plan 9**: Native /dev/draw
11. **UEFI**: GOP > UGA
12. **Terminal**: SIXEL > iTerm2 > kitty > Tektronix

## Build System

Each backend is self-contained in its directory with:
- Implementation file (bgi_*.c)
- Optional header file
- Optional Makefile

Backends are selected at compile time via:
- Preprocessor defines
- Configure script detection
- Manual Makefile selection

## Testing

Each backend should pass the BGI test suite:
- Line drawing (all styles)
- Shapes (rectangles, circles, ellipses)
- Fill patterns
- Text output
- Color palette
- Pixel operations
- Coordinate transformations

## Implementation Status Summary

**Implemented (17)**:
- SDL2, SDL1
- Linux framebuffer
- VGA, VESA, EGA, CGA, Hercules
- X11, X11+Motif
- Amiga Intuition
- Atari GEM
- Win32 GDI
- macOS Classic (QuickDraw)
- macOS Cocoa
- Plan 9 (/dev/draw)
- OS/2 PM/DIVE
- SIXEL terminal graphics

**Planned (40+)**:
- Modern: EGL, DRM, GLX, WGL, CGL, Direct2D
- BSD: FreeBSD, OpenBSD, NetBSD framebuffers
- DOS Legacy: SVGAlib, XGA
- DOS Video Cards: Paradise PVGA, Tseng ET3000/4000, S3, 8514/A, ATI, PCjr, Tandy
- Windows: Win16/WinG
- OS/2: 16-bit PM
- macOS: Catalyst
- Mobile: Android, iOS, Symbian
- Retro: RISC OS, PC/GEOS
- UEFI: GOP, UGA
- Terminal: iTerm2, kitty, Tektronix
- Graphics Libraries: OpenGL, OpenGL ES, Cairo, Skia, GGI, xf86vm
- Specialized: CGA Composite, RIPscript

## Contributing

When adding a new backend:
1. Create directory under libbgi/
2. Implement all BGI functions
3. Add entry to this document
4. Test with BGI test suite
5. Document dependencies and limitations
