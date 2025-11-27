# PCC Library Target Matrix

This document describes which libraries are available and recommended for each target platform.

## Core Compiler Libraries (All Targets)
These are essential for the compiler infrastructure and should be available on all platforms:

- **libmangle** - Symbol name mangling/demangling
- **libpccir** - PCC intermediate representation
- **libvm** - Virtual machine for bootstrapping
- **libxmath** - Extended math (VAX, Cray, IBM floating point)

## Runtime Libraries (Most Targets)

### Memory Management
- **libgc** - Garbage collector and ARC
  - Full: Unix, Windows 32-bit, OS/2 32-bit
  - Limited: DOS 32-bit (smaller heap limits)
  - Stub: DOS 16-bit, Windows 16-bit, OS/2 16-bit

### Exception Handling
- **libexcept** - Exception handling
  - Full: Unix (DWARF), Windows 32-bit (SEH), DOS (interrupts)
  - Limited: DOS 16-bit, Windows 16-bit

### Threading
- **libgthread** - Green threads and native threads
  - Native threads: Unix (pthread), Windows 32-bit, OS/2 32-bit
  - Green threads: DOS, Windows 16-bit, OS/2 16-bit, embedded
  - **libcoro** - Coroutines/fibers (all targets)

### String/Text
- **libustring** - Unicode string handling (all targets)
- **libmangle** - Symbol mangling (all targets)

## System Libraries (Platform-Specific)

### File System
- **libfs** - File system abstraction
  - DOS/, Win16/, Win32/, OS2_16/, OS2_32/
  - Unix/, OpenVMS/, BeOS/

### Networking
- **libnet** - Network I/O
  - Full: Unix, Windows 32-bit, OS/2 32-bit
  - Limited: DOS 32-bit (packet drivers)
  - Not available: DOS 16-bit, Windows 16-bit, embedded

### Async I/O
- **libaio** - Async I/O and event loop (NEW)
  - Full: Unix (epoll/kqueue), Windows 32-bit (IOCP)
  - Limited: DOS 32-bit
  - Not available: DOS 16-bit, Windows 16-bit

## UI/Graphics Libraries

### Console/Terminal
- **libcrt** - Console/terminal control
  - DOS: BIOS/DOS interrupts
  - Windows: Console API
  - Unix: ANSI/termcap

### Graphics
- **libbgi** - Borland Graphics Interface
  - DOS: VGA/VESA direct access
  - Windows: GDI
  - Unix: X11
  - Drivers: Loadable font/graphics drivers

### Terminal Emulation
- **libterm** - Full terminal emulator with VT100/ANSI
  - Windows: GUI window
  - Unix: X11 or curses
  - Future: Atari GEM, Amiga, MacOS Classic

## Debugging/Development

### Stack Unwinding
- **libunwind** - Stack unwinding and backtraces
  - Full: Unix (x86-64, i386, ARM)
  - Limited: Windows, DOS
  - Arch-specific: x86_64/, i386/, arm/, generic/

### VTable Layout
- **libvtable** - C++ vtable layout handling (all targets)

## Target Recommendations

### Minimal Embedded Target
- libmangle, libpccir, libvm
- libcoro (for cooperative multitasking)
- libcrt (basic console)

### DOS 16-bit
- Core: libmangle, libpccir, libvm, libxmath
- Runtime: libexcept (limited), libcoro, libustring
- System: libfs (DOS/), libcrt (DOS/)
- Graphics: libbgi (DOS/VGA)

### DOS 32-bit (DJGPP/Watcom)
- All DOS 16-bit libraries
- Additional: libgc (limited), libgthread, libnet (limited)

### Windows 16-bit
- Core + Runtime (similar to DOS 16-bit)
- System: libfs (Win16/), libcrt (Win16/)
- Graphics: libbgi (Win16/GDI)

### Windows 32-bit
- All libraries except embedded-only
- Full async I/O, networking, threading support

### Unix/Linux/BSD/macOS
- All libraries
- Full POSIX support
- Native threading, networking, graphics (X11)

### OpenVMS
- Core + Runtime
- System: libfs (OpenVMS/), libcrt (OpenVMS/)
- Limited: libnet, libaio

### OS/2 (16-bit and 32-bit)
- Similar to Windows equivalents
- System: libfs (OS2_16/ or OS2_32/)

## Build System Integration

Configure script can select libraries based on target:
```
./configure --target=dos-16 --enable-minimal
./configure --target=linux-x86_64 --enable-full
./configure --target=embedded-arm --enable-core-only
```
