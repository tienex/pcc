# PCC Library Sets for Different Targets
# Include this in your build system to select appropriate libraries

# Core libraries (always required)
CORE_LIBS = \
	libmangle \
	libpccir \
	libvm \
	libxmath

# Basic runtime libraries
RUNTIME_LIBS = \
	libexcept \
	libcoro \
	libustring \
	libvtable

# System libraries (platform abstraction)
SYSTEM_LIBS = \
	libfs \
	libcrt

# Advanced runtime (requires more resources)
ADVANCED_RUNTIME_LIBS = \
	libgc \
	libgthread \
	libunwind

# Network libraries
NETWORK_LIBS = \
	libnet \
	libaio

# Graphics libraries
GRAPHICS_LIBS = \
	libbgi \
	libterm

# Language-specific libraries
LANGUAGE_LIBS = \
	libwirth \
	libpascal \
	libbliss

# Development/debugging libraries
DEBUG_LIBS = \
	libseh \
	libx86asm

# Target-specific library sets
# DOS 16-bit (minimal)
DOS16_LIBS = $(CORE_LIBS) $(RUNTIME_LIBS) $(SYSTEM_LIBS) libbgi

# DOS 32-bit (DJGPP/Watcom)
DOS32_LIBS = $(DOS16_LIBS) $(ADVANCED_RUNTIME_LIBS)

# Windows 16-bit
WIN16_LIBS = $(CORE_LIBS) $(RUNTIME_LIBS) $(SYSTEM_LIBS) libbgi

# Windows 32-bit
WIN32_LIBS = $(CORE_LIBS) $(RUNTIME_LIBS) $(SYSTEM_LIBS) \
             $(ADVANCED_RUNTIME_LIBS) $(NETWORK_LIBS) $(GRAPHICS_LIBS)

# Unix/Linux/BSD/macOS (full)
UNIX_LIBS = $(CORE_LIBS) $(RUNTIME_LIBS) $(SYSTEM_LIBS) \
            $(ADVANCED_RUNTIME_LIBS) $(NETWORK_LIBS) $(GRAPHICS_LIBS) \
            $(LANGUAGE_LIBS) $(DEBUG_LIBS)

# OS/2 16-bit
OS2_16_LIBS = $(WIN16_LIBS)

# OS/2 32-bit
OS2_32_LIBS = $(WIN32_LIBS)

# OpenVMS
OPENVMS_LIBS = $(CORE_LIBS) $(RUNTIME_LIBS) $(SYSTEM_LIBS) \
               libgc libgthread

# Embedded (minimal)
EMBEDDED_LIBS = $(CORE_LIBS) libcoro libcrt

# BeOS/Haiku
BEOS_LIBS = $(UNIX_LIBS)

# Select library set based on target
ifeq ($(TARGET),dos-16)
    SELECTED_LIBS = $(DOS16_LIBS)
endif

ifeq ($(TARGET),dos-32)
    SELECTED_LIBS = $(DOS32_LIBS)
endif

ifeq ($(TARGET),win-16)
    SELECTED_LIBS = $(WIN16_LIBS)
endif

ifeq ($(TARGET),win-32)
    SELECTED_LIBS = $(WIN32_LIBS)
endif

ifeq ($(TARGET),unix)
    SELECTED_LIBS = $(UNIX_LIBS)
endif

ifeq ($(TARGET),os2-16)
    SELECTED_LIBS = $(OS2_16_LIBS)
endif

ifeq ($(TARGET),os2-32)
    SELECTED_LIBS = $(OS2_32_LIBS)
endif

ifeq ($(TARGET),openvms)
    SELECTED_LIBS = $(OPENVMS_LIBS)
endif

ifeq ($(TARGET),embedded)
    SELECTED_LIBS = $(EMBEDDED_LIBS)
endif

ifeq ($(TARGET),beos)
    SELECTED_LIBS = $(BEOS_LIBS)
endif

# Default to full Unix build
ifndef SELECTED_LIBS
    SELECTED_LIBS = $(UNIX_LIBS)
endif
