#!/bin/sh
# PCC Target Configuration Helper
# Generates configure commands for different targets

show_help() {
    cat << EOF
PCC Target Configuration Helper

Usage: $0 <target> [options]

Targets:
  dos-16        DOS 16-bit (minimal libraries)
  dos-32        DOS 32-bit (DJGPP/Watcom)
  win-16        Windows 16-bit
  win-32        Windows 32-bit
  unix          Unix/Linux/BSD/macOS (full)
  os2-16        OS/2 16-bit
  os2-32        OS/2 32-bit
  openvms       OpenVMS
  embedded      Embedded/bare-metal (minimal)
  beos          BeOS/Haiku

Options:
  --prefix=DIR      Installation prefix (default: /usr/local)
  --enable-debug    Enable debug build
  --enable-static   Build static libraries only
  --enable-shared   Build shared libraries only

Examples:
  $0 dos-32 --prefix=/opt/pcc-dos32
  $0 unix --enable-debug
  $0 embedded --enable-static --prefix=/opt/pcc-embedded

EOF
}

if [ $# -eq 0 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    show_help
    exit 0
fi

TARGET=$1
shift

# Default options
PREFIX="/usr/local"
DEBUG=""
STATIC=""
SHARED=""

# Parse remaining options
while [ $# -gt 0 ]; do
    case "$1" in
        --prefix=*)
            PREFIX="${1#*=}"
            ;;
        --enable-debug)
            DEBUG="--enable-debug"
            ;;
        --enable-static)
            STATIC="--enable-static --disable-shared"
            ;;
        --enable-shared)
            SHARED="--enable-shared --disable-static"
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
    shift
done

# Build configure command based on target
case "$TARGET" in
    dos-16)
        LIBS="--disable-gc --disable-net --disable-aio --disable-term"
        CC="ia16-elf-gcc"
        ;;
    dos-32)
        LIBS="--enable-gc --disable-net --disable-aio --disable-term"
        CC="i586-pc-msdosdjgpp-gcc"
        ;;
    win-16)
        LIBS="--disable-gc --disable-net --disable-aio --disable-term"
        CC="i686-w64-mingw32-gcc -m16"
        ;;
    win-32)
        LIBS=""
        CC="i686-w64-mingw32-gcc"
        ;;
    unix)
        LIBS=""
        CC="gcc"
        ;;
    os2-16)
        LIBS="--disable-gc --disable-net --disable-aio --disable-term"
        CC="owcc"
        ;;
    os2-32)
        LIBS=""
        CC="owcc"
        ;;
    openvms)
        LIBS="--disable-net --disable-aio --disable-term --disable-bgi"
        CC="gcc"
        ;;
    embedded)
        LIBS="--enable-minimal --disable-gc --disable-net --disable-aio --disable-term --disable-bgi"
        CC="arm-none-eabi-gcc"
        ;;
    beos)
        LIBS=""
        CC="gcc"
        ;;
    *)
        echo "Unknown target: $TARGET"
        echo "Run '$0 --help' for available targets"
        exit 1
        ;;
esac

# Generate configure command
CONFIGURE_CMD="./configure --prefix=$PREFIX --target=$TARGET $LIBS $DEBUG $STATIC $SHARED CC=$CC"

echo "Target: $TARGET"
echo "Configure command:"
echo "$CONFIGURE_CMD"
echo ""
echo "Run this command to configure PCC for $TARGET"
echo ""

# Optionally run configure if --auto flag is present
if [ "$1" = "--auto" ]; then
    echo "Running configure..."
    eval $CONFIGURE_CMD
fi
