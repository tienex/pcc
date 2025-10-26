#!/bin/sh
# Example: Cross-compilation bootstrap
#
# This demonstrates building PCC as a cross-compiler
# Example: Build on x86_64, target ARM
#
# Prerequisites:
#   - Cross-compilation toolchain (e.g., gcc-arm-linux-gnueabihf)
#   - Run: sudo apt-get install gcc-arm-linux-gnueabihf

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PCC_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PCC_ROOT"

# Verify cross-compiler is installed
if ! command -v arm-linux-gnueabihf-gcc >/dev/null 2>&1; then
    echo "Error: ARM cross-compiler not found"
    echo "Please install it with:"
    echo "  sudo apt-get install gcc-arm-linux-gnueabihf"
    exit 1
fi

echo "=== PCC Cross-Compilation Bootstrap Example ==="
echo ""
echo "This will build PCC as a cross-compiler:"
echo "  Build system:  x86_64-linux-gnu (current system)"
echo "  Host system:   arm-linux-gnueabihf (where PCC will run)"
echo "  Target system: arm-linux-gnueabihf (code PCC generates)"
echo ""
echo "Build directory: $PCC_ROOT/bootstrap-build-cross"
echo "Install prefix: /tmp/pcc-cross"
echo ""

# Run bootstrap
./bootstrap.sh \
  --host=arm-linux-gnueabihf \
  --target=arm-linux-gnueabihf \
  --stages=2 \
  --prefix=/tmp/pcc-cross \
  --builddir=bootstrap-build-cross \
  --clean

echo ""
echo "=== Cross-Compilation Bootstrap Complete ==="
echo ""
echo "The cross-compiled PCC is installed at:"
echo "  /tmp/pcc-cross/stage1/bin/pcc"
echo ""
echo "This is an ARM binary that needs to run on an ARM system."
echo "To verify:"
echo "  file /tmp/pcc-cross/stage1/bin/pcc"
echo ""
echo "To use it, transfer to an ARM system and run:"
echo "  scp -r /tmp/pcc-cross user@arm-system:/tmp/"
echo "  ssh user@arm-system"
echo "  export PATH=/tmp/pcc-cross/stage1/bin:\$PATH"
echo "  pcc test.c -o test"
echo ""
