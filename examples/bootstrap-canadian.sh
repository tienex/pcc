#!/bin/sh
# Example: Canadian cross-compilation bootstrap
#
# This demonstrates the "Canadian Cross" build:
#   Build:  x86_64-linux-gnu (current system, where compilation happens)
#   Host:   aarch64-linux-gnu (where the compiler will run)
#   Target: mips-linux-gnu (architecture the compiler generates code for)
#
# Result: A compiler that:
#   - Is built on x86_64
#   - Runs on aarch64
#   - Generates code for MIPS
#
# Prerequisites:
#   sudo apt-get install gcc-aarch64-linux-gnu gcc-mips-linux-gnu

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PCC_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PCC_ROOT"

# Verify cross-compilers are installed
missing=0
if ! command -v aarch64-linux-gnu-gcc >/dev/null 2>&1; then
    echo "Error: aarch64 cross-compiler not found"
    missing=1
fi

if ! command -v mips-linux-gnu-gcc >/dev/null 2>&1; then
    echo "Error: MIPS cross-compiler not found"
    missing=1
fi

if [ $missing -eq 1 ]; then
    echo ""
    echo "Please install cross-compilers with:"
    echo "  sudo apt-get install gcc-aarch64-linux-gnu gcc-mips-linux-gnu"
    exit 1
fi

echo "=== PCC Canadian Cross Bootstrap Example ==="
echo ""
echo "This will build PCC as a Canadian cross-compiler:"
echo "  Build system:  x86_64-linux-gnu (current system)"
echo "  Host system:   aarch64-linux-gnu (where PCC will run)"
echo "  Target system: mips-linux-gnu (code PCC generates)"
echo ""
echo "Build directory: $PCC_ROOT/bootstrap-build-canadian"
echo "Install prefix: /tmp/pcc-canadian"
echo ""

# Run bootstrap
./bootstrap.sh \
  --build=x86_64-linux-gnu \
  --host=aarch64-linux-gnu \
  --target=mips-linux-gnu \
  --stages=1 \
  --prefix=/tmp/pcc-canadian \
  --builddir=bootstrap-build-canadian \
  --clean

echo ""
echo "=== Canadian Cross Bootstrap Complete ==="
echo ""
echo "The Canadian cross PCC is installed at:"
echo "  /tmp/pcc-canadian/stage0/bin/pcc"
echo ""
echo "This is an aarch64 binary that generates MIPS code."
echo "To verify the binary type:"
echo "  file /tmp/pcc-canadian/stage0/bin/pcc"
echo ""
echo "To use it:"
echo "1. Transfer to an aarch64 system:"
echo "   scp -r /tmp/pcc-canadian user@aarch64-system:/tmp/"
echo ""
echo "2. On the aarch64 system, compile for MIPS:"
echo "   export PATH=/tmp/pcc-canadian/stage0/bin:\$PATH"
echo "   pcc test.c -o test"
echo ""
echo "3. The output 'test' binary will be a MIPS executable:"
echo "   file test  # Should show 'MIPS'"
echo ""
