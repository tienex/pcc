#!/bin/sh
# Example: Native 3-stage bootstrap with verification
#
# This builds PCC three times:
# - Stage 0: Using system compiler (gcc/clang)
# - Stage 1: Using stage 0 PCC
# - Stage 2: Using stage 1 PCC
#
# Then compares stage 1 and stage 2 to verify reproducibility

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PCC_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PCC_ROOT"

echo "=== PCC Native Bootstrap Example ==="
echo ""
echo "This will perform a 3-stage bootstrap build of PCC"
echo "using the default system compiler."
echo ""
echo "Build directory: $PCC_ROOT/bootstrap-build"
echo "Install prefix: /tmp/pcc-bootstrap"
echo ""

# Run bootstrap
./bootstrap.sh \
  --stages=3 \
  --compare-stages \
  --prefix=/tmp/pcc-bootstrap \
  --builddir=bootstrap-build \
  --clean

echo ""
echo "=== Bootstrap Complete ==="
echo ""
echo "The bootstrapped PCC compiler is installed at:"
echo "  /tmp/pcc-bootstrap/stage2/bin/pcc"
echo ""
echo "To test it:"
echo "  export PATH=/tmp/pcc-bootstrap/stage2/bin:\$PATH"
echo "  echo 'int main() { return 0; }' > test.c"
echo "  pcc test.c -o test"
echo "  ./test && echo 'Success!'"
echo ""
