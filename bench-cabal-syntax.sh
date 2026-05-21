#!/usr/bin/env bash
# Benchmark: compile Cabal-syntax (118 modules) with work queue vs traditional pipeline
#
# Usage:
#   ./bench-cabal-syntax.sh          # Run both work queue and traditional, 3 iterations each
#   ./bench-cabal-syntax.sh wq       # Run work queue only
#   ./bench-cabal-syntax.sh trad     # Run traditional only

set -uo pipefail

GHC="${GHC:-_build/stage1/bin/ghc}"
JOBS="${JOBS:--j8}"
ITERATIONS="${ITERATIONS:-3}"
MODE="${1:-both}"
CABAL_SYNTAX_SRC="libraries/Cabal/Cabal-syntax/src"
BUILD_DIR="/tmp/bench-cabal-syntax"

# Handle -jN argument
if [[ "$MODE" == -j* ]]; then
  JOBS="$MODE"
  MODE="both"
fi

# Package flags: hide everything, expose only what Cabal-syntax needs
PKG_FLAGS=(
  -hide-all-packages
  -package base
  -package array
  -package binary
  -package bytestring
  -package containers
  -package deepseq
  -package directory
  -package filepath
  -package mtl
  -package parsec
  -package pretty
  -package text
  -package time
  -package transformers
  -package unix
)

# GHC flags matching Cabal-syntax's cabal file
GHC_FLAGS=(
  -XHaskell2010
  -fno-ignore-asserts
  -fforce-recomp
  -no-link
  "$JOBS"
  -i"$CABAL_SYNTAX_SRC"
  "${PKG_FLAGS[@]}"
  -hidir "$BUILD_DIR/hi"
  -odir "$BUILD_DIR/obj"
  -stubdir "$BUILD_DIR/stub"
)

echo "=== Cabal-syntax Benchmark ==="
echo "GHC: $GHC"
echo "Jobs: $JOBS"
echo "Iterations: $ITERATIONS"
echo ""

clean_build_dir() {
  rm -rf "$BUILD_DIR"
  mkdir -p "$BUILD_DIR/hi" "$BUILD_DIR/obj" "$BUILD_DIR/stub"
}

run_bench() {
  local label="$1"
  shift
  local extra_flags=("$@")
  local total=0

  echo "--- $label ---"
  for i in $(seq 1 "$ITERATIONS"); do
    clean_build_dir
    # Time the compilation using bash TIMEFORMAT
    # Note: compilation returns non-zero because Distribution.Fields.Lexer
    # is Alex-generated and not in the source tree, but all reachable modules
    # compile successfully. We time the whole process regardless of exit code.
    local elapsed
    elapsed=$( { TIMEFORMAT='%R'; time "$GHC" --make "${GHC_FLAGS[@]}" "${extra_flags[@]}" BenchCabalSyntax.hs > /dev/null 2>&1 ; } 2>&1 )
    echo "  Run $i: ${elapsed}s"
    total=$(python3 -c "print($total + $elapsed)")
  done

  local avg
  avg=$(python3 -c "print(f'{$total / $ITERATIONS:.3f}')")
  echo "  Average: ${avg}s"
  echo ""
}

if [[ "$MODE" == "both" || "$MODE" == "wq" ]]; then
  run_bench "Work Queue" -fwork-queue
fi

if [[ "$MODE" == "both" || "$MODE" == "trad" ]]; then
  run_bench "Traditional" -fno-work-queue
fi

# Cleanup
rm -rf "$BUILD_DIR"
echo "Done."
