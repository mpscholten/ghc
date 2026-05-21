#!/usr/bin/env bash
set -euo pipefail

ROOT="/Users/marc/digitallyinduced/ghc2"
GHC="$ROOT/_build/stage0/bin/ghc"
CABAL_SRC="$ROOT/libraries/Cabal/Cabal/src"
CABAL_SYNTAX_SRC="$ROOT/libraries/Cabal/Cabal-syntax/src"
RUNS=5

echo "=== Pipe-asm benchmark (deferred assembler start) ==="
echo "GHC: $($GHC --version)"
echo "Platform: $(uname -m) $(uname -s)"
echo "CPUs: $(sysctl -n hw.ncpu)"
echo "Runs per config: $RUNS"
echo ""

bench_cabal() {
    local flags="$1"
    local label="$2"
    find "$CABAL_SRC" "$CABAL_SYNTAX_SRC" \( -name "*.o" -o -name "*.hi" \) -delete 2>/dev/null || true
    local start end elapsed
    start=$(python3 -c "import time; print(time.time())")
    "$GHC" --make $flags \
        -i"$CABAL_SRC" -i"$CABAL_SYNTAX_SRC" \
        -hide-all-packages \
        -package base -package bytestring -package containers -package deepseq \
        -package directory -package filepath -package pretty -package process \
        -package time -package unix -package array -package binary -package text \
        -package parsec -package mtl -package transformers \
        "$CABAL_SRC/Distribution/Simple.hs" \
        -no-link > /dev/null 2>&1
    local exit_code=$?
    end=$(python3 -c "import time; print(time.time())")
    elapsed=$(python3 -c "print(f'{$end - $start:.3f}')")
    if [ $exit_code -ne 0 ]; then
        echo "$label: FAILED (exit $exit_code)"
    else
        echo "$label: ${elapsed}s"
    fi
}

for JOBS in "-j1" "-j8"; do
    echo "=== Jobs: $JOBS ==="
    echo ""

    echo "--- With -fpipe-asm $JOBS ---"
    for i in $(seq 1 $RUNS); do
        bench_cabal "$JOBS -fpipe-asm" "pipe-asm run $i"
    done

    echo ""
    echo "--- With -fno-pipe-asm $JOBS (baseline) ---"
    for i in $(seq 1 $RUNS); do
        bench_cabal "$JOBS -fno-pipe-asm" "no-pipe-asm run $i"
    done
    echo ""
done

echo "=== Done ==="
