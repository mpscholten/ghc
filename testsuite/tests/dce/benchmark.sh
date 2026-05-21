#!/bin/bash
set -e

GHC="/Users/marc/digitallyinduced/ghc3/_build/stage0/bin/ghc"
SRC="T_dce_benchmark.hs"
RUNS=5

echo "=== Whole-Program DCE Benchmark ==="
echo ""

# Clean up
rm -f T_dce_benchmark_nodce T_dce_benchmark_dce T_dce_benchmark.o T_dce_benchmark.hi

echo "--- Compile WITHOUT DCE ---"
total_nodce=0
for i in $(seq 1 $RUNS); do
    rm -f T_dce_benchmark_nodce T_dce_benchmark.o T_dce_benchmark.hi
    start=$(python3 -c 'import time; print(time.time())')
    $GHC -fforce-recomp $SRC -o T_dce_benchmark_nodce 2>/dev/null
    end=$(python3 -c 'import time; print(time.time())')
    elapsed=$(python3 -c "print(f'{($end - $start)*1000:.1f}')")
    echo "  Run $i: ${elapsed}ms"
    total_nodce=$(python3 -c "print($total_nodce + $end - $start)")
done
avg_nodce=$(python3 -c "print(f'{($total_nodce / $RUNS)*1000:.1f}')")
size_nodce=$(stat -f%z T_dce_benchmark_nodce 2>/dev/null || stat -c%s T_dce_benchmark_nodce)
echo "  Average: ${avg_nodce}ms"
echo "  Binary size: $size_nodce bytes"
echo ""

echo "--- Compile WITH DCE ---"
total_dce=0
for i in $(seq 1 $RUNS); do
    rm -f T_dce_benchmark_dce T_dce_benchmark.o T_dce_benchmark.hi
    start=$(python3 -c 'import time; print(time.time())')
    $GHC -fwhole-program-dce -fforce-recomp $SRC -o T_dce_benchmark_dce 2>&1 | grep -v "^Whole-program"
    end=$(python3 -c 'import time; print(time.time())')
    elapsed=$(python3 -c "print(f'{($end - $start)*1000:.1f}')")
    echo "  Run $i: ${elapsed}ms"
    total_dce=$(python3 -c "print($total_dce + $end - $start)")
done
avg_dce=$(python3 -c "print(f'{($total_dce / $RUNS)*1000:.1f}')")
size_dce=$(stat -f%z T_dce_benchmark_dce 2>/dev/null || stat -c%s T_dce_benchmark_dce)
echo "  Average: ${avg_dce}ms"
echo "  Binary size: $size_dce bytes"
echo ""

# Show DCE stats
echo "--- DCE Statistics ---"
$GHC -fwhole-program-dce -fforce-recomp $SRC -o T_dce_benchmark_dce 2>&1 | grep -A4 "Whole-program DCE"

echo ""
echo "--- Summary ---"
speedup=$(python3 -c "print(f'{(($avg_nodce - $avg_dce) / $avg_nodce * 100):.1f}' if $avg_nodce > 0 else '0')")
size_reduction=$(python3 -c "print(f'{(($size_nodce - $size_dce) / $size_nodce * 100):.1f}')")
echo "Compile time: ${avg_nodce}ms -> ${avg_dce}ms (${speedup}% change)"
echo "Binary size: $size_nodce -> $size_dce bytes (${size_reduction}% reduction)"

# Verify correctness
echo ""
echo "--- Verification ---"
./T_dce_benchmark_nodce > /tmp/out_nodce.txt
./T_dce_benchmark_dce > /tmp/out_dce.txt
if diff /tmp/out_nodce.txt /tmp/out_dce.txt > /dev/null; then
    echo "Output matches: PASS"
    cat /tmp/out_dce.txt
else
    echo "Output differs: FAIL"
    diff /tmp/out_nodce.txt /tmp/out_dce.txt
fi

# Object file comparison (uses linked compilation to trigger DCE)
echo ""
echo "--- Object file comparison ---"
# Get object files from the linked builds (DCE only works in --make mode)
rm -f T_dce_benchmark.o T_dce_benchmark.hi
$GHC -fforce-recomp $SRC -o /tmp/nodce_tmp 2>/dev/null
nodce_size=$(stat -f%z T_dce_benchmark.o 2>/dev/null || stat -c%s T_dce_benchmark.o)
nodce_dead=$(nm T_dce_benchmark.o 2>/dev/null | grep "_Main_dead" | wc -l | tr -d ' ')
rm -f T_dce_benchmark.o T_dce_benchmark.hi
$GHC -fwhole-program-dce -fforce-recomp $SRC -o /tmp/dce_tmp 2>/dev/null
dce_size=$(stat -f%z T_dce_benchmark.o 2>/dev/null || stat -c%s T_dce_benchmark.o)
dce_dead=$(nm T_dce_benchmark.o 2>/dev/null | grep "_Main_dead" | wc -l | tr -d ' ')
obj_reduction=$(python3 -c "print(f'{(($nodce_size - $dce_size) / $nodce_size * 100):.1f}')")
echo "Without DCE: $nodce_size bytes, $nodce_dead dead function symbols"
echo "With DCE:    $dce_size bytes, $dce_dead dead function symbols"
echo "Object file reduction: ${obj_reduction}%"
