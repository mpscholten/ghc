#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: utils/repro-mr15378.sh [options]

Reproduces MR 15378 interface-hash nondeterminism signals:
1) false recompilation on unchanged build ("[A changed]")
2) -j1 vs -j2 producing different B.hi output

Options:
  --stage1-ghc PATH   Path to stage1 ghc (default: <repo>/_build/stage1/bin/ghc)
  --system-ghc PATH   Path to system ghc (default: command -v ghc)
  --out-dir PATH      Output directory (default: /tmp/ghc-mr15378-vs-system-<timestamp>)
  --no-system         Skip system ghc run; only run stage1
  -h, --help          Show this help text
EOF
}

require_cmd() {
  local cmd="$1"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "error: required command not found: $cmd" >&2
    exit 1
  fi
}

sha256_of_file() {
  local file="$1"
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$file" | awk '{print $1}'
  else
    shasum -a 256 "$file" | awk '{print $1}'
  fi
}

bool_eq() {
  local a="$1"
  local b="$2"
  if [[ "$a" == "$b" ]]; then
    echo "YES"
  else
    echo "NO"
  fi
}

bool_neq() {
  local a="$1"
  local b="$2"
  if [[ "$a" != "$b" ]]; then
    echo "YES"
  else
    echo "NO"
  fi
}

bool_nonempty() {
  local x="$1"
  if [[ -n "$x" ]]; then
    echo "YES"
  else
    echo "NO"
  fi
}

bool_neq_nonempty() {
  local a="$1"
  local b="$2"
  if [[ -n "$a" && -n "$b" && "$a" != "$b" ]]; then
    echo "YES"
  else
    echo "NO"
  fi
}

extract_commit() {
  local ghc_bin="$1"
  "$ghc_bin" --info \
    | rg -o 'Project Git commit id","[0-9a-f]+' \
    | head -n1 \
    | awk -F'","' '{print $2}' || true
}

extract_iface_hash() {
  local ghc_bin="$1"
  local hi="$2"
  "$ghc_bin" --show-iface "$hi" | sed -n 's/^  interface hash: //p' | head -n1
}

extract_abi_hash() {
  local ghc_bin="$1"
  local hi="$2"
  "$ghc_bin" --show-iface "$hi" | sed -n 's/^  ABI hash: //p' | head -n1
}

extract_usage_main_a() {
  local ghc_bin="$1"
  local hi="$2"
  "$ghc_bin" --show-iface "$hi" | rg -o 'main:A [0-9a-f]+' -m1 | awk '{print $2}' || true
}

run_repro() {
  local tag="$1"
  local ghc_bin="$2"
  local out="$OUT_DIR/$tag"

  mkdir -p "$out"
  cd "$out"

  cat > A.hs <<'EOF'
module A (f) where
f :: Int
f = 1
EOF

  cat > B.hs <<'EOF'
module B where
import A
b :: Int
b = f + 1
EOF

  local version
  version="$("$ghc_bin" --numeric-version)"
  local commit
  commit="$(extract_commit "$ghc_bin")"

  "$ghc_bin" --make -O -j2 -fforce-recomp B.hs -v0
  local a_sha_first b_sha_first
  a_sha_first="$(sha256_of_file A.hi)"
  b_sha_first="$(sha256_of_file B.hi)"

  local a_iface a_abi b_use_first_j2
  a_iface="$(extract_iface_hash "$ghc_bin" A.hi)"
  a_abi="$(extract_abi_hash "$ghc_bin" A.hi)"
  b_use_first_j2="$(extract_usage_main_a "$ghc_bin" B.hi)"

  "$ghc_bin" --make -O -j2 B.hs -v1 > second-build.log 2>&1 || true
  local a_sha_second b_sha_second second_signal
  a_sha_second="$(sha256_of_file A.hi)"
  b_sha_second="$(sha256_of_file B.hi)"
  second_signal="$(rg -n 'Compiling B \[A changed\]' second-build.log | head -n1 || true)"

  "$ghc_bin" --make -O -j1 -fforce-recomp B.hs -v0
  cp B.hi B.j1.hi
  "$ghc_bin" --make -O -j2 -fforce-recomp B.hs -v0
  cp B.hi B.j2.hi

  local b_j1_sha b_j2_sha b_use_j1 b_use_j2
  b_j1_sha="$(sha256_of_file B.j1.hi)"
  b_j2_sha="$(sha256_of_file B.j2.hi)"
  b_use_j1="$(extract_usage_main_a "$ghc_bin" B.j1.hi)"
  b_use_j2="$(extract_usage_main_a "$ghc_bin" B.j2.hi)"

  printf '%s\n' "$second_signal" > second-signal.txt
  cat > metrics.env <<EOF
TAG=$tag
GHC_BIN=$ghc_bin
GHC_VERSION=$version
GHC_COMMIT=$commit
A_IFACE=$a_iface
A_ABI=$a_abi
B_USE_FIRST_J2=$b_use_first_j2
BUG1_HASH_MISMATCH=$(bool_neq "$a_iface" "$b_use_first_j2")
BUG1_REBUILD_SIGNAL=$(bool_nonempty "$second_signal")
A_SHA_FIRST=$a_sha_first
A_SHA_SECOND=$a_sha_second
B_SHA_FIRST=$b_sha_first
B_SHA_SECOND=$b_sha_second
BUG1_A_UNCHANGED=$(bool_eq "$a_sha_first" "$a_sha_second")
BUG1_B_CHANGED=$(bool_neq "$b_sha_first" "$b_sha_second")
B_J1_SHA=$b_j1_sha
B_J2_SHA=$b_j2_sha
B_USE_J1=$b_use_j1
B_USE_J2=$b_use_j2
BUG2_HI_DIFF=$(bool_neq "$b_j1_sha" "$b_j2_sha")
BUG2_USAGE_DIFF=$(bool_neq_nonempty "$b_use_j1" "$b_use_j2")
EOF
}

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
TIMESTAMP="$(date +%Y%m%d-%H%M%S)"

STAGE1_GHC="$REPO_ROOT/_build/stage1/bin/ghc"
SYSTEM_GHC="$(command -v ghc || true)"
OUT_DIR="/tmp/ghc-mr15378-vs-system-$TIMESTAMP"
RUN_SYSTEM=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    --stage1-ghc)
      STAGE1_GHC="$2"
      shift 2
      ;;
    --system-ghc)
      SYSTEM_GHC="$2"
      shift 2
      ;;
    --out-dir)
      OUT_DIR="$2"
      shift 2
      ;;
    --no-system)
      RUN_SYSTEM=0
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

require_cmd rg
require_cmd sed
require_cmd awk
if ! command -v sha256sum >/dev/null 2>&1 && ! command -v shasum >/dev/null 2>&1; then
  echo "error: require either sha256sum or shasum" >&2
  exit 1
fi

if [[ ! -x "$STAGE1_GHC" ]]; then
  echo "error: stage1 ghc not found or not executable: $STAGE1_GHC" >&2
  exit 1
fi

if [[ $RUN_SYSTEM -eq 1 && -z "${SYSTEM_GHC:-}" ]]; then
  echo "warning: system ghc not found on PATH; proceeding with stage1 only" >&2
  RUN_SYSTEM=0
fi

mkdir -p "$OUT_DIR"

run_repro "stage1" "$STAGE1_GHC"

if [[ $RUN_SYSTEM -eq 1 && "$SYSTEM_GHC" != "$STAGE1_GHC" ]]; then
  run_repro "system" "$SYSTEM_GHC"
  HAS_SYSTEM=1
else
  HAS_SYSTEM=0
fi

REPO_HEAD="$(git -C "$REPO_ROOT" rev-parse HEAD)"
MR_HEAD="$(git -C "$REPO_ROOT" rev-parse refs/remotes/origin/mr-15378 2>/dev/null || echo N/A)"

source "$OUT_DIR/stage1/metrics.env"
STAGE1_GHC_BIN="$GHC_BIN"
STAGE1_GHC_VERSION="$GHC_VERSION"
STAGE1_GHC_COMMIT="$GHC_COMMIT"
STAGE1_A_IFACE="$A_IFACE"
STAGE1_A_ABI="$A_ABI"
STAGE1_B_USE_FIRST_J2="$B_USE_FIRST_J2"
STAGE1_BUG1_HASH_MISMATCH="$BUG1_HASH_MISMATCH"
STAGE1_BUG1_REBUILD_SIGNAL="$BUG1_REBUILD_SIGNAL"
STAGE1_A_SHA_FIRST="$A_SHA_FIRST"
STAGE1_A_SHA_SECOND="$A_SHA_SECOND"
STAGE1_B_SHA_FIRST="$B_SHA_FIRST"
STAGE1_B_SHA_SECOND="$B_SHA_SECOND"
STAGE1_BUG1_A_UNCHANGED="$BUG1_A_UNCHANGED"
STAGE1_BUG1_B_CHANGED="$BUG1_B_CHANGED"
STAGE1_B_J1_SHA="$B_J1_SHA"
STAGE1_B_J2_SHA="$B_J2_SHA"
STAGE1_B_USE_J1="$B_USE_J1"
STAGE1_B_USE_J2="$B_USE_J2"
STAGE1_BUG2_HI_DIFF="$BUG2_HI_DIFF"
STAGE1_BUG2_USAGE_DIFF="$BUG2_USAGE_DIFF"
STAGE1_SIGNAL_LINE="$(cat "$OUT_DIR/stage1/second-signal.txt")"

if [[ $HAS_SYSTEM -eq 1 ]]; then
  source "$OUT_DIR/system/metrics.env"
  SYSTEM_GHC_BIN="$GHC_BIN"
  SYSTEM_GHC_VERSION="$GHC_VERSION"
  SYSTEM_GHC_COMMIT="$GHC_COMMIT"
  SYSTEM_A_IFACE="$A_IFACE"
  SYSTEM_A_ABI="$A_ABI"
  SYSTEM_B_USE_FIRST_J2="$B_USE_FIRST_J2"
  SYSTEM_BUG1_HASH_MISMATCH="$BUG1_HASH_MISMATCH"
  SYSTEM_BUG1_REBUILD_SIGNAL="$BUG1_REBUILD_SIGNAL"
  SYSTEM_A_SHA_FIRST="$A_SHA_FIRST"
  SYSTEM_A_SHA_SECOND="$A_SHA_SECOND"
  SYSTEM_B_SHA_FIRST="$B_SHA_FIRST"
  SYSTEM_B_SHA_SECOND="$B_SHA_SECOND"
  SYSTEM_BUG1_A_UNCHANGED="$BUG1_A_UNCHANGED"
  SYSTEM_BUG1_B_CHANGED="$BUG1_B_CHANGED"
  SYSTEM_B_J1_SHA="$B_J1_SHA"
  SYSTEM_B_J2_SHA="$B_J2_SHA"
  SYSTEM_B_USE_J1="$B_USE_J1"
  SYSTEM_B_USE_J2="$B_USE_J2"
  SYSTEM_BUG2_HI_DIFF="$BUG2_HI_DIFF"
  SYSTEM_BUG2_USAGE_DIFF="$BUG2_USAGE_DIFF"
  SYSTEM_SIGNAL_LINE="$(cat "$OUT_DIR/system/second-signal.txt")"
fi

REPORT="$OUT_DIR/comparison-report.txt"
{
  echo "MR15378 Repro Comparison"
  echo "Timestamp: $TIMESTAMP"
  echo "Repo: $REPO_ROOT"
  echo "repo HEAD: $REPO_HEAD"
  echo "origin/mr-15378: $MR_HEAD"
  echo
  echo "Compiler A (stage1)"
  echo "- bin: $STAGE1_GHC_BIN"
  echo "- version: $STAGE1_GHC_VERSION"
  echo "- commit: ${STAGE1_GHC_COMMIT:-N/A}"
  echo

  if [[ $HAS_SYSTEM -eq 1 ]]; then
    echo "Compiler B (system)"
    echo "- bin: $SYSTEM_GHC_BIN"
    echo "- version: $SYSTEM_GHC_VERSION"
    echo "- commit: ${SYSTEM_GHC_COMMIT:-N/A}"
    echo
  fi

  echo "Bug 1 checks (stale/mismatched usage hash + false recomp)"
  echo "- stage1 hash mismatch (A.interface vs B usage): $STAGE1_BUG1_HASH_MISMATCH"
  echo "- stage1 second build has [A changed]: $STAGE1_BUG1_REBUILD_SIGNAL"
  echo "- stage1 signal line: ${STAGE1_SIGNAL_LINE:-<none>}"
  echo "- stage1 A.hi unchanged and B.hi changed: $STAGE1_BUG1_A_UNCHANGED/$STAGE1_BUG1_B_CHANGED"
  if [[ $HAS_SYSTEM -eq 1 ]]; then
    echo "- system hash mismatch (A.interface vs B usage): $SYSTEM_BUG1_HASH_MISMATCH"
    echo "- system second build has [A changed]: $SYSTEM_BUG1_REBUILD_SIGNAL"
    echo "- system signal line: ${SYSTEM_SIGNAL_LINE:-<none>}"
    echo "- system A.hi unchanged and B.hi changed: $SYSTEM_BUG1_A_UNCHANGED/$SYSTEM_BUG1_B_CHANGED"
  fi
  echo

  echo "Bug 2 checks (-j1 vs -j2)"
  echo "- stage1 B.hi differs between -j1 and -j2: $STAGE1_BUG2_HI_DIFF"
  echo "- stage1 usage(main:A) differs between -j1 and -j2: $STAGE1_BUG2_USAGE_DIFF"
  if [[ $HAS_SYSTEM -eq 1 ]]; then
    echo "- system B.hi differs between -j1 and -j2: $SYSTEM_BUG2_HI_DIFF"
    echo "- system usage(main:A) differs between -j1 and -j2: $SYSTEM_BUG2_USAGE_DIFF"
  fi
  echo

  echo "Detail hashes"
  echo "- stage1 A.interface: $STAGE1_A_IFACE"
  echo "- stage1 A.abi: $STAGE1_A_ABI"
  echo "- stage1 B usage(first -j2 build): $STAGE1_B_USE_FIRST_J2"
  echo "- stage1 B.hi sha first/second unchanged build: $STAGE1_B_SHA_FIRST / $STAGE1_B_SHA_SECOND"
  echo "- stage1 B.j1/B.j2 sha: $STAGE1_B_J1_SHA / $STAGE1_B_J2_SHA"
  echo "- stage1 B.j1/B.j2 usage: $STAGE1_B_USE_J1 / $STAGE1_B_USE_J2"
  if [[ $HAS_SYSTEM -eq 1 ]]; then
    echo "- system A.interface: $SYSTEM_A_IFACE"
    echo "- system A.abi: $SYSTEM_A_ABI"
    echo "- system B usage(first -j2 build): $SYSTEM_B_USE_FIRST_J2"
    echo "- system B.hi sha first/second unchanged build: $SYSTEM_B_SHA_FIRST / $SYSTEM_B_SHA_SECOND"
    echo "- system B.j1/B.j2 sha: $SYSTEM_B_J1_SHA / $SYSTEM_B_J2_SHA"
    echo "- system B.j1/B.j2 usage: $SYSTEM_B_USE_J1 / $SYSTEM_B_USE_J2"
  fi
} > "$REPORT"

echo "Wrote report: $REPORT"
echo "Artifacts: $OUT_DIR"
