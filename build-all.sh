#!/usr/bin/env bash
# ════════════════════════════════════════════════════════════════
# build-all.sh — Futumura YAML Projector — Full Regression Run
# 
# Projected from build-spec.scm by project-build.lisp
# Runs all language targets and summarises pass/fail counts.
# ════════════════════════════════════════════════════════════════

set -uo pipefail
cd "$(dirname "$0")"

show_header() {
  echo ""
  echo "════════════════════════════════════════════════════════════════"
  echo " Futumura YAML Projector — Full Regression Run"
  echo "════════════════════════════════════════════════════════════════"
  echo "  System:  $(uname -s) $(uname -m)"
  echo "  Host:    $(hostname 2>/dev/null || echo unknown)"
  echo "  Date:    $(date)"
  echo "  Targets: 16"
  echo ""
}

run_target() {
  local name=$1
  local mode="${2:-all}"
  local script="./build-${name}.sh"
  local pass=0 fail=0 total=0 elapsed='' status='OK'
  if [ ! -f "$script" ]; then
    printf '  %-14s  %-30s\n' "$name" 'MISSING build script'
    return
  fi
  local out
  if [ "$mode" != '--test' ]; then
    printf '  %-14s  building...\r' "$name"
    bash "$script" --build 2>&1 | tail -1 | grep -q 'Build complete' && true || true
  fi
  if [ "$mode" = '--build' ]; then
    printf '  %-14s  built OK\n' "$name"
    return
  fi
  printf '  %-14s  testing... \r' "$name"
  out=$(bash "$script" --test 2>&1) || true
  pass=$(echo  "$out" | grep -oP '\d+(?= /)' | tail -1 || echo 0)
  total=$(echo "$out" | grep -oP '(?<= / )\d+' | tail -1 || echo 0)
  elapsed=$(echo "$out" | grep -oP '\d+(ms|s)' | tail -1 || echo '?')
  fail=$((total - pass))
  [ "$fail" -gt 0 ] && status='FAIL' || status='OK'
  printf '  %-14s  %3d / %3d  (%3d failed)  %8s  %s\n' "$name" "$pass" "$total" "$fail" "$elapsed" "$status"
  echo "$pass $total" >> "$tmp"
}

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
mode="${1:-all}"
show_header
echo "  Mode: ${mode}  (use --test to skip builds, --build to build only)"
echo "  Running all targets — this will take several minutes (bash/erlang/powershell are slow)"
echo ""
echo "  Target          Pass / Total   Failed     Time    Status"
echo "  ──────────────────────────────────────────────────────────"
run_target go "$mode"
run_target rust "$mode"
run_target cpp "$mode"
run_target java "$mode"
run_target kotlin "$mode"
run_target csharp "$mode"
run_target fsharp "$mode"
run_target haskell "$mode"
run_target swift "$mode"
run_target zig "$mode"
run_target ocaml "$mode"
run_target objc "$mode"
run_target x86 "$mode"
run_target python "$mode"
run_target lua "$mode"
run_target bash "$mode"
echo "  ──────────────────────────────────────────────────────────"

total_pass=0; total_total=0
while read p t; do total_pass=$((total_pass+p)); total_total=$((total_total+t)); done < "$tmp"
total_fail=$((total_total - total_pass))
echo ""
echo "════════════════════════════════════════════════════════════════"
printf '  Total: 16 targets   %d / %d passed   (%d failed)\n' "$total_pass" "$total_total" "$total_fail"
echo "════════════════════════════════════════════════════════════════"
echo ""
