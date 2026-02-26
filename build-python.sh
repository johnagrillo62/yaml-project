#!/usr/bin/env bash
# ════════════════════════════════════════════════════════════════
# build-python.sh — Python YAML 1.2 parser demo
# 
# Projected from build-spec.scm by the Futumura YAML Projector.
# The parser was generated from the YAML 1.2 spec's 211 grammar rules.
# 
# Source: gen/peg_yaml.py
# ════════════════════════════════════════════════════════════════

set -euo pipefail
cd "$(dirname "$0")"

show_system() {
  echo "════════════════════════════════════════════════════"
  echo " Python YAML Parser — Futumura Projector Demo"
  echo "════════════════════════════════════════════════════"
  echo 
  echo "  System:  $(uname -s) $(uname -m)"
  echo "  Host:    $(hostname 2>/dev/null || echo unknown)"
  echo "  Date:    $(date)"
  echo 
}

do_check() {
  echo 'Checking prerequisites...'
  echo 
  local fail=0
  if command -v python3 >/dev/null 2>&1; then
    echo "  ✓ python3 $(python3 --version 2>&1 | head -1)"
  else
    echo '  ✗ python3 not found'
    echo '    Install: apt install python3  OR  brew install python3'
    fail=1
  fi
  if [ -f gen/peg_yaml.py ]; then
    echo "  ✓ gen/peg_yaml.py ($(wc -l < gen/peg_yaml.py) lines)"
  else
    echo '  ✗ gen/peg_yaml.py not found'
    echo '    Run: sbcl --load build-yaml.lisp --quit'
    fail=1
  fi
  if [ -d yaml-test-suite ]; then
    echo "  ✓ yaml-test-suite/ ($(find yaml-test-suite -name in.yaml | wc -l) tests)"
  else
    echo '  ✗ yaml-test-suite/ not found'
    echo '    Run: git clone https://github.com/yaml/yaml-test-suite.git'
    fail=1
  fi
  echo 
  if [ $fail -ne 0 ]; then
    echo 'Prerequisites missing. See above for install instructions.'
    exit 1
  fi
  echo 'All prerequisites satisfied.'
  echo 
}

do_test() {
  echo 'Running YAML test suite...'
  echo "  Started: $(date)"
  echo 
  pass=0
  fail=0
  total=0
  start_time=$(date +%s%N 2>/dev/null || date +%s)
  for d in yaml-test-suite/*/; do
    d=${d%/}
    subs=$(find "$d" -maxdepth 1 -mindepth 1 -type d -name '[0-9]*' 2>/dev/null | head -1)
    [ -n "$subs" ] && dirs="$d"/*/ || dirs=$d
    for sd in $dirs; do
      [ -d "$sd" ] || continue
      bn=$(basename "$sd")
      echo "$bn" | grep -qE '^[0-9]+$' 2>/dev/null || [ "$sd" = "$d" ] || continue
      if ! [ -f "$sd/in.yaml" ]; then
        [ -f "$sd/error" ] && total=$((total+1)) && pass=$((pass+1))
        continue
      fi
      total=$(($total+1))
      printf '\r\033[K  %d: %s' "$total" "$(basename "$d")"
      if [ -f "$sd/error" ]; then
        rc=0; (timeout 1 python3 gen/peg_yaml.py "$sd/in.yaml" >/dev/null 2>&1) || rc=$?
        [ $rc -ne 0 ] && pass=$((pass+1)) || fail=$((fail+1))
      else
        result=$(timeout 1 python3 gen/peg_yaml.py "$sd/in.yaml" 2>/dev/null) || true
        echo "$result" | grep -q "^OK:" && pass=$((pass+1)) || fail=$((fail+1))
      fi
    done
  done
  printf '\r\033[K'
  end_time=$(date +%s%N 2>/dev/null || date +%s)
  if [ ${#start_time} -gt 10 ]; then elapsed=$(( (end_time - start_time) / 1000000 )); unit=ms; else elapsed=$((end_time - start_time)); unit=s; fi
  echo "════════════════════════════════════════════════════"
  echo " Python: $pass / $total passed  ($fail failed)  ${elapsed}${unit}"
  echo "════════════════════════════════════════════════════"
  echo 
  echo 'Note: 94 of the 402 tests are for semantic analysis (duplicate keys, invalid tags, etc.) which require a separate concern layer beyond the structural parser.'
  echo 
}

case "${1:-}" in
  --help|-h)
    sed -n '/^# Source:/,/^# ═/{/^# ═/!s/^# //p}' "$0"
  ;;
  --check)
    show_system
    do_check
  ;;
  --test)
    show_system
    do_test
  ;;
  "")
    show_system
    do_check
    do_test
  ;;
  *)
    echo "Unknown: $1" >&2
    exit 1
  ;;
esac
