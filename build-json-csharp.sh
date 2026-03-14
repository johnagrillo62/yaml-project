#!/usr/bin/env bash
# ════════════════════════════════════════════════════════════════
# build-csharp.sh — Csharp YAML 1.2 parser demo
# 
# Projected from build-spec.scm by the Futumura YAML Projector.
# The parser was generated from the YAML 1.2 spec's 211 grammar rules.
# 
# Source: gen/PegJson.cs
# Spec:   spec/json-peg-csharp.lisp
# Binary: bin/csharp
# ════════════════════════════════════════════════════════════════

set -euo pipefail
cd "$(dirname "$0")"

show_system() {
  echo "════════════════════════════════════════════════════"
  echo " Csharp YAML Parser — Futumura Projector Demo"
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
  if command -v mcs >/dev/null 2>&1; then
    echo "  ✓ mcs $(mcs --version 2>&1 | head -1)"
  else
    echo '  ✗ mcs not found'
    echo '    Install: apt install mono-mcs  OR  brew install mono'
    fail=1
  fi
  if command -v mono >/dev/null 2>&1; then
    echo "  ✓ mono $(mono --version 2>&1 | head -1)"
  else
    echo '  ✗ mono not found'
    echo '    Install: apt install mono-runtime  OR  brew install mono'
    fail=1
  fi
  if [ -f gen/PegJson.cs ]; then
    echo "  ✓ gen/PegJson.cs ($(wc -l < gen/PegJson.cs) lines)"
  else
    echo '  ✗ gen/PegJson.cs not found'
    echo '    Run: sbcl --load build-yaml.lisp --quit'
    fail=1
  fi
  if [ -f spec/json-peg-csharp.lisp ]; then
    echo "  ✓ spec/json-peg-csharp.lisp ($(wc -l < spec/json-peg-csharp.lisp) lines)"
  else
    echo '  ✗ spec/json-peg-csharp.lisp not found'
    echo '    Check spec/ directory'
    fail=1
  fi
  if [ -d JSONTestSuite/test_parsing ]; then
    echo "  ✓ JSONTestSuite/test_parsing/ ($(find JSONTestSuite/test_parsing -name '*.json' | wc -l) tests)"
  else
    echo '  ✗ JSONTestSuite/test_parsing/ not found'
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

do_build() {
  echo 'Building...'
  mkdir -p bin
  mcs -out:bin/csharp.exe gen/PegJson.cs
  echo '  ✓ Build complete'
  echo 
}

do_test() {
  echo 'Running JSON test suite...'
  echo "  Started: $(date)"
  echo 
  pass=0
  fail=0
  skip=0
  total=0
  start_time=$(date +%s%N 2>/dev/null || date +%s)
  for f in JSONTestSuite/test_parsing/y_*.json; do
    [ -f "$f" ] || continue
    total=$(($total+1))
    printf '\r\033[K  %d: %s' "$total" "$(basename "$f")"
    rc=0; (timeout 1 mono bin/csharp.exe "$f" >/dev/null 2>&1) || rc=$?
    [ $rc -eq 0 ] && pass=$((pass+1)) || fail=$((fail+1))
  done
  for f in JSONTestSuite/test_parsing/n_*.json; do
    [ -f "$f" ] || continue
    total=$(($total+1))
    printf '\r\033[K  %d: %s' "$total" "$(basename "$f")"
    rc=0; (timeout 1 mono bin/csharp.exe "$f" >/dev/null 2>&1) || rc=$?
    [ $rc -ne 0 ] && pass=$((pass+1)) || fail=$((fail+1))
  done
  for f in JSONTestSuite/test_parsing/i_*.json; do
    [ -f "$f" ] || continue
    total=$(($total+1))
    skip=$(($skip+1))
  done
  printf '\r\033[K'
  end_time=$(date +%s%N 2>/dev/null || date +%s)
  if [ ${#start_time} -gt 10 ]; then elapsed=$(( (end_time - start_time) / 1000000 )); unit=ms; else elapsed=$((end_time - start_time)); unit=s; fi
  echo "════════════════════════════════════════════════════"
  echo " Csharp: $pass / $total passed  ($fail failed, $skip impl-defined)  ${elapsed}${unit}"
  echo "════════════════════════════════════════════════════"
  echo 
  echo 'Note: i_* tests are implementation-defined — both accept and reject are valid.'
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
  --build)
    show_system
    do_check
    do_build
  ;;
  --test)
    show_system
    do_test
  ;;
  "")
    show_system
    do_check
    do_build
    do_test
  ;;
  *)
    echo "Unknown: $1" >&2
    exit 1
  ;;
esac
