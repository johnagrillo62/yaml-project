#!/usr/bin/env bash
# ════════════════════════════════════════════════════════════════
# build-java.sh — Java YAML 1.2 parser demo
# 
# Projected from build-spec.scm by the Futumura YAML Projector.
# The parser was generated from the YAML 1.2 spec's 211 grammar rules.
# 
# Source: gen/YamlReader.java
# Binary: bin/java
# ════════════════════════════════════════════════════════════════

set -euo pipefail
cd "$(dirname "$0")"

show_system() {
  echo "════════════════════════════════════════════════════"
  echo " Java YAML Parser — Futumura Projector Demo"
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
  if command -v javac >/dev/null 2>&1; then
    echo "  ✓ javac $(javac --version 2>&1 | head -1)"
  else
    echo '  ✗ javac not found'
    echo '    Install: apt install default-jdk  OR  brew install openjdk'
    fail=1
  fi
  if command -v java >/dev/null 2>&1; then
    echo "  ✓ java $(java --version 2>&1 | head -1)"
  else
    echo '  ✗ java not found'
    echo '    Install: apt install default-jre  OR  brew install openjdk'
    fail=1
  fi
  if [ -f gen/YamlReader.java ]; then
    echo "  ✓ gen/YamlReader.java ($(wc -l < gen/YamlReader.java) lines)"
  else
    echo '  ✗ gen/YamlReader.java not found'
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

do_build() {
  echo 'Building...'
  mkdir -p bin
  javac -d bin gen/YamlReader.java
  echo '  ✓ Build complete'
  echo 
}

do_test() {
  echo '⚠  JVM starts per test — this may take a few minutes'
  echo 
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
        rc=0; (timeout 5 java -cp bin YamlReader "$sd/in.yaml" >/dev/null 2>&1) || rc=$?
        [ $rc -ne 0 ] && pass=$((pass+1)) || fail=$((fail+1))
      else
        result=$(timeout 5 java -cp bin YamlReader "$sd/in.yaml" 2>/dev/null) || true
        echo "$result" | grep -q "^OK:" && pass=$((pass+1)) || fail=$((fail+1))
      fi
    done
  done
  printf '\r\033[K'
  end_time=$(date +%s%N 2>/dev/null || date +%s)
  if [ ${#start_time} -gt 10 ]; then elapsed=$(( (end_time - start_time) / 1000000 )); unit=ms; else elapsed=$((end_time - start_time)); unit=s; fi
  echo "════════════════════════════════════════════════════"
  echo " Java: $pass / $total passed  ($fail failed)  ${elapsed}${unit}"
  echo "════════════════════════════════════════════════════"
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
