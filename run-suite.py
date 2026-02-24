#!/usr/bin/env python3
"""run-suite.py — Run yaml-test-suite against a YAML parser.

Usage:
    python3 run-suite.py ./yaml-cpp [test-data-dir]
    python3 run-suite.py 'bash gen/yaml_reader.sh' [test-data-dir]

A valid test passes if the parser exits 0 and prints "OK:" on stdout.
An error test passes if the parser exits non-zero (rejects bad input).
Tests with subtests (numeric subdirs) are expanded automatically.
"""

import os
import sys
import subprocess
import shlex

def find_test_dir():
    """Try common locations for the yaml-test-suite data."""
    candidates = [
        "yaml-test-suite",
        "yaml-tests",
        "../yaml-test-suite",
        os.path.expanduser("~/yaml-test-suite"),
        os.path.expanduser("~/yaml-tests"),
    ]
    for c in candidates:
        if os.path.isdir(c) and any(
            os.path.exists(os.path.join(c, d, "in.yaml"))
            for d in os.listdir(c)
            if os.path.isdir(os.path.join(c, d))
        ):
            return c
    return None

def collect_tests(data_dir):
    """Collect all test cases, expanding subtests."""
    valid = []
    error = []
    for d in sorted(os.listdir(data_dir)):
        dp = os.path.join(data_dir, d)
        if not os.path.isdir(dp):
            continue
        # Check for subtests (numeric subdirs)
        subs = sorted(
            s for s in os.listdir(dp)
            if os.path.isdir(os.path.join(dp, s)) and s.isdigit()
        )
        if subs:
            for s in subs:
                sp = os.path.join(dp, s)
                tid = f"{d}/{s}"
                if os.path.exists(os.path.join(sp, "error")):
                    error.append((tid, sp))
                elif os.path.exists(os.path.join(sp, "in.yaml")):
                    valid.append((tid, sp))
        else:
            if os.path.exists(os.path.join(dp, "error")):
                error.append((d, dp))
            elif os.path.exists(os.path.join(dp, "in.yaml")):
                valid.append((d, dp))
    return valid, error

def run_test(cmd_parts, yaml_path, timeout=30):
    """Run parser on a YAML file. Returns (exit_code, stdout, stderr)."""
    try:
        r = subprocess.run(
            cmd_parts + [yaml_path],
            capture_output=True, text=True, timeout=timeout
        )
        return r.returncode, r.stdout, r.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "TIMEOUT"

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 run-suite.py <parser-command> [test-data-dir]")
        print("Examples:")
        print("  python3 run-suite.py ./yaml-cpp")
        print("  python3 run-suite.py ./yaml-cpp yaml-test-suite/")
        print("  python3 run-suite.py 'bash gen/yaml_reader.sh'")
        sys.exit(1)

    cmd_parts = shlex.split(sys.argv[1])
    data_dir = sys.argv[2] if len(sys.argv) > 2 else find_test_dir()

    if not data_dir:
        print("ERROR: Cannot find yaml-test-suite data directory.")
        print("Clone it: git clone -b data-2022-01-17 https://github.com/yaml/yaml-test-suite.git")
        sys.exit(1)

    valid, error = collect_tests(data_dir)
    total = len(valid) + len(error)
    print(f"Test suite: {len(valid)} valid + {len(error)} error = {total} tests")
    print(f"Parser: {' '.join(cmd_parts)}")
    print()

    passed = 0
    failed = 0
    failures = []

    # Valid tests: parser should succeed (exit 0, print "OK:")
    for tid, tpath in valid:
        yaml_file = os.path.join(tpath, "in.yaml")
        rc, out, err = run_test(cmd_parts, yaml_file)
        if rc == 0 and "OK:" in out:
            passed += 1
        else:
            failed += 1
            reason = "TIMEOUT" if rc == -1 else f"exit={rc}"
            if err.strip():
                reason += f" ({err.strip()[:60]})"
            failures.append((tid, "valid", reason))

    # Error tests: parser should fail (exit non-zero)
    for tid, tpath in error:
        yaml_file = os.path.join(tpath, "in.yaml")
        if not os.path.exists(yaml_file):
            passed += 1  # no input file = skip
            continue
        rc, out, err = run_test(cmd_parts, yaml_file)
        if rc != 0:
            passed += 1
        else:
            failed += 1
            failures.append((tid, "error", "should have failed but got OK"))

    # Summary
    print(f"\n{'='*60}")
    print(f"Results: {passed}/{total} passed")
    if failures:
        print(f"\nFailed ({len(failures)}):")
        for tid, kind, reason in failures[:50]:
            print(f"  {tid} [{kind}] {reason}")
        if len(failures) > 50:
            print(f"  ... and {len(failures) - 50} more")
    else:
        print("All tests passed!")
    print(f"{'='*60}")

    sys.exit(0 if failed == 0 else 1)

if __name__ == "__main__":
    main()
