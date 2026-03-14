# yaml-project TODO
`johnagrillo62/yaml-project` — Futamura-style projector, YAML + JSON grammars → 17 language targets

---

## JSON Projector — Status

| Target   | Score    | Status                  |
|----------|----------|-------------------------|
| Python   | 283/318  | ✅ 0 failures            |
| Lua      | 283/318  | ✅ 0 failures            |
| Bash     | 283/318  | ✅ 0 failures            |
| C++      | 283/318  | ✅ 0 failures            |
| C#       | 283/318  | ✅ 0 failures            |
| F#       | 283/318  | ✅ 0 failures            |
| Go       | 283/318  | ✅ 0 failures            |
| Haskell  | 283/318  | ✅ 0 failures            |
| Rust     | 283/318  | ✅ 0 failures            |
| Swift    | 283/318  | ✅ 0 failures            |
| OCaml    | 283/318  | ✅ 0 failures            |
| Zig      | 281/318  | ⚠️ 2 failures (deferred) |
| Kotlin   | —        | 🔲 not yet run           |
| ObjC     | —        | ⛔ deferred (platform)   |
| x86      | —        | ⛔ excluded intentionally |

---

## Deferred — Platform Issues

### Zig — WSL stack overflow
- **Tests:** `n_structure_100000_opening_arrays.json`, `n_structure_open_array_object.json`
- **Cause:** WSL ignores `pthread_attr_setstacksize` — 64MB thread stack request silently dropped, default ~8MB blows on 100k-deep nesting
- **Fix:** Add `depth: usize = 0` to `G` struct + depth guard in `json_array` / `json_object` / `value` functions; reject when `depth > 10000`
- **Files:** `spec/json-peg-zig.lisp`, `gen/peg_json.zig`
- **Defer until:** native Linux or Zig upgrade

### ObjC — GNUstep blocks segfault
- **Cause:** GNUstep blocks runtime crashes on Linux
- **Fix:** macOS only — test with Apple clang
- **Files:** `spec/json-peg-objc.lisp`, `gen/PegJson.m`
- **Defer until:** macOS runner

---

## In Progress

### Kotlin
- `gen/PegJson.kt` generated, not yet run
- **Next:** wire up `build-json-kotlin.sh` and run test suite

---

## PDF Projector

- `grammar/pdf-grammar.scm` — written (34 rules)
- `build-pdf.lisp` — written, not yet run
- **Next:** first run, debug output

---

## Emitter Fixes Made (today)
- `emit/emit-yaml-peg.lisp` — `ce-char` characterp branch
- `emit/emit-yaml-peg.lisp` — `ce-hex` decimal-as-hex
- `emit/emit-yaml-peg.lisp` — `ce-range` same fixes
- `spec/json-peg-zig.lisp` — `peg_alt` `g.failed = false` fix
- `spec/json-peg-zig.lisp` — thread spawn for stack (partial — WSL limitation)
