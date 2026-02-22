# Futumura YAML Projector — Session Continuation

## What We Built

Two projections from one machine:

1. **Grammar → Parser**: `yaml-grammar.scm` (211 PEG rules) → parser in N languages
2. **Concern → Native API**: concern vocab in each `peg-{lang}.lisp` → value types, schema coercion, AST→native conversion, anchor resolution, `load()` function

## Current State: 6 Languages, 332/332 Tests

```
Language   Parser  Concerns  Path             Tests
────────   ──────  ────────  ────             ─────
C++        ✓       ✓         legacy block     332/332
Rust       ✓       ✓         legacy block     332/332
Go         ✓       ✓         cv vocab         332/332
Python     ✓       ✓         cv vocab         332/332
Obj-C      ✓       ✓         cv vocab         332/332
Java       ✓       ✗         direct emitter   332/332
```

## Architecture

```
futumura/
├── build-yaml.lisp              # Build script — drives everything
├── grammar/
│   └── yaml-grammar.scm         # 211 YAML 1.2 PEG rules (THE grammar)
├── concern/
│   └── yaml-concerns.scm        # Concern algorithm as s-expressions (spec)
├── emit/
│   ├── emit-yaml-peg.lisp       # PEG grammar compiler (projection 1)
│   ├── emit-yaml-concerns.lisp  # Concern compiler (projection 2)
│   ├── emit-format.lisp         # AST formatter
│   ├── emit-yaml-java.lisp      # Java direct emitter (legacy, until peg-java.lisp)
│   └── yaml-concerns.lisp       # String-block concerns (legacy for C++/Rust)
├── spec/
│   ├── peg-cpp.lisp             # C++ PEG spec + concerns (legacy string block)
│   ├── peg-rust.lisp            # Rust PEG spec + concerns (legacy string block)
│   ├── peg-go.lisp              # Go PEG spec + cv vocab concerns
│   ├── peg-python.lisp          # Python PEG spec + cv vocab concerns
│   └── peg-objc.lisp            # Obj-C PEG spec + cv vocab concerns
└── gen/                          # Generated outputs (do not edit)
    ├── yaml-reader-peg.cpp
    ├── yaml_reader_peg.rs
    ├── yaml_reader.go
    ├── yaml_reader.py
    ├── YAMLReader.m
    └── YamlReader.java
```

## How to Add a New Language

Create one file: `spec/peg-{lang}.lisp`. It needs:

### 1. Identity & Output
```lisp
(def-tgt "target-name" "Kotlin")
(def-tgt "default-output" "gen/yaml_reader.kt")
(def-tgt "comment-prefix" "//")
```

### 2. Identifier Rules
```lisp
(def-tgt "ident-prefix" "l_")           ; function name prefix
(def-tgt "ident-transform" :snake)      ; :snake or :camel
(def-tgt "keywords" '("val" "var" ...)) ; reserved words to escape
```

### 3. Closures & Combinators
```lisp
(def-tgt "box-wrap" (lambda (body) ...))    ; wrap parser call in closure
(def-tgt "ref-wrap" (lambda (body) ...))    ; wrap reference
(def-tgt "seq-emit" (lambda (fns) ...))     ; emit sequence combinator call
(def-tgt "alt-emit" (lambda (fns) ...))     ; emit alternative combinator call
(def-tgt "switch-emit" (lambda (p cs) ...)) ; emit context switch
```

### 4. Function Signatures & Bodies
```lisp
(def-tgt "fn-sig" (lambda (name params) ...))
(def-tgt "fn-body" (lambda (sig body) ...))
(def-tgt "fwd-decl" nil)  ; or lambda for forward declarations
```

### 5. Runtime Sections (as string blocks)
```lisp
(def-tgt "header" "package ... / import ...")
(def-tgt "runtime-sections" (list "// Input struct ..." "// Result ..." "// Combinators ..."))
```

### 6. API & Main
```lisp
(def-tgt "api" "...printAst function...")
(def-tgt "main-fn" "...main function...")
```

### 7. Concern Vocab (the `"cv"` hash)
```lisp
(let ((cv (make-hash-table :test 'equal)))
  (setf (gethash "value-type-decl" cv) "...type definitions...")
  (setf (gethash "accessors" cv)       "...get/at/str methods...")
  (setf (gethash "coerce-fn" cv)       "...coerce_scalar function...")
  (setf (gethash "converter-decl" cv)  "...converter struct/class...")
  (setf (gethash "convert-fn" cv)      "...convert(node) function...")
  (setf (gethash "load-fn" cv)         "...public load() function...")
  (def-tgt "cv" cv))
```

### 8. Register in build-yaml.lisp
```lisp
(push '("kotlin" "spec/peg-kotlin.lisp" "gen/yaml_reader.kt") *peg-emitters*)
```

## What's Next

### Immediate: More Languages
Each new language = one `peg-{lang}.lisp` file. Priority candidates:
- **Kotlin** — JVM, similar to Java but modern
- **C#** — .NET ecosystem
- **Swift** — Apple ecosystem (replaces Obj-C long term)
- **Haskell** — functional, different combinator style
- **TypeScript/JavaScript** — web
- **Zig, Nim, D** — systems
- **Lua, Ruby, Perl** — scripting
- **Dart, Scala, Elixir, OCaml, F#, Julia** — various

### Migrate C++/Rust to cv Vocab
C++ and Rust still use legacy string-block concerns. Move them to `cv` vocab like Go/Python/Obj-C. Then delete `yaml-concerns.lisp`.

### Migrate Java to PEG
Java uses a separate direct emitter (`emit-yaml-java.lisp`). Write `peg-java.lisp` to bring it into the unified system. Then delete the direct emitter.

### Wire yaml-concerns.scm Through body-to-target
The `.scm` captures the concern algorithm as s-expressions. `body-to-target` in `codegen-compiler.lisp` can compile it. When wired, the concern vocab reduces from ~6 string blocks per language to ~20 expression-level entries. One algorithm, N compilations. Same pattern as `strip-jinja2.scm`.

## Key Bugs Found & Fixed This Session

1. **Python `//` comments** — emitter hardcoded `//`, fixed to use `(tgt "comment-prefix")`
2. **Obj-C peg_seq/peg_alt count bug** — multi-line alts wrapped into one list element, count was always 1. Fixed with NULL-terminated arrays.
3. **Obj-C GNUstep subscripting** — `dict[key]` not supported on GNUstep. Fixed to use `[dict objectForKey:]`.
4. **Obj-C `__unsafe_unretained`** — not needed without ARC, removed.

## Build & Test

```bash
cd futumura
sbcl --noinform --non-interactive --load build-yaml.lisp

# Compile
g++ -std=c++17 -O2 -o yaml-cpp gen/yaml-reader-peg.cpp
rustc --edition 2021 -O -o yaml-rs gen/yaml_reader_peg.rs
mkdir go-build && cp gen/yaml_reader.go go-build/main.go && cd go-build && go mod init yr && go build -o ../yaml-go . && cd ..
javac gen/YamlReader.java
clang -fblocks -I/tmp -I/usr/lib/gcc/x86_64-linux-gnu/13/include $(gnustep-config --objc-flags) \
  -o yaml-objc gen/YAMLReader.m $(gnustep-config --base-libs) -lBlocksRuntime -lobjc

# Test
python3 run-suite.py ./yaml-cpp      # 332/332
python3 run-suite.py ./yaml-rs       # 332/332
python3 run-suite.py ./yaml-go       # 332/332
python3 run-suite.py ./yaml-objc     # 332/332
echo "a: 1" | python3 gen/yaml_reader.py   # OK: 5 chars
echo "a: 1" | java -cp gen YamlReader      # OK: 5 chars
```

