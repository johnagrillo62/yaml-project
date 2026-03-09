;; ═══════════════════════════════════════════════════════════════
;; build-spec.scm — Declarative build specification
;; ═══════════════════════════════════════════════════════════════
;;
;; Emits a Makefile with auto-detection, build, and test targets.
;; Add a language by adding a target entry.
(Build yaml-grammar
  (projector "sbcl --load build-yaml.lisp --quit")
  (test-suite "yaml-test-suite")
  (test-note "Note: 94 of the 402 tests are for semantic analysis (duplicate keys, invalid tags, etc.) which require a separate concern layer beyond the structural parser.")
  (gen-dir "gen")
  (bin-dir "bin")
  ;; ── Compiled languages ────────────────────────────────────
  (target go
    (file "peg_yaml.go")
    (spec "spec/yaml-peg-go.lisp")
    (compile "go build -o {bin} {src}")
    (version "go version 2>&1 | head -1")
    (deps "go"))
  (target rust
    (file "peg_yaml.rs")
    (spec "spec/yaml-peg-rust.lisp")
    (compile "rustc -O -o {bin} {src}")
    (deps "rustc"))
  (target cpp
    (file "peg_yaml.cpp")
    (spec "spec/yaml-peg-cpp.lisp")
    (compile "g++ -std=c++17 -O2 -o {bin} {src}")
    (deps "g++"))
  (target java
    (file "YamlReader.java")
    (compile "javac -d {bindir} {src}")
    (run "java -cp {bindir} YamlReader")
    (timeout "5")
    (slow "JVM starts per test — this may take a few minutes")
    (deps "javac" "java"))
  (target kotlin
    (file "PegYaml.kt")
    (spec "spec/yaml-peg-kotlin.lisp")
    (compile "kotlinc {src} -include-runtime -d {bin}.jar 2>/dev/null")
    (run "java -jar {bin}.jar")
    (timeout "5")
    (slow "JVM starts per test — this may take a few minutes")
    (version "kotlinc -version 2>&1 | grep -i kotlin | head -1")
    (deps "kotlinc" "java"))
  (target csharp
    (file "PegYaml.cs")
    (spec "spec/yaml-peg-csharp.lisp")
    (compile "mcs -out:{bin}.exe {src}")
    (run "mono {bin}.exe")
    (deps "mcs" "mono"))
  (target fsharp
    (file "PegYaml.fs")
    (spec "spec/yaml-peg-fsharp.lisp")
    (compile "mkdir -p {bindir}/fsharp && [ -f {bindir}/fsharp/fsharp.fsproj ] || dotnet new console -lang F# --force -o {bindir}/fsharp >/dev/null 2>&1 && cp {src} {bindir}/fsharp/Program.fs && dotnet publish {bindir}/fsharp -c Release -o {bindir}/fsharp-out --self-contained >/dev/null 2>&1")
    (run "{bindir}/fsharp-out/fsharp")
    (timeout "3")
    (deps "dotnet"))
  (target haskell
    (file "PegYaml.hs")
    (spec "spec/yaml-peg-haskell.lisp")
    (compile "ghc -O2 -o {bin} {src}")
    (deps "ghc"))
  (target swift
    (file "PegYaml.swift")
    (spec "spec/yaml-peg-swift.lisp")
    (compile "swiftc -O -o {bin} {src}")
    (deps "swiftc"))
  (target zig
    (file "peg_yaml.zig")
    (spec "spec/yaml-peg-zig.lisp")
    (compile "zig build-exe -O ReleaseFast -femit-bin={bin} {src}")
    (deps "zig"))
  (target ocaml
    (file "peg_yaml.ml")
    (spec "spec/yaml-peg-ocaml.lisp")
    (compile "ocamlopt -O2 -o {bin} unix.cmxa {src}")
    (deps "ocamlopt"))
  
  (target objc
    (file "PegYaml.m")
    (spec "spec/yaml-peg-objc.lisp")
    (compile-darwin "clang -framework Foundation -O2 -o {bin} {src}")
    (compile-linux "clang $(gnustep-config --objc-flags) -I/usr/lib/gcc/x86_64-linux-gnu/13/include -fblocks -o {bin} {src} $(gnustep-config --base-libs) -lBlocksRuntime")
    (deps "clang" "gnustep-config"))
  
  (target x86
    (file "peg_yaml.x86")
    (spec "spec/yaml-peg-x86.lisp")
    (compile "nasm -f elf64 -o {bin}.o {src} && ld -o {bin} {bin}.o")
    (deps "nasm" "ld"))

  ;; ── Interpreted languages ─────────────────────────────────
  (target python
    (file "peg_yaml.py")
    (spec "spec/yaml-peg-python.lisp")
    (interp "python3")
    (deps "python3"))
  (target lua
    (file "peg_yaml.lua")
    (spec "spec/yaml-peg-lua.lisp")
    (interp "lua")
    (version "lua -v 2>&1 | head -1")
    (deps "lua"))
  (target bash
    (file "peg_yaml.sh")
    (spec "spec/yaml-peg-bash.lisp")
    (interp "bash")
    (slow "Bash is interpreted line-by-line — this may take several minutes")
    (deps "bash"))



  )
