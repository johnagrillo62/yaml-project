;; ═══════════════════════════════════════════════════════════════
;; build-spec-json.scm — Declarative build specification for JSON
;; ═══════════════════════════════════════════════════════════════
;;
;; Projects build-json-LANG.sh scripts for all language targets.
;; Test suite: JSONTestSuite (nst/JSONTestSuite)
;;   y_*.json — must parse (accept)
;;   n_*.json — must reject
;;   i_*.json — implementation defined (either OK)
(Build json-grammar
  (projector "sbcl --load build-json.lisp --quit")
  (test-suite "JSONTestSuite/test_parsing")
  (test-note "Note: i_* tests are implementation-defined — both accept and reject are valid.")
  (test-mode "json")
  (gen-dir "gen")
  (bin-dir "bin")
  ;; ── Interpreted languages ─────────────────────────────────
  (target python
    (file "peg_json.py")
    (spec "spec/json-peg-python.lisp")
    (interp "python3")
    (deps "python3"))
  (target lua
    (file "peg_json.lua")
    (spec "spec/json-peg-lua.lisp")
    (interp "lua")
    (version "lua -v 2>&1 | head -1")
    (deps "lua"))
  (target bash
    (file "peg_json.sh")
    (spec "spec/json-peg-bash.lisp")
    (interp "bash")
    (slow "Bash is interpreted line-by-line — this may take several minutes")
    (deps "bash"))
  (target erlang
    (file "peg_json.erl")
    (spec "spec/json-peg-erlang.lisp")
    (interp "escript")
    (version "erl +V 2>&1 | head -1")
    (slow "Erlang spawns the BEAM VM per test — this may take several minutes")
    (deps "escript"))
  (target powershell
    (file "peg_json.ps1")
    (spec "spec/json-peg-powershell.lisp")
    (interp "pwsh")
    (version "pwsh --version 2>&1 | head -1")
    (slow "PowerShell is interpreted — this may take several minutes")
    (deps "pwsh"))
  ;; ── Compiled languages ────────────────────────────────────
  (target go
    (file "peg_json.go")
    (spec "spec/json-peg-go.lisp")
    (compile "go build -o {bin} {src}")
    (version "go version 2>&1 | head -1")
    (deps "go"))
  (target rust
    (file "peg_json.rs")
    (spec "spec/json-peg-rust.lisp")
    (compile "rustc -O -o {bin} {src}")
    (deps "rustc"))
  (target cpp
    (file "peg_json.cpp")
    (spec "spec/json-peg-cpp.lisp")
    (compile "g++ -std=c++17 -O2 -o {bin} {src}")
    (deps "g++"))
  (target kotlin
    (file "PegJson.kt")
    (spec "spec/json-peg-kotlin.lisp")
    (compile "kotlinc {src} -include-runtime -d {bin}.jar 2>/dev/null")
    (run "java -jar {bin}.jar")
    (timeout "5")
    (slow "JVM starts per test — this may take a few minutes")
    (version "kotlinc -version 2>&1 | grep -i kotlin | head -1")
    (deps "kotlinc" "java"))
  (target csharp
    (file "PegJson.cs")
    (spec "spec/json-peg-csharp.lisp")
    (compile "mcs -out:{bin}.exe {src}")
    (run "mono {bin}.exe")
    (deps "mcs" "mono"))
  (target fsharp
    (file "PegJson.fs")
    (spec "spec/json-peg-fsharp.lisp")
    (compile "mkdir -p {bindir}/fsharp-json && [ -f {bindir}/fsharp-json/fsharp-json.fsproj ] || dotnet new console -lang F# --force -o {bindir}/fsharp-json >/dev/null 2>&1 && cp {src} {bindir}/fsharp-json/Program.fs && dotnet publish {bindir}/fsharp-json -c Release -o {bindir}/fsharp-json-out --self-contained >/dev/null 2>&1")
    (run "{bindir}/fsharp-json-out/fsharp-json")
    (timeout "3")
    (deps "dotnet"))
  (target haskell
    (file "PegJson.hs")
    (spec "spec/json-peg-haskell.lisp")
    (compile "ghc -O2 -o {bin} {src}")
    (deps "ghc"))
  (target swift
    (file "PegJson.swift")
    (spec "spec/json-peg-swift.lisp")
    (compile "swiftc -O -o {bin} {src}")
    (deps "swiftc"))
  (target zig
    (file "peg_json.zig")
    (spec "spec/json-peg-zig.lisp")
    (compile "zig build-exe -O ReleaseFast -femit-bin={bin} {src}")
    (deps "zig"))
  (target ocaml
    (file "peg_json.ml")
    (spec "spec/json-peg-ocaml.lisp")
    (compile "ocamlopt -O2 -o {bin} unix.cmxa {src}")
    (deps "ocamlopt"))
  (target objc
    (file "PegJson.m")
    (spec "spec/json-peg-objc.lisp")
    (compile-darwin "clang -framework Foundation -O2 -o {bin} {src}")
    (compile-linux "clang $(gnustep-config --objc-flags) -I/usr/lib/gcc/x86_64-linux-gnu/13/include -fblocks -o {bin} {src} $(gnustep-config --base-libs) -lBlocksRuntime")
    (deps "clang" "gnustep-config"))
  (target x86
    (file "peg_json.x86")
    (spec "spec/json-peg-x86.lisp")
    (compile "nasm -f elf64 -o {bin}.o {src} && ld -o {bin} {bin}.o")
    (deps "nasm" "ld")))
