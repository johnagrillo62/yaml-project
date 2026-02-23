# ════════════════════════════════════════════════════════════════
# Makefile — build, test, and project YAML parsers
# ════════════════════════════════════════════════════════════════

SBCL       ?= sbcl
GRAMMAR    := grammar/yaml-grammar.scm
PROJECTOR  := build-yaml.lisp
TEST_DIR   := yaml-test-suite
SHELL      := /bin/bash

# ── Platform detection ────────────────────────────────────────

UNAME_S := $(shell uname -s)

# ── Generated files ──────────────────────────────────────────

GEN_BASH    := gen/peg_yaml.sh
GEN_CPP     := gen/peg_yaml.cpp
GEN_CSHARP  := gen/PegYaml.cs
GEN_ERLANG  := gen/peg_yaml.erl
GEN_FSHARP  := gen/PegYaml.fs
GEN_GO      := gen/peg_yaml.go
GEN_HASKELL := gen/PegYaml.hs
GEN_JAVA    := gen/YamlReader.java
GEN_KOTLIN  := gen/PegYaml.kt
GEN_LUA     := gen/peg_yaml.lua
GEN_OBJC    := gen/PegYaml.m
GEN_OCAML   := gen/peg_yaml.ml
GEN_PYTHON  := gen/peg_yaml.py
GEN_RUST    := gen/peg_yaml.rs
GEN_SWIFT   := gen/PegYaml.swift
GEN_X86     := gen/peg_yaml.x86
GEN_ZIG     := gen/peg_yaml.zig

# ── Compiled binaries ────────────────────────────────────────

BIN_CPP     := bin/peg_yaml_cpp
BIN_CSHARP  := bin/PegYaml.exe
BIN_FSHARP  := bin/PegYaml_fs
BIN_GO      := bin/peg_yaml_go
BIN_HASKELL := bin/PegYaml_hs
BIN_JAVA    := bin/YamlReader.jar
BIN_KOTLIN  := bin/PegYaml.jar
BIN_OBJC    := bin/PegYaml_m
BIN_OCAML   := bin/peg_yaml_ml
BIN_RUST    := bin/peg_yaml_rs
BIN_SWIFT   := bin/PegYaml_swift
BIN_ZIG     := bin/peg_yaml_zig

ALL_GEN := $(GEN_BASH) $(GEN_CPP) $(GEN_CSHARP) $(GEN_ERLANG) $(GEN_FSHARP) \
           $(GEN_GO) $(GEN_HASKELL) $(GEN_JAVA) $(GEN_KOTLIN) $(GEN_LUA) \
           $(GEN_OBJC) $(GEN_OCAML) $(GEN_PYTHON) $(GEN_RUST) $(GEN_SWIFT) \
           $(GEN_X86) $(GEN_ZIG)

.PHONY: all project clean test test-% build build-%

# ── Project ──────────────────────────────────────────────────

all: project

project: $(ALL_GEN)

project:
	@mkdir -p gen
	$(SBCL) --load $(PROJECTOR) --quit

# ── Build compiled languages ─────────────────────────────────

build: build-go build-python build-lua build-bash

build-go: $(GEN_GO)
	@mkdir -p bin
	go build -o $(BIN_GO) $(GEN_GO)

build-cpp: $(GEN_CPP)
	@mkdir -p bin
	g++ -std=c++17 -O2 -o $(BIN_CPP) $(GEN_CPP)

build-rust: $(GEN_RUST)
	@mkdir -p bin
	rustc -O -o $(BIN_RUST) $(GEN_RUST)

build-java: $(GEN_JAVA)
	@mkdir -p bin
	javac -d bin $(GEN_JAVA)
	cd bin && jar cfe YamlReader.jar YamlReader *.class

build-kotlin: $(GEN_KOTLIN)
	@mkdir -p bin
	kotlinc $(GEN_KOTLIN) -include-runtime -d $(BIN_KOTLIN) 2>/dev/null

build-csharp: $(GEN_CSHARP)
	@mkdir -p bin
	@if which dotnet >/dev/null 2>&1; then \
		cd bin && dotnet-script ../$(GEN_CSHARP) --no-restore 2>/dev/null || \
		(cd .. && dotnet build -o bin $(GEN_CSHARP)); \
	elif which mcs >/dev/null 2>&1; then \
		mcs -out:$(BIN_CSHARP) $(GEN_CSHARP); \
	else \
		echo "C#: no compiler found (need dotnet or mcs)"; exit 1; \
	fi

build-fsharp: $(GEN_FSHARP)
	@mkdir -p bin/fsharp
	@if [ ! -f bin/fsharp/fsharp.fsproj ]; then \
		cd bin/fsharp && dotnet new console -lang F# --force -o . >/dev/null 2>&1; \
	fi
	cp $(GEN_FSHARP) bin/fsharp/Program.fs
	cd bin/fsharp && dotnet build -c Release -o ../fsharp-out >/dev/null 2>&1

build-haskell: $(GEN_HASKELL)
	@mkdir -p bin
	ghc -O2 -o $(BIN_HASKELL) $(GEN_HASKELL)

build-objc: $(GEN_OBJC)
	@mkdir -p bin
ifeq ($(UNAME_S),Darwin)
	clang -framework Foundation -O2 -fblocks -o $(BIN_OBJC) $(GEN_OBJC)
else
	@# ── Linux/GNUstep prerequisites (run once) ──
	@# sudo apt install gnustep-devel gobjc clang libblocksruntime-dev libobjc-13-dev
	@#
	@# If build fails with 'objc/blocks_runtime.h' not found:
	@#   sudo mkdir -p /usr/include/objc
	@#   sudo ln -sf /usr/include/Block.h /usr/include/objc/blocks_runtime.h
	@#
	@# If build fails with 'cannot find -lobjc':
	@#   sudo ln -sf /usr/lib/gcc/x86_64-linux-gnu/13/libobjc.so /usr/lib/x86_64-linux-gnu/libobjc.so
	@if [ ! -f /usr/include/objc/blocks_runtime.h ] && [ -f /usr/include/Block.h ]; then \
		echo "NOTE: creating symlink for objc/blocks_runtime.h (needs sudo)"; \
		sudo mkdir -p /usr/include/objc && \
		sudo ln -sf /usr/include/Block.h /usr/include/objc/blocks_runtime.h; \
	fi
	@if [ ! -f /usr/lib/x86_64-linux-gnu/libobjc.so ] && [ -f /usr/lib/gcc/x86_64-linux-gnu/13/libobjc.so ]; then \
		echo "NOTE: creating symlink for libobjc.so (needs sudo)"; \
		sudo ln -sf /usr/lib/gcc/x86_64-linux-gnu/13/libobjc.so /usr/lib/x86_64-linux-gnu/libobjc.so; \
	fi
	clang $$(gnustep-config --objc-flags) -fblocks \
		-I$$(find /usr/lib/gcc -name objc.h -printf '%h\n' 2>/dev/null | head -1)/.. \
		-O2 -o $(BIN_OBJC) $(GEN_OBJC) \
		$$(gnustep-config --base-libs) -lBlocksRuntime
endif

build-ocaml: $(GEN_OCAML)
	@mkdir -p bin
	ocamlopt -o $(BIN_OCAML) unix.cmxa $(GEN_OCAML)

build-swift: $(GEN_SWIFT)
	@mkdir -p bin
	swiftc -O -o $(BIN_SWIFT) $(GEN_SWIFT)

build-zig: $(GEN_ZIG)
	@mkdir -p bin
	zig build-exe -O ReleaseFast -femit-bin=$(BIN_ZIG) $(GEN_ZIG)

build-erlang: $(GEN_ERLANG)
	@mkdir -p bin
	erlc -o bin $(GEN_ERLANG)

# ── Test ─────────────────────────────────────────────────────

define run_test
	@echo "Testing $(2)..."
	@pass=0; fail=0; total=0; \
	for d in $(TEST_DIR)/*/; do \
		d="$${d%/}"; \
		subs=$$(find "$$d" -maxdepth 1 -mindepth 1 -type d -name '[0-9]*' 2>/dev/null | head -1); \
		if [ -n "$$subs" ]; then \
			for sd in "$$d"/*/; do \
				[ -d "$$sd" ] || continue; \
				bn=$$(basename "$$sd"); \
				echo "$$bn" | grep -qE '^[0-9]+$$' || continue; \
				if [ -f "$$sd/in.yaml" ]; then \
					total=$$((total+1)); \
					if [ -f "$$sd/error" ]; then \
						timeout 10 $(1) "$$sd/in.yaml" >/dev/null 2>&1; \
						[ $$? -ne 0 ] && pass=$$((pass+1)) || fail=$$((fail+1)); \
					else \
						result=$$(timeout 10 $(1) "$$sd/in.yaml" 2>/dev/null); \
						if [ $$? -eq 0 ] && echo "$$result" | grep -q "^OK:"; then \
							pass=$$((pass+1)); \
						else \
							fail=$$((fail+1)); \
						fi; \
					fi; \
				elif [ -f "$$sd/error" ]; then \
					total=$$((total+1)); pass=$$((pass+1)); \
				fi; \
			done; \
		else \
			if [ -f "$$d/in.yaml" ]; then \
				total=$$((total+1)); \
				if [ -f "$$d/error" ]; then \
					timeout 10 $(1) "$$d/in.yaml" >/dev/null 2>&1; \
					[ $$? -ne 0 ] && pass=$$((pass+1)) || fail=$$((fail+1)); \
				else \
					result=$$(timeout 10 $(1) "$$d/in.yaml" 2>/dev/null); \
					if [ $$? -eq 0 ] && echo "$$result" | grep -q "^OK:"; then \
						pass=$$((pass+1)); \
					else \
						fail=$$((fail+1)); \
					fi; \
				fi; \
			elif [ -f "$$d/error" ]; then \
				total=$$((total+1)); pass=$$((pass+1)); \
			fi; \
		fi; \
	done; \
	echo "$(2): $$pass/$$total passed ($$fail failed)"
endef

test:
	@echo "=== Testing all available languages ==="
	@which python3 >/dev/null 2>&1 && $(MAKE) --no-print-directory test-python || echo "Python: SKIPPED (python3 not found)"
	@which lua lua5.4 >/dev/null 2>&1 && $(MAKE) --no-print-directory test-lua || echo "Lua: SKIPPED (lua not found)"
	@which bash >/dev/null 2>&1 && $(MAKE) --no-print-directory test-bash || echo "Bash: SKIPPED (bash not found)"
	@which go >/dev/null 2>&1 && $(MAKE) --no-print-directory test-go || echo "Go: SKIPPED (go not found)"
	@which javac >/dev/null 2>&1 && $(MAKE) --no-print-directory test-java || echo "Java: SKIPPED (javac not found)"
	@which kotlinc >/dev/null 2>&1 && $(MAKE) --no-print-directory test-kotlin || echo "Kotlin: SKIPPED (kotlinc not found)"
	@which g++ >/dev/null 2>&1 && $(MAKE) --no-print-directory test-cpp || echo "C++: SKIPPED (g++ not found)"
	@which rustc >/dev/null 2>&1 && $(MAKE) --no-print-directory test-rust || echo "Rust: SKIPPED (rustc not found)"
	@(which dotnet >/dev/null 2>&1 || which mcs >/dev/null 2>&1) && $(MAKE) --no-print-directory test-csharp || echo "C#: SKIPPED (dotnet/mcs not found)"
	@which ghc >/dev/null 2>&1 && $(MAKE) --no-print-directory test-haskell || echo "Haskell: SKIPPED (ghc not found)"
	@which swiftc >/dev/null 2>&1 && $(MAKE) --no-print-directory test-swift || echo "Swift: SKIPPED (swiftc not found)"
	@which zig >/dev/null 2>&1 && $(MAKE) --no-print-directory test-zig || echo "Zig: SKIPPED (zig not found)"
	@which ocamlopt >/dev/null 2>&1 && $(MAKE) --no-print-directory test-ocaml || echo "OCaml: SKIPPED (ocamlopt not found)"
	@which erlc >/dev/null 2>&1 && $(MAKE) --no-print-directory test-erlang || echo "Erlang: SKIPPED (erlc not found)"
	@which clang >/dev/null 2>&1 && $(MAKE) --no-print-directory test-objc || echo "Objective-C: SKIPPED (clang not found)"
	@which dotnet >/dev/null 2>&1 && $(MAKE) --no-print-directory test-fsharp || echo "F#: SKIPPED (dotnet not found)"

test-python: $(GEN_PYTHON)
	$(call run_test,python3 $(GEN_PYTHON),Python)

test-lua: $(GEN_LUA)
	$(call run_test,lua $(GEN_LUA),Lua)

test-bash: $(GEN_BASH)
	$(call run_test,bash $(GEN_BASH),Bash)

test-go: build-go
	$(call run_test,$(BIN_GO),Go)

test-cpp: build-cpp
	$(call run_test,$(BIN_CPP),C++)

test-rust: build-rust
	$(call run_test,$(BIN_RUST),Rust)

test-java: build-java
	$(call run_test,java -jar $(BIN_JAVA),Java)

test-kotlin: build-kotlin
	$(call run_test,java -jar $(BIN_KOTLIN),Kotlin)

test-csharp: build-csharp
	$(call run_test,mono $(BIN_CSHARP),C#)

test-fsharp: build-fsharp
	$(call run_test,dotnet bin/fsharp-out/fsharp.dll,F#)

test-haskell: build-haskell
	$(call run_test,$(BIN_HASKELL),Haskell)

test-objc: build-objc
	$(call run_test,$(BIN_OBJC),Objective-C)

test-ocaml: build-ocaml
	$(call run_test,$(BIN_OCAML),OCaml)

test-swift: build-swift
	$(call run_test,$(BIN_SWIFT),Swift)

test-zig: build-zig
	$(call run_test,$(BIN_ZIG),Zig)

test-erlang: build-erlang
	$(call run_test,escript $(GEN_ERLANG),Erlang)

test-all: test-python test-lua test-bash test-go test-cpp test-rust \
          test-java test-kotlin test-csharp test-haskell test-objc \
          test-ocaml test-swift test-zig test-erlang test-fsharp

# ── Check installed toolchains ────────────────────────────────

check:
	@echo "=== Installed toolchains ==="
	@which python3  >/dev/null 2>&1 && echo "  Python:      $$(python3 --version 2>&1)"  || echo "  Python:      NOT FOUND"
	@which lua lua5.4 lua5.3 >/dev/null 2>&1 && echo "  Lua:         $$(lua -v 2>&1 || lua5.4 -v 2>&1)" || echo "  Lua:         NOT FOUND"
	@which bash     >/dev/null 2>&1 && echo "  Bash:        $$(bash --version | head -1)" || echo "  Bash:        NOT FOUND"
	@which go       >/dev/null 2>&1 && echo "  Go:          $$(go version)"               || echo "  Go:          NOT FOUND"
	@which javac    >/dev/null 2>&1 && echo "  Java:        $$(javac -version 2>&1)"       || echo "  Java:        NOT FOUND"
	@which kotlinc  >/dev/null 2>&1 && echo "  Kotlin:      $$(kotlinc -version 2>&1)"     || echo "  Kotlin:      NOT FOUND"
	@which g++      >/dev/null 2>&1 && echo "  C++:         $$(g++ --version | head -1)"   || echo "  C++:         NOT FOUND"
	@which rustc    >/dev/null 2>&1 && echo "  Rust:        $$(rustc --version)"            || echo "  Rust:        NOT FOUND"
	@which dotnet mcs >/dev/null 2>&1 && echo "  C#:          $$(dotnet --version 2>&1 || mcs --version 2>&1)" || echo "  C#:          NOT FOUND"
	@which dotnet   >/dev/null 2>&1 && echo "  F#:          $$(dotnet --version 2>&1)"     || echo "  F#:          NOT FOUND"
	@which ghc      >/dev/null 2>&1 && echo "  Haskell:     $$(ghc --version)"              || echo "  Haskell:     NOT FOUND"
	@which swiftc   >/dev/null 2>&1 && echo "  Swift:       $$(swiftc --version 2>&1 | head -1)" || echo "  Swift:       NOT FOUND"
	@which zig      >/dev/null 2>&1 && echo "  Zig:         $$(zig version)"                || echo "  Zig:         NOT FOUND"
	@which ocamlopt >/dev/null 2>&1 && echo "  OCaml:       $$(ocamlopt -version)"          || echo "  OCaml:       NOT FOUND"
	@which erlc     >/dev/null 2>&1 && echo "  Erlang:      found"                          || echo "  Erlang:      NOT FOUND"
	@which clang    >/dev/null 2>&1 && echo "  Objective-C: $$(clang --version | head -1)"  || echo "  Objective-C: NOT FOUND"
	@gnustep-config --version >/dev/null 2>&1 && echo "  GNUstep:     $$(gnustep-config --version)" || echo "  GNUstep:     NOT FOUND (needed for ObjC on Linux)"

# ── Clean ────────────────────────────────────────────────────

clean:
	rm -rf bin/
	rm -f gen/*.class gen/*.o gen/*.hi

distclean: clean
	rm -rf gen/
