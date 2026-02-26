# The Yaml Project


Acknowledgments
The s-expression grammar in this project is derived from the YamlReference Haskell package by Oren Ben-Kiki, co-creator of the YAML specification. His reference implementation translated the YAML 1.2 spec into precise, machine-verified BNF productions — without which this project would not exist.

I spent 20 years chasing metaprogramming. This is where I ended up.

One grammar. Eighteen languages. 308/308 tests.

| Language | File | Lines | Tests |
|---|---|---|---|
| C++ | `peg_yaml.cpp` | 2,197 | 308/308 |
| Rust | `peg_yaml.rs` | 1,793 | 308/308 |
| Go | `peg_yaml.go` | 1,995 | 308/308 |
| Python | `peg_yaml.py` | 1,575 | 308/308 |
| C# | `PegYaml.cs` | 1,894 | 308/308 |
| F# | `PegYaml.fs` | 1,611 | 308/308 |
| Java | `YamlReader.java` | 1,757 | 308/308 |
| Kotlin | `PegYaml.kt` | 1,805 | 308/308 |
| Haskell | `PegYaml.hs` | 1,926 | 308/308 |
| Swift | `PegYaml.swift` | 1,911 | 308/308 |
| Objective-C | `PegYaml.m` | 2,174 | 308/308 |
| Zig | `peg_yaml.zig` | 2,156 | 308/308 |
| OCaml | `peg_yaml.ml` | 1,504 | 308/308 |
| Erlang | `peg_yaml.erl` | 1,677 | 308/308 |
| Lua | `peg_yaml.lua` | 1,850 | 308/308 |
| Bash | `peg_yaml.sh` | 2,397 | 308/308 |
| PowerShell | `peg_yaml.ps1` | 1,845 | 308/308 |
| x86-64 asm | `peg_yaml.x86` | 8,570 | 94/402 |

45,557 lines of generated parser code. All passing the
[YAML Test Suite](https://github.com/yaml/yaml-test-suite).

## How

The YAML 1.2 spec defines 211 grammar rules. They look like functions.
They have parameters. They call each other. But nobody runs them —
everybody reads the spec and hand-codes a parser.

I put the 211 rules in S-expressions and ran them.

The spec says Rule 28 is:
```
[28] b-break ::=   ( b-carriage-return b-line-feed )
                 | b-carriage-return
                 | b-line-feed
```

I wrote:
```scheme
(Rule 28 b-break
  (Alt (Seq (Ref b-carriage-return) (Ref b-line-feed))
       (Ref b-carriage-return)
       (Ref b-line-feed)))
```

Same rule. Now it's data a program can walk. The projector reads it
and emits that rule in whatever language you want:

**Python:**
```python
def b_break(inp):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: b_carriage_return(inp),
            lambda inp: b_line_feed(inp)]),
        lambda inp: b_carriage_return(inp),
        lambda inp: b_line_feed(inp)])
```

**Rust:**
```rust
fn b_break(inp: &mut Input) -> Result {
    peg_alt(inp, &[
        &|inp| peg_seq(inp, &[
            &|inp| b_carriage_return(inp),
            &|inp| b_line_feed(inp)]),
        &|inp| b_carriage_return(inp),
        &|inp| b_line_feed(inp)])
}
```

**Go:**
```go
func b_break(inp *Input) Result {
    return pegAlt(inp, []ParseFn{
        func(inp *Input) Result {
            return pegSeq(inp, []ParseFn{
                func(inp *Input) Result { return bCarriageReturn(inp) },
                func(inp *Input) Result { return bLineFeed(inp) },
            })
        },
        func(inp *Input) Result { return bCarriageReturn(inp) },
        func(inp *Input) Result { return bLineFeed(inp) },
    })
}
```

**Bash:**
```bash
b_break() {
    peg_alt "$1" \
        'peg_seq "$1" b_carriage_return b_line_feed' \
        'b_carriage_return "$1"' \
        'b_line_feed "$1"'
}
```

Same rule. Same structure. Different syntax. One projector.

The core loop that produced all 45,557 lines:

```lisp
(dolist (cr compiled-rules)
  (destructuring-bind (num name sig body-str) cr
    (emitf "~A [~D] ~A ~A~%" cmt num name cmte)
    (emit-block (funcall (tgt "fn-body") sig body-str))
    (blank)))
```

Walk the rules. Emit a function for each one. Done.

## Rule 78

When you run the spec literally, spec bugs have nowhere to hide.

I found a [bug in Rule 78](https://github.com/yaml/yaml-spec/issues/356)
that's been there since 2009. `b-comment` can match empty via an
end-of-file fallback, which lets `l-comment` eat indentation on content
lines and silently flatten nested structures. Every hand-written parser
works around it by accident. A parser generated from the spec can't.

Fix: `b-comment` → `b-non-content`. One symbol.

## Five gaps in the spec

The spec is formal except in five places where it drops into English.
Every implementer reads these differently. That's why parsers disagree.
I formalized all five:

1. **Auto-detect indentation** — spec says "detect the indentation
   level" without saying how. Scan forward, derive the delta.
2. **Flow context switching** — prose table, now an inline function.
3. **Sequence spaces** — a footnote: "n-1 for BLOCK-OUT, n otherwise."
4. **Block header binding** — rules 162, 170, 174 parse a chomping
   indicator mid-rule and thread results. Handled with `Let`.
5. **Bounded indentation** — "less than n spaces" — runtime-checked.

## Adding a new language

Write a target spec — 300-500 lines that says how the language
expresses functions, closures, sequences, and alternatives. The
projector does the rest in seconds.

```
grammar        ─┐
                 ├──▶  projector  ──▶  parser in any language
target-spec    ─┘
```

Everything is projected. Not just the parsers — the build scripts,
the test harness, the concern layer. Add a language, get everything.

## Futamura projection

In 1971 Futamura described how partially evaluating an interpreter
against a program produces a compiled version of that program. The
grammar is the program. The PEG combinators are the interpreter.
The projector specializes one against the other.

Each output is a single file, no dependencies. Not a code generator
with templates — a projector. The output is determined by the grammar,
not by interpretation. Rule 63 is an alternation of 64 and 65 because
the spec says so. No human decision in between.

## Usage

```bash
echo "a: 1" | ./yaml-reader
echo "a: 1" | python3 peg_yaml.py
echo "a: 1" | bash peg_yaml.sh
```
```
OK: 4 chars
STREAM
  DOC
    MAPPING
      PAIR
        SCALAR: "a"
        SCALAR: "1"
```

## Go library

```
go get github.com/johnagrillo62/yaml-peg-go
```

## Building

```bash
./build-go.sh
./build-rust.sh
./build-python.sh
./build-cpp.sh
./build-java.sh
./build-kotlin.sh
./build-csharp.sh
./build-fsharp.sh
./build-haskell.sh
./build-swift.sh
./build-objc.sh
./build-zig.sh
./build-ocaml.sh
./build-erlang.sh
./build-lua.sh
./build-bash.sh
./build-powershell.sh
./build-x86.sh
```

## Why not use an existing YAML library?

Most don't fully implement 1.2. Many fail on block scalars, chomping
indicators, multi-line plain scalars, flow collections, directive
handling. These parsers handle all of it by construction — they *are*
the spec.

## Author

John Grillo — C++ programmer, 20 years of metaprogramming.

More languages coming.

## License

MIT

