# The Yaml Project

I spent 20 years chasing metaprogramming. Always wanting something
better, always searching for the next level of code generation.
This is where I ended up: a Futamura projector that generates
YAML 1.2 parsers from a single grammar.

One grammar. Eighteen languages. 308/308 tests. All of them.

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

45,557 lines of parser code. All generated. All passing the
[YAML Test Suite](https://github.com/yaml/yaml-test-suite).

## The key insight

The YAML 1.2 spec already defines its grammar formally — 211
productions in Haskell-like notation. The spec authors were *right
there*. They had the formal grammar, they had a functional language,
they had code-as-data staring them in the face.

But Haskell isn't code-as-data. You can't walk Haskell source as a
data structure. You can't destructure a Haskell production into its
parts and emit it in Go. So for 16 years, every implementer in every
language went off and hand-wrote their own parser from scratch.

I put the grammar in Lisp. Then it was just a `dolist`.

## The grammar is data

The YAML spec says Rule 28 is:

```
[28] b-break ::=   ( b-carriage-return b-line-feed )
                 | b-carriage-return
                 | b-line-feed
```

In the grammar file, that's an S-expression:

```scheme
(Rule 28 b-break
  (Alt (Seq (Ref b-carriage-return) (Ref b-line-feed))
       (Ref b-carriage-return)
       (Ref b-line-feed)))
```

Same rule. But now it's data a program can walk. The projector reads
this, and depending on the target spec, emits:

**Python** — lambdas:
```python
def b_break(inp):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: b_carriage_return(inp),
            lambda inp: b_line_feed(inp)]),
        lambda inp: b_carriage_return(inp),
        lambda inp: b_line_feed(inp)])
```

**Rust** — closures:
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

**Go** — func literals:
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

**Haskell** — where the spec started, but now executable:
```haskell
b_break inp =
    peg_alt inp [
        \inp -> peg_seq inp [
            \inp -> b_carriage_return inp,
            \inp -> b_line_feed inp],
        \inp -> b_carriage_return inp,
        \inp -> b_line_feed inp]
```

**Bash** — even bash:
```bash
b_break() {
    peg_alt "$1" \
        'peg_seq "$1" b_carriage_return b_line_feed' \
        'b_carriage_return "$1"' \
        'b_line_feed "$1"'
}
```

Same rule. Same structure. Different language. Every one emitted by
the same projector walking the same S-expression. The target spec
just says how that language expresses `Alt`, `Seq`, functions, and
closures.

## The projector

Here's the core loop:

```lisp
(dolist (cr compiled-rules)
  (destructuring-bind (num name sig body-str) cr
    (emitf "~A [~D] ~A ~A~%" cmt num name cmte)
    (emit-block (funcall (tgt "fn-body") sig body-str))
    (blank)))
```

Walk the compiled rules. Destructure each one. Emit a function
using the target spec's template. That loop produced 45,557 lines
of working parser code across 18 languages.

## What happened when I ran the spec literally

The projector does exactly what the spec says. No workarounds, no
interpretation. So when the spec has a bug, the parser has a bug.

That's how I found a bug in
[Rule 78 of the YAML 1.2 specification](https://github.com/yaml/yaml-spec/issues/356)
— present since 2009. Every hand-written parser silently works around
it without noticing. A projector can't. Spec bugs have nowhere to
hide.

## The five places the spec gives up

The YAML spec is formal — except in five places where it drops into
English prose. Every implementer interprets these differently. That's
why parsers disagree. I formalized all five:

1. **Auto-detect indentation** — The spec says "detect the indentation
   level" without saying how. The grammar scans forward to the next
   non-empty line and derives the indent delta.

2. **Flow context switching** — Prose table for context transitions.
   Now an inline function.

3. **Sequence spaces** — A footnote says "n-1 for BLOCK-OUT, n
   otherwise." One-line function.

4. **Block header binding** — Rules 162, 170, 174 need to parse a
   chomping indicator mid-rule and thread results forward. The grammar
   uses `Let`:

   ```scheme
   (Rule 170 c-l+literal (n)
     (Seq (Char '|')
          (Let ((m (Alt (Parse-Int (Ref ns-dec-digit))
                        (Detect-Indent n)))
                (t (Alt (Parse-Sym (Char '-') strip)
                        (Parse-Sym (Char '+') keep)
                        (Val clip))))
            (Seq (Ref s-b-comment)
                 (Ref l-literal-content (+ n m) t)))))
   ```

5. **Bounded indentation** — "less than n spaces" — implemented as
   runtime-checked space counts.

## How new languages are added

Write a target spec — 300–500 lines that tells the projector how the
language expresses functions, closures, sequences, and alternatives.
The projector does the rest in seconds.

```
grammar        ─┐
                 ├──▶  projector  ──▶  parser in any language
target-spec    ─┘
```

## Everything is projected

Not just the parsers. The build and test scripts are projected from
`build-spec.scm`. The concern layer (semantic validation, error
messages) is a separate projection phase. Add a new language, you
get the parser, the build script, and the validation layer.

One fix, every language, in seconds.

## Futamura projection

In 1971, Yoshihiko Futamura described how partially evaluating an
interpreter with respect to a program produces a compiled version
of that program. The grammar is the program. The PEG combinator
framework is the interpreter. The projector specializes one against
the other to produce standalone parsers.

Each parser is a single file with no external dependencies. Extract
it from the included main and drop it into any project.

This isn't a code generator with templates. A generator's output
depends on the author's interpretation. The projector's output is
determined by the grammar. If the spec says rule 63 is an
alternation of rules 64 and 65, the output is an alternation of
rules 64 and 65. No human decision in between.

## Usage

```bash
echo "a: 1" | ./yaml-reader       # C++, Go, Rust, etc.
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

## Test suite

Tested against the [YAML Test Suite](https://github.com/yaml/yaml-test-suite)
(data-2022-01-17 release). All parsers pass **308/308 valid tests**.
The x86-64 assembly target passes 94/402 (experimental).

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

Most don't fully implement the 1.2 spec. Many fail on block scalars,
chomping indicators, multi-line plain scalars, flow collections, and
directive handling. These parsers handle the full spec by
construction — because they *are* the spec.

## Author

John Grillo — full-time C++ programmer, with 20 years of metaprogramming.

More languages soon.

## License

MIT
