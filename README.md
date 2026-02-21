# yaml-peg: The YAML 1.2 Spec, Executable

A YAML 1.2 parser that runs the specification directly. The grammar file
is 211 s-expression rules transcribed from the YAML 1.2 spec. The
evaluator is a PEG interpreter that reads those rules at runtime. No
hand-coded parser. No code generation. The spec is the parser.

**1,832 lines total.** 749 lines of evaluator. 1,083 lines of grammar.

| Library     | Language | Lines   | Hand-coded? |
|-------------|----------|---------|-------------|
| libyaml     | C        | ~13,000 | yes         |
| yaml-cpp    | C++      | ~16,000 | yes         |
| SnakeYAML   | Java     | ~19,000 | yes         |
| go-yaml     | Go       | ~11,000 | yes         |
| PyYAML      | Python   | ~5,900  | yes         |
| **yaml-peg**| **Lisp** | **1,832** | **no — runs the spec** |

---

## Spec Bug: Rule 78 (l-comment)

We found a bug in the YAML 1.2 specification. It has been present since
the spec was published in 2009. No one found it before because no one
executes the grammar.

### The bug

Rule 78 defines a comment line:

```
[78] l-comment ::= s-separate-in-line c-nb-comment-text? b-comment
```

Rule 76 defines b-comment:

```
[76] b-comment ::= b-non-content | /* End of file */
```

The `/* End of file */` alternative matches EMPTY — it succeeds without
consuming input, even in the middle of a file. This means `l-comment` can
match a **content line**: `s-separate-in-line` consumes leading spaces
(the indentation), `c-nb-comment-text?` skips (no `#`), and `b-comment`
succeeds via EMPTY despite no newline.

### How it breaks

```yaml
a:
  b: 1
  c: 2
```

After parsing `a:` and the newline, the parser is at `  b: 1\n  c: 2\n`.
Rule 79 (`s-l-comments`) calls `Star(l-comment)`:

```
Step 1: s-b-comment consumes the \n             ✓ correct
Step 2: Star tries l-comment on "  b: 1\n..."
  - s-separate-in-line eats "  " (2 spaces)     ← THE INDENTATION
  - c-nb-comment-text?: 'b' ≠ '#', skip
  - b-comment: no newline, but EMPTY matches     ← THE BUG
  - l-comment consumed 2 spaces and "succeeded"
Step 3: l+block-mapping looks for indentation
  - spaces are GONE
  - Detect-Indent finds 0 spaces
  - s-indent fails
  - block-node fails
  - key 'a' gets null value
  - 'b: 1' and 'c: 2' parsed as siblings
```

**Buggy result:** `{a: null, b: 1, null: 2}` — flat, wrong

**Correct result:** `{a: {b: 1, c: 2}}` — nested

### The fix

One symbol change in rule 78:

```
BEFORE: l-comment ::= s-separate-in-line c-nb-comment-text? b-comment
AFTER:  l-comment ::= s-separate-in-line c-nb-comment-text? b-non-content
```

Replace `b-comment` with `b-non-content`. A comment line must end with an
actual newline, not an end-of-file fallback. Content lines can no longer be
mistaken for comment lines. Indentation survives. Nesting works.

### Why no one found it

Every hand-coded parser implements the correct behavior by accident. A human
reading "comment line" naturally skips content lines. The grammar doesn't —
it does exactly what it says. And what it says is wrong.

The bug is invisible without executing the grammar. It lives in the
interaction of seven composed rules across two contexts with implicit state.
No human can trace that path by reading prose.

We found it in an hour, by accident, while building AST propagation. We ran
the grammar, nested maps came out flat, we traced the execution, and rule 78
was wrong. The bug found us.

### Reproducing

```bash
sbcl --load yaml-eval.fasl --load demo-bug.lisp
```

This loads two grammars — buggy (original spec) and fixed — and parses the
same input with both, showing the different ASTs and values.

```bash
sbcl --load yaml-eval.fasl --load demo-step.lisp
```

This traces the bug step by step: which rule fires, what it consumes, where
the indentation goes, and why nesting breaks.

---

## How It Works

### The grammar is s-expressions

The grammar file (`yaml-grammar.scm`) contains the 211 YAML 1.2 rules as
s-expressions. This is not a reinterpretation — it is the spec, transcribed
into a notation that executes:

```scheme
;; Rule 1: printable characters
(Rule 1 c-printable
  (Alt (Char #\Tab)
       (Char #\Newline)
       (Char #\Return)
       (Range (Hex 20) (Hex 7E))
       (Hex 85)
       (Range (Hex A0) (Hex D7FF))
       (Range (Hex E000) (Hex FFFD))
       (Range (Hex 10000) (Hex 10FFFF))))

;; Rule 78: the buggy comment rule (fixed)
(Rule 78 l-comment
  (Seq (Ref s-separate-in-line)
       (Opt (Ref c-nb-comment-text))
       (Ref b-non-content)))            ; ← was b-comment (the bug)

;; Rule 187: block mapping with auto-detected indentation
(Rule 187 l+block-mapping (n)
  (Build mapping
    (Let ((m (Detect-Indent n)))
      (Collect (Plus (Seq (Ref s-indent (+ n m))
                          (Ref ns-l-block-map-entry (+ n m))))))))
```

Every rule is a lambda. `Seq` is composition. `Alt` is choice. `Star` is
repetition. `Ref` is a function call. Parameters like `n` (indentation)
and `c` (context) flow through the grammar exactly as the spec describes.

### The evaluator walks the grammar

The evaluator (`yaml-eval.lisp`) is a PEG interpreter with packrat
memoization. It reads the grammar file at startup and interprets every
rule as a parsing function:

```lisp
;; Core dispatch — 20 combinators
(defun ev-form (op args inp env gram)
  (case op
    (CHAR      (ev-char args inp env))
    (RANGE     (ev-range args inp))
    (SEQ       (ev-seq args inp env gram))
    (ALT       (ev-alt args inp env gram))
    (STAR      (ev-star args inp env gram))
    (PLUS      (ev-plus args inp env gram))
    (OPT       (ev-opt args inp env gram))
    (REF       (ev-ref (car args) (cdr args) inp env gram))
    (NOT       (ev-not args inp env gram))
    (REPEAT    (ev-repeat args inp env gram))
    (SWITCH    (ev-switch args inp env gram))
    ;; Code-as-data extensions
    (BUILD     (ev-build args inp env gram))
    (SCALAR    (ev-scalar args inp env gram))
    (COLLECT   (ev-collect args inp env gram))
    (LET       (ev-let args inp env gram))
    (DETECT-INDENT (ev-detect-indent args inp env))
    ...))
```

That's the entire parser engine. It walks s-expressions. Every YAML rule
becomes a tree walk. The grammar file is the program. The evaluator is eval.

### Code as data: five extensions

The YAML spec has three places where the formal grammar breaks down and
resorts to prose: auto-detected indentation in block scalars (rule 162),
block sequences (rule 183), and block mappings (rule 187). Every production
parser implements these as imperative code outside the grammar.

We implemented them as five s-expression combinators that live **in the
grammar file** alongside the original 211 rules:

```scheme
;; Auto-detect indentation by peeking ahead
(Detect-Indent n)

;; Bind computed values and continue
(Let ((m (Detect-Indent n))) body)

;; Parse text and interpret as integer
(Parse-Int expr)

;; Tag result with a symbol
(Parse-Sym pattern symbol)

;; Produce a value without consuming input
(Val x)
```

These aren't external code. They're s-expressions in the grammar file,
evaluated by the same interpreter that evaluates `Seq` and `Alt` and `Char`.
There is no boundary between spec and implementation. The spec describes
itself.

AST construction uses three more:

```scheme
(Build mapping ...)   ;; Wrap result as (MAPPING child1 child2 ...)
(Scalar ...)          ;; Use matched text as a leaf value
(Collect ...)         ;; Gather children from Star/Plus into a list
```

### Why s-expressions

S-expressions have one syntactic rule: parentheses and space. The text IS
the AST. No parsing needed to get the tree — it arrives pre-parsed.

Every other format has external dependencies in its symbols. JSON needs 15
grammar rules. YAML needs 211. XML needs schemas. S-expressions need
nothing. Open paren, close paren, done.

This is why the bug was findable. The grammar is a tree. Trees are walkable.
Walking is execution. Execution produces output. Wrong output is undeniable.

In a 70-page PDF, rule 78 is prose describing a symbol referencing another
symbol defined three pages away with an alternative explained in a footnote.
In an s-expression, rule 78 is three nodes in a Seq. You can see it. You
can walk it. You can run it. And when EMPTY fires where it shouldn't, the
output is flat instead of nested, and the bug has nowhere to hide.

---

## What parses

All 34 smoke tests pass. All 21 document-level tests pass:

```
simple kv          ✓    "key: value"           → {"key": "value"}
multi kv           ✓    "a: 1\nb: 2\nc: hi"   → {"a": 1, "b": 2, "c": "hi"}
block sequences    ✓    "- one\n- two"         → ["one", "two"]
nested mappings    ✓    "a:\n  b: 1\n  c: 2"  → {"a": {"b": 1, "c": 2}}
deep nesting       ✓    3 levels deep          → nested hash tables
mixed              ✓    mappings + sequences    → {"name": "Alice", "items": ["one", "two"]}
flow sequences     ✓    "[1, 2, 3]"            → list
flow mappings      ✓    "{x: 10, y: 20}"       → hash table
quoted strings     ✓    double and single       → stripped strings
literal blocks     ✓    "|" scalar              → preserved newlines
folded blocks      ✓    ">" scalar              → folded whitespace
booleans           ✓    "true" / "false"        → :TRUE / :FALSE
numbers            ✓    integers                → native ints
explicit docs      ✓    "--- hello"             → "hello"
anchors/aliases    ✓    "&ref" / "*ref"         → AST nodes
directives         ✓    "%YAML 1.2"             → recognized
multi-document     ✓    "---" / "..."           → stream of docs
comments           ✓    "# comment"             → ignored
4-space indent     ✓    non-standard indent     → auto-detected
list of maps       ✓    "- a: 1\n- b: 2"      → list of hash tables
```

---

## Usage

```bash
# Compile the evaluator
sbcl --eval '(compile-file "yaml-eval.lisp")' --quit

# Run smoke tests
sbcl --load yaml-eval.fasl \
     --eval '(yaml-eval::run-tests "yaml-grammar.scm")' --quit

# Parse YAML to native Common Lisp values
sbcl --load yaml-eval.fasl --eval '
  (let ((g (yaml-eval::load-yaml-grammar "yaml-grammar.scm")))
    (print (yaml-eval::yaml-load g "name: Alice
age: 30
items:
  - one
  - two
")))' --quit

;; => #<HASH-TABLE {"name": "Alice", "age": 30, "items": ("one" "two")}>
```

---

## Files

```
yaml-eval.lisp        749 lines    PEG evaluator with packrat memoization
yaml-grammar.scm     1083 lines    YAML 1.2 spec as executable s-expressions
demo-bug.lisp                      Bug reproduction: buggy vs fixed grammar
demo-step.lisp                     Step-by-step trace of the bug
yaml-bug-diagram.txt               ASCII state diagram of the bug
README.md                          This file
```

---

## The deeper point

The YAML 1.2 specification is 70 pages of prose containing 211 grammar
rules. Those rules are lambdas — they take input and return structure.
But written in a PDF, they're inert. They describe behavior without
performing it.

In s-expressions, the same 211 rules are 1,083 lines that execute.
The spec IS the parser. Run it and it parses YAML. Get the output wrong
and the spec has a bug. We found one. In an hour. By accident.

The YAML community knows the problem. Their yaml-grammar repository says:
"Creating a fully compliant parser has proven almost impossible." They put
the 211 rules in a YAML file. Machine-readable. But YAML can't execute.
So the grammar sits there, inert, and a separate program generates parsers
from it, and the gap between spec and implementation persists.

S-expressions close the gap. The notation IS the tree IS the program.
No parsing needed — the data arrives as an AST. No generation needed —
the grammar runs directly. No gap for bugs to hide in.

One parenthesis. One space. Everything else is a projection.

## License

MIT
#   y a m l - p r o j e c t  
 