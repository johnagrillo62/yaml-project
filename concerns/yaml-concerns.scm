; ==========================================================================
; yaml-concerns.scm — Post-parse concern layer as algorithm spec
; ==========================================================================
;
; Three concerns that turn an AST into a usable YAML library:
;   1. Schema coercion  — "true" → bool, "42" → int, "~" → null
;   2. Anchor resolution — &name / *name → resolved references
;   3. Native conversion — MAPPING→map, SEQUENCE→list, SCALAR→typed value
;
; Project with any target emit-spec → concern functions in that language.
; body-to-target compiles these forms using codegen-algo primitives.
;
; ==========================================================================

(Algorithm YamlConcerns

  ; ── Function: coerce scalar string to typed value ───────────────────
  ; YAML Core Schema: null, bool, int, float, string

  (Function coerceScalar ((s String))

    ; Null
    (When (or (eq s "null") (or (eq s "Null") (or (eq s "NULL")
          (or (eq s "~")    (empty s)))))
      (Return (make-null)))

    ; Bool
    (When (or (eq s "true") (or (eq s "True") (eq s "TRUE")))
      (Return (make-bool true)))
    (When (or (eq s "false") (or (eq s "False") (eq s "FALSE")))
      (Return (make-bool false)))

    ; Infinity
    (When (or (eq s ".inf") (or (eq s ".Inf") (or (eq s ".INF") (eq s "+.inf"))))
      (Return (make-float-inf)))
    (When (or (eq s "-.inf") (or (eq s "-.Inf") (eq s "-.INF")))
      (Return (make-float-neg-inf)))

    ; NaN
    (When (or (eq s ".nan") (or (eq s ".NaN") (eq s ".NAN")))
      (Return (make-float-nan)))

    ; Try integer parse
    (Let i (try-parse-int s))
    (When (parse-ok i)
      (Return (make-int (parse-val i))))

    ; Try float parse
    (Let f (try-parse-float s))
    (When (parse-ok f)
      (Return (make-float (parse-val f))))

    ; Default: string
    (make-str s))

  ; ── Function: convert AST node to native value ─────────────────────
  ; Recursive tree walk with anchor resolution.
  ; The anchors map is passed by reference.

  (Function convert ((node AstNode) (anchors AnchorMap))

    (When (ast-is-leaf node)
      (Return (coerceScalar (ast-text node))))

    (Let tag (ast-tag node))
    (Let children (ast-children node))

    ; ── ANCHOR: store value, return it ──
    (When (eq tag "ANCHOR")
      (Let name "")
      (Let val (make-null))
      (ForEach ch children
        (If (and (ast-is-leaf ch) (empty name))
          (Then (Set name (ast-text ch)))
          (Else (Set val (convert ch anchors)))))
      (When (not (empty name))
        (anchor-store anchors name val))
      (Return val))

    ; ── ALIAS: look up anchor ──
    (When (eq tag "ALIAS")
      (ForEach ch children
        (When (ast-is-leaf ch)
          (Let found (anchor-lookup anchors (ast-text ch)))
          (When (not (is-null found))
            (Return found))))
      (Return (make-null)))

    ; ── MAPPING: build key→value map ──
    (When (eq tag "MAPPING")
      (Let m (make-empty-map))
      (ForEach ch children
        (When (and (eq (ast-tag ch) "PAIR")
                   (ge (ast-child-count ch) 2))
          (Let key (convert (ast-child ch 0) anchors))
          (Let val (convert (ast-child ch 1) anchors))
          ; Merge key support (<<)
          (If (and (eq (val-str key) "<<") (is-map val))
            (Then (merge-map m val))
            (Else (map-set m (val-str key) val)))))
      (Return (make-map-val m)))

    ; ── SEQUENCE: build list ──
    (When (eq tag "SEQUENCE")
      (Let seq (make-empty-seq))
      (ForEach ch children
        (seq-push seq (convert ch anchors)))
      (Return (make-seq-val seq)))

    ; ── DOC / STREAM: unwrap single child ──
    (When (or (eq tag "DOC") (eq tag "STREAM"))
      (When (eq (ast-child-count node) 1)
        (Return (convert (ast-child node 0) anchors)))
      (Let docs (make-empty-seq))
      (ForEach ch children
        (seq-push docs (convert ch anchors)))
      (When (eq (seq-len docs) 1)
        (Return (seq-at docs 0)))
      (Return (make-seq-val docs)))

    ; ── PAIR outside mapping: return value ──
    (When (eq tag "PAIR")
      (When (ge (ast-child-count node) 2)
        (Return (convert (ast-child node 1) anchors))))

    ; ── Fallback: recurse ──
    (When (eq (ast-child-count node) 1)
      (Return (convert (ast-child node 0) anchors)))
    (Let items (make-empty-seq))
    (ForEach ch children
      (seq-push items (convert ch anchors)))
    (make-seq-val items))

  ; ── Function: public load API ──────────────────────────────────────

  (Function yamlLoad ((text String))
    (Let result (parse text))
    (When (parse-failed result)
      (Return (make-null)))
    (Let anchors (make-empty-anchor-map))
    (convert (parse-ast result) anchors)))
