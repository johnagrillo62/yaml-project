; ==========================================================================
; yaml-grammar.scm  YAML 1.2 grammar as s-expressions
; ==========================================================================
;
; 211 rules from the YAML 1.2 spec, expressed as s-expressions.
; This is the grammar that every YAML parser implements (or tries to).
;
; Notation:
;   (Rule N name body)            production rule #N
;   (Alt a b c)                   ordered alternatives (first match wins)
;   (Seq a b c)                   sequence (all must match in order)
;   (Range lo hi)                 Unicode code point range
;   (Char x)                      single character literal
;   (Hex x)                       Unicode code point by hex value
;   (Opt x)                       optional (zero or one)
;   (Star x)                      zero or more (greedy)
;   (Plus x)                      one or more (greedy)
;   (Ref name)                    reference to another rule
;   (Not x)                       negative lookahead
;   (Param name args body)        parameterized rule
;   (Switch param cases)          context-dependent dispatch
;   (Minus a b)                   set difference (a but not b)
;   (Empty)                       matches nothing (epsilon)
;   (Repeat n x)                  exactly n repetitions
;
; Parameters used in YAML's "parameterized BNF":
;   n, m    indentation level (integer)
;   c       context: block-in, block-out, flow-in, flow-out, block-key, flow-key
;   t       chomping: strip, clip, keep
;
; ==========================================================================

(Grammar YAML-1.2

  ; 
  ; Chapter 5: Character Productions
  ; 

  ;  5.1 Character Set 

  (Rule 1 c-printable
    (Alt (Hex 09)                         ; Tab
         (Hex 0A)                         ; LF
         (Hex 0D)                         ; CR
         (Range (Hex 20) (Hex 7E))        ; Printable ASCII
         (Hex 85)                         ; NEL
         (Range (Hex A0) (Hex D7FF))      ; Basic Multilingual Plane
         (Range (Hex E000) (Hex FFFD))    ; BMP continued
         (Range (Hex 10000) (Hex 10FFFF)))) ; Supplementary planes

  (Rule 2 nb-json
    (Alt (Hex 09)                         ; Tab
         (Range (Hex 20) (Hex 10FFFF))))  ; All non-C0

  ;  5.2 Character Encodings 

  (Rule 3 c-byte-order-mark
    (Hex FEFF))

  ;  5.3 Indicator Characters 

  (Rule 4  c-sequence-entry   (Char '-'))
  (Rule 5  c-mapping-key      (Char '?'))
  (Rule 6  c-mapping-value    (Char ':'))
  (Rule 7  c-collect-entry    (Char ','))
  (Rule 8  c-sequence-start   (Char '['))
  (Rule 9  c-sequence-end     (Char ']'))
  (Rule 10 c-mapping-start    (Char '{'))
  (Rule 11 c-mapping-end      (Char '}'))
  (Rule 12 c-comment          (Char '#'))
  (Rule 13 c-anchor           (Char '&'))
  (Rule 14 c-alias            (Char '*'))
  (Rule 15 c-tag              (Char '!'))
  (Rule 16 c-literal          (Char '|'))
  (Rule 17 c-folded           (Char '>'))
  (Rule 18 c-single-quote     (Char SQUOTE))
  (Rule 19 c-double-quote     (Char DQUOTE))
  (Rule 20 c-directive        (Char '%'))

  (Rule 21 c-reserved
    (Alt (Char '@')
         (Char '`')))

  (Rule 22 c-indicator
    (Alt (Ref c-sequence-entry)
         (Ref c-mapping-key)
         (Ref c-mapping-value)
         (Ref c-collect-entry)
         (Ref c-sequence-start)
         (Ref c-sequence-end)
         (Ref c-mapping-start)
         (Ref c-mapping-end)
         (Ref c-comment)
         (Ref c-anchor)
         (Ref c-alias)
         (Ref c-tag)
         (Ref c-literal)
         (Ref c-folded)
         (Ref c-single-quote)
         (Ref c-double-quote)
         (Ref c-directive)
         (Ref c-reserved)))

  (Rule 23 c-flow-indicator
    (Alt (Ref c-collect-entry)
         (Ref c-sequence-start)
         (Ref c-sequence-end)
         (Ref c-mapping-start)
         (Ref c-mapping-end)))

  ;  5.4 Line Break Characters 

  (Rule 24 b-line-feed      (Hex 0A))
  (Rule 25 b-carriage-return (Hex 0D))

  (Rule 26 b-char
    (Alt (Ref b-line-feed)
         (Ref b-carriage-return)))

  (Rule 27 nb-char
    (Minus (Ref c-printable)
           (Alt (Ref b-char) (Ref c-byte-order-mark))))

  (Rule 28 b-break
    (Alt (Seq (Ref b-carriage-return) (Ref b-line-feed))  ; CRLF
         (Ref b-carriage-return)                           ; CR
         (Ref b-line-feed)))                               ; LF

  (Rule 29 b-as-line-feed
    (Ref b-break))

  (Rule 30 b-non-content
    (Ref b-break))

  ;  5.5 White Space Characters 

  (Rule 31 s-space (Hex 20))
  (Rule 32 s-tab   (Hex 09))

  (Rule 33 s-white
    (Alt (Ref s-space)
         (Ref s-tab)))

  (Rule 34 ns-char
    (Minus (Ref nb-char)
           (Ref s-white)))

  ;  5.6 Miscellaneous Characters 

  (Rule 35 ns-dec-digit
    (Range (Hex 30) (Hex 39)))  ; 0-9

  (Rule 36 ns-hex-digit
    (Alt (Ref ns-dec-digit)
         (Range (Hex 41) (Hex 46))    ; A-F
         (Range (Hex 61) (Hex 66))))  ; a-f

  (Rule 37 ns-ascii-letter
    (Alt (Range (Hex 41) (Hex 5A))    ; A-Z
         (Range (Hex 61) (Hex 7A))))  ; a-z

  (Rule 38 ns-word-char
    (Alt (Ref ns-dec-digit)
         (Ref ns-ascii-letter)
         (Char '-')))

  (Rule 39 ns-uri-char
    (Alt (Seq (Char '%') (Ref ns-hex-digit) (Ref ns-hex-digit))
         (Ref ns-word-char)
         (Char '#') (Char ';') (Char '/') (Char '?') (Char ':')
         (Char '@') (Char '&') (Char '=') (Char '+') (Char '$')
         (Char ',') (Char '_') (Char '.') (Char '!') (Char '~')
         (Char '*') (Char SQUOTE) (Char '(') (Char ')') (Char '[')
         (Char ']')))

  (Rule 40 ns-tag-char
    (Minus (Ref ns-uri-char)
           (Alt (Ref c-tag) (Ref c-flow-indicator))))

  ;  5.7 Escaped Characters 

  (Rule 41 c-escape (Char '\\'))

  (Rule 42 ns-esc-null          (Char '0'))
  (Rule 43 ns-esc-bell          (Char 'a'))
  (Rule 44 ns-esc-backspace     (Char 'b'))
  (Rule 45 ns-esc-horizontal-tab (Char 't'))
  (Rule 46 ns-esc-line-feed     (Char 'n'))
  (Rule 47 ns-esc-vertical-tab  (Char 'v'))
  (Rule 48 ns-esc-form-feed     (Char 'f'))
  (Rule 49 ns-esc-carriage-return (Char 'r'))
  (Rule 50 ns-esc-escape        (Char 'e'))
  (Rule 51 ns-esc-space         (Hex 20))
  (Rule 52 ns-esc-double-quote  (Char DQUOTE))
  (Rule 53 ns-esc-slash         (Char '/'))
  (Rule 54 ns-esc-backslash     (Char '\\'))
  (Rule 55 ns-esc-next-line     (Char 'N'))
  (Rule 56 ns-esc-non-breaking-space (Char '_'))
  (Rule 57 ns-esc-line-separator (Char 'L'))
  (Rule 58 ns-esc-paragraph-separator (Char 'P'))

  (Rule 59 ns-esc-8-bit
    (Seq (Char 'x')
         (Repeat 2 (Ref ns-hex-digit))))

  (Rule 60 ns-esc-16-bit
    (Seq (Char 'u')
         (Repeat 4 (Ref ns-hex-digit))))

  (Rule 61 ns-esc-32-bit
    (Seq (Char 'U')
         (Repeat 8 (Ref ns-hex-digit))))

  (Rule 62 c-ns-esc-char
    (Seq (Ref c-escape)
         (Alt (Ref ns-esc-null) (Ref ns-esc-bell)
              (Ref ns-esc-backspace) (Ref ns-esc-horizontal-tab)
              (Ref ns-esc-line-feed) (Ref ns-esc-vertical-tab)
              (Ref ns-esc-form-feed) (Ref ns-esc-carriage-return)
              (Ref ns-esc-escape) (Ref ns-esc-space)
              (Ref ns-esc-double-quote) (Ref ns-esc-slash)
              (Ref ns-esc-backslash) (Ref ns-esc-next-line)
              (Ref ns-esc-non-breaking-space) (Ref ns-esc-line-separator)
              (Ref ns-esc-paragraph-separator)
              (Ref ns-esc-8-bit) (Ref ns-esc-16-bit)
              (Ref ns-esc-32-bit))))

  ; 
  ; Chapter 6: Structural Productions
  ; 

  ;  6.1 Indentation Spaces 

  (Rule 63 s-indent (n)
    (Repeat n (Ref s-space)))

  (Rule 64 s-indent-lt (n)
    (Star (Ref s-space))
    ; where count < n
    )

  (Rule 65 s-indent-le (n)
    (Star (Ref s-space))
    ; where count <= n
    )

  ;  6.2 Separation Spaces 

  (Rule 66 s-separate-in-line
    (Alt (Plus (Ref s-white))
         (Empty)))  ; start of line

  ;  6.3 Line Prefixes 

  (Rule 67 s-line-prefix (n c)
    (Switch c
      (block-in  (Ref s-block-line-prefix n))
      (block-out (Ref s-block-line-prefix n))
      (flow-in   (Ref s-flow-line-prefix n))
      (flow-out  (Ref s-flow-line-prefix n))))

  (Rule 68 s-block-line-prefix (n)
    (Ref s-indent n))

  (Rule 69 s-flow-line-prefix (n)
    (Seq (Ref s-indent n)
         (Opt (Ref s-separate-in-line))))

  ;  6.4 Empty Lines 

  (Rule 70 l-empty (n c)
    (Seq (Alt (Ref s-line-prefix n c)
              (Ref s-indent-lt n))
         (Ref b-as-line-feed)))

  ;  6.5 Line Folding 

  (Rule 71 b-l-trimmed (n c)
    (Seq (Ref b-non-content)
         (Plus (Ref l-empty n c))))

  (Rule 72 b-as-space
    (Ref b-break))

  (Rule 73 b-l-folded (n c)
    (Alt (Ref b-l-trimmed n c)
         (Ref b-as-space)))

  (Rule 74 s-flow-folded (n)
    (Seq (Opt (Ref s-separate-in-line))
         (Ref b-l-folded n flow-in)
         (Ref s-flow-line-prefix n)))

  ;  6.6 Comments 

  (Rule 75 c-nb-comment-text
    (Seq (Ref c-comment)
         (Star (Ref nb-char))))

  (Rule 76 b-comment
    (Alt (Ref b-non-content)
         (Empty)))  ; end of input

  (Rule 77 s-b-comment
    (Seq (Opt (Seq (Ref s-separate-in-line)
                   (Opt (Ref c-nb-comment-text))))
         (Ref b-comment)))

  (Rule 78 l-comment
    (Seq (Ref s-separate-in-line)
         (Opt (Ref c-nb-comment-text))
         (Ref b-non-content)))

  (Rule 79 s-l-comments
    (Seq (Alt (Ref s-b-comment) (Empty))  ; start of line
         (Star (Ref l-comment))))

  ;  6.7 Separation Lines 

  (Rule 80 s-separate (n c)
    (Switch c
      (block-out  (Ref s-separate-lines n))
      (block-in   (Ref s-separate-lines n))
      (flow-out   (Ref s-separate-lines n))
      (flow-in    (Ref s-separate-lines n))
      (block-key  (Ref s-separate-in-line))
      (flow-key   (Ref s-separate-in-line))))

  (Rule 81 s-separate-lines (n)
    (Alt (Seq (Ref s-l-comments)
              (Ref s-flow-line-prefix n))
         (Ref s-separate-in-line)))

  ; 
  ; Chapter 6 continued: Directives, Tags, Anchors
  ; 

  ;  6.8 Directives 

  (Rule 82 l-directive
    (Seq (Ref c-directive)
         (Alt (Ref ns-yaml-directive)
              (Ref ns-tag-directive)
              (Ref ns-reserved-directive))
         (Ref s-l-comments)))

  (Rule 83 ns-reserved-directive
    (Seq (Ref ns-directive-name)
         (Star (Seq (Ref s-separate-in-line)
                    (Ref ns-directive-parameter)))))

  (Rule 84 ns-directive-name
    (Plus (Ref ns-char)))

  (Rule 85 ns-directive-parameter
    (Plus (Ref ns-char)))

  ;  6.8.1 YAML Directives 

  (Rule 86 ns-yaml-directive
    (Seq (Str "YAML")
         (Ref s-separate-in-line)
         (Ref ns-yaml-version)))

  (Rule 87 ns-yaml-version
    (Seq (Plus (Ref ns-dec-digit))
         (Char '.')
         (Plus (Ref ns-dec-digit))))

  ;  6.8.2 TAG Directives 

  (Rule 88 ns-tag-directive
    (Seq (Str "TAG")
         (Ref s-separate-in-line)
         (Ref c-tag-handle)
         (Ref s-separate-in-line)
         (Ref ns-tag-prefix)))

  (Rule 89 c-tag-handle
    (Alt (Ref c-named-tag-handle)
         (Ref c-secondary-tag-handle)
         (Ref c-primary-tag-handle)))

  (Rule 90 c-primary-tag-handle
    (Char '!'))

  (Rule 91 c-secondary-tag-handle
    (Str "!!"))

  (Rule 92 c-named-tag-handle
    (Seq (Char '!')
         (Plus (Ref ns-word-char))
         (Char '!')))

  (Rule 93 ns-tag-prefix
    (Alt (Ref c-ns-local-tag-prefix)
         (Ref ns-global-tag-prefix)))

  (Rule 94 c-ns-local-tag-prefix
    (Seq (Char '!')
         (Star (Ref ns-uri-char))))

  (Rule 95 ns-global-tag-prefix
    (Seq (Ref ns-tag-char)
         (Star (Ref ns-uri-char))))

  ;  6.9 Node Properties 

  (Rule 96 c-ns-properties (n c)
    (Alt (Seq (Ref c-ns-tag-property)
              (Opt (Seq (Ref s-separate n c)
                        (Ref c-ns-anchor-property))))
         (Seq (Ref c-ns-anchor-property)
              (Opt (Seq (Ref s-separate n c)
                        (Ref c-ns-tag-property))))))

  (Rule 97 c-ns-tag-property
    (Alt (Ref c-verbatim-tag)
         (Ref c-ns-shorthand-tag)
         (Ref c-non-specific-tag)))

  (Rule 98 c-verbatim-tag
    (Seq (Str "!<")
         (Plus (Ref ns-uri-char))
         (Char '>')))

  (Rule 99 c-ns-shorthand-tag
    (Seq (Ref c-tag-handle)
         (Plus (Ref ns-tag-char))))

  (Rule 100 c-non-specific-tag
    (Char '!'))

  (Rule 101 c-ns-anchor-property
    (Build anchor
      (Seq (Ref c-anchor)
           (Scalar (Ref ns-anchor-name)))))

  (Rule 102 ns-anchor-char
    (Minus (Ref ns-char)
           (Ref c-flow-indicator)))

  (Rule 103 ns-anchor-name
    (Plus (Ref ns-anchor-char)))

  ; 
  ; Chapter 7: Flow Style Productions
  ; 

  ;  7.1 Alias Nodes 

  (Rule 104 c-ns-alias-node
    (Build alias
      (Seq (Ref c-alias)
           (Scalar (Ref ns-anchor-name)))))

  ;  7.2 Empty Nodes 

  (Rule 105 e-scalar
    (Empty))

  (Rule 106 e-node
    (Ref e-scalar))

  ;  7.3 Flow Scalar Styles 

  ;  7.3.1 Double-Quoted Style 

  (Rule 107 nb-double-char
    (Alt (Ref c-ns-esc-char)
         (Minus (Ref nb-json)
                (Alt (Char '\\') (Char DQUOTE)))))

  (Rule 108 ns-double-char
    (Minus (Ref nb-double-char)
           (Ref s-white)))

  (Rule 109 c-double-quoted (n c)
    (Scalar (Seq (Char DQUOTE)
                 (Ref nb-double-text n c)
                 (Char DQUOTE))))

  (Rule 110 nb-double-text (n c)
    (Switch c
      (flow-out  (Ref nb-double-multi-line n))
      (flow-in   (Ref nb-double-multi-line n))
      (block-key (Ref nb-double-one-line))
      (flow-key  (Ref nb-double-one-line))))

  (Rule 111 nb-double-one-line
    (Star (Ref nb-double-char)))

  (Rule 112 s-double-escaped (n)
    (Seq (Star (Ref s-white))
         (Char '\\')
         (Ref b-non-content)
         (Star (Ref l-empty n flow-in))
         (Ref s-flow-line-prefix n)))

  (Rule 113 s-double-break (n)
    (Alt (Ref s-double-escaped n)
         (Ref s-flow-folded n)))

  (Rule 114 nb-ns-double-in-line
    (Star (Seq (Star (Ref s-white))
               (Ref ns-double-char))))

  (Rule 115 s-double-next-line (n)
    (Seq (Ref s-double-break n)
         (Opt (Seq (Ref ns-double-char)
                   (Ref nb-ns-double-in-line)
                   (Alt (Ref s-double-next-line n)
                        (Star (Ref s-white)))))))

  (Rule 116 nb-double-multi-line (n)
    (Seq (Ref nb-ns-double-in-line)
         (Alt (Ref s-double-next-line n)
              (Star (Ref s-white)))))

  ;  7.3.2 Single-Quoted Style 

  (Rule 117 c-quoted-quote
    (Str "''"))

  (Rule 118 nb-single-char
    (Alt (Ref c-quoted-quote)
         (Minus (Ref nb-json)
                (Char SQUOTE))))

  (Rule 119 ns-single-char
    (Minus (Ref nb-single-char)
           (Ref s-white)))

  (Rule 120 c-single-quoted (n c)
    (Scalar (Seq (Char SQUOTE)
                 (Ref nb-single-text n c)
                 (Char SQUOTE))))

  (Rule 121 nb-single-text (n c)
    (Switch c
      (flow-out  (Ref nb-single-multi-line n))
      (flow-in   (Ref nb-single-multi-line n))
      (block-key (Ref nb-single-one-line))
      (flow-key  (Ref nb-single-one-line))))

  (Rule 122 nb-single-one-line
    (Star (Ref nb-single-char)))

  (Rule 123 ns-single-in-line
    (Star (Seq (Star (Ref s-white))
               (Ref ns-single-char))))

  (Rule 124 s-single-next-line (n)
    (Seq (Ref s-flow-folded n)
         (Opt (Seq (Ref ns-single-char)
                   (Ref ns-single-in-line)
                   (Alt (Ref s-single-next-line n)
                        (Star (Ref s-white)))))))

  (Rule 125 nb-single-multi-line (n)
    (Seq (Ref ns-single-in-line)
         (Alt (Ref s-single-next-line n)
              (Star (Ref s-white)))))

  ;  7.3.3 Plain Style 

  (Rule 126 ns-plain-first (c)
    (Alt (Minus (Ref ns-char)
                (Ref c-indicator))
         (Seq (Alt (Char '?') (Char ':') (Char '-'))
              (Lookahead (Ref ns-plain-safe c)))))

  (Rule 127 ns-plain-safe (c)
    (Switch c
      (flow-out  (Ref ns-plain-safe-out))
      (flow-in   (Ref ns-plain-safe-in))
      (block-key (Ref ns-plain-safe-out))
      (flow-key  (Ref ns-plain-safe-in))))

  (Rule 128 ns-plain-safe-out
    (Ref ns-char))

  (Rule 129 ns-plain-safe-in
    (Minus (Ref ns-char)
           (Ref c-flow-indicator)))

  (Rule 130 ns-plain-char (c)
    (Alt (Minus (Ref ns-plain-safe c)
                (Alt (Char ':') (Char '#')))
         (Seq (Lookbehind (Ref ns-char))
              (Char '#'))
         (Seq (Char ':')
              (Lookahead (Ref ns-plain-safe c)))))

  (Rule 131 ns-plain (n c)
    (Scalar
      (Switch c
        (flow-out  (Ref ns-plain-multi-line n c))
        (flow-in   (Ref ns-plain-multi-line n c))
        (block-key (Ref ns-plain-one-line c))
        (flow-key  (Ref ns-plain-one-line c)))))

  (Rule 132 nb-ns-plain-in-line (c)
    (Star (Seq (Star (Ref s-white))
               (Ref ns-plain-char c))))

  (Rule 133 ns-plain-one-line (c)
    (Seq (Ref ns-plain-first c)
         (Ref nb-ns-plain-in-line c)))

  (Rule 134 s-ns-plain-next-line (n c)
    (Seq (Ref s-flow-folded n)
         (Not (Ref c-forbidden))
         (Ref ns-plain-char c)
         (Ref nb-ns-plain-in-line c)))

  (Rule 135 ns-plain-multi-line (n c)
    (Seq (Ref ns-plain-one-line c)
         (Star (Ref s-ns-plain-next-line n c))))

  ;  7.4 Flow Collection Styles 

  ;  7.4.1 Flow Sequences 

  (Rule 136 in-flow (c)
    (Switch c
      (flow-out  flow-in)
      (flow-in   flow-in)
      (block-key flow-key)
      (flow-key  flow-key)))

  (Rule 137 c-flow-sequence (n c)
    (Build sequence
      (Seq (Char '[')
           (Opt (Ref s-separate n c))
           (Opt (Collect (Ref ns-s-flow-seq-entries n (in-flow c))))
           (Char ']'))))

  (Rule 138 ns-s-flow-seq-entries (n c)
    (Seq (Ref ns-flow-seq-entry n c)
         (Opt (Ref s-separate n c))
         (Opt (Seq (Char ',')
                   (Opt (Ref s-separate n c))
                   (Opt (Ref ns-s-flow-seq-entries n c))))))

  (Rule 139 ns-flow-seq-entry (n c)
    (Alt (Ref ns-flow-pair n c)
         (Ref ns-flow-node n c)))

  ;  7.4.2 Flow Mappings 

  (Rule 140 c-flow-mapping (n c)
    (Build mapping
      (Seq (Char '{')
           (Opt (Ref s-separate n c))
           (Opt (Collect (Ref ns-s-flow-map-entries n (in-flow c))))
           (Char '}'))))

  (Rule 141 ns-s-flow-map-entries (n c)
    (Seq (Ref ns-flow-map-entry n c)
         (Opt (Ref s-separate n c))
         (Opt (Seq (Char ',')
                   (Opt (Ref s-separate n c))
                   (Opt (Ref ns-s-flow-map-entries n c))))))

  (Rule 142 ns-flow-map-entry (n c)
    (Alt (Seq (Char '?')
              (Ref s-separate n c)
              (Ref ns-flow-map-explicit-entry n c))
         (Ref ns-flow-map-implicit-entry n c)))

  (Rule 143 ns-flow-map-explicit-entry (n c)
    (Alt (Ref ns-flow-map-implicit-entry n c)
         (Seq (Ref e-node)
              (Ref e-node))))

  (Rule 144 ns-flow-map-implicit-entry (n c)
    (Build pair
      (Alt (Ref ns-flow-map-yaml-key-entry n c)
           (Ref c-ns-flow-map-empty-key-entry n c)
           (Ref c-ns-flow-map-json-key-entry n c))))

  (Rule 145 ns-flow-map-yaml-key-entry (n c)
    (Seq (Ref ns-flow-yaml-node n c)
         (Alt (Seq (Opt (Ref s-separate n c))
                   (Ref c-ns-flow-map-separate-value n c))
              (Ref e-node))))

  (Rule 146 c-ns-flow-map-empty-key-entry (n c)
    (Seq (Ref e-node)
         (Ref c-ns-flow-map-separate-value n c)))

  (Rule 147 c-ns-flow-map-separate-value (n c)
    (Seq (Char ':')
         (Not (Ref ns-plain-safe c))
         (Alt (Seq (Ref s-separate n c)
                   (Ref ns-flow-node n c))
              (Ref e-node))))

  (Rule 148 c-ns-flow-map-json-key-entry (n c)
    (Seq (Ref c-flow-json-node n c)
         (Alt (Seq (Opt (Ref s-separate n c))
                   (Ref c-ns-flow-map-adjacent-value n c))
              (Ref e-node))))

  (Rule 149 c-ns-flow-map-adjacent-value (n c)
    (Seq (Char ':')
         (Alt (Seq (Opt (Ref s-separate n c))
                   (Ref ns-flow-node n c))
              (Ref e-node))))

  (Rule 150 ns-flow-pair (n c)
    (Alt (Seq (Char '?')
              (Ref s-separate n c)
              (Ref ns-flow-map-explicit-entry n c))
         (Ref ns-flow-pair-entry n c)))

  (Rule 151 ns-flow-pair-entry (n c)
    (Alt (Ref ns-flow-pair-yaml-key-entry n c)
         (Ref c-ns-flow-map-empty-key-entry n c)
         (Ref c-ns-flow-pair-json-key-entry n c)))

  (Rule 152 ns-flow-pair-yaml-key-entry (n c)
    (Seq (Ref ns-s-implicit-yaml-key flow-key)
         (Ref c-ns-flow-map-separate-value n c)))

  (Rule 153 c-ns-flow-pair-json-key-entry (n c)
    (Seq (Ref c-s-implicit-json-key flow-key)
         (Ref c-ns-flow-map-adjacent-value n c)))

  (Rule 154 ns-s-implicit-yaml-key (c)
    ; limited to 1024 chars
    (Seq (Ref ns-flow-yaml-node Empty c)
         (Opt (Ref s-separate-in-line))))

  (Rule 155 c-s-implicit-json-key (c)
    ; limited to 1024 chars
    (Seq (Ref c-flow-json-node Empty c)
         (Opt (Ref s-separate-in-line))))

  ;  7.5 Flow Nodes 

  (Rule 156 ns-flow-yaml-content (n c)
    (Ref ns-plain n c))

  (Rule 157 c-flow-json-content (n c)
    (Alt (Ref c-flow-sequence n c)
         (Ref c-flow-mapping n c)
         (Ref c-single-quoted n c)
         (Ref c-double-quoted n c)))

  (Rule 158 ns-flow-content (n c)
    (Alt (Ref ns-flow-yaml-content n c)
         (Ref c-flow-json-content n c)))

  (Rule 159 ns-flow-yaml-node (n c)
    (Alt (Ref c-ns-alias-node)
         (Ref ns-flow-yaml-content n c)
         (Seq (Ref c-ns-properties n c)
              (Alt (Seq (Ref s-separate n c)
                        (Ref ns-flow-yaml-content n c))
                   (Ref e-scalar)))))

  (Rule 160 c-flow-json-node (n c)
    (Seq (Opt (Seq (Ref c-ns-properties n c)
                   (Ref s-separate n c)))
         (Ref c-flow-json-content n c)))

  (Rule 161 ns-flow-node (n c)
    (Alt (Ref c-ns-alias-node)
         (Ref ns-flow-content n c)
         (Seq (Ref c-ns-properties n c)
              (Alt (Seq (Ref s-separate n c)
                        (Ref ns-flow-content n c))
                   (Ref e-scalar)))))

  ; 
  ; Chapter 8: Block Style Productions
  ; 

  ;  8.1 Block Scalar Styles 

  ;  8.1.1 Block Scalar Headers 

  (Rule 162 c-b-block-header (n)
    (Alt
      (Let ((m (Alt (Parse-Int (Ref ns-dec-digit))
                    (Detect-Indent n)))
            (t (Alt (Parse-Sym (Char '-') strip)
                    (Parse-Sym (Char '+') keep)
                    (Val clip))))
        (Ref s-b-comment))
      (Let ((t (Alt (Parse-Sym (Char '-') strip)
                    (Parse-Sym (Char '+') keep)
                    (Val clip)))
            (m (Alt (Parse-Int (Ref ns-dec-digit))
                    (Detect-Indent n))))
        (Ref s-b-comment))))

  (Rule 163 c-indentation-indicator (n)
    ; digit 1-9 sets m explicitly, empty = auto-detect
    (Alt (Ref ns-dec-digit)   ; explicit
         (Empty)))            ; auto-detect

  (Rule 164 c-chomping-indicator
    (Alt (Char '-')           ; strip
         (Char '+')           ; keep
         (Empty)))            ; clip (default)

  ;  8.1.1.2 Block Chomping 

  (Rule 165 b-chomped-last (t)
    (Switch t
      (strip (Ref b-non-content))
      (clip  (Ref b-as-line-feed))
      (keep  (Ref b-as-line-feed))))

  (Rule 166 l-chomped-empty (n t)
    (Switch t
      (strip (Ref l-strip-empty n))
      (clip  (Ref l-strip-empty n))
      (keep  (Ref l-keep-empty n))))

  (Rule 167 l-strip-empty (n)
    (Seq (Star (Seq (Ref s-indent-le n) (Ref b-non-content)))
         (Opt (Ref l-trail-comments n))))

  (Rule 168 l-keep-empty (n)
    (Seq (Star (Ref l-empty n block-in))
         (Opt (Ref l-trail-comments n))))

  (Rule 169 l-trail-comments (n)
    (Seq (Ref s-indent-lt n)
         (Ref c-nb-comment-text)
         (Ref b-comment)
         (Star (Ref l-comment))))

  ;  8.1.2 Literal Style 

  (Rule 170 c-l+literal (n)
    (Seq (Char '|')
         (Let ((m (Alt (Parse-Int (Ref ns-dec-digit))
                       (Detect-Indent n)))
               (t (Alt (Parse-Sym (Char '-') strip)
                       (Parse-Sym (Char '+') keep)
                       (Val clip))))
           (Seq (Ref s-b-comment)
                (Ref l-literal-content (+ n m) t)))))

  (Rule 171 l-nb-literal-text (n)
    (Seq (Star (Ref l-empty n block-in))
         (Ref s-indent n)
         (Plus (Ref nb-char))))

  (Rule 172 b-nb-literal-next (n)
    (Seq (Ref b-as-line-feed)
         (Ref l-nb-literal-text n)))

  (Rule 173 l-literal-content (n t)
    (Scalar
      (Seq (Opt (Seq (Ref l-nb-literal-text n)
                     (Star (Ref b-nb-literal-next n))
                     (Ref b-chomped-last t)))
           (Ref l-chomped-empty n t))))

  ;  8.1.3 Folded Style 

  (Rule 174 c-l+folded (n)
    (Seq (Char '>')
         (Let ((m (Alt (Parse-Int (Ref ns-dec-digit))
                       (Detect-Indent n)))
               (t (Alt (Parse-Sym (Char '-') strip)
                       (Parse-Sym (Char '+') keep)
                       (Val clip))))
           (Seq (Ref s-b-comment)
                (Ref l-folded-content (+ n m) t)))))

  (Rule 175 s-nb-folded-text (n)
    (Seq (Ref s-indent n)
         (Ref ns-char)
         (Star (Ref nb-char))))

  (Rule 176 l-nb-folded-lines (n)
    (Seq (Ref s-nb-folded-text n)
         (Star (Seq (Ref b-l-folded n block-in)
                    (Ref s-nb-folded-text n)))))

  (Rule 177 s-nb-spaced-text (n)
    (Seq (Ref s-indent n)
         (Ref s-white)
         (Star (Ref nb-char))))

  (Rule 178 b-l-spaced (n)
    (Seq (Ref b-as-line-feed)
         (Star (Ref l-empty n block-in))))

  (Rule 179 l-nb-spaced-lines (n)
    (Seq (Ref s-nb-spaced-text n)
         (Star (Seq (Ref b-l-spaced n)
                    (Ref s-nb-spaced-text n)))))

  (Rule 180 l-nb-same-lines (n)
    (Seq (Star (Ref l-empty n block-in))
         (Alt (Ref l-nb-folded-lines n)
              (Ref l-nb-spaced-lines n))))

  (Rule 181 l-nb-diff-lines (n)
    (Seq (Ref l-nb-same-lines n)
         (Star (Seq (Ref b-as-line-feed)
                    (Ref l-nb-same-lines n)))))

  (Rule 182 l-folded-content (n t)
    (Scalar
      (Seq (Opt (Seq (Ref l-nb-diff-lines n)
                     (Ref b-chomped-last t)))
           (Ref l-chomped-empty n t))))

  ;  8.2 Block Collection Styles 

  ;  8.2.1 Block Sequences 

  (Rule 183 l+block-sequence (n)
    (Build sequence
      (Let ((m (Detect-Indent n)))
        (Collect (Plus (Seq (Ref s-indent (+ n m))
                            (Ref c-l-block-seq-entry (+ n m))))))))

  (Rule 184 c-l-block-seq-entry (n)
    (Seq (Char '-')
         (Not (Ref ns-char))
         (Ref s-l+block-indented n block-in)))

  (Rule 185 s-l+block-indented (n c)
    (Alt (Let ((m (Detect-Indent 0)))
           (Seq (Ref s-indent m)
                (Alt (Ref ns-l-compact-sequence (+ n 1 m))
                     (Ref ns-l-compact-mapping (+ n 1 m)))))
         (Ref s-l+block-node n c)
         (Seq (Ref e-node) (Ref s-l-comments))))

  (Rule 186 ns-l-compact-sequence (n)
    (Seq (Ref c-l-block-seq-entry n)
         (Star (Seq (Ref s-indent n)
                    (Ref c-l-block-seq-entry n)))))

  ;  8.2.2 Block Mappings 

  (Rule 187 l+block-mapping (n)
    (Build mapping
      (Let ((m (Detect-Indent n)))
        (Collect (Plus (Seq (Ref s-indent (+ n m))
                            (Ref ns-l-block-map-entry (+ n m))))))))

  (Rule 188 ns-l-block-map-entry (n)
    (Alt (Ref c-l-block-map-explicit-entry n)
         (Ref ns-l-block-map-implicit-entry n)))

  (Rule 189 c-l-block-map-explicit-entry (n)
    (Seq (Ref c-l-block-map-explicit-key n)
         (Alt (Ref l-block-map-explicit-value n)
              (Ref e-node))))

  (Rule 190 c-l-block-map-explicit-key (n)
    (Seq (Char '?')
         (Ref s-l+block-indented n block-out)))

  (Rule 191 l-block-map-explicit-value (n)
    (Seq (Ref s-indent n)
         (Char ':')
         (Ref s-l+block-indented n block-out)))

  (Rule 192 ns-l-block-map-implicit-entry (n)
    (Build pair
      (Seq (Scalar (Alt (Ref ns-s-block-map-implicit-key)
                        (Ref e-node)))
           (Ref c-l-block-map-implicit-value n))))

  (Rule 193 ns-s-block-map-implicit-key
    (Alt (Ref c-s-implicit-json-key block-key)
         (Ref ns-s-implicit-yaml-key block-key)))

  (Rule 194 c-l-block-map-implicit-value (n)
    (Seq (Char ':')
         (Alt (Ref s-l+block-node n block-out)
              (Scalar (Seq (Ref e-node) (Ref s-l-comments))))))

  (Rule 195 ns-l-compact-mapping (n)
    (Seq (Ref ns-l-block-map-entry n)
         (Star (Seq (Ref s-indent n)
                    (Ref ns-l-block-map-entry n)))))

  ;  8.2.3 Block Nodes 

  (Rule 196 s-l+block-node (n c)
    (Alt (Ref s-l+block-in-block n c)
         (Ref s-l+flow-in-block n)))

  (Rule 197 s-l+flow-in-block (n)
    (Seq (Ref s-separate (+ n 1) flow-out)
         (Ref ns-flow-node (+ n 1) flow-out)
         (Ref s-l-comments)))

  (Rule 198 s-l+block-in-block (n c)
    (Alt (Ref s-l+block-scalar n c)
         (Ref s-l+block-collection n c)))

  (Rule 199 s-l+block-scalar (n c)
    (Seq (Ref s-separate (+ n 1) c)
         (Opt (Seq (Ref c-ns-properties (+ n 1) c)
                   (Ref s-separate (+ n 1) c)))
         (Alt (Ref c-l+literal n)
              (Ref c-l+folded n))))

  (Rule 200 s-l+block-collection (n c)
    (Seq (Opt (Seq (Ref s-separate (+ n 1) c)
                   (Ref c-ns-properties (+ n 1) c)))
         (Ref s-l-comments)
         (Alt (Ref l+block-sequence (seq-spaces n c))
              (Ref l+block-mapping n))))

  (Rule 201 seq-spaces (n c)
    (Switch c
      (block-out (- n 1))
      (block-in  n)))

  ; 
  ; Chapter 9: Document Stream Productions
  ; 

  ;  9.1 Documents 

  (Rule 202 l-document-prefix
    (Seq (Opt (Ref c-byte-order-mark))
         (Star (Ref l-comment))))

  (Rule 203 c-directives-end
    (Str "---"))

  (Rule 204 c-document-end
    (Str "..."))

  (Rule 205 l-document-suffix
    (Seq (Ref c-document-end)
         (Ref s-l-comments)))

  (Rule 206 c-forbidden
    ; start-of-line followed by --- or ... then whitespace/EOF
    (Seq (StartOfLine)
         (Alt (Ref c-directives-end)
              (Ref c-document-end))
         (Alt (Ref b-char) (Ref s-white) (EndOfInput))))

  (Rule 207 l-bare-document
    (Build doc (Ref s-l+block-node -1 block-in)))
    ; excluding c-forbidden

  (Rule 208 l-explicit-document
    (Build doc
      (Seq (Ref c-directives-end)
           (Alt (Ref l-bare-document)
                (Seq (Ref e-node) (Ref s-l-comments))))))

  (Rule 209 l-directive-document
    (Seq (Plus (Ref l-directive))
         (Ref l-explicit-document)))

  ;  9.2 Streams 

  (Rule 210 l-any-document
    (Alt (Ref l-directive-document)
         (Ref l-explicit-document)
         (Ref l-bare-document)))

  (Rule 211 l-yaml-stream
    (Build stream
      (Seq (Star (Ref l-document-prefix))
           (Opt (Ref l-any-document))
           (Star (Alt (Seq (Plus (Ref l-document-suffix))
                           (Star (Ref l-document-prefix))
                           (Opt (Ref l-any-document)))
                      (Seq (Star (Ref l-document-prefix))
                           (Opt (Ref l-explicit-document))))))))

) ; end Grammar
