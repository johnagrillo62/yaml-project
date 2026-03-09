; ==========================================================================
; json-grammar.scm  JSON grammar as s-expressions
; ==========================================================================
;
; 15 rules derived from RFC 8259 (the JSON spec).
; Same notation as yaml-grammar.scm.
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
;   (Str s)                       literal string
;
; No parameters. No context switching. No indentation.
; JSON has no ambiguity — PEG is a perfect fit.
;
; ==========================================================================

(Grammar JSON-RFC8259

  ; Rule 1 — top level value
  (Rule 1 json-text
    (Seq (Ref ws)
         (Ref value)
         (Ref ws)))

  ; Rule 2 — any JSON value
  (Rule 2 value
    (Alt (Ref object)
         (Ref array)
         (Ref string)
         (Ref number)
         (Str "true")
         (Str "false")
         (Str "null")))

  ; Rule 3 — object: { "key": value, ... }
  (Rule 3 object
    (Alt (Seq (Char #\{)
              (Ref ws)
              (Ref members)
              (Ref ws)
              (Char #\}))
         (Seq (Char #\{)
              (Ref ws)
              (Char #\}))))

  ; Rule 4 — one or more key/value pairs
  (Rule 4 members
    (Seq (Ref member)
         (Star (Seq (Ref ws)
                    (Char #\,)
                    (Ref ws)
                    (Ref member)))))

  ; Rule 5 — single key/value pair
  (Rule 5 member
    (Seq (Ref ws)
         (Ref string)
         (Ref ws)
         (Char #\:)
         (Ref ws)
         (Ref value)
         (Ref ws)))

  ; Rule 6 — array: [ value, ... ]
  (Rule 6 array
    (Alt (Seq (Char #\[)
              (Ref ws)
              (Ref elements)
              (Ref ws)
              (Char #\]))
         (Seq (Char #\[)
              (Ref ws)
              (Char #\]))))

  ; Rule 7 — one or more array elements
  (Rule 7 elements
    (Seq (Ref value)
         (Star (Seq (Ref ws)
                    (Char #\,)
                    (Ref ws)
                    (Ref value)))))

  ; Rule 8 — quoted string
  (Rule 8 string
    (Seq (Char #\")
         (Star (Ref char))
         (Char #\")))

  ; Rule 9 — string character (unescaped or escaped)
  (Rule 9 char
    (Alt (Ref escaped)
         (Seq (Not (Char #\"))
              (Not (Char #\\))
              (Not (Hex 00))
              (Not (Range (Hex 00) (Hex 1F)))
              (Range (Hex 20) (Hex 10FFFF)))))

  ; Rule 10 — escape sequences
  (Rule 10 escaped
    (Seq (Char #\\)
         (Alt (Char #\")
              (Char #\\)
              (Char #\/)
              (Char #\b)
              (Char #\f)
              (Char #\n)
              (Char #\r)
              (Char #\t)
              (Seq (Char #\u)
                   (Ref hex4)))))

  ; Rule 11 — four hex digits for \uXXXX
  (Rule 11 hex4
    (Seq (Ref hexdig)
         (Ref hexdig)
         (Ref hexdig)
         (Ref hexdig)))

  ; Rule 12 — single hex digit
  (Rule 12 hexdig
    (Alt (Range #\0 #\9)
         (Range #\a #\f)
         (Range #\A #\F)))

  ; Rule 13 — number: -?integer fraction? exponent?
  (Rule 13 number
    (Seq (Opt (Char #\-))
         (Ref integer)
         (Opt (Ref fraction))
         (Opt (Ref exponent))))

  ; Rule 14 — integer part: 0 or [1-9][0-9]*
  (Rule 14 integer
    (Alt (Char #\0)
         (Seq (Range #\1 #\9)
              (Star (Range #\0 #\9)))))

  ; Rule 15 — decimal fraction
  (Rule 15 fraction
    (Seq (Char #\.)
         (Plus (Range #\0 #\9))))

  ; Rule 16 — exponent
  (Rule 16 exponent
    (Seq (Alt (Char #\e) (Char #\E))
         (Opt (Alt (Char #\+) (Char #\-)))
         (Plus (Range #\0 #\9))))

  ; Rule 17 — whitespace: space, tab, newline, carriage return
  (Rule 17 ws
    (Star (Alt (Hex 20)
               (Hex 09)
               (Hex 0A)
               (Hex 0D))))

) ; Grammar JSON-RFC8259
