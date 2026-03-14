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
    (Alt (Seq (Hex 7B)
              (Ref ws)
              (Ref members)
              (Ref ws)
              (Hex 7D))
         (Seq (Hex 7B)
              (Ref ws)
              (Hex 7D))))

  ; Rule 4 — one or more key/value pairs
  (Rule 4 members
    (Seq (Ref member)
         (Star (Seq (Ref ws)
                    (Hex 2C)
                    (Ref ws)
                    (Ref member)))))

  ; Rule 5 — single key/value pair
  (Rule 5 member
    (Seq (Ref ws)
         (Ref string)
         (Ref ws)
         (Hex 3A)
         (Ref ws)
         (Ref value)
         (Ref ws)))

  ; Rule 6 — array: [ value, ... ]
  (Rule 6 array
    (Alt (Seq (Hex 5B)
              (Ref ws)
              (Ref elements)
              (Ref ws)
              (Hex 5D))
         (Seq (Hex 5B)
              (Ref ws)
              (Hex 5D))))

  ; Rule 7 — one or more array elements
  (Rule 7 elements
    (Seq (Ref value)
         (Star (Seq (Ref ws)
                    (Hex 2C)
                    (Ref ws)
                    (Ref value)))))

  ; Rule 8 — quoted string
  (Rule 8 string
    (Seq (Hex 22)
         (Star (Ref char))
         (Hex 22)))

  ; Rule 9 — string character (unescaped or escaped)
  (Rule 9 char
    (Alt (Ref escaped)
         (Seq (Not (Hex 22))
              (Not (Hex 5C))
              (Not (Range (Hex 01) (Hex 1F)))
              (Range (Hex 20) (Hex 10FFFF)))))

  ; Rule 10 — escape sequences
  (Rule 10 escaped
    (Seq (Hex 5C)
         (Alt (Hex 22)
              (Hex 5C)
              (Hex 2F)
              (Hex 08)
              (Hex 0C)
              (Hex 0A)
              (Hex 0D)
              (Hex 09)
              (Seq (Hex 75)
                   (Ref hex4)))))

  ; Rule 11 — four hex digits for \uXXXX
  (Rule 11 hex4
    (Seq (Ref hexdig)
         (Ref hexdig)
         (Ref hexdig)
         (Ref hexdig)))

  ; Rule 12 — single hex digit
  (Rule 12 hexdig
    (Alt (Range (Hex 30) (Hex 39))
         (Range (Hex 61) (Hex 66))
         (Range (Hex 41) (Hex 46))))

  ; Rule 13 — number: -?integer fraction? exponent?
  (Rule 13 number
    (Seq (Opt (Hex 2D))
         (Ref integer)
         (Opt (Ref fraction))
         (Opt (Ref exponent))))

  ; Rule 14 — integer part: 0 or [1-9][0-9]*
  (Rule 14 integer
    (Alt (Hex 30)
         (Seq (Range (Hex 31) (Hex 39))
              (Star (Range (Hex 30) (Hex 39))))))

  ; Rule 15 — decimal fraction
  (Rule 15 fraction
    (Seq (Hex 2E)
         (Plus (Range (Hex 30) (Hex 39)))))

  ; Rule 16 — exponent
  (Rule 16 exponent
    (Seq (Alt (Hex 65) (Hex 45))
         (Opt (Alt (Hex 2B) (Hex 2D)))
         (Plus (Range (Hex 30) (Hex 39)))))

  ; Rule 17 — whitespace: space, tab, newline, carriage return
  (Rule 17 ws
    (Star (Alt (Hex 20)
               (Hex 09)
               (Hex 0A)
               (Hex 0D))))

) ; Grammar JSON-RFC8259
