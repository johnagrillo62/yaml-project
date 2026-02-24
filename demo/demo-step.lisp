(in-package :yaml-eval)

;;; ============================================================
;;; Step-by-step: How rule 78 eats indentation
;;; ============================================================
;;; Input: "a:\n  b: 1\n  c: 2\n"
;;; We trace exactly what happens after parsing "a:"
;;; when the parser tries to find the nested value.

(defvar *bg* 
  (let ((g (load-yaml-grammar "yaml-grammar.scm")))
    (let ((r78 (gethash 'L-COMMENT (gram-rules g))))
      (setf (rdef-body r78)
            '(SEQ (REF S-SEPARATE-IN-LINE)
                  (OPT (REF C-NB-COMMENT-TEXT))
                  (REF B-COMMENT))))
    g))

(format t "~%============================================================~%")
(format t " Step-by-step: How the spec bug destroys nesting~%")
(format t "============================================================~%")
(format t "~%Input: a:\\n  b: 1\\n  c: 2\\n~%")
(format t "~%The parser has just consumed 'a:' and is at position 2.~%")
(format t "The remaining input is: \\n  b: 1\\n  c: 2\\n~%")
(format t "~%Now rule 194 (c-l-block-map-implicit-value) tries to~%")
(format t "find the VALUE for key 'a'. It tries s-l+block-node,~%")
(format t "which calls s-l+block-collection (rule 200), which does:~%")
(format t "~%  (Seq (Opt properties) s-l-comments (Alt l+block-sequence l+block-mapping))~%")
(format t "~%So s-l-comments (rule 79) runs first. Let's trace it.~%")

(let* ((*memo* (make-hash-table :test 'equal))
       (*depth* 0)
       ;; Position after "a:" — at "\n  b: 1\n  c: 2\n"
       (inp (mkinp (format nil "~%  b: 1~%  c: 2~%"))))

  (format t "~%============================================================~%")
  (format t " STEP 1: s-l-comments (rule 79)~%")
  (format t "============================================================~%")
  (format t "~%  Rule: (Alt s-b-comment EMPTY) (Star l-comment)~%")
  
  ;; Step 1a: s-b-comment
  (format t "~%  Step 1a: Try s-b-comment (rule 77)~%")
  (format t "    Rule: (Seq (Opt (Seq s-separate-in-line comment-text)) b-comment)~%")
  (format t "    Position: 0, char: \\n (newline), column: 0~%")
  (format t "    - (Opt ...) → s-separate-in-line needs spaces or StartOfLine~%")
  (format t "    - Column IS 0 → StartOfLine matches (consuming nothing)~%")
  (format t "    - Opt matches empty (no comment text follows)~%")
  (format t "    - b-comment → b-non-content matches the \\n~%")
  (let ((*memo* (make-hash-table :test 'equal)))
    (let ((r (ev-ref 'S-B-COMMENT nil inp nil *bg*)))
      (format t "    RESULT: ok=~A, consumed to pos ~D~%" 
              (ok-p r) (inp-pos (ok-rest r)))
      (format t "    Now at: ~S~%" 
              (subseq (inp-str (ok-rest r)) (inp-pos (ok-rest r))
                      (min (+ (inp-pos (ok-rest r)) 15) (length (inp-str (ok-rest r))))))))
  
  (format t "~%  ✓ Good so far. The newline was consumed. We're at '  b: 1...'~%")
  
  ;; Step 1b: Star l-comment — THIS IS WHERE THE BUG BITES
  (format t "~%============================================================~%")
  (format t " STEP 2: Star(l-comment) — THE BUG~%")
  (format t "============================================================~%")
  (format t "~%  Now Star tries l-comment repeatedly.~%")
  (format t "  We're at position 1: '  b: 1\\n  c: 2\\n'  (column 0)~%")
  
  (format t "~%  Step 2a: l-comment attempt 1~%")
  (format t "    Rule: (Seq s-separate-in-line (Opt comment-text) b-comment)~%")
  (format t "~%    s-separate-in-line = (Alt (Plus s-white) StartOfLine)~%")
  (format t "    - Try (Plus s-white): char is SPACE (0x20)... MATCHES!~%")
  (format t "    - Plus keeps going: next char is SPACE... MATCHES!~%")
  (format t "    - Plus keeps going: next char is 'b'... stops.~%")
  (format t "    - s-separate-in-line consumed 2 SPACES ← the indentation!~%")
  (format t "~%    (Opt c-nb-comment-text): 'b' is not '#', skip~%")
  
  (format t "~%    *** HERE IS THE BUG ***~%")
  (format t "~%    b-comment = (Alt b-non-content EMPTY)~%")
  (format t "    - b-non-content: needs newline, got 'b'... FAILS~%")
  (format t "    - EMPTY: always succeeds, consumes nothing~%")
  (format t "    - b-comment SUCCEEDS via EMPTY fallback!~%")
  (format t "~%    l-comment consumed '  ' (2 spaces) and matched.~%")
  (format t "    Those 2 spaces were the INDENTATION for 'b: 1'.~%")
  (format t "    They're gone now. Eaten by a comment rule on a non-comment line.~%")

  ;; Actually run it to prove it
  (let* ((*memo* (make-hash-table :test 'equal))
         (after-newline (%inp (inp-str inp) 1 2 0)))
    (let ((r (ev-ref 'L-COMMENT nil after-newline nil *bg*)))
      (format t "~%    PROOF: l-comment at pos 1 → ok=~A, consumed to pos ~D~%"
              (ok-p r) (and (ok-p r) (inp-pos (ok-rest r))))))

  (format t "~%  Step 2b: l-comment attempt 2~%")
  (format t "    Now at 'b: 1\\n  c: 2\\n', column 2~%")
  (format t "    s-separate-in-line: not SOL (col=2), 'b' not whitespace → FAILS~%")
  (format t "    l-comment fails. Star stops.~%")

  (format t "~%============================================================~%")
  (format t " STEP 3: The damage~%")
  (format t "============================================================~%")
  (format t "~%  s-l-comments consumed: \\n + '  ' = 3 chars~%")
  (format t "  Position is now at 'b: 1\\n  c: 2\\n'~%")
  (format t "~%  Next: (Alt l+block-sequence l+block-mapping) tries to parse.~%")
  (format t "  l+block-mapping does Detect-Indent, gets m=0 (no leading spaces!)~%")
  (format t "  Because the spaces are GONE. l-comment ate them.~%")
  (format t "~%  l+block-mapping FAILS at this position.~%")
  (format t "  s-l+block-node FAILS.~%")
  (format t "  Rule 194 falls through to: (Scalar (Seq e-node s-l-comments))~%")
  (format t "  Key 'a' gets value: NULL (empty node)~%")
  (format t "~%  The outer mapping loop continues and parses 'b: 1' and 'c: 2'~%")
  (format t "  as SIBLINGS of 'a', not children.~%")

  ;; Show the actual buggy parse
  (format t "~%============================================================~%")
  (format t " RESULT: Buggy parse~%")
  (format t "============================================================~%")
  (let ((*memo* (make-hash-table :test 'equal)))
    (multiple-value-bind (ok val pos ast)
        (yaml-parse *bg* (format nil "a:~%  b: 1~%  c: 2~%"))
      (declare (ignore ok val pos))
      (format t "~%  AST: ~S~%" ast)
      (format t "~%  a → null     (should be a nested mapping)~%")
      (format t "  b → 1        (should be inside a)~%")
      (format t "  c → 2        (should be inside a)~%")))

  (format t "~%============================================================~%")
  (format t " THE FIX: One symbol~%")
  (format t "============================================================~%")
  (format t "~%  Rule 78 original:  ... b-comment~%")
  (format t "  Rule 78 fixed:     ... b-non-content~%")
  (format t "~%  b-comment      = (Alt b-non-content EMPTY)  ← EMPTY is the bug~%")
  (format t "  b-non-content  = newline character             ← requires real newline~%")
  (format t "~%  Remove the EMPTY fallback. A comment line must end with~%")
  (format t "  an actual newline. Content lines are no longer consumed.~%")
  (format t "  Indentation survives. Nesting works.~%")

  ;; Show fixed parse
  (format t "~%============================================================~%")
  (format t " RESULT: Fixed parse~%")
  (format t "============================================================~%")
  (let* ((*memo* (make-hash-table :test 'equal))
         (fg (load-yaml-grammar "yaml-grammar.scm")))
    (multiple-value-bind (ok val pos ast)
        (yaml-parse fg (format nil "a:~%  b: 1~%  c: 2~%"))
      (declare (ignore ok val pos))
      (format t "~%  AST: ~S~%" ast)
      (format t "~%  a → {b: 1, c: 2}   ✓ nested mapping~%")))

  (format t "~%============================================================~%"))
