;;;; emit-yaml-java.lisp — Project yaml-grammar.scm → YamlReader.java
;;;;
;;;; Same 211 rules. Java target. Uses lambdas + PFn interface.
;;;;
;;;; Usage:
;;;;   sbcl --load yaml-eval.lisp --load emit-yaml-java.lisp --quit
;;;;   javac YamlReader.java && java YamlReader test.yaml

(in-package #:yaml-eval)

;;; ═══════════════════════════════════════════════════════════════════
;;; NAME MANGLING
;;; ═══════════════════════════════════════════════════════════════════

(defun java-ident (sym)
  (let* ((s (string-downcase (symbol-name sym)))
         (s (substitute #\_ #\- s))
         (s (remove #\+ s)))
    (if (or (digit-char-p (char s 0))
            (member s '("abstract" "assert" "boolean" "break" "byte" "case"
                        "catch" "char" "class" "const" "continue" "default"
                        "do" "double" "else" "enum" "extends" "final"
                        "finally" "float" "for" "goto" "if" "implements"
                        "import" "instanceof" "int" "interface" "long"
                        "native" "new" "package" "private" "protected"
                        "public" "return" "short" "static" "strictfp"
                        "super" "switch" "synchronized" "this" "throw"
                        "throws" "transient" "try" "void" "volatile" "while"
                        "var" "yield" "record" "sealed" "permits")
                    :test #'string=))
        (format nil "r_~A" s)
        s)))

;;; ═══════════════════════════════════════════════════════════════════
;;; OUTPUT
;;; ═══════════════════════════════════════════════════════════════════

(defvar *out* nil)
(defun emit (&rest args) (dolist (a args) (princ a *out*)) (terpri *out*))
(defun emitf (fmt &rest args) (apply #'format *out* fmt args))
(defun blank () (terpri *out*))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPRESSION COMPILER
;;; ═══════════════════════════════════════════════════════════════════

(defvar *gram* nil)
(defvar *let-counter* 0)

;; Java lambdas: use p as param since it cannot shadow method's inp
;; The body uses 'inp' so we wrap in a tiny static call
(defun lw (body env)
  (declare (ignore env))
  (format nil "new PFn() { public Result apply(Input inp) { return ~A; } }" body))

(defun bw (body env)
  (declare (ignore env))
  (format nil "new PFn() { public Result apply(Input inp) { return ~A; } }" body))

(defun ce (expr env)
  (cond
    ((null expr) "ok(inp)")
    ((eq expr 'EMPTY) "ok(inp)")
    ((integerp expr) (format nil "matchCp(inp, ~D)" expr))
    ((symbolp expr)
     (if (member expr env) (format nil "~A" (java-ident expr))
         (format nil "~A(inp)" (java-ident expr))))
    ((listp expr)
     (let ((op (car expr)) (args (cdr expr)))
       (case op
         (EMPTY  "ok(inp)")
         (CHAR   (ce-char (car args) env))
         (HEX    (ce-hex (car args)))
         (RANGE  (ce-range (car args) (cadr args)))
         (STR    (format nil "matchStr(inp, ~S)" (string (car args))))
         (SEQ    (ce-seq args env))
         (ALT    (ce-alt args env))
         (STAR   (format nil "star(inp, ~A)" (lw (ce (car args) env) env)))
         (PLUS   (format nil "plus_(inp, ~A)" (lw (ce (car args) env) env)))
         (OPT    (format nil "opt(inp, ~A)" (lw (ce (car args) env) env)))
         (REF    (ce-ref (car args) (cdr args) env))
         (NOT    (format nil "neg(inp, ~A)" (lw (ce (car args) env) env)))
         (MINUS  (format nil "minus(inp, ~A, ~A)"
                         (lw (ce (car args) env) env) (lw (ce (cadr args) env) env)))
         (REPEAT (format nil "rep(inp, ~A, ~A)"
                         (ca (car args) env) (lw (ce (cadr args) env) env)))
         (SWITCH (ce-switch (car args) (cdr args) env))
         (LOOKAHEAD  (format nil "ahead(inp, ~A)" (lw (ce (car args) env) env)))
         (LOOKBEHIND (format nil "behind(inp, ~A)" (lw (ce (car args) env) env)))
         (STARTOFLINE "sol(inp)")
         (ENDOFINPUT  "eofOk(inp)")
         (BUILD   (format nil "build(inp, ~S, ~A)"
                          (string (car args)) (lw (ce (cadr args) env) env)))
         (SCALAR  (format nil "scalar(inp, ~A)" (lw (ce (car args) env) env)))
         (COLLECT (format nil "collect(inp, ~A)" (lw (ce (car args) env) env)))
         (LET           (ce-let (car args) (cadr args) env))
         (DETECT-INDENT (format nil "detectIndent(inp, ~A)" (ca (car args) env)))
         (PARSE-INT     (format nil "parseInt_(inp, ~A)" (lw (ce (car args) env) env)))
         (PARSE-SYM     (format nil "parseSym(inp, ~A, ~S)"
                                (lw (ce (car args) env) env) (string (cadr args))))
         (VAL           (format nil "val(inp, ~S)" (string (car args))))
         ((+ -)  (ca expr env))
         (IN-FLOW    (format nil "inFlow(~A)" (ca (cadr expr) env)))
         (SEQ-SPACES (format nil "seqSpaces(~A, ~A)" (ca (cadr expr) env) (ca (caddr expr) env)))
         (t (let ((rd (gethash op (gram-rules *gram*))))
              (if rd
                  (format nil "~A(inp~{, ~A~})" (java-ident op)
                          (mapcar (lambda (a) (ca a env)) args))
                  (format nil "/* UNKNOWN ~S */" expr)))))))
    (t (format nil "/* ?? ~S */" expr))))

(defun ce-char (x env)
  (cond
    ((integerp x) (format nil "matchCp(inp, ~D)" x))
    ((and (symbolp x) (string= (symbol-name x) "SQUOTE")) "matchCp(inp, 39)")
    ((and (symbolp x) (string= (symbol-name x) "DQUOTE")) "matchCp(inp, 34)")
    ((and (symbolp x) (member x env)) (format nil "matchCp(inp, (int)~A)" (java-ident x)))
    ((symbolp x) (format nil "matchCp(inp, ~D)" (char-code (char (symbol-name x) 0))))
    (t (format nil "matchCp(inp, ~D)" x))))

(defun ce-hex (v)
  (let ((s (string-upcase (if (integerp v) (format nil "~D" v) (symbol-name v)))))
    (format nil "matchCp(inp, 0x~A)" s)))

(defun ce-range (lo hi)
  (flet ((hv (x)
           (if (and (listp x) (eq (car x) 'HEX))
               (format nil "0x~A" (string-upcase
                                   (if (integerp (cadr x)) (format nil "~D" (cadr x))
                                       (symbol-name (cadr x)))))
               (if (integerp x) (format nil "~D" x)
                   (format nil "~D" (char-code (char (symbol-name x) 0)))))))
    (format nil "matchRange(inp, ~A, ~A)" (hv lo) (hv hi))))

(defun ce-seq (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      (maybe-ml
       (lambda (w) (format nil "seq(inp, new PFn[]{~{~A~^, ~}})" w))
       (lambda () (mapcar (lambda (e) (bw (ce e env) env)) exprs)))))

(defun ce-alt (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      (maybe-ml
       (lambda (w) (format nil "alt(inp, new PFn[]{~{~A~^, ~}})" w))
       (lambda () (mapcar (lambda (e) (bw (ce e env) env)) exprs)))))

(defun ce-ref (name call-args env)
  (if call-args
      (format nil "~A(inp~{, ~A~})" (java-ident name)
              (mapcar (lambda (a) (ca a env)) call-args))
      (format nil "~A(inp)" (java-ident name))))

(defun ce-switch (param cases env)
  (with-output-to-string (s)
    (format s "((Sup)(() -> {")
    (dolist (c cases)
      (format s " if (~A.equals(~S)) return ~A;"
              (java-ident param)
              (string-upcase (symbol-name (car c)))
              (ce (cadr c) env)))
    (format s " return fail(inp, \"no case\"); })).get()")))

(defun ce-let (bindings body env)
  (if (null bindings) (ce body env)
      (let* ((b (car bindings))
             (var (car b))
             (val-expr (cadr b))
             (vn (java-ident var))
             (idx (incf *let-counter*))
             (rvar (format nil "r~D_" idx))
             (ivar (format nil "inp~D_" idx))
             (numeric-p (member (symbol-name var) '("N" "M") :test #'string-equal))
             (inner (ce-let (cdr bindings) body (cons var env))))
        (with-output-to-string (s)
          (format s "((Sup)(() -> { Result ~A = ~A; if (~A.fail) return ~A; "
                  rvar (ce val-expr env) rvar rvar)
          (if numeric-p
              (format s "int ~A = ~A.tagInt; " vn rvar)
              (format s "String ~A = ~A.tag; " vn rvar))
          ;; Instead of rebinding inp, we inline-replace inp with ivar in the inner body
          ;; by wrapping in a method call
          (format s "final Input ~A = ~A.rest; " ivar rvar)
          (format s "return new PFn() { public Result apply(Input inp) { return ~A; } }.apply(~A); })).get()"
                  inner ivar)))))

(defun ca (expr env)
  (cond
    ((integerp expr) (format nil "~D" expr))
    ((and (symbolp expr) (eq expr 'EMPTY)) "0")
    ((and (symbolp expr) (member expr env))
     (java-ident expr))
    ((and (symbolp expr)
          (member (symbol-name expr)
                  '("BLOCK-IN" "BLOCK-OUT" "FLOW-IN" "FLOW-OUT"
                    "BLOCK-KEY" "FLOW-KEY" "STRIP" "CLIP" "KEEP" "EMPTY")
                  :test #'string-equal))
     (format nil "~S" (string-upcase (symbol-name expr))))
    ((symbolp expr) (java-ident expr))
    ((and (listp expr) (eq (car expr) '+))
     (format nil "(~{~A~^ + ~})" (mapcar (lambda (e) (ca e env)) (cdr expr))))
    ((and (listp expr) (eq (car expr) '-))
     (let ((ps (mapcar (lambda (e) (ca e env)) (cdr expr))))
       (if (= 1 (length ps)) (format nil "(-~A)" (car ps))
           (format nil "(~A~{ - ~A~})" (car ps) (cdr ps)))))
    ((and (listp expr) (eq (car expr) 'IN-FLOW))
     (format nil "inFlow(~A)" (ca (cadr expr) env)))
    ((and (listp expr) (eq (car expr) 'SEQ-SPACES))
     (format nil "seqSpaces(~A, ~A)" (ca (cadr expr) env) (ca (caddr expr) env)))
    (t (format nil "/* arg?~S */" expr))))

;;; ═══════════════════════════════════════════════════════════════════
;;; EMITTER
;;; ═══════════════════════════════════════════════════════════════════

(defun grammar-rules-ordered (gram)
  (let ((rules nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v rules)) (gram-rules gram))
    (sort rules #'< :key #'rdef-num)))

(defun emit-yaml-reader-java (gram path)
  (let ((*gram* gram) (*let-counter* 0))
    (with-open-file (*out* path :direction :output :if-exists :supersede :external-format :utf-8)
      (emit-java-all))))

(defun emit-java-all ()
  (blank)
  (emit "import java.io.*;")
  (emit "import java.nio.file.*;")
  (emit "import java.util.*;")
  (blank)
  (emit "@SuppressWarnings(\"all\")")
  (emit "public class YamlReader {")
  (blank)
  (emit "    @FunctionalInterface interface PFn { Result apply(Input inp); }")
  (emit "    @FunctionalInterface interface Sup { Result get(); }")
  (blank)

  ;; ── Input ──
  (emit "    static final class Input {")
  (emit "        final String src; final int pos, line, col;")
  (emit "        Input(String src, int pos, int line, int col) { this.src=src; this.pos=pos; this.line=line; this.col=col; }")
  (emit "        static Input of(String s) { return new Input(s, 0, 1, 0); }")
  (emit "        boolean atEof() { return pos >= src.length(); }")
  (emit "        int peek() { if (atEof()) return -1; return src.codePointAt(pos); }")
  (emit "        Input adv() {")
  (emit "            if (atEof()) return this;")
  (emit "            int cp = src.codePointAt(pos); int npos = pos + Character.charCount(cp);")
  (emit "            boolean nl = cp == '\\n';")
  (emit "            return new Input(src, npos, nl ? line+1 : line, nl ? 0 : col+1);")
  (emit "        }")
  (emit "    }")
  (blank)

  ;; ── AST ──
  (emit "    static class Ast {")
  (emit "        String tag, text; List<Ast> children; boolean isLeaf;")
  (emit "        static Ast branch(String tag) { Ast a=new Ast(); a.tag=tag; a.children=new ArrayList<>(); return a; }")
  (emit "        static Ast leaf(String text) { Ast a=new Ast(); a.text=text; a.isLeaf=true; return a; }")
  (emit "    }")
  (blank)

  ;; ── Result ──
  (emit "    static final class Result {")
  (emit "        boolean fail; String val=\"\"; Input rest; String tag=\"\"; int tagInt;")
  (emit "        Ast ast; List<Ast> astList; String err=\"\";")
  (emit "        Result() { astList = new ArrayList<>(); }")
  (emit "    }")
  (blank)
  (emit "    static Result ok(Input inp) { Result r=new Result(); r.rest=inp; return r; }")
  (emit "    static Result okV(Input inp, String v) { Result r=ok(inp); r.val=v; return r; }")
  (emit "    static Result fail(Input inp, String m) { Result r=new Result(); r.fail=true; r.rest=inp; r.err=m; return r; }")
  (blank)

  ;; ── Context ──
  (emit "    static String inFlow(String c) { return c.equals(\"FLOW-OUT\")||c.equals(\"FLOW-IN\") ? \"FLOW-IN\" : \"FLOW-KEY\"; }")
  (emit "    static int seqSpaces(int n, String c) { return c.equals(\"BLOCK-OUT\") ? n-1 : n; }")
  (blank)

  ;; ── Combinators ──
  (emit "    static Result matchCp(Input inp, int cp) {")
  (emit "        int c=inp.peek(); if (c==cp) { String s=new String(Character.toChars(c));")
  (emit "        Input nx=inp; for(int i=0;i<s.length();i++) nx=nx.adv(); return okV(nx, s); } return fail(inp,\"cp\"); }")
  (blank)
  (emit "    static Result matchRange(Input inp, int lo, int hi) {")
  (emit "        int c=inp.peek(); if (c>=lo&&c<=hi) { String s=new String(Character.toChars(c));")
  (emit "        Input nx=inp; for(int i=0;i<s.length();i++) nx=nx.adv(); return okV(nx, s); } return fail(inp,\"rng\"); }")
  (blank)
  (emit "    static Result matchStr(Input inp, String t) {")
  (emit "        int n=t.length(); if (inp.pos+n>inp.src.length()) return fail(inp,\"str\");")
  (emit "        if (!inp.src.substring(inp.pos,inp.pos+n).equals(t)) return fail(inp,\"str\");")
  (emit "        Input cur=inp; for (int i=0;i<n;i++) cur=cur.adv(); return okV(cur, t); }")
  (blank)
  (emit "    static void mergeAsts(List<Ast> dst, Result r) {")
  (emit "        if (r.ast!=null) dst.add(r.ast);")
  (emit "        if (r.astList!=null&&!r.astList.isEmpty()) dst.addAll(r.astList); }")
  (blank)
  (emit "    static Result seq(Input inp, PFn[] fns) {")
  (emit "        Input cur=inp; StringBuilder acc=new StringBuilder(); List<Ast> asts=new ArrayList<>();")
  (emit "        for (PFn f:fns) { Result r=f.apply(cur); if (r.fail) return r; acc.append(r.val); mergeAsts(asts,r); cur=r.rest; }")
  (emit "        Result res=okV(cur,acc.toString());")
  (emit "        if (asts.size()==1) res.ast=asts.get(0); else if (asts.size()>1) res.astList=asts;")
  (emit "        return res; }")
  (blank)
  (emit "    static Result alt(Input inp, PFn[] fns) {")
  (emit "        for (PFn f:fns) { Result r=f.apply(inp); if (!r.fail) return r; }")
  (emit "        return fail(inp,\"alt\"); }")
  (blank)
  (emit "    static Result star(Input inp, PFn f) {")
  (emit "        Input cur=inp; StringBuilder acc=new StringBuilder(); List<Ast> asts=new ArrayList<>();")
  (emit "        while (true) { Result r=f.apply(cur); if (r.fail||r.rest.pos<=cur.pos) break; acc.append(r.val); mergeAsts(asts,r); cur=r.rest; }")
  (emit "        Result res=okV(cur,acc.toString()); if (!asts.isEmpty()) res.astList=asts; return res; }")
  (blank)
  (emit "    static Result plus_(Input inp, PFn f) {")
  (emit "        Result first=f.apply(inp); if (first.fail) return first;")
  (emit "        Result rest=star(first.rest,f);")
  (emit "        Result res=okV(rest.rest, first.val+rest.val);")
  (emit "        List<Ast> asts=new ArrayList<>(); mergeAsts(asts,first); mergeAsts(asts,rest);")
  (emit "        if (!asts.isEmpty()) res.astList=asts; return res; }")
  (blank)
  (emit "    static Result opt(Input inp, PFn f) { Result r=f.apply(inp); return r.fail ? ok(inp) : r; }")
  (emit "    static Result neg(Input inp, PFn f) { Result r=f.apply(inp); return r.fail ? ok(inp) : fail(inp,\"neg\"); }")
  (emit "    static Result minus(Input inp, PFn fa, PFn fb) {")
  (emit "        Result ra=fa.apply(inp); if (ra.fail) return ra;")
  (emit "        Result rb=fb.apply(inp); return (!rb.fail&&rb.rest.pos==ra.rest.pos) ? fail(inp,\"excl\") : ra; }")
  (emit "    static Result rep(Input inp, int n, PFn f) {")
  (emit "        Input cur=inp; StringBuilder acc=new StringBuilder();")
  (emit "        for (int i=0;i<n;i++) { Result r=f.apply(cur); if (r.fail) return r; acc.append(r.val); cur=r.rest; }")
  (emit "        return okV(cur,acc.toString()); }")
  (emit "    static Result ahead(Input inp, PFn f) { Result r=f.apply(inp); return r.fail ? r : ok(inp); }")
  (emit "    static Result behind(Input inp, PFn f) {")
  (emit "        if (inp.pos==0) return fail(inp,\"bh\");")
  (emit "        Input t=new Input(inp.src,inp.pos-1,inp.line,Math.max(0,inp.col-1));")
  (emit "        Result r=f.apply(t); return r.fail ? fail(inp,\"bh\") : ok(inp); }")
  (emit "    static Result sol(Input inp) { return inp.col==0 ? ok(inp) : fail(inp,\"sol\"); }")
  (emit "    static Result eofOk(Input inp) { return inp.atEof() ? ok(inp) : fail(inp,\"eof\"); }")
  (blank)

  ;; ── YAML extensions ──
  (emit "    static Result build(Input inp, String typ, PFn f) {")
  (emit "        Result r=f.apply(inp); if (r.fail) return r;")
  (emit "        Ast node=Ast.branch(typ);")
  (emit "        if (r.ast!=null) node.children.add(r.ast);")
  (emit "        if (r.astList!=null&&!r.astList.isEmpty()) node.children.addAll(r.astList);")
  (emit "        r.ast=node; r.astList=new ArrayList<>(); return r; }")
  (blank)
  (emit "    static Result scalar(Input inp, PFn f) {")
  (emit "        Result r=f.apply(inp); if (r.fail) return r; r.ast=Ast.leaf(r.val); return r; }")
  (blank)
  (emit "    static Result collect(Input inp, PFn f) { return f.apply(inp); }")
  (blank)
  (emit "    static Result detectIndent(Input inp, int n) {")
  (emit "        String s=inp.src; int len=s.length(); int i=inp.pos;")
  (emit "        int sp=0; while (i+sp<len&&s.charAt(i+sp)==' ') sp++;")
  (emit "        if (i+sp<len&&s.charAt(i+sp)!='\\n') { Result r=ok(inp); r.tagInt=Math.max(1,sp-n); return r; }")
  (emit "        int j=i; while (j<len&&s.charAt(j)!='\\n') j++;")
  (emit "        while (j<len) {")
  (emit "            if (s.charAt(j)=='\\n') j++; if (j>=len) break;")
  (emit "            sp=0; while (j+sp<len&&s.charAt(j+sp)==' ') sp++;")
  (emit "            int nx=j+sp; if (nx>=len||s.charAt(nx)=='\\n') { j=nx; continue; }")
  (emit "            Result r=ok(inp); r.tagInt=Math.max(1,sp-n); return r; }")
  (emit "        Result r=ok(inp); r.tagInt=1; return r; }")
  (blank)
  (emit "    static Result parseInt_(Input inp, PFn f) {")
  (emit "        Result r=f.apply(inp); if (r.fail) return r;")
  (emit "        int v=0; for (char c:r.val.toCharArray()) if (c>='0'&&c<='9') v=v*10+(c-'0');")
  (emit "        r.tagInt=v; return r; }")
  (emit "    static Result parseSym(Input inp, PFn f, String sym) {")
  (emit "        Result r=f.apply(inp); if (r.fail) return r; r.tag=sym; return r; }")
  (emit "    static Result val(Input inp, String v) { Result r=ok(inp); r.tag=v; return r; }")
  (blank)

  ;; ── Rules ──
  (emit "    // ════════════════════════════════════════════════════════════════")
  (emit "    // YAML 1.2 Grammar — 211 rules")
  (emit "    // ════════════════════════════════════════════════════════════════")
  (blank)
  (dolist (rd (grammar-rules-ordered *gram*))
    (unless (member (rdef-name rd) '(IN-FLOW SEQ-SPACES))
      (let* ((nm (java-ident (rdef-name rd)))
             (ps (rdef-params rd))
             (sig (if ps (format nil "Input inp~{, ~A~}"
                                 (mapcar (lambda (p)
                                           (let ((pn (symbol-name p)))
                                             (if (member pn '("N" "M") :test #'string-equal)
                                                 (format nil "final int ~A" (java-ident p))
                                                 (format nil "final String ~A" (java-ident p)))))
                                         ps))
                      "Input inp"))
             (body-str (ce (rdef-body rd) ps)))
        (emitf "    // [~D] ~A~%" (rdef-num rd) (rdef-name rd))
        (emitf "    static Result ~A(~A) {~%" nm sig)
        (emitf "        return ~A;~%" body-str)
        (emit "    }")
        (blank))))

  ;; ── API + main ──
  (emit "    static void printAst(Ast node, int depth) {")
  (emit "        String indent = \"  \".repeat(depth);")
  (emit "        if (node.isLeaf) System.out.println(indent+\"SCALAR: \\\"\"+node.text+\"\\\"\");")
  (emit "        else { System.out.println(indent+node.tag); if (node.children!=null) for (Ast c:node.children) printAst(c,depth+1); }")
  (emit "    }")
  (blank)
  (emit "    public static void main(String[] args) throws Exception {")
  (emit "        String text;")
  (emit "        if (args.length>0) text=new String(Files.readAllBytes(Paths.get(args[0])));")
  (emit "        else { text=new String(System.in.readAllBytes()); }")
  (emit "        Input inp=Input.of(text);")
  (emit "        Result r=l_yaml_stream(inp);")
  (emit "        if (!r.fail) { System.out.println(\"OK: \"+r.rest.pos+\" chars\"); if (r.ast!=null) printAst(r.ast,0); }")
  (emit "        else { System.err.println(\"FAIL @\"+r.rest.pos+\": \"+r.err); System.exit(1); }")
  (emit "    }")
  (emit "}"))

;;; ═══════════════════════════════════════════════════════════════════
;;; ENTRY POINT
;;; ═══════════════════════════════════════════════════════════════════

(defun project-yaml-to-java (&optional (grammar-path "yaml-grammar.scm")
                                        (output-path "YamlReader.java"))
  (format t "~&; Loading grammar: ~A~%" grammar-path)
  (let ((*gram* (load-yaml-grammar grammar-path)))
    (format t "; Projecting ~D rules to ~A~%" (hash-table-count (gram-rules *gram*)) output-path)
    (emit-yaml-reader-java *gram* output-path)
    (format t "; Done. ~A written.~%" output-path)))

