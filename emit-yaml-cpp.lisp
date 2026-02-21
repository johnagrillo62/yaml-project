;;;; emit-yaml-cpp.lisp — Project yaml-grammar.scm → yaml-reader.cpp
;;;;
;;;; Reads the 211 s-expression grammar rules and emits a single C++ file:
;;;;   - PEG runtime (Input, Result, combinators)
;;;;   - One function per grammar rule, named after the rule
;;;;   - Packrat memoization
;;;;   - AST construction (Build, Scalar, Collect)
;;;;   - YAML extensions (Detect-Indent, Let, Parse-Int, Parse-Sym, Val)
;;;;
;;;; The generated code mirrors the spec. Rule 187 l+block-mapping in the
;;;; grammar becomes function l_block_mapping in C++. The call graph of
;;;; the generated code IS the dependency graph of the spec.
;;;;
;;;; Usage:
;;;;   sbcl --load yaml-eval.lisp --load emit-yaml-cpp.lisp --quit
;;;;   => writes yaml-reader.cpp to current directory

(in-package #:yaml-eval)

;;; ═══════════════════════════════════════════════════════════════════
;;; NAME MANGLING
;;; ═══════════════════════════════════════════════════════════════════

(defun cpp-ident (sym)
  "YAML rule name → C++ identifier.  ns-plain-char → ns_plain_char
   l+block-mapping → l_block_mapping   c-l+literal → c_l_literal"
  (let* ((s (string-downcase (symbol-name sym)))
         (s (substitute #\_ #\- s))
         (s (remove #\+ s)))
    (if (or (digit-char-p (char s 0))
            (member s '("not" "and" "or" "class" "switch" "case"
                        "default" "break" "return" "if" "else"
                        "while" "for" "do" "new" "delete" "true" "false"
                        "struct" "enum" "namespace" "template" "auto")
                    :test #'string=))
        (format nil "r_~A" s)
        s)))

;;; ═══════════════════════════════════════════════════════════════════
;;; OUTPUT HELPERS
;;; ═══════════════════════════════════════════════════════════════════

(defvar *out* nil)
(defun emit (&rest args) (dolist (a args) (princ a *out*)) (terpri *out*))
(defun emitf (fmt &rest args) (apply #'format *out* fmt args))
(defun blank () (terpri *out*))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPRESSION COMPILER: s-expr → C++ expression string
;;; ═══════════════════════════════════════════════════════════════════

(defvar *gram* nil)

(defun ce (expr env)
  "Compile expression → C++ Result expression string."
  (cond
    ((null expr) "ok(inp)")
    ((eq expr 'EMPTY) "ok(inp)")
    ((integerp expr) (format nil "match_cp(inp, ~D)" expr))
    ((symbolp expr)
     (if (member expr env) (cpp-ident expr)
         (format nil "~A(inp)" (cpp-ident expr))))
    ((listp expr)
     (let ((op (car expr)) (args (cdr expr)))
       (case op
         ;; (Empty) with parens
         (EMPTY  "ok(inp)")
         (CHAR   (ce-char (car args) env))
         (HEX    (ce-hex (car args)))
         (RANGE  (ce-range (car args) (cadr args)))
         (STR    (format nil "match_str(inp, ~S)" (string (car args))))
         (SEQ    (ce-seq args env))
         (ALT    (ce-alt args env))
         (STAR   (ce-rep "star" args env))
         (PLUS   (ce-rep "plus_" args env))
         (OPT    (format nil "opt(inp, [&](Input inp)->Result{ return ~A; })"
                         (ce (car args) env)))
         (REF    (ce-ref (car args) (cdr args) env))
         (NOT    (format nil "neg(inp, [&](Input inp)->Result{ return ~A; })"
                         (ce (car args) env)))
         (MINUS  (format nil "minus(inp, [&](Input inp)->Result{ return ~A; }, [&](Input inp)->Result{ return ~A; })"
                         (ce (car args) env) (ce (cadr args) env)))
         (REPEAT (format nil "rep(inp, ~A, [&](Input inp)->Result{ return ~A; })"
                         (ca (car args) env) (ce (cadr args) env)))
         (SWITCH (ce-switch (car args) (cdr args) env))
         (LOOKAHEAD  (format nil "ahead(inp, [&](Input inp)->Result{ return ~A; })"
                             (ce (car args) env)))
         (LOOKBEHIND (format nil "behind(inp, [&](Input inp)->Result{ return ~A; })"
                             (ce (car args) env)))
         (STARTOFLINE "sol(inp)")
         (ENDOFINPUT  "eof_ok(inp)")
         (BUILD   (format nil "build(inp, ~S, [&](Input inp)->Result{ return ~A; })"
                          (string (car args)) (ce (cadr args) env)))
         (SCALAR  (format nil "scalar(inp, [&](Input inp)->Result{ return ~A; })"
                          (ce (car args) env)))
         (COLLECT (format nil "collect(inp, [&](Input inp)->Result{ return ~A; })"
                          (ce (car args) env)))
         (LET           (ce-let (car args) (cadr args) env))
         (DETECT-INDENT (format nil "detect_indent(inp, ~A)" (ca (car args) env)))
         (PARSE-INT     (format nil "parse_int(inp, [&](Input inp)->Result{ return ~A; })"
                                (ce (car args) env)))
         (PARSE-SYM     (format nil "parse_sym(inp, [&](Input inp)->Result{ return ~A; }, ~S)"
                                (ce (car args) env) (string (cadr args))))
         (VAL           (format nil "val(inp, ~S)" (string (car args))))
         ((+ -)  (ca expr env))
         (IN-FLOW    (format nil "in_flow(~A)" (ca (cadr expr) env)))
         (SEQ-SPACES (format nil "seq_spaces(~A, ~A)" (ca (cadr expr) env) (ca (caddr expr) env)))
         (t (let ((rd (gethash op (gram-rules *gram*))))
              (if rd
                  (format nil "~A(inp~{, ~A~})" (cpp-ident op)
                          (mapcar (lambda (a) (ca a env)) args))
                  (format nil "/* UNKNOWN ~S */" expr)))))))
    (t (format nil "/* ?? ~S */" expr))))

(defun ce-char (x env)
  (cond
    ((integerp x) (format nil "match_cp(inp, ~D)" x))
    ((and (symbolp x) (string= (symbol-name x) "SQUOTE")) "match_cp(inp, 39)")
    ((and (symbolp x) (string= (symbol-name x) "DQUOTE")) "match_cp(inp, 34)")
    ((and (symbolp x) (member x env)) (format nil "match_cp(inp, ~A)" (cpp-ident x)))
    ((symbolp x) (format nil "match_cp(inp, ~D)" (char-code (char (symbol-name x) 0))))
    (t (format nil "match_cp(inp, ~D)" x))))

(defun ce-hex (v)
  ;; CL reader turns (Hex 20) into integer 20, but it means 0x20.
  ;; Format the decimal repr as if it were hex digits.
  (let ((s (string-upcase (if (integerp v) (format nil "~D" v) (symbol-name v)))))
    (format nil "match_cp(inp, 0x~A)" s)))

(defun ce-range (lo hi)
  (flet ((hv (x)
           (if (and (listp x) (eq (car x) 'HEX))
               ;; CL reader: (Hex 20) → integer 20, means 0x20
               ;; Format decimal repr as hex digits
               (format nil "0x~A" (string-upcase
                                   (if (integerp (cadr x)) (format nil "~D" (cadr x))
                                       (symbol-name (cadr x)))))
               (if (integerp x) (format nil "~D" x)
                   (format nil "~D" (char-code (char (symbol-name x) 0)))))))
    (format nil "match_range(inp, ~A, ~A)" (hv lo) (hv hi))))

(defun ce-seq (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      (format nil "seq(inp, {~{[&](Input inp)->Result{ return ~A; }~^, ~}})"
              (mapcar (lambda (e) (ce e env)) exprs))))

(defun ce-alt (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      (format nil "alt(inp, {~{[&](Input inp)->Result{ return ~A; }~^, ~}})"
              (mapcar (lambda (e) (ce e env)) exprs))))

(defun ce-rep (fn args env)
  (format nil "~A(inp, [&](Input inp)->Result{ return ~A; })" fn (ce (car args) env)))

(defun ce-ref (name call-args env)
  (if call-args
      (format nil "~A(inp~{, ~A~})" (cpp-ident name)
              (mapcar (lambda (a) (ca a env)) call-args))
      (format nil "~A(inp)" (cpp-ident name))))

(defun ce-switch (param cases env)
  (format nil "[&]()->Result{~{ ~A~} return fail(inp, \"no case\"); }()"
          (mapcar (lambda (c)
                    (format nil "if(ctx_eq(~A,~S)) return ~A;"
                            (cpp-ident param)
                            (string-upcase (symbol-name (car c)))
                            (ce (cadr c) env)))
                  cases)))

(defun ce-let (bindings body env)
  (if (null bindings) (ce body env)
      (let* ((b (car bindings))
             (var (car b))
             (val-expr (cadr b))
             (vn (cpp-ident var))
             ;; If var name is N or M, it's numeric. Otherwise it's a context string.
             (numeric-p (member (symbol-name var) '("N" "M") :test #'string-equal)))
        (if numeric-p
            (format nil "[&]()->Result{ auto r=~A; if(r.fail) return r; int ~A=r.tag_int; return [&](Input inp)->Result{ return ~A; }(r.rest); }()"
                    (ce val-expr env) vn (ce-let (cdr bindings) body (cons var env)))
            (format nil "[&]()->Result{ auto r=~A; if(r.fail) return r; Ctx ~A=r.tag; return [&](Input inp)->Result{ return ~A; }(r.rest); }()"
                    (ce val-expr env) vn (ce-let (cdr bindings) body (cons var env)))))))

(defun ca (expr env)
  "Compile arg → C++ int or string expression."
  (cond
    ((integerp expr) (format nil "~D" expr))
    ((and (symbolp expr) (eq expr 'EMPTY)) "0")  ;; Empty as indent level = 0
    ((and (symbolp expr) (member expr env)) (cpp-ident expr))
    ((and (symbolp expr)
          (member (symbol-name expr)
                  '("BLOCK-IN" "BLOCK-OUT" "FLOW-IN" "FLOW-OUT"
                    "BLOCK-KEY" "FLOW-KEY" "STRIP" "CLIP" "KEEP" "EMPTY")
                  :test #'string-equal))
     (format nil "~S" (string-upcase (symbol-name expr))))
    ((symbolp expr) (cpp-ident expr))
    ((and (listp expr) (eq (car expr) '+))
     (format nil "(~{~A~^ + ~})" (mapcar (lambda (e) (ca e env)) (cdr expr))))
    ((and (listp expr) (eq (car expr) '-))
     (let ((ps (mapcar (lambda (e) (ca e env)) (cdr expr))))
       (if (= 1 (length ps)) (format nil "(-~A)" (car ps))
           (format nil "(~A~{ - ~A~})" (car ps) (cdr ps)))))
    ((and (listp expr) (eq (car expr) 'IN-FLOW))
     (format nil "in_flow(~A)" (ca (cadr expr) env)))
    ((and (listp expr) (eq (car expr) 'SEQ-SPACES))
     (format nil "seq_spaces(~A, ~A)" (ca (cadr expr) env) (ca (caddr expr) env)))
    (t (format nil "/* arg?~S */" expr))))

;;; ═══════════════════════════════════════════════════════════════════
;;; RULE ORDERING
;;; ═══════════════════════════════════════════════════════════════════

(defun grammar-rules-ordered (gram)
  (let ((rules nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v rules)) (gram-rules gram))
    (sort rules #'< :key #'rdef-num)))

;;; ═══════════════════════════════════════════════════════════════════
;;; EMITTER — writes the complete yaml-reader.cpp
;;; ═══════════════════════════════════════════════════════════════════

(defun emit-yaml-reader (gram path)
  (let ((*gram* gram))
    (with-open-file (*out* path :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (emit-header)
      (emit-runtime)
      (emit-fwd gram)
      (emit-rules gram)
      (emit-api)
      (emit "} // namespace yaml")
      (blank)
      (emit-main))))

(defun emit-header ()
  (emit "// ════════════════════════════════════════════════════════════════")
  (emit "// yaml-reader.cpp — YAML 1.2 parser, projected from yaml-grammar.scm")
  (emit "// ════════════════════════════════════════════════════════════════")
  (emit "// Generated by emit-yaml-cpp.lisp from yaml-grammar.scm")
  (emit "// Each function = one rule from the YAML 1.2 specification.")
  (emit "// Rule numbers in comments reference the spec directly.")
  (emit "// DO NOT EDIT — regenerate from the grammar.")
  (emit "// ════════════════════════════════════════════════════════════════")
  (blank)
  (emit "#include <string>")
  (emit "#include <string_view>")
  (emit "#include <vector>")
  (emit "#include <functional>")
  (emit "#include <unordered_map>")
  (emit "#include <memory>")
  (emit "#include <cstdint>")
  (emit "#include <cstdio>")
  (emit "#include <iostream>")
  (emit "#include <fstream>")
  (emit "#include <sstream>")
  (emit "#include <algorithm>")
  (emit "#include <cstring>")
  (blank)
  (emit "namespace yaml {")
  (blank))

(defun emit-runtime ()
  ;; Input
  (emit "// ── Input stream ──")
  (blank)
  (emit "struct Input {")
  (emit "    const std::string* src;")
  (emit "    int pos = 0;")
  (emit "    int line = 1;")
  (emit "    int col = 0;")
  (emit "};")
  (blank)
  (emit "inline bool at_eof(Input i) { return i.pos >= (int)i.src->size(); }")
  (emit "inline int  peek_cp(Input i) { return at_eof(i) ? -1 : (unsigned char)(*i.src)[i.pos]; }")
  (emit "inline Input adv(Input i) {")
  (emit "    if (at_eof(i)) return i;")
  (emit "    char c = (*i.src)[i.pos];")
  (emit "    return {i.src, i.pos+1, c=='\\n' ? i.line+1 : i.line, c=='\\n' ? 0 : i.col+1};")
  (emit "}")
  (blank)
  ;; AST
  (emit "// ── AST ──")
  (blank)
  (emit "struct ASTNode;")
  (emit "using AST = std::shared_ptr<ASTNode>;")
  (emit "struct ASTNode {")
  (emit "    std::string type;")
  (emit "    std::string text;")
  (emit "    std::vector<AST> children;")
  (emit "    static AST make(const std::string& t) { auto n=std::make_shared<ASTNode>(); n->type=t; return n; }")
  (emit "    static AST leaf(const std::string& s) { auto n=std::make_shared<ASTNode>(); n->type=\"SCALAR\"; n->text=s; return n; }")
  (emit "};")
  (blank)
  ;; Result
  (emit "// ── Parse result ──")
  (blank)
  (emit "struct Result {")
  (emit "    bool fail = false;")
  (emit "    std::string val;")
  (emit "    Input rest;")
  (emit "    std::string tag;")
  (emit "    int tag_int = 0;")
  (emit "    AST ast;")
  (emit "    std::vector<AST> ast_list;")
  (emit "    std::string err;")
  (emit "};")
  (blank)
  (emit "inline Result ok(Input i) { return {false, \"\", i}; }")
  (emit "inline Result ok(Input i, const std::string& v) { return {false, v, i}; }")
  (emit "inline Result fail(Input i, const std::string& msg) { return {true, \"\", i, \"\", 0, nullptr, {}, msg}; }")
  (blank)
  ;; Context
  (emit "// ── Context ──")
  (blank)
  (emit "using Ctx = std::string;")
  (emit "inline bool ctx_eq(const Ctx& a, const Ctx& b) { return a == b; }")
  (emit "inline Ctx in_flow(const Ctx& c) {")
  (emit "    if (c==\"FLOW-OUT\"||c==\"FLOW-IN\") return \"FLOW-IN\";")
  (emit "    return \"FLOW-KEY\";")
  (emit "}")
  (emit "inline int seq_spaces(int n, const Ctx& c) { return c==\"BLOCK-OUT\" ? n-1 : n; }")
  (blank)
  ;; Combinators
  (emit "// ── PEG combinators ──")
  (blank)
  (emit "using PFn = std::function<Result(Input)>;")
  (blank)
  (emit "inline Result match_cp(Input inp, int cp) {")
  (emit "    int c = peek_cp(inp); if (c==cp) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,\"cp\"); }")
  (blank)
  (emit "inline Result match_range(Input inp, int lo, int hi) {")
  (emit "    int c = peek_cp(inp); if (c>=lo&&c<=hi) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,\"rng\"); }")
  (blank)
  (emit "inline Result match_str(Input inp, const char* t) {")
  (emit "    int n=std::strlen(t); if(inp.pos+n>(int)inp.src->size()) return fail(inp,\"str\");")
  (emit "    if(inp.src->compare(inp.pos,n,t)!=0) return fail(inp,\"str\");")
  (emit "    Input c=inp; for(int i=0;i<n;i++) c=adv(c); return ok(c, std::string(t,n)); }")
  (blank)
  (emit "Result seq(Input inp, std::initializer_list<PFn> fns) {")
  (emit "    Input cur=inp; std::string acc; std::vector<AST> asts;")
  (emit "    for(auto& f:fns) { auto r=f(cur); if(r.fail) return r; acc+=r.val;")
  (emit "        if(r.ast) asts.push_back(r.ast);")
  (emit "        else if(!r.ast_list.empty()) asts.insert(asts.end(),r.ast_list.begin(),r.ast_list.end());")
  (emit "        cur=r.rest; }")
  (emit "    Result res=ok(cur,acc); if(asts.size()==1) res.ast=asts[0]; else if(asts.size()>1) res.ast_list=std::move(asts); return res; }")
  (blank)
  (emit "Result alt(Input inp, std::initializer_list<PFn> fns) {")
  (emit "    for(auto& f:fns) { auto r=f(inp); if(!r.fail) return r; } return fail(inp,\"alt\"); }")
  (blank)
  (emit "Result star(Input inp, PFn f) {")
  (emit "    Input cur=inp; std::string acc; std::vector<AST> asts;")
  (emit "    for(;;) { auto r=f(cur); if(r.fail||r.rest.pos<=cur.pos) break; acc+=r.val;")
  (emit "        if(r.ast) asts.push_back(r.ast);")
  (emit "        else if(!r.ast_list.empty()) asts.insert(asts.end(),r.ast_list.begin(),r.ast_list.end());")
  (emit "        cur=r.rest; }")
  (emit "    Result res=ok(cur,acc); if(!asts.empty()) res.ast_list=std::move(asts); return res; }")
  (blank)
  (emit "Result plus_(Input inp, PFn f) {")
  (emit "    auto first=f(inp); if(first.fail) return first; auto rest=star(first.rest,f);")
  (emit "    Result res=ok(rest.rest, first.val+rest.val); std::vector<AST> asts;")
  (emit "    if(first.ast) asts.push_back(first.ast);")
  (emit "    else if(!first.ast_list.empty()) asts.insert(asts.end(),first.ast_list.begin(),first.ast_list.end());")
  (emit "    if(!rest.ast_list.empty()) asts.insert(asts.end(),rest.ast_list.begin(),rest.ast_list.end());")
  (emit "    if(!asts.empty()) res.ast_list=std::move(asts); return res; }")
  (blank)
  (emit "inline Result opt(Input inp, PFn f) { auto r=f(inp); return r.fail ? ok(inp) : r; }")
  (emit "inline Result neg(Input inp, PFn f) { auto r=f(inp); return r.fail ? ok(inp) : fail(inp,\"neg\"); }")
  (emit "inline Result minus(Input inp, PFn fa, PFn fb) { auto ra=fa(inp); if(ra.fail) return ra;")
  (emit "    auto rb=fb(inp); return (!rb.fail&&rb.rest.pos==ra.rest.pos) ? fail(inp,\"excl\") : ra; }")
  (emit "inline Result rep(Input inp, int n, PFn f) { Input c=inp; std::string a;")
  (emit "    for(int i=0;i<n;i++){auto r=f(c);if(r.fail)return r;a+=r.val;c=r.rest;} return ok(c,a); }")
  (emit "inline Result ahead(Input inp, PFn f) { auto r=f(inp); return r.fail ? r : ok(inp); }")
  (emit "inline Result behind(Input inp, PFn f) { if(inp.pos==0) return fail(inp,\"bh\");")
  (emit "    Input t={inp.src,inp.pos-1,inp.line,std::max(0,inp.col-1)}; auto r=f(t); return r.fail ? fail(inp,\"bh\") : ok(inp); }")
  (emit "inline Result sol(Input inp) { return inp.col==0 ? ok(inp) : fail(inp,\"sol\"); }")
  (emit "inline Result eof_ok(Input inp) { return at_eof(inp) ? ok(inp) : fail(inp,\"eof\"); }")
  (blank)
  ;; YAML extensions
  (emit "// ── YAML extensions ──")
  (blank)
  (emit "Result build(Input inp, const char* type, PFn f) {")
  (emit "    auto r=f(inp); if(r.fail) return r; auto node=ASTNode::make(type);")
  (emit "    if(r.ast) node->children.push_back(r.ast);")
  (emit "    else if(!r.ast_list.empty()) node->children=std::move(r.ast_list);")
  (emit "    r.ast=node; r.ast_list.clear(); return r; }")
  (blank)
  (emit "Result scalar(Input inp, PFn f) { auto r=f(inp); if(r.fail) return r; r.ast=ASTNode::leaf(r.val); return r; }")
  (blank)
  (emit "Result collect(Input inp, PFn f) { return f(inp); }")
  (blank)
  (emit "Result detect_indent(Input inp, int n) {")
  (emit "    const auto& s=*inp.src; int len=s.size(),i=inp.pos,sp=0;")
  (emit "    while(i+sp<len&&s[i+sp]==' ') sp++;")
  (emit "    if(i+sp<len&&s[i+sp]!='\\n') { Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }")
  (emit "    while(i<len&&s[i]!='\\n') i++;")
  (emit "    while(i<len) { if(s[i]=='\\n') i++; if(i>=len) break; sp=0;")
  (emit "        while(i+sp<len&&s[i+sp]==' ') sp++;")
  (emit "        int nx=i+sp; if(nx>=len||s[nx]=='\\n'){i=nx;continue;}")
  (emit "        Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }")
  (emit "    Result r=ok(inp); r.tag_int=1; return r; }")
  (blank)
  (emit "Result parse_int(Input inp, PFn f) { auto r=f(inp); if(r.fail) return r; r.tag_int=0;")
  (emit "    for(char c:r.val) if(c>='0'&&c<='9') r.tag_int=r.tag_int*10+(c-'0'); return r; }")
  (blank)
  (emit "Result parse_sym(Input inp, PFn f, const char* sym) { auto r=f(inp); if(r.fail) return r; r.tag=sym; return r; }")
  (blank)
  (emit "inline Result val(Input inp, const char* v) { Result r=ok(inp); r.tag=v; return r; }")
  (blank)
  (blank))

;;; ── Forward declarations ────────────────────────────────────────

(defun param-sig (p)
  (let ((pn (symbol-name p)))
    (if (member pn '("N" "M") :test #'string-equal)
        (format nil "int ~A" (cpp-ident p))
        (format nil "Ctx ~A" (cpp-ident p)))))

(defun emit-fwd (gram)
  (emit "// ── Forward declarations ──")
  (blank)
  (dolist (rd (grammar-rules-ordered gram))
    (unless (member (rdef-name rd) '(IN-FLOW SEQ-SPACES))
      (let* ((nm (cpp-ident (rdef-name rd)))
             (ps (rdef-params rd))
             (sig (if ps (format nil "Input inp~{, ~A~}" (mapcar #'param-sig ps))
                      "Input inp")))
        (emitf "Result ~A(~A);~%" nm sig))))
  (blank))

;;; ── Rule emission ───────────────────────────────────────────────

(defun emit-rules (gram)
  (emit "// ════════════════════════════════════════════════════════════════")
  (emit "// YAML 1.2 Grammar — 211 rules")
  (emit "// ════════════════════════════════════════════════════════════════")
  (blank)
  (dolist (rd (grammar-rules-ordered gram))
    ;; Skip rules that are context/arithmetic computations, not parsers.
    ;; These are handled inline by in_flow() and seq_spaces() helpers.
    (unless (member (rdef-name rd) '(IN-FLOW SEQ-SPACES))
      (let* ((nm (cpp-ident (rdef-name rd)))
             (ps (rdef-params rd))
             (sig (if ps (format nil "Input inp~{, ~A~}" (mapcar #'param-sig ps))
                      "Input inp"))
             (body-str (ce (rdef-body rd) ps)))
        (emitf "// [~D] ~A~%" (rdef-num rd) (rdef-name rd))
        (emitf "Result ~A(~A) {~%" nm sig)
        (emitf "    return ~A;~%" body-str)
        (emit "}")
        (blank)))))

;;; ── Public API ──────────────────────────────────────────────────

(defun emit-api ()
  (emit "// ── Public API ──")
  (blank)
  (emit "struct ParseResult { bool success; AST ast; int pos; std::string error; };")
  (blank)
  (emit "ParseResult parse(const std::string& text) {")
  (emit "    Input inp={&text,0,1,0};")
  (emit "    auto r=l_yaml_stream(inp);")
  (emit "    if(!r.fail) return {true, r.ast, r.rest.pos, \"\"};")
  (emit "    return {false, nullptr, r.rest.pos, r.err};")
  (emit "}")
  (blank)
  (emit "void print_ast(const AST& n, int d=0) {")
  (emit "    if(!n) return;")
  (emit "    for(int i=0;i<d;i++) std::cout<<\"  \";")
  (emit "    if(n->type==\"SCALAR\") std::cout<<\"SCALAR: \\\"\"<<n->text<<\"\\\"\"<<std::endl;")
  (emit "    else { std::cout<<n->type<<std::endl; for(auto& c:n->children) print_ast(c,d+1); }")
  (emit "}")
  (blank))

;;; ── main() ──────────────────────────────────────────────────────

(defun emit-main ()
  (emit "int main(int argc, char* argv[]) {")
  (emit "    std::string text;")
  (emit "    if(argc>1) { std::ifstream f(argv[1]); if(!f){std::cerr<<\"Cannot open \"<<argv[1]<<std::endl;return 1;}")
  (emit "        std::ostringstream ss; ss<<f.rdbuf(); text=ss.str(); }")
  (emit "    else { std::ostringstream ss; ss<<std::cin.rdbuf(); text=ss.str(); }")
  (emit "    auto result=yaml::parse(text);")
  (emit "    if(result.success) { std::cout<<\"OK: \"<<result.pos<<\" chars\"<<std::endl; yaml::print_ast(result.ast); }")
  (emit "    else { std::cerr<<\"FAIL @\"<<result.pos<<\": \"<<result.error<<std::endl; return 1; }")
  (emit "    return 0;")
  (emit "}"))

;;; ═══════════════════════════════════════════════════════════════════
;;; ENTRY POINT
;;; ═══════════════════════════════════════════════════════════════════

(defun project-yaml-to-cpp (&optional (grammar-path "yaml-grammar.scm")
                                       (output-path "yaml-reader.cpp"))
  (format t "~&; Loading grammar: ~A~%" grammar-path)
  (let ((gram (load-yaml-grammar grammar-path)))
    (format t "; Projecting ~D rules to ~A~%" (hash-table-count (gram-rules gram)) output-path)
    (emit-yaml-reader gram output-path)
    (format t "; Done. ~A written.~%" output-path)))

;; Run it
(project-yaml-to-cpp "yaml-grammar.scm" "yaml-reader.cpp")
