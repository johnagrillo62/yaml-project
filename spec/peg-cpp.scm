; ==========================================================================
; peg-cpp.scm — PEG parser target spec for C++
; ==========================================================================
;
; Defines how PEG combinators, runtime types, and boilerplate map to C++.
; Used by emit-yaml-peg.lisp to project yaml-grammar.scm → yaml-reader.cpp.
;
; Switch this file to peg-rust.scm, peg-go.scm, etc. to retarget.
;
; ==========================================================================

(Language PegCpp

  (Include "cpp-types.scm")

  (Preamble
    "#include <string>"
    "#include <string_view>"
    "#include <vector>"
    "#include <functional>"
    "#include <unordered_map>"
    "#include <memory>"
    "#include <cstdint>"
    "#include <cstdio>"
    "#include <iostream>"
    "#include <fstream>"
    "#include <sstream>"
    "#include <algorithm>"
    "#include <cstring>")

  ;; ── Target language identity ──
  (Target
    (lang       "cpp")
    (extension  "cpp")
    (namespace-open  (lambda (name) (str "namespace " name " {")))
    (namespace-close (lambda () "} // namespace"))
    (comment    (lambda (text) (str "// " text))))

  ;; ── Identifiers ──
  (Ident
    (rule-ident    (lambda (name)
                     ;; ns-plain-char → ns_plain_char, l+block → l_block
                     (let* ((s (substitute #\_ #\- name))
                            (s (remove #\+ s)))
                       (if (member s '("not" "and" "or" "class" "switch" "case"
                                       "default" "break" "return" "if" "else"
                                       "while" "for" "do" "new" "delete" "true" "false"
                                       "struct" "enum" "namespace" "template" "auto")
                                   :test string=)
                           (str "r_" s) s))))
    (ctx-type      "std::string")
    (int-type      "int"))

  ;; ── Runtime types ──
  (RuntimeTypes
    ;; Input stream
    (input-type    "Input")
    (input-struct  "struct Input {
    const std::string* src;
    int pos = 0;
    int line = 1;
    int col = 0;
};")
    (eof-fn        "inline bool at_eof(Input i) { return i.pos >= (int)i.src->size(); }")
    (peek-fn       "inline int  peek_cp(Input i) { return at_eof(i) ? -1 : (unsigned char)(*i.src)[i.pos]; }")
    (advance-fn    "inline Input adv(Input i) {
    if (at_eof(i)) return i;
    char c = (*i.src)[i.pos];
    return {i.src, i.pos+1, c=='\\n' ? i.line+1 : i.line, c=='\\n' ? 0 : i.col+1};
}")

    ;; AST
    (ast-type      "AST")
    (ast-struct    "struct ASTNode;
using AST = std::shared_ptr<ASTNode>;
struct ASTNode {
    std::string type;
    std::string text;
    std::vector<AST> children;
    static AST make(const std::string& t) { auto n=std::make_shared<ASTNode>(); n->type=t; return n; }
    static AST leaf(const std::string& s) { auto n=std::make_shared<ASTNode>(); n->type=\"SCALAR\"; n->text=s; return n; }
};")

    ;; Result
    (result-type   "Result")
    (result-struct "struct Result {
    bool fail = false;
    std::string val;
    Input rest;
    std::string tag;
    int tag_int = 0;
    AST ast;
    std::vector<AST> ast_list;
    std::string err;
};")

    ;; Constructors
    (ok-empty      "inline Result ok(Input i) { return {false, \"\", i}; }")
    (ok-val        "inline Result ok(Input i, const std::string& v) { return {false, v, i}; }")
    (fail-fn       "inline Result fail(Input i, const std::string& msg) { return {true, \"\", i, \"\", 0, nullptr, {}, msg}; }"))

  ;; ── Context helpers ──
  (ContextHelpers
    (ctx-eq        "inline bool ctx_eq(const Ctx& a, const Ctx& b) { return a == b; }")
    (in-flow-fn    "inline Ctx in_flow(const Ctx& c) {
    if (c==\"FLOW-OUT\"||c==\"FLOW-IN\") return \"FLOW-IN\";
    return \"FLOW-KEY\";
}")
    (seq-spaces-fn "inline int seq_spaces(int n, const Ctx& c) { return c==\"BLOCK-OUT\" ? n-1 : n; }"))

  ;; ── PEG combinator templates ──
  ;; Each returns a format string. ~A placeholders for sub-expressions.
  (Combinators
    (pfn-type      "using PFn = std::function<Result(Input)>;")

    ;; Matchers
    (match-cp      (lambda (cp)     (str "match_cp(inp, " cp ")")))
    (match-range   (lambda (lo hi)  (str "match_range(inp, " lo ", " hi ")")))
    (match-str     (lambda (s)      (str "match_str(inp, " s ")")))

    ;; Combinators: wrap sub-exprs in lambdas
    (lambda-wrap   (lambda (body)   (str "[&](Input inp)->Result{ return " body "; }")))
    (seq-call      (lambda (fns)    (str "seq(inp, {" fns "})")))
    (alt-call      (lambda (fns)    (str "alt(inp, {" fns "})")))
    (star-call     (lambda (fn)     (str "star(inp, " fn ")")))
    (plus-call     (lambda (fn)     (str "plus_(inp, " fn ")")))
    (opt-call      (lambda (fn)     (str "opt(inp, " fn ")")))
    (neg-call      (lambda (fn)     (str "neg(inp, " fn ")")))
    (minus-call    (lambda (fa fb)  (str "minus(inp, " fa ", " fb ")")))
    (rep-call      (lambda (n fn)   (str "rep(inp, " n ", " fn ")")))
    (ahead-call    (lambda (fn)     (str "ahead(inp, " fn ")")))
    (behind-call   (lambda (fn)     (str "behind(inp, " fn ")")))
    (sol-call      "sol(inp)")
    (eof-call      "eof_ok(inp)")
    (ok-call       "ok(inp)")

    ;; AST
    (build-call    (lambda (type fn) (str "build(inp, " type ", " fn ")")))
    (scalar-call   (lambda (fn)      (str "scalar(inp, " fn ")")))
    (collect-call  (lambda (fn)      (str "collect(inp, " fn ")")))

    ;; YAML extensions
    (detect-call   (lambda (n)       (str "detect_indent(inp, " n ")")))
    (pint-call     (lambda (fn)      (str "parse_int(inp, " fn ")")))
    (psym-call     (lambda (fn sym)  (str "parse_sym(inp, " fn ", " sym ")")))
    (val-call      (lambda (v)       (str "val(inp, " v ")")))

    ;; Let binding
    (let-int       (lambda (vname expr rest)
                     (str "[&]()->Result{ auto r=" expr "; if(r.fail) return r; int " vname "=r.tag_int; return [&](Input inp)->Result{ return " rest "; }(r.rest); }()")))
    (let-ctx       (lambda (vname expr rest)
                     (str "[&]()->Result{ auto r=" expr "; if(r.fail) return r; Ctx " vname "=r.tag; return [&](Input inp)->Result{ return " rest "; }(r.rest); }()")))

    ;; Switch/dispatch
    (switch-case   (lambda (param val body)
                     (str "if(ctx_eq(" param "," val ")) return " body ";")))
    (switch-wrap   (lambda (cases fallback)
                     (str "[&]()->Result{" cases " return " fallback "; }()")))

    ;; Rule call
    (rule-call     (lambda (name args)
                     (if args (str name "(inp, " args ")")
                              (str name "(inp)"))))

    ;; Function declaration
    (fn-decl       (lambda (name params)
                     (str "Result " name "(" params ");")))
    (fn-open       (lambda (name params)
                     (str "Result " name "(" params ") {")))
    (fn-return     (lambda (body)
                     (str "    return " body ";")))
    (fn-close      "}"))

  ;; ── Combinator implementations (emitted as C++ functions) ──
  ;; These are the runtime bodies. They're target-specific.
  (CombinatorImpls
    (match-cp-impl "inline Result match_cp(Input inp, int cp) {
    int c = peek_cp(inp); if (c==cp) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,\"cp\"); }")

    (match-range-impl "inline Result match_range(Input inp, int lo, int hi) {
    int c = peek_cp(inp); if (c>=lo&&c<=hi) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,\"rng\"); }")

    (match-str-impl "inline Result match_str(Input inp, const char* t) {
    int n=std::strlen(t); if(inp.pos+n>(int)inp.src->size()) return fail(inp,\"str\");
    if(inp.src->compare(inp.pos,n,t)!=0) return fail(inp,\"str\");
    Input c=inp; for(int i=0;i<n;i++) c=adv(c); return ok(c, std::string(t,n)); }")

    (seq-impl "Result seq(Input inp, std::initializer_list<PFn> fns) {
    Input cur=inp; std::string acc; std::vector<AST> asts;
    for(auto& f:fns) { auto r=f(cur); if(r.fail) return r; acc+=r.val;
        if(r.ast) asts.push_back(r.ast);
        else if(!r.ast_list.empty()) asts.insert(asts.end(),r.ast_list.begin(),r.ast_list.end());
        cur=r.rest; }
    Result res=ok(cur,acc); if(asts.size()==1) res.ast=asts[0]; else if(asts.size()>1) res.ast_list=std::move(asts); return res; }")

    (alt-impl "Result alt(Input inp, std::initializer_list<PFn> fns) {
    for(auto& f:fns) { auto r=f(inp); if(!r.fail) return r; } return fail(inp,\"alt\"); }")

    (star-impl "Result star(Input inp, PFn f) {
    Input cur=inp; std::string acc; std::vector<AST> asts;
    for(;;) { auto r=f(cur); if(r.fail||r.rest.pos<=cur.pos) break; acc+=r.val;
        if(r.ast) asts.push_back(r.ast);
        else if(!r.ast_list.empty()) asts.insert(asts.end(),r.ast_list.begin(),r.ast_list.end());
        cur=r.rest; }
    Result res=ok(cur,acc); if(!asts.empty()) res.ast_list=std::move(asts); return res; }")

    (plus-impl "Result plus_(Input inp, PFn f) {
    auto first=f(inp); if(first.fail) return first; auto rest=star(first.rest,f);
    Result res=ok(rest.rest, first.val+rest.val); std::vector<AST> asts;
    if(first.ast) asts.push_back(first.ast);
    else if(!first.ast_list.empty()) asts.insert(asts.end(),first.ast_list.begin(),first.ast_list.end());
    if(!rest.ast_list.empty()) asts.insert(asts.end(),rest.ast_list.begin(),rest.ast_list.end());
    if(!asts.empty()) res.ast_list=std::move(asts); return res; }")

    (opt-impl  "inline Result opt(Input inp, PFn f) { auto r=f(inp); return r.fail ? ok(inp) : r; }")
    (neg-impl  "inline Result neg(Input inp, PFn f) { auto r=f(inp); return r.fail ? ok(inp) : fail(inp,\"neg\"); }")
    (minus-impl "inline Result minus(Input inp, PFn fa, PFn fb) { auto ra=fa(inp); if(ra.fail) return ra;
    auto rb=fb(inp); return (!rb.fail&&rb.rest.pos==ra.rest.pos) ? fail(inp,\"excl\") : ra; }")
    (rep-impl  "inline Result rep(Input inp, int n, PFn f) { Input c=inp; std::string a;
    for(int i=0;i<n;i++){auto r=f(c);if(r.fail)return r;a+=r.val;c=r.rest;} return ok(c,a); }")
    (ahead-impl  "inline Result ahead(Input inp, PFn f) { auto r=f(inp); return r.fail ? r : ok(inp); }")
    (behind-impl "inline Result behind(Input inp, PFn f) { if(inp.pos==0) return fail(inp,\"bh\");
    Input t={inp.src,inp.pos-1,inp.line,std::max(0,inp.col-1)}; auto r=f(t); return r.fail ? fail(inp,\"bh\") : ok(inp); }")
    (sol-impl    "inline Result sol(Input inp) { return inp.col==0 ? ok(inp) : fail(inp,\"sol\"); }")
    (eof-impl    "inline Result eof_ok(Input inp) { return at_eof(inp) ? ok(inp) : fail(inp,\"eof\"); }")

    ;; YAML extensions
    (build-impl "Result build(Input inp, const char* type, PFn f) {
    auto r=f(inp); if(r.fail) return r; auto node=ASTNode::make(type);
    if(r.ast) node->children.push_back(r.ast);
    else if(!r.ast_list.empty()) node->children=std::move(r.ast_list);
    r.ast=node; r.ast_list.clear(); return r; }")
    (scalar-impl "Result scalar(Input inp, PFn f) { auto r=f(inp); if(r.fail) return r; r.ast=ASTNode::leaf(r.val); return r; }")
    (collect-impl "Result collect(Input inp, PFn f) { return f(inp); }")
    (detect-impl "Result detect_indent(Input inp, int n) {
    const auto& s=*inp.src; int len=s.size(),i=inp.pos,sp=0;
    while(i+sp<len&&s[i+sp]==' ') sp++;
    if(i+sp<len&&s[i+sp]!='\\n') { Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }
    while(i<len&&s[i]!='\\n') i++;
    while(i<len) { if(s[i]=='\\n') i++; if(i>=len) break; sp=0;
        while(i+sp<len&&s[i+sp]==' ') sp++;
        int nx=i+sp; if(nx>=len||s[nx]=='\\n'){i=nx;continue;}
        Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }
    Result r=ok(inp); r.tag_int=1; return r; }")
    (pint-impl  "Result parse_int(Input inp, PFn f) { auto r=f(inp); if(r.fail) return r; r.tag_int=0;
    for(char c:r.val) if(c>='0'&&c<='9') r.tag_int=r.tag_int*10+(c-'0'); return r; }")
    (psym-impl  "Result parse_sym(Input inp, PFn f, const char* sym) { auto r=f(inp); if(r.fail) return r; r.tag=sym; return r; }")
    (val-impl   "inline Result val(Input inp, const char* v) { Result r=ok(inp); r.tag=v; return r; }"))

  ;; ── Public API template ──
  (API
    (parse-result  "struct ParseResult { bool success; AST ast; int pos; std::string error; };")
    (parse-fn      "ParseResult parse(const std::string& text) {
    Input inp={&text,0,1,0};
    auto r=l_yaml_stream(inp);
    if(!r.fail) return {true, r.ast, r.rest.pos, \"\"};
    return {false, nullptr, r.rest.pos, r.err};
}")
    (print-ast-fn  "void print_ast(const AST& n, int d=0) {
    if(!n) return;
    for(int i=0;i<d;i++) std::cout<<\"  \";
    if(n->type==\"SCALAR\") std::cout<<\"SCALAR: \\\"\"<<n->text<<\"\\\"\"<<std::endl;
    else { std::cout<<n->type<<std::endl; for(auto& c:n->children) print_ast(c,d+1); }
}"))

  ;; ── Main function template ──
  (Main
    "int main(int argc, char* argv[]) {
    std::string text;
    if(argc>1) { std::ifstream f(argv[1]); if(!f){std::cerr<<\"Cannot open \"<<argv[1]<<std::endl;return 1;}
        std::ostringstream ss; ss<<f.rdbuf(); text=ss.str(); }
    else { std::ostringstream ss; ss<<std::cin.rdbuf(); text=ss.str(); }
    auto result=yaml::parse(text);
    if(result.success) { std::cout<<\"OK: \"<<result.pos<<\" chars\"<<std::endl; yaml::print_ast(result.ast); }
    else { std::cerr<<\"FAIL @\"<<result.pos<<\": \"<<result.error<<std::endl; return 1; }
    return 0;
}"))
