;;;; peg-cpp.lisp — C++ target for emit-yaml-peg.lisp
;;;; Load before emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "C++")
(def-tgt "default-output" "yaml-reader.cpp")

(def-tgt "keywords"
  '("not" "and" "or" "class" "switch" "case" "default" "break" "return"
    "if" "else" "while" "for" "do" "new" "delete" "true" "false"
    "struct" "enum" "namespace" "template" "auto"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

;; C++ ref closures: [&](Input inp)->Result{ body }
(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "[&](Input inp)->Result{ return ~A; }" body)))

;; C++ boxed closures (same as ref for initializer_list)
(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "[&](Input inp)->Result{ return ~A; }" body)))

;;; ── Seq/Alt emission ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "seq(inp, {~{~A~^, ~}})" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "alt(inp, {~{~A~^, ~}})" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "[&]()->Result{~{ if(ctx_eq(~A,~S)) return ~A;~} return fail(inp, \"no case\"); }()"
            (loop for (val body) in cases
                  collect param collect val collect body))))

;;; ── Let bindings ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "[&]()->Result{ auto r=~A; if(r.fail) return r; int ~A=r.tag_int; return [&](Input inp)->Result{ return ~A; }(r.rest); }()"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "[&]()->Result{ auto r=~A; if(r.fail) return r; Ctx ~A=r.tag; return [&](Input inp)->Result{ return ~A; }(r.rest); }()"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) name))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "in_flow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seq_spaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(Input inp~{, ~A~})" name
                (mapcar (lambda (p)
                          (let ((pn (symbol-name p)))
                            (if (member pn '("N" "M") :test #'string-equal)
                                (format nil "int ~A" (peg-ident p))
                                (format nil "Ctx ~A" (peg-ident p)))))
                        params))
        (format nil "~A(Input inp)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "Result ~A {~%    return ~A;~%}" sig body)))

(def-tgt "fwd-decl"
  (lambda (name params)
    (let ((sig (funcall (tgt "fn-sig") name params)))
      (format nil "Result ~A;" sig))))

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
// yaml-reader.cpp — YAML 1.2 parser, projected from yaml-grammar.scm
// ════════════════════════════════════════════════════════════════
// Generated. DO NOT EDIT — regenerate from the grammar.
// ════════════════════════════════════════════════════════════════

#include <string>
#include <string_view>
#include <vector>
#include <functional>
#include <map>
#include <unordered_map>
#include <memory>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cstring>

namespace yaml {")

;;; ── Runtime sections ──

(def-tgt "runtime-sections"
  (list
;; Input
"// ── Input ──

struct Input {
    const std::string* src;
    int pos = 0;
    int line = 1;
    int col = 0;
};

inline bool at_eof(Input i) { return i.pos >= (int)i.src->size(); }
inline int  peek_cp(Input i) { return at_eof(i) ? -1 : (unsigned char)(*i.src)[i.pos]; }
inline Input adv(Input i) {
    if (at_eof(i)) return i;
    char c = (*i.src)[i.pos];
    return {i.src, i.pos+1, c=='\\n' ? i.line+1 : i.line, c=='\\n' ? 0 : i.col+1};
}"

;; AST
"// ── AST ──

struct ASTNode;
using AST = std::shared_ptr<ASTNode>;
struct ASTNode {
    std::string type;
    std::string text;
    std::vector<AST> children;
    static AST make(const std::string& t) { auto n=std::make_shared<ASTNode>(); n->type=t; return n; }
    static AST leaf(const std::string& s) { auto n=std::make_shared<ASTNode>(); n->type=\"SCALAR\"; n->text=s; return n; }
};"

;; Result
"// ── Result ──

struct Result {
    bool fail = false;
    std::string val;
    Input rest;
    std::string tag;
    int tag_int = 0;
    AST ast;
    std::vector<AST> ast_list;
    std::string err;
};

inline Result ok(Input i) { return {false, \"\", i}; }
inline Result ok(Input i, const std::string& v) { return {false, v, i}; }
inline Result fail(Input i, const std::string& msg) { return {true, \"\", i, \"\", 0, nullptr, {}, msg}; }"

;; Context
"// ── Context ──

using Ctx = std::string;
inline bool ctx_eq(const Ctx& a, const Ctx& b) { return a == b; }
inline Ctx in_flow(const Ctx& c) {
    if (c==\"FLOW-OUT\"||c==\"FLOW-IN\") return \"FLOW-IN\";
    return \"FLOW-KEY\";
}
inline int seq_spaces(int n, const Ctx& c) { return c==\"BLOCK-OUT\" ? n-1 : n; }"

;; Combinators
"// ── PEG combinators ──

using PFn = std::function<Result(Input)>;

inline Result match_cp(Input inp, int cp) {
    int c = peek_cp(inp); if (c==cp) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,\"cp\"); }

inline Result match_range(Input inp, int lo, int hi) {
    int c = peek_cp(inp); if (c>=lo&&c<=hi) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,\"rng\"); }

inline Result match_str(Input inp, const char* t) {
    int n=std::strlen(t); if(inp.pos+n>(int)inp.src->size()) return fail(inp,\"str\");
    if(inp.src->compare(inp.pos,n,t)!=0) return fail(inp,\"str\");
    Input c=inp; for(int i=0;i<n;i++) c=adv(c); return ok(c, std::string(t,n)); }

Result seq(Input inp, std::initializer_list<PFn> fns) {
    Input cur=inp; std::string acc; std::vector<AST> asts;
    for(auto& f:fns) { auto r=f(cur); if(r.fail) return r; acc+=r.val;
        if(r.ast) asts.push_back(r.ast);
        else if(!r.ast_list.empty()) asts.insert(asts.end(),r.ast_list.begin(),r.ast_list.end());
        cur=r.rest; }
    Result res=ok(cur,acc); if(asts.size()==1) res.ast=asts[0]; else if(asts.size()>1) res.ast_list=std::move(asts); return res; }

Result alt(Input inp, std::initializer_list<PFn> fns) {
    for(auto& f:fns) { auto r=f(inp); if(!r.fail) return r; } return fail(inp,\"alt\"); }

Result star(Input inp, PFn f) {
    Input cur=inp; std::string acc; std::vector<AST> asts;
    for(;;) { auto r=f(cur); if(r.fail||r.rest.pos<=cur.pos) break; acc+=r.val;
        if(r.ast) asts.push_back(r.ast);
        else if(!r.ast_list.empty()) asts.insert(asts.end(),r.ast_list.begin(),r.ast_list.end());
        cur=r.rest; }
    Result res=ok(cur,acc); if(!asts.empty()) res.ast_list=std::move(asts); return res; }

Result plus_(Input inp, PFn f) {
    auto first=f(inp); if(first.fail) return first; auto rest=star(first.rest,f);
    Result res=ok(rest.rest, first.val+rest.val); std::vector<AST> asts;
    if(first.ast) asts.push_back(first.ast);
    else if(!first.ast_list.empty()) asts.insert(asts.end(),first.ast_list.begin(),first.ast_list.end());
    if(!rest.ast_list.empty()) asts.insert(asts.end(),rest.ast_list.begin(),rest.ast_list.end());
    if(!asts.empty()) res.ast_list=std::move(asts); return res; }

inline Result opt(Input inp, PFn f) { auto r=f(inp); return r.fail ? ok(inp) : r; }
inline Result neg(Input inp, PFn f) { auto r=f(inp); return r.fail ? ok(inp) : fail(inp,\"neg\"); }
inline Result minus(Input inp, PFn fa, PFn fb) { auto ra=fa(inp); if(ra.fail) return ra;
    auto rb=fb(inp); return (!rb.fail&&rb.rest.pos==ra.rest.pos) ? fail(inp,\"excl\") : ra; }
inline Result rep(Input inp, int n, PFn f) { Input c=inp; std::string a;
    for(int i=0;i<n;i++){auto r=f(c);if(r.fail)return r;a+=r.val;c=r.rest;} return ok(c,a); }
inline Result ahead(Input inp, PFn f) { auto r=f(inp); return r.fail ? r : ok(inp); }
inline Result behind(Input inp, PFn f) { if(inp.pos==0) return fail(inp,\"bh\");
    Input t={inp.src,inp.pos-1,inp.line,std::max(0,inp.col-1)}; auto r=f(t); return r.fail ? fail(inp,\"bh\") : ok(inp); }
inline Result sol(Input inp) { return inp.col==0 ? ok(inp) : fail(inp,\"sol\"); }
inline Result eof_ok(Input inp) { return at_eof(inp) ? ok(inp) : fail(inp,\"eof\"); }"

;; YAML extensions
"// ── YAML extensions ──

Result build(Input inp, const char* type, PFn f) {
    auto r=f(inp); if(r.fail) return r; auto node=ASTNode::make(type);
    if(r.ast) node->children.push_back(r.ast);
    else if(!r.ast_list.empty()) node->children=std::move(r.ast_list);
    r.ast=node; r.ast_list.clear(); return r; }

Result scalar(Input inp, PFn f) { auto r=f(inp); if(r.fail) return r; r.ast=ASTNode::leaf(r.val); return r; }
Result collect(Input inp, PFn f) { return f(inp); }

Result detect_indent(Input inp, int n) {
    const auto& s=*inp.src; int len=s.size(),i=inp.pos,sp=0;
    while(i+sp<len&&s[i+sp]==' ') sp++;
    if(i+sp<len&&s[i+sp]!='\\n') { Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }
    while(i<len&&s[i]!='\\n') i++;
    while(i<len) { if(s[i]=='\\n') i++; if(i>=len) break; sp=0;
        while(i+sp<len&&s[i+sp]==' ') sp++;
        int nx=i+sp; if(nx>=len||s[nx]=='\\n'){i=nx;continue;}
        Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }
    Result r=ok(inp); r.tag_int=1; return r; }

Result parse_int(Input inp, PFn f) { auto r=f(inp); if(r.fail) return r; r.tag_int=0;
    for(char c:r.val) if(c>='0'&&c<='9') r.tag_int=r.tag_int*10+(c-'0'); return r; }
Result parse_sym(Input inp, PFn f, const char* sym) { auto r=f(inp); if(r.fail) return r; r.tag=sym; return r; }
inline Result val(Input inp, const char* v) { Result r=ok(inp); r.tag=v; return r; }"
))

;;; ── API ──

(def-tgt "api"
"// ── Public API ──

struct ParseResult { bool success; AST ast; int pos; std::string error; };

ParseResult parse(const std::string& text) {
    Input inp={&text,0,1,0};
    auto r=l_yaml_stream(inp);
    if(!r.fail) return {true, r.ast, r.rest.pos, \"\"};
    return {false, nullptr, r.rest.pos, r.err};
}

void print_ast(const AST& n, int d=0) {
    if(!n) return;
    for(int i=0;i<d;i++) std::cout<<\"  \";
    if(n->type==\"SCALAR\") std::cout<<\"SCALAR: \\\"\"<<n->text<<\"\\\"\"<<std::endl;
    else { std::cout<<n->type<<std::endl; for(auto& c:n->children) print_ast(c,d+1); }
}")

(def-tgt "namespace-close" "} // namespace yaml")

;;; ── Main ──

(def-tgt "main-fn"
"int main(int argc, char* argv[]) {
    std::string text;
    if(argc>1) { std::ifstream f(argv[1]); if(!f){std::cerr<<\"Cannot open \"<<argv[1]<<std::endl;return 1;}
        std::ostringstream ss; ss<<f.rdbuf(); text=ss.str(); }
    else { std::ostringstream ss; ss<<std::cin.rdbuf(); text=ss.str(); }
    auto result=yaml::parse(text);
    if(!result.success) { std::cerr<<\"FAIL @\"<<result.pos<<\": \"<<result.error<<std::endl; return 1; }
    std::cout<<\"OK: \"<<result.pos<<\" chars\"<<std::endl;
    yaml::print_ast(result.ast);
    // Also demonstrate native API
    auto val = yaml::load(text);
    std::cout<<\"\\n── Native API ──\"<<std::endl;
    if (val.tag == yaml::YamlValue::Map) {
        for (auto& [k,v] : val.m) {
            std::cout << k << \": \";
            switch(v.tag) {
                case yaml::YamlValue::Null:  std::cout<<\"null\"; break;
                case yaml::YamlValue::Bool:  std::cout<<(v.b?\"true\":\"false\"); break;
                case yaml::YamlValue::Int:   std::cout<<v.i; break;
                case yaml::YamlValue::Float: std::cout<<v.f; break;
                case yaml::YamlValue::Str:   std::cout<<'\"'<<v.s<<'\"'; break;
                case yaml::YamlValue::Map:   std::cout<<\"{map, \"<<v.m.size()<<\" keys}\"; break;
                case yaml::YamlValue::Seq:   std::cout<<\"[seq, \"<<v.v.size()<<\" items]\"; break;
            }
            std::cout << std::endl;
        }
    }
    return 0;
}")

;;; ── Concerns (native API layer) ──

(load "emit/yaml-concerns.lisp")
(def-tgt "yaml-concerns" *yaml-concerns-cpp*)
