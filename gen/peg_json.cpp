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

namespace json {

// ── Input ──

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
    return {i.src, i.pos+1, c=='\n' ? i.line+1 : i.line, c=='\n' ? 0 : i.col+1};
}

// ── AST ──

struct ASTNode;
using AST = std::shared_ptr<ASTNode>;
struct ASTNode {
    std::string type;
    std::string text;
    std::vector<AST> children;
    static AST make(const std::string& t) { auto n=std::make_shared<ASTNode>(); n->type=t; return n; }
    static AST leaf(const std::string& s) { auto n=std::make_shared<ASTNode>(); n->type="SCALAR"; n->text=s; return n; }
};

// ── Result ──

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

inline Result ok(Input i) { return {false, "", i}; }
inline Result ok(Input i, const std::string& v) { return {false, v, i}; }
inline Result fail(Input i, const std::string& msg) { return {true, "", i, "", 0, nullptr, {}, msg}; }

// ── Context ──

using Ctx = std::string;
inline bool ctx_eq(const Ctx& a, const Ctx& b) { return a == b; }
inline Ctx in_flow(const Ctx& c) {
    if (c=="FLOW-OUT"||c=="FLOW-IN") return "FLOW-IN";
    return "FLOW-KEY";
}
inline int seq_spaces(int n, const Ctx& c) { return c=="BLOCK-OUT" ? n-1 : n; }

// ── PEG combinators ──

using PFn = std::function<Result(Input)>;

inline Result match_cp(Input inp, int cp) {
    int c = peek_cp(inp); if (c==cp) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,"cp"); }

inline Result match_range(Input inp, int lo, int hi) {
    int c = peek_cp(inp); if (c>=lo&&c<=hi) return ok(adv(inp), std::string(1,(char)c)); return fail(inp,"rng"); }

inline Result match_str(Input inp, const char* t) {
    int n=std::strlen(t); if(inp.pos+n>(int)inp.src->size()) return fail(inp,"str");
    if(inp.src->compare(inp.pos,n,t)!=0) return fail(inp,"str");
    Input c=inp; for(int i=0;i<n;i++) c=adv(c); return ok(c, std::string(t,n)); }

Result seq(Input inp, std::initializer_list<PFn> fns) {
    Input cur=inp; std::string acc; std::vector<AST> asts;
    for(auto& f:fns) { auto r=f(cur); if(r.fail) return r; acc+=r.val;
        if(r.ast) asts.push_back(r.ast);
        else if(!r.ast_list.empty()) asts.insert(asts.end(),r.ast_list.begin(),r.ast_list.end());
        cur=r.rest; }
    Result res=ok(cur,acc); if(asts.size()==1) res.ast=asts[0]; else if(asts.size()>1) res.ast_list=std::move(asts); return res; }

Result alt(Input inp, std::initializer_list<PFn> fns) {
    for(auto& f:fns) { auto r=f(inp); if(!r.fail) return r; } return fail(inp,"alt"); }

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
inline Result neg(Input inp, PFn f) { auto r=f(inp); return r.fail ? ok(inp) : fail(inp,"neg"); }
inline Result minus(Input inp, PFn fa, PFn fb) { auto ra=fa(inp); if(ra.fail) return ra;
    auto rb=fb(inp); return (!rb.fail&&rb.rest.pos==ra.rest.pos) ? fail(inp,"excl") : ra; }
inline Result rep(Input inp, int n, PFn f) { Input c=inp; std::string a;
    for(int i=0;i<n;i++){auto r=f(c);if(r.fail)return r;a+=r.val;c=r.rest;} return ok(c,a); }
inline Result ahead(Input inp, PFn f) { auto r=f(inp); return r.fail ? r : ok(inp); }
inline Result behind(Input inp, PFn f) { if(inp.pos==0) return fail(inp,"bh");
    Input t={inp.src,inp.pos-1,inp.line,std::max(0,inp.col-1)}; auto r=f(t); return r.fail ? fail(inp,"bh") : ok(inp); }
inline Result sol(Input inp) { return inp.col==0 ? ok(inp) : fail(inp,"sol"); }
inline Result eof_ok(Input inp) { return at_eof(inp) ? ok(inp) : fail(inp,"eof"); }

Result json_text(Input inp);
Result value(Input inp);
Result object(Input inp);
Result members(Input inp);
Result member(Input inp);
Result array(Input inp);
Result elements(Input inp);
Result r_string(Input inp);
Result r_char(Input inp);
Result escaped(Input inp);
Result hex4(Input inp);
Result hexdig(Input inp);
Result number(Input inp);
Result integer(Input inp);
Result fraction(Input inp);
Result exponent(Input inp);
Result ws(Input inp);

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
Result json_text(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return ws(inp); },
        [&](Input inp)->Result{ return value(inp); },
        [&](Input inp)->Result{ return ws(inp); },
        [&](Input inp)->Result{ return eof_ok(inp); }});
}

// [2] VALUE 
Result value(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return object(inp); },
        [&](Input inp)->Result{ return array(inp); },
        [&](Input inp)->Result{ return r_string(inp); },
        [&](Input inp)->Result{ return number(inp); },
        [&](Input inp)->Result{ return match_str(inp, "true"); },
        [&](Input inp)->Result{ return match_str(inp, "false"); },
        [&](Input inp)->Result{ return match_str(inp, "null"); }});
}

// [3] OBJECT 
Result object(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 123); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return members(inp); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return match_cp(inp, 125); }}); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 123); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return match_cp(inp, 125); }}); }});
}

// [4] MEMBERS 
Result members(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return member(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return match_cp(inp, 44); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return member(inp); }}); }); }});
}

// [5] MEMBER 
Result member(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return ws(inp); },
        [&](Input inp)->Result{ return r_string(inp); },
        [&](Input inp)->Result{ return ws(inp); },
        [&](Input inp)->Result{ return match_cp(inp, 58); },
        [&](Input inp)->Result{ return ws(inp); },
        [&](Input inp)->Result{ return value(inp); },
        [&](Input inp)->Result{ return ws(inp); }});
}

// [6] ARRAY 
Result array(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 91); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return elements(inp); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return match_cp(inp, 93); }}); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 91); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return match_cp(inp, 93); }}); }});
}

// [7] ELEMENTS 
Result elements(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return value(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return match_cp(inp, 44); },
            [&](Input inp)->Result{ return ws(inp); },
            [&](Input inp)->Result{ return value(inp); }}); }); }});
}

// [8] STRING 
Result r_string(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 34); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return r_char(inp); }); },
        [&](Input inp)->Result{ return match_cp(inp, 34); }});
}

// [9] CHAR 
Result r_char(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return escaped(inp); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return neg(inp, [&](Input inp)->Result{ return match_cp(inp, 34); }); },
            [&](Input inp)->Result{ return neg(inp, [&](Input inp)->Result{ return match_cp(inp, 92); }); },
            [&](Input inp)->Result{ return neg(inp, [&](Input inp)->Result{ return match_cp(inp, 0x0); }); },
            [&](Input inp)->Result{ return neg(inp, [&](Input inp)->Result{ return match_range(inp, 0x0, 0x1F); }); },
            [&](Input inp)->Result{ return match_range(inp, 0x20, 0x10FFFF); }}); }});
}

// [10] ESCAPED 
Result escaped(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 92); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 34); },
            [&](Input inp)->Result{ return match_cp(inp, 92); },
            [&](Input inp)->Result{ return match_cp(inp, 47); },
            [&](Input inp)->Result{ return match_cp(inp, 98); },
            [&](Input inp)->Result{ return match_cp(inp, 102); },
            [&](Input inp)->Result{ return match_cp(inp, 110); },
            [&](Input inp)->Result{ return match_cp(inp, 114); },
            [&](Input inp)->Result{ return match_cp(inp, 116); },
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return match_cp(inp, 117); },
                [&](Input inp)->Result{ return hex4(inp); }}); }}); }});
}

// [11] HEX4 
Result hex4(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return hexdig(inp); },
        [&](Input inp)->Result{ return hexdig(inp); },
        [&](Input inp)->Result{ return hexdig(inp); },
        [&](Input inp)->Result{ return hexdig(inp); }});
}

// [12] HEXDIG 
Result hexdig(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return match_range(inp, 48, 57); },
        [&](Input inp)->Result{ return match_range(inp, 97, 102); },
        [&](Input inp)->Result{ return match_range(inp, 65, 70); }});
}

// [13] NUMBER 
Result number(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return match_cp(inp, 45); }); },
        [&](Input inp)->Result{ return integer(inp); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return fraction(inp); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return exponent(inp); }); }});
}

// [14] INTEGER 
Result integer(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 48); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_range(inp, 49, 57); },
            [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return match_range(inp, 48, 57); }); }}); }});
}

// [15] FRACTION 
Result fraction(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 46); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return match_range(inp, 48, 57); }); }});
}

// [16] EXPONENT 
Result exponent(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 101); },
            [&](Input inp)->Result{ return match_cp(inp, 69); }}); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 43); },
            [&](Input inp)->Result{ return match_cp(inp, 45); }}); }); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return match_range(inp, 48, 57); }); }});
}

// [17] WS 
Result ws(Input inp) {
    return star(inp, [&](Input inp)->Result{ return alt(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 0x20); },
        [&](Input inp)->Result{ return match_cp(inp, 0x9); },
        [&](Input inp)->Result{ return match_cp(inp, 0x0A); },
        [&](Input inp)->Result{ return match_cp(inp, 0x0D); }}); });
}

// ── Public API ──

struct ParseResult { bool success; AST ast; int pos; std::string error; };

ParseResult parse(const std::string& text) {
    Input inp={&text,0,1,0};
    auto r=json_text(inp);
    if(!r.fail) return {true, r.ast, r.rest.pos, ""};
    return {false, nullptr, r.rest.pos, r.err};
}

void print_ast(const AST& n, int d=0) {
    if(!n) return;
    for(int i=0;i<d;i++) std::cout<<"  ";
    if(n->type=="SCALAR") std::cout<<"SCALAR: \""<<n->text<<"\""<<std::endl;
    else { std::cout<<n->type<<std::endl; for(auto& c:n->children) print_ast(c,d+1); }
}

} // namespace json

int main(int argc, char* argv[]) {
    std::string text;
    if(argc>1) { std::ifstream f(argv[1]); if(!f){std::cerr<<"Cannot open "<<argv[1]<<std::endl;return 1;}
        std::ostringstream ss; ss<<f.rdbuf(); text=ss.str(); }
    else { std::ostringstream ss; ss<<std::cin.rdbuf(); text=ss.str(); }
    auto result=json::parse(text);
    if(!result.success) { std::cerr<<"FAIL @"<<result.pos<<": "<<result.error<<std::endl; return 1; }
    std::cout<<"OK: "<<result.pos<<" chars"<<std::endl;
    json::print_ast(result.ast);
    return 0;
}
