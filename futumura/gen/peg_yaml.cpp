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

namespace yaml {

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

// ── YAML extensions ──

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
    if(i+sp<len&&s[i+sp]!='\n') { Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }
    while(i<len&&s[i]!='\n') i++;
    while(i<len) { if(s[i]=='\n') i++; if(i>=len) break; sp=0;
        while(i+sp<len&&s[i+sp]==' ') sp++;
        int nx=i+sp; if(nx>=len||s[nx]=='\n'){i=nx;continue;}
        Result r=ok(inp); r.tag_int=std::max(1,sp-n); return r; }
    Result r=ok(inp); r.tag_int=1; return r; }

Result parse_int(Input inp, PFn f) { auto r=f(inp); if(r.fail) return r; r.tag_int=0;
    for(char c:r.val) if(c>='0'&&c<='9') r.tag_int=r.tag_int*10+(c-'0'); return r; }
Result parse_sym(Input inp, PFn f, const char* sym) { auto r=f(inp); if(r.fail) return r; r.tag=sym; return r; }
inline Result val(Input inp, const char* v) { Result r=ok(inp); r.tag=v; return r; }

Result c_printable(Input inp);
Result nb_json(Input inp);
Result c_byte_order_mark(Input inp);
Result c_sequence_entry(Input inp);
Result c_mapping_key(Input inp);
Result c_mapping_value(Input inp);
Result c_collect_entry(Input inp);
Result c_sequence_start(Input inp);
Result c_sequence_end(Input inp);
Result c_mapping_start(Input inp);
Result c_mapping_end(Input inp);
Result c_comment(Input inp);
Result c_anchor(Input inp);
Result c_alias(Input inp);
Result c_tag(Input inp);
Result c_literal(Input inp);
Result c_folded(Input inp);
Result c_single_quote(Input inp);
Result c_double_quote(Input inp);
Result c_directive(Input inp);
Result c_reserved(Input inp);
Result c_indicator(Input inp);
Result c_flow_indicator(Input inp);
Result b_line_feed(Input inp);
Result b_carriage_return(Input inp);
Result b_char(Input inp);
Result nb_char(Input inp);
Result b_break(Input inp);
Result b_as_line_feed(Input inp);
Result b_non_content(Input inp);
Result s_space(Input inp);
Result s_tab(Input inp);
Result s_white(Input inp);
Result ns_char(Input inp);
Result ns_dec_digit(Input inp);
Result ns_hex_digit(Input inp);
Result ns_ascii_letter(Input inp);
Result ns_word_char(Input inp);
Result ns_uri_char(Input inp);
Result ns_tag_char(Input inp);
Result c_escape(Input inp);
Result ns_esc_null(Input inp);
Result ns_esc_bell(Input inp);
Result ns_esc_backspace(Input inp);
Result ns_esc_horizontal_tab(Input inp);
Result ns_esc_line_feed(Input inp);
Result ns_esc_vertical_tab(Input inp);
Result ns_esc_form_feed(Input inp);
Result ns_esc_carriage_return(Input inp);
Result ns_esc_escape(Input inp);
Result ns_esc_space(Input inp);
Result ns_esc_double_quote(Input inp);
Result ns_esc_slash(Input inp);
Result ns_esc_backslash(Input inp);
Result ns_esc_next_line(Input inp);
Result ns_esc_non_breaking_space(Input inp);
Result ns_esc_line_separator(Input inp);
Result ns_esc_paragraph_separator(Input inp);
Result ns_esc_8_bit(Input inp);
Result ns_esc_16_bit(Input inp);
Result ns_esc_32_bit(Input inp);
Result c_ns_esc_char(Input inp);
Result s_indent(Input inp, int n);
Result s_indent_lt(Input inp, int n);
Result s_indent_le(Input inp, int n);
Result s_separate_in_line(Input inp);
Result s_line_prefix(Input inp, int n, Ctx c);
Result s_block_line_prefix(Input inp, int n);
Result s_flow_line_prefix(Input inp, int n);
Result l_empty(Input inp, int n, Ctx c);
Result b_l_trimmed(Input inp, int n, Ctx c);
Result b_as_space(Input inp);
Result b_l_folded(Input inp, int n, Ctx c);
Result s_flow_folded(Input inp, int n);
Result c_nb_comment_text(Input inp);
Result b_comment(Input inp);
Result s_b_comment(Input inp);
Result l_comment(Input inp);
Result s_l_comments(Input inp);
Result s_separate(Input inp, int n, Ctx c);
Result s_separate_lines(Input inp, int n);
Result l_directive(Input inp);
Result ns_reserved_directive(Input inp);
Result ns_directive_name(Input inp);
Result ns_directive_parameter(Input inp);
Result ns_yaml_directive(Input inp);
Result ns_yaml_version(Input inp);
Result ns_tag_directive(Input inp);
Result c_tag_handle(Input inp);
Result c_primary_tag_handle(Input inp);
Result c_secondary_tag_handle(Input inp);
Result c_named_tag_handle(Input inp);
Result ns_tag_prefix(Input inp);
Result c_ns_local_tag_prefix(Input inp);
Result ns_global_tag_prefix(Input inp);
Result c_ns_properties(Input inp, int n, Ctx c);
Result c_ns_tag_property(Input inp);
Result c_verbatim_tag(Input inp);
Result c_ns_shorthand_tag(Input inp);
Result c_non_specific_tag(Input inp);
Result c_ns_anchor_property(Input inp);
Result ns_anchor_char(Input inp);
Result ns_anchor_name(Input inp);
Result c_ns_alias_node(Input inp);
Result e_scalar(Input inp);
Result e_node(Input inp);
Result nb_double_char(Input inp);
Result ns_double_char(Input inp);
Result c_double_quoted(Input inp, int n, Ctx c);
Result nb_double_text(Input inp, int n, Ctx c);
Result nb_double_one_line(Input inp);
Result s_double_escaped(Input inp, int n);
Result s_double_break(Input inp, int n);
Result nb_ns_double_in_line(Input inp);
Result s_double_next_line(Input inp, int n);
Result nb_double_multi_line(Input inp, int n);
Result c_quoted_quote(Input inp);
Result nb_single_char(Input inp);
Result ns_single_char(Input inp);
Result c_single_quoted(Input inp, int n, Ctx c);
Result nb_single_text(Input inp, int n, Ctx c);
Result nb_single_one_line(Input inp);
Result ns_single_in_line(Input inp);
Result s_single_next_line(Input inp, int n);
Result nb_single_multi_line(Input inp, int n);
Result ns_plain_first(Input inp, Ctx c);
Result ns_plain_safe(Input inp, Ctx c);
Result ns_plain_safe_out(Input inp);
Result ns_plain_safe_in(Input inp);
Result ns_plain_char(Input inp, Ctx c);
Result ns_plain(Input inp, int n, Ctx c);
Result nb_ns_plain_in_line(Input inp, Ctx c);
Result ns_plain_one_line(Input inp, Ctx c);
Result s_ns_plain_next_line(Input inp, int n, Ctx c);
Result ns_plain_multi_line(Input inp, int n, Ctx c);
Result c_flow_sequence(Input inp, int n, Ctx c);
Result ns_s_flow_seq_entries(Input inp, int n, Ctx c);
Result ns_flow_seq_entry(Input inp, int n, Ctx c);
Result c_flow_mapping(Input inp, int n, Ctx c);
Result ns_s_flow_map_entries(Input inp, int n, Ctx c);
Result ns_flow_map_entry(Input inp, int n, Ctx c);
Result ns_flow_map_explicit_entry(Input inp, int n, Ctx c);
Result ns_flow_map_implicit_entry(Input inp, int n, Ctx c);
Result ns_flow_map_yaml_key_entry(Input inp, int n, Ctx c);
Result c_ns_flow_map_empty_key_entry(Input inp, int n, Ctx c);
Result c_ns_flow_map_separate_value(Input inp, int n, Ctx c);
Result c_ns_flow_map_json_key_entry(Input inp, int n, Ctx c);
Result c_ns_flow_map_adjacent_value(Input inp, int n, Ctx c);
Result ns_flow_pair(Input inp, int n, Ctx c);
Result ns_flow_pair_entry(Input inp, int n, Ctx c);
Result ns_flow_pair_yaml_key_entry(Input inp, int n, Ctx c);
Result c_ns_flow_pair_json_key_entry(Input inp, int n, Ctx c);
Result ns_s_implicit_yaml_key(Input inp, Ctx c);
Result c_s_implicit_json_key(Input inp, Ctx c);
Result ns_flow_yaml_content(Input inp, int n, Ctx c);
Result c_flow_json_content(Input inp, int n, Ctx c);
Result ns_flow_content(Input inp, int n, Ctx c);
Result ns_flow_yaml_node(Input inp, int n, Ctx c);
Result c_flow_json_node(Input inp, int n, Ctx c);
Result ns_flow_node(Input inp, int n, Ctx c);
Result c_b_block_header(Input inp, int n);
Result c_indentation_indicator(Input inp, int n);
Result c_chomping_indicator(Input inp);
Result b_chomped_last(Input inp, Ctx t);
Result l_chomped_empty(Input inp, int n, Ctx t);
Result l_strip_empty(Input inp, int n);
Result l_keep_empty(Input inp, int n);
Result l_trail_comments(Input inp, int n);
Result c_lliteral(Input inp, int n);
Result l_nb_literal_text(Input inp, int n);
Result b_nb_literal_next(Input inp, int n);
Result l_literal_content(Input inp, int n, Ctx t);
Result c_lfolded(Input inp, int n);
Result s_nb_folded_text(Input inp, int n);
Result l_nb_folded_lines(Input inp, int n);
Result s_nb_spaced_text(Input inp, int n);
Result b_l_spaced(Input inp, int n);
Result l_nb_spaced_lines(Input inp, int n);
Result l_nb_same_lines(Input inp, int n);
Result l_nb_diff_lines(Input inp, int n);
Result l_folded_content(Input inp, int n, Ctx t);
Result lblock_sequence(Input inp, int n);
Result c_l_block_seq_entry(Input inp, int n);
Result s_lblock_indented(Input inp, int n, Ctx c);
Result ns_l_compact_sequence(Input inp, int n);
Result lblock_mapping(Input inp, int n);
Result ns_l_block_map_entry(Input inp, int n);
Result c_l_block_map_explicit_entry(Input inp, int n);
Result c_l_block_map_explicit_key(Input inp, int n);
Result l_block_map_explicit_value(Input inp, int n);
Result ns_l_block_map_implicit_entry(Input inp, int n);
Result ns_s_block_map_implicit_key(Input inp);
Result c_l_block_map_implicit_value(Input inp, int n);
Result ns_l_compact_mapping(Input inp, int n);
Result s_lblock_node(Input inp, int n, Ctx c);
Result s_lflow_in_block(Input inp, int n);
Result s_lblock_in_block(Input inp, int n, Ctx c);
Result s_lblock_scalar(Input inp, int n, Ctx c);
Result s_lblock_collection(Input inp, int n, Ctx c);
Result l_document_prefix(Input inp);
Result c_directives_end(Input inp);
Result c_document_end(Input inp);
Result l_document_suffix(Input inp);
Result c_forbidden(Input inp);
Result l_bare_document(Input inp);
Result l_explicit_document(Input inp);
Result l_directive_document(Input inp);
Result l_any_document(Input inp);
Result l_yaml_stream(Input inp);

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] C-PRINTABLE 
Result c_printable(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 0x9); },
        [&](Input inp)->Result{ return match_cp(inp, 0x0A); },
        [&](Input inp)->Result{ return match_cp(inp, 0x0D); },
        [&](Input inp)->Result{ return match_range(inp, 0x20, 0x7E); },
        [&](Input inp)->Result{ return match_cp(inp, 0x85); },
        [&](Input inp)->Result{ return match_range(inp, 0xA0, 0xD7FF); },
        [&](Input inp)->Result{ return match_range(inp, 0xE000, 0xFFFD); },
        [&](Input inp)->Result{ return match_range(inp, 0x10000, 0x10FFFF); }});
}

// [2] NB-JSON 
Result nb_json(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 0x9); },
        [&](Input inp)->Result{ return match_range(inp, 0x20, 0x10FFFF); }});
}

// [3] C-BYTE-ORDER-MARK 
Result c_byte_order_mark(Input inp) {
    return match_cp(inp, 0xFEFF);
}

// [4] C-SEQUENCE-ENTRY 
Result c_sequence_entry(Input inp) {
    return match_cp(inp, 45);
}

// [5] C-MAPPING-KEY 
Result c_mapping_key(Input inp) {
    return match_cp(inp, 63);
}

// [6] C-MAPPING-VALUE 
Result c_mapping_value(Input inp) {
    return match_cp(inp, 58);
}

// [7] C-COLLECT-ENTRY 
Result c_collect_entry(Input inp) {
    return match_cp(inp, 44);
}

// [8] C-SEQUENCE-START 
Result c_sequence_start(Input inp) {
    return match_cp(inp, 91);
}

// [9] C-SEQUENCE-END 
Result c_sequence_end(Input inp) {
    return match_cp(inp, 93);
}

// [10] C-MAPPING-START 
Result c_mapping_start(Input inp) {
    return match_cp(inp, 123);
}

// [11] C-MAPPING-END 
Result c_mapping_end(Input inp) {
    return match_cp(inp, 125);
}

// [12] C-COMMENT 
Result c_comment(Input inp) {
    return match_cp(inp, 35);
}

// [13] C-ANCHOR 
Result c_anchor(Input inp) {
    return match_cp(inp, 38);
}

// [14] C-ALIAS 
Result c_alias(Input inp) {
    return match_cp(inp, 42);
}

// [15] C-TAG 
Result c_tag(Input inp) {
    return match_cp(inp, 33);
}

// [16] C-LITERAL 
Result c_literal(Input inp) {
    return match_cp(inp, 124);
}

// [17] C-FOLDED 
Result c_folded(Input inp) {
    return match_cp(inp, 62);
}

// [18] C-SINGLE-QUOTE 
Result c_single_quote(Input inp) {
    return match_cp(inp, 39);
}

// [19] C-DOUBLE-QUOTE 
Result c_double_quote(Input inp) {
    return match_cp(inp, 34);
}

// [20] C-DIRECTIVE 
Result c_directive(Input inp) {
    return match_cp(inp, 37);
}

// [21] C-RESERVED 
Result c_reserved(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 64); },
        [&](Input inp)->Result{ return match_cp(inp, 96); }});
}

// [22] C-INDICATOR 
Result c_indicator(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_sequence_entry(inp); },
        [&](Input inp)->Result{ return c_mapping_key(inp); },
        [&](Input inp)->Result{ return c_mapping_value(inp); },
        [&](Input inp)->Result{ return c_collect_entry(inp); },
        [&](Input inp)->Result{ return c_sequence_start(inp); },
        [&](Input inp)->Result{ return c_sequence_end(inp); },
        [&](Input inp)->Result{ return c_mapping_start(inp); },
        [&](Input inp)->Result{ return c_mapping_end(inp); },
        [&](Input inp)->Result{ return c_comment(inp); },
        [&](Input inp)->Result{ return c_anchor(inp); },
        [&](Input inp)->Result{ return c_alias(inp); },
        [&](Input inp)->Result{ return c_tag(inp); },
        [&](Input inp)->Result{ return c_literal(inp); },
        [&](Input inp)->Result{ return c_folded(inp); },
        [&](Input inp)->Result{ return c_single_quote(inp); },
        [&](Input inp)->Result{ return c_double_quote(inp); },
        [&](Input inp)->Result{ return c_directive(inp); },
        [&](Input inp)->Result{ return c_reserved(inp); }});
}

// [23] C-FLOW-INDICATOR 
Result c_flow_indicator(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_collect_entry(inp); },
        [&](Input inp)->Result{ return c_sequence_start(inp); },
        [&](Input inp)->Result{ return c_sequence_end(inp); },
        [&](Input inp)->Result{ return c_mapping_start(inp); },
        [&](Input inp)->Result{ return c_mapping_end(inp); }});
}

// [24] B-LINE-FEED 
Result b_line_feed(Input inp) {
    return match_cp(inp, 0x0A);
}

// [25] B-CARRIAGE-RETURN 
Result b_carriage_return(Input inp) {
    return match_cp(inp, 0x0D);
}

// [26] B-CHAR 
Result b_char(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return b_line_feed(inp); },
        [&](Input inp)->Result{ return b_carriage_return(inp); }});
}

// [27] NB-CHAR 
Result nb_char(Input inp) {
    return minus(inp, [&](Input inp)->Result{ return c_printable(inp); }, [&](Input inp)->Result{ return alt(inp, {
        [&](Input inp)->Result{ return b_char(inp); },
        [&](Input inp)->Result{ return c_byte_order_mark(inp); }}); });
}

// [28] B-BREAK 
Result b_break(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return b_carriage_return(inp); },
            [&](Input inp)->Result{ return b_line_feed(inp); }}); },
        [&](Input inp)->Result{ return b_carriage_return(inp); },
        [&](Input inp)->Result{ return b_line_feed(inp); }});
}

// [29] B-AS-LINE-FEED 
Result b_as_line_feed(Input inp) {
    return b_break(inp);
}

// [30] B-NON-CONTENT 
Result b_non_content(Input inp) {
    return b_break(inp);
}

// [31] S-SPACE 
Result s_space(Input inp) {
    return match_cp(inp, 0x20);
}

// [32] S-TAB 
Result s_tab(Input inp) {
    return match_cp(inp, 0x9);
}

// [33] S-WHITE 
Result s_white(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return s_space(inp); },
        [&](Input inp)->Result{ return s_tab(inp); }});
}

// [34] NS-CHAR 
Result ns_char(Input inp) {
    return minus(inp, [&](Input inp)->Result{ return nb_char(inp); }, [&](Input inp)->Result{ return s_white(inp); });
}

// [35] NS-DEC-DIGIT 
Result ns_dec_digit(Input inp) {
    return match_range(inp, 0x30, 0x39);
}

// [36] NS-HEX-DIGIT 
Result ns_hex_digit(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return ns_dec_digit(inp); },
        [&](Input inp)->Result{ return match_range(inp, 0x41, 0x46); },
        [&](Input inp)->Result{ return match_range(inp, 0x61, 0x66); }});
}

// [37] NS-ASCII-LETTER 
Result ns_ascii_letter(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return match_range(inp, 0x41, 0x5A); },
        [&](Input inp)->Result{ return match_range(inp, 0x61, 0x7A); }});
}

// [38] NS-WORD-CHAR 
Result ns_word_char(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return ns_dec_digit(inp); },
        [&](Input inp)->Result{ return ns_ascii_letter(inp); },
        [&](Input inp)->Result{ return match_cp(inp, 45); }});
}

// [39] NS-URI-CHAR 
Result ns_uri_char(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 37); },
            [&](Input inp)->Result{ return ns_hex_digit(inp); },
            [&](Input inp)->Result{ return ns_hex_digit(inp); }}); },
        [&](Input inp)->Result{ return ns_word_char(inp); },
        [&](Input inp)->Result{ return match_cp(inp, 35); },
        [&](Input inp)->Result{ return match_cp(inp, 59); },
        [&](Input inp)->Result{ return match_cp(inp, 47); },
        [&](Input inp)->Result{ return match_cp(inp, 63); },
        [&](Input inp)->Result{ return match_cp(inp, 58); },
        [&](Input inp)->Result{ return match_cp(inp, 64); },
        [&](Input inp)->Result{ return match_cp(inp, 38); },
        [&](Input inp)->Result{ return match_cp(inp, 61); },
        [&](Input inp)->Result{ return match_cp(inp, 43); },
        [&](Input inp)->Result{ return match_cp(inp, 36); },
        [&](Input inp)->Result{ return match_cp(inp, 44); },
        [&](Input inp)->Result{ return match_cp(inp, 95); },
        [&](Input inp)->Result{ return match_cp(inp, 46); },
        [&](Input inp)->Result{ return match_cp(inp, 33); },
        [&](Input inp)->Result{ return match_cp(inp, 126); },
        [&](Input inp)->Result{ return match_cp(inp, 42); },
        [&](Input inp)->Result{ return match_cp(inp, 39); },
        [&](Input inp)->Result{ return match_cp(inp, 40); },
        [&](Input inp)->Result{ return match_cp(inp, 41); },
        [&](Input inp)->Result{ return match_cp(inp, 91); },
        [&](Input inp)->Result{ return match_cp(inp, 93); }});
}

// [40] NS-TAG-CHAR 
Result ns_tag_char(Input inp) {
    return minus(inp, [&](Input inp)->Result{ return ns_uri_char(inp); }, [&](Input inp)->Result{ return alt(inp, {
        [&](Input inp)->Result{ return c_tag(inp); },
        [&](Input inp)->Result{ return c_flow_indicator(inp); }}); });
}

// [41] C-ESCAPE 
Result c_escape(Input inp) {
    return match_cp(inp, 92);
}

// [42] NS-ESC-NULL 
Result ns_esc_null(Input inp) {
    return match_cp(inp, 48);
}

// [43] NS-ESC-BELL 
Result ns_esc_bell(Input inp) {
    return match_cp(inp, 97);
}

// [44] NS-ESC-BACKSPACE 
Result ns_esc_backspace(Input inp) {
    return match_cp(inp, 98);
}

// [45] NS-ESC-HORIZONTAL-TAB 
Result ns_esc_horizontal_tab(Input inp) {
    return match_cp(inp, 116);
}

// [46] NS-ESC-LINE-FEED 
Result ns_esc_line_feed(Input inp) {
    return match_cp(inp, 110);
}

// [47] NS-ESC-VERTICAL-TAB 
Result ns_esc_vertical_tab(Input inp) {
    return match_cp(inp, 118);
}

// [48] NS-ESC-FORM-FEED 
Result ns_esc_form_feed(Input inp) {
    return match_cp(inp, 102);
}

// [49] NS-ESC-CARRIAGE-RETURN 
Result ns_esc_carriage_return(Input inp) {
    return match_cp(inp, 114);
}

// [50] NS-ESC-ESCAPE 
Result ns_esc_escape(Input inp) {
    return match_cp(inp, 101);
}

// [51] NS-ESC-SPACE 
Result ns_esc_space(Input inp) {
    return match_cp(inp, 0x20);
}

// [52] NS-ESC-DOUBLE-QUOTE 
Result ns_esc_double_quote(Input inp) {
    return match_cp(inp, 34);
}

// [53] NS-ESC-SLASH 
Result ns_esc_slash(Input inp) {
    return match_cp(inp, 47);
}

// [54] NS-ESC-BACKSLASH 
Result ns_esc_backslash(Input inp) {
    return match_cp(inp, 92);
}

// [55] NS-ESC-NEXT-LINE 
Result ns_esc_next_line(Input inp) {
    return match_cp(inp, 78);
}

// [56] NS-ESC-NON-BREAKING-SPACE 
Result ns_esc_non_breaking_space(Input inp) {
    return match_cp(inp, 95);
}

// [57] NS-ESC-LINE-SEPARATOR 
Result ns_esc_line_separator(Input inp) {
    return match_cp(inp, 76);
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR 
Result ns_esc_paragraph_separator(Input inp) {
    return match_cp(inp, 80);
}

// [59] NS-ESC-8-BIT 
Result ns_esc_8_bit(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 120); },
        [&](Input inp)->Result{ return rep(inp, 2, [&](Input inp)->Result{ return ns_hex_digit(inp); }); }});
}

// [60] NS-ESC-16-BIT 
Result ns_esc_16_bit(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 117); },
        [&](Input inp)->Result{ return rep(inp, 4, [&](Input inp)->Result{ return ns_hex_digit(inp); }); }});
}

// [61] NS-ESC-32-BIT 
Result ns_esc_32_bit(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 85); },
        [&](Input inp)->Result{ return rep(inp, 8, [&](Input inp)->Result{ return ns_hex_digit(inp); }); }});
}

// [62] C-NS-ESC-CHAR 
Result c_ns_esc_char(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_escape(inp); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return ns_esc_null(inp); },
            [&](Input inp)->Result{ return ns_esc_bell(inp); },
            [&](Input inp)->Result{ return ns_esc_backspace(inp); },
            [&](Input inp)->Result{ return ns_esc_horizontal_tab(inp); },
            [&](Input inp)->Result{ return ns_esc_line_feed(inp); },
            [&](Input inp)->Result{ return ns_esc_vertical_tab(inp); },
            [&](Input inp)->Result{ return ns_esc_form_feed(inp); },
            [&](Input inp)->Result{ return ns_esc_carriage_return(inp); },
            [&](Input inp)->Result{ return ns_esc_escape(inp); },
            [&](Input inp)->Result{ return ns_esc_space(inp); },
            [&](Input inp)->Result{ return ns_esc_double_quote(inp); },
            [&](Input inp)->Result{ return ns_esc_slash(inp); },
            [&](Input inp)->Result{ return ns_esc_backslash(inp); },
            [&](Input inp)->Result{ return ns_esc_next_line(inp); },
            [&](Input inp)->Result{ return ns_esc_non_breaking_space(inp); },
            [&](Input inp)->Result{ return ns_esc_line_separator(inp); },
            [&](Input inp)->Result{ return ns_esc_paragraph_separator(inp); },
            [&](Input inp)->Result{ return ns_esc_8_bit(inp); },
            [&](Input inp)->Result{ return ns_esc_16_bit(inp); },
            [&](Input inp)->Result{ return ns_esc_32_bit(inp); }}); }});
}

// [63] S-INDENT 
Result s_indent(Input inp, int n) {
    return rep(inp, n, [&](Input inp)->Result{ return s_space(inp); });
}

// [64] S-INDENT-LT 
Result s_indent_lt(Input inp, int n) {
    return star(inp, [&](Input inp)->Result{ return s_space(inp); });
}

// [65] S-INDENT-LE 
Result s_indent_le(Input inp, int n) {
    return star(inp, [&](Input inp)->Result{ return s_space(inp); });
}

// [66] S-SEPARATE-IN-LINE 
Result s_separate_in_line(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return s_white(inp); }); },
        [&](Input inp)->Result{ return ok(inp); }});
}

// [67] S-LINE-PREFIX 
Result s_line_prefix(Input inp, int n, Ctx c) {
    return [&]()->Result{ if(ctx_eq(c,"BLOCK-IN")) return s_block_line_prefix(inp, n); if(ctx_eq(c,"BLOCK-OUT")) return s_block_line_prefix(inp, n); if(ctx_eq(c,"FLOW-IN")) return s_flow_line_prefix(inp, n); if(ctx_eq(c,"FLOW-OUT")) return s_flow_line_prefix(inp, n); return fail(inp, "no case"); }();
}

// [68] S-BLOCK-LINE-PREFIX 
Result s_block_line_prefix(Input inp, int n) {
    return s_indent(inp, n);
}

// [69] S-FLOW-LINE-PREFIX 
Result s_flow_line_prefix(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_indent(inp, n); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate_in_line(inp); }); }});
}

// [70] L-EMPTY 
Result l_empty(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return s_line_prefix(inp, n, c); },
            [&](Input inp)->Result{ return s_indent_lt(inp, n); }}); },
        [&](Input inp)->Result{ return b_as_line_feed(inp); }});
}

// [71] B-L-TRIMMED 
Result b_l_trimmed(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return b_non_content(inp); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return l_empty(inp, n, c); }); }});
}

// [72] B-AS-SPACE 
Result b_as_space(Input inp) {
    return b_break(inp);
}

// [73] B-L-FOLDED 
Result b_l_folded(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return b_l_trimmed(inp, n, c); },
        [&](Input inp)->Result{ return b_as_space(inp); }});
}

// [74] S-FLOW-FOLDED 
Result s_flow_folded(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate_in_line(inp); }); },
        [&](Input inp)->Result{ return b_l_folded(inp, n, "FLOW-IN"); },
        [&](Input inp)->Result{ return s_flow_line_prefix(inp, n); }});
}

// [75] C-NB-COMMENT-TEXT 
Result c_nb_comment_text(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_comment(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return nb_char(inp); }); }});
}

// [76] B-COMMENT 
Result b_comment(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return b_non_content(inp); },
        [&](Input inp)->Result{ return ok(inp); }});
}

// [77] S-B-COMMENT 
Result s_b_comment(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_separate_in_line(inp); },
            [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return c_nb_comment_text(inp); }); }}); }); },
        [&](Input inp)->Result{ return b_comment(inp); }});
}

// [78] L-COMMENT 
Result l_comment(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_separate_in_line(inp); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return c_nb_comment_text(inp); }); },
        [&](Input inp)->Result{ return b_non_content(inp); }});
}

// [79] S-L-COMMENTS 
Result s_l_comments(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return s_b_comment(inp); },
            [&](Input inp)->Result{ return ok(inp); }}); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_comment(inp); }); }});
}

// [80] S-SEPARATE 
Result s_separate(Input inp, int n, Ctx c) {
    return [&]()->Result{ if(ctx_eq(c,"BLOCK-OUT")) return s_separate_lines(inp, n); if(ctx_eq(c,"BLOCK-IN")) return s_separate_lines(inp, n); if(ctx_eq(c,"FLOW-OUT")) return s_separate_lines(inp, n); if(ctx_eq(c,"FLOW-IN")) return s_separate_lines(inp, n); if(ctx_eq(c,"BLOCK-KEY")) return s_separate_in_line(inp); if(ctx_eq(c,"FLOW-KEY")) return s_separate_in_line(inp); return fail(inp, "no case"); }();
}

// [81] S-SEPARATE-LINES 
Result s_separate_lines(Input inp, int n) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_l_comments(inp); },
            [&](Input inp)->Result{ return s_flow_line_prefix(inp, n); }}); },
        [&](Input inp)->Result{ return s_separate_in_line(inp); }});
}

// [82] L-DIRECTIVE 
Result l_directive(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_directive(inp); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return ns_yaml_directive(inp); },
            [&](Input inp)->Result{ return ns_tag_directive(inp); },
            [&](Input inp)->Result{ return ns_reserved_directive(inp); }}); },
        [&](Input inp)->Result{ return s_l_comments(inp); }});
}

// [83] NS-RESERVED-DIRECTIVE 
Result ns_reserved_directive(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_directive_name(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_separate_in_line(inp); },
            [&](Input inp)->Result{ return ns_directive_parameter(inp); }}); }); }});
}

// [84] NS-DIRECTIVE-NAME 
Result ns_directive_name(Input inp) {
    return plus_(inp, [&](Input inp)->Result{ return ns_char(inp); });
}

// [85] NS-DIRECTIVE-PARAMETER 
Result ns_directive_parameter(Input inp) {
    return plus_(inp, [&](Input inp)->Result{ return ns_char(inp); });
}

// [86] NS-YAML-DIRECTIVE 
Result ns_yaml_directive(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_str(inp, "YAML"); },
        [&](Input inp)->Result{ return s_separate_in_line(inp); },
        [&](Input inp)->Result{ return ns_yaml_version(inp); }});
}

// [87] NS-YAML-VERSION 
Result ns_yaml_version(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return ns_dec_digit(inp); }); },
        [&](Input inp)->Result{ return match_cp(inp, 46); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return ns_dec_digit(inp); }); }});
}

// [88] NS-TAG-DIRECTIVE 
Result ns_tag_directive(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_str(inp, "TAG"); },
        [&](Input inp)->Result{ return s_separate_in_line(inp); },
        [&](Input inp)->Result{ return c_tag_handle(inp); },
        [&](Input inp)->Result{ return s_separate_in_line(inp); },
        [&](Input inp)->Result{ return ns_tag_prefix(inp); }});
}

// [89] C-TAG-HANDLE 
Result c_tag_handle(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_named_tag_handle(inp); },
        [&](Input inp)->Result{ return c_secondary_tag_handle(inp); },
        [&](Input inp)->Result{ return c_primary_tag_handle(inp); }});
}

// [90] C-PRIMARY-TAG-HANDLE 
Result c_primary_tag_handle(Input inp) {
    return match_cp(inp, 33);
}

// [91] C-SECONDARY-TAG-HANDLE 
Result c_secondary_tag_handle(Input inp) {
    return match_str(inp, "!!");
}

// [92] C-NAMED-TAG-HANDLE 
Result c_named_tag_handle(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 33); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return ns_word_char(inp); }); },
        [&](Input inp)->Result{ return match_cp(inp, 33); }});
}

// [93] NS-TAG-PREFIX 
Result ns_tag_prefix(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_ns_local_tag_prefix(inp); },
        [&](Input inp)->Result{ return ns_global_tag_prefix(inp); }});
}

// [94] C-NS-LOCAL-TAG-PREFIX 
Result c_ns_local_tag_prefix(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 33); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return ns_uri_char(inp); }); }});
}

// [95] NS-GLOBAL-TAG-PREFIX 
Result ns_global_tag_prefix(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_tag_char(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return ns_uri_char(inp); }); }});
}

// [96] C-NS-PROPERTIES 
Result c_ns_properties(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return c_ns_tag_property(inp); },
            [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return s_separate(inp, n, c); },
                [&](Input inp)->Result{ return c_ns_anchor_property(inp); }}); }); }}); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return c_ns_anchor_property(inp); },
            [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return s_separate(inp, n, c); },
                [&](Input inp)->Result{ return c_ns_tag_property(inp); }}); }); }}); }});
}

// [97] C-NS-TAG-PROPERTY 
Result c_ns_tag_property(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_verbatim_tag(inp); },
        [&](Input inp)->Result{ return c_ns_shorthand_tag(inp); },
        [&](Input inp)->Result{ return c_non_specific_tag(inp); }});
}

// [98] C-VERBATIM-TAG 
Result c_verbatim_tag(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_str(inp, "!<"); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return ns_uri_char(inp); }); },
        [&](Input inp)->Result{ return match_cp(inp, 62); }});
}

// [99] C-NS-SHORTHAND-TAG 
Result c_ns_shorthand_tag(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_tag_handle(inp); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return ns_tag_char(inp); }); }});
}

// [100] C-NON-SPECIFIC-TAG 
Result c_non_specific_tag(Input inp) {
    return match_cp(inp, 33);
}

// [101] C-NS-ANCHOR-PROPERTY 
Result c_ns_anchor_property(Input inp) {
    return build(inp, "ANCHOR", [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return c_anchor(inp); },
        [&](Input inp)->Result{ return scalar(inp, [&](Input inp)->Result{ return ns_anchor_name(inp); }); }}); });
}

// [102] NS-ANCHOR-CHAR 
Result ns_anchor_char(Input inp) {
    return minus(inp, [&](Input inp)->Result{ return ns_char(inp); }, [&](Input inp)->Result{ return c_flow_indicator(inp); });
}

// [103] NS-ANCHOR-NAME 
Result ns_anchor_name(Input inp) {
    return plus_(inp, [&](Input inp)->Result{ return ns_anchor_char(inp); });
}

// [104] C-NS-ALIAS-NODE 
Result c_ns_alias_node(Input inp) {
    return build(inp, "ALIAS", [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return c_alias(inp); },
        [&](Input inp)->Result{ return scalar(inp, [&](Input inp)->Result{ return ns_anchor_name(inp); }); }}); });
}

// [105] E-SCALAR 
Result e_scalar(Input inp) {
    return ok(inp);
}

// [106] E-NODE 
Result e_node(Input inp) {
    return e_scalar(inp);
}

// [107] NB-DOUBLE-CHAR 
Result nb_double_char(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_ns_esc_char(inp); },
        [&](Input inp)->Result{ return minus(inp, [&](Input inp)->Result{ return nb_json(inp); }, [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 92); },
            [&](Input inp)->Result{ return match_cp(inp, 34); }}); }); }});
}

// [108] NS-DOUBLE-CHAR 
Result ns_double_char(Input inp) {
    return minus(inp, [&](Input inp)->Result{ return nb_double_char(inp); }, [&](Input inp)->Result{ return s_white(inp); });
}

// [109] C-DOUBLE-QUOTED 
Result c_double_quoted(Input inp, int n, Ctx c) {
    return scalar(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 34); },
        [&](Input inp)->Result{ return nb_double_text(inp, n, c); },
        [&](Input inp)->Result{ return match_cp(inp, 34); }}); });
}

// [110] NB-DOUBLE-TEXT 
Result nb_double_text(Input inp, int n, Ctx c) {
    return [&]()->Result{ if(ctx_eq(c,"FLOW-OUT")) return nb_double_multi_line(inp, n); if(ctx_eq(c,"FLOW-IN")) return nb_double_multi_line(inp, n); if(ctx_eq(c,"BLOCK-KEY")) return nb_double_one_line(inp); if(ctx_eq(c,"FLOW-KEY")) return nb_double_one_line(inp); return fail(inp, "no case"); }();
}

// [111] NB-DOUBLE-ONE-LINE 
Result nb_double_one_line(Input inp) {
    return star(inp, [&](Input inp)->Result{ return nb_double_char(inp); });
}

// [112] S-DOUBLE-ESCAPED 
Result s_double_escaped(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); },
        [&](Input inp)->Result{ return match_cp(inp, 92); },
        [&](Input inp)->Result{ return b_non_content(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_empty(inp, n, "FLOW-IN"); }); },
        [&](Input inp)->Result{ return s_flow_line_prefix(inp, n); }});
}

// [113] S-DOUBLE-BREAK 
Result s_double_break(Input inp, int n) {
    return alt(inp, {
        [&](Input inp)->Result{ return s_double_escaped(inp, n); },
        [&](Input inp)->Result{ return s_flow_folded(inp, n); }});
}

// [114] NB-NS-DOUBLE-IN-LINE 
Result nb_ns_double_in_line(Input inp) {
    return star(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); },
        [&](Input inp)->Result{ return ns_double_char(inp); }}); });
}

// [115] S-DOUBLE-NEXT-LINE 
Result s_double_next_line(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_double_break(inp, n); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return ns_double_char(inp); },
            [&](Input inp)->Result{ return nb_ns_double_in_line(inp); },
            [&](Input inp)->Result{ return alt(inp, {
                [&](Input inp)->Result{ return s_double_next_line(inp, n); },
                [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); }}); }}); }); }});
}

// [116] NB-DOUBLE-MULTI-LINE 
Result nb_double_multi_line(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return nb_ns_double_in_line(inp); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return s_double_next_line(inp, n); },
            [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); }}); }});
}

// [117] C-QUOTED-QUOTE 
Result c_quoted_quote(Input inp) {
    return match_str(inp, "''");
}

// [118] NB-SINGLE-CHAR 
Result nb_single_char(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_quoted_quote(inp); },
        [&](Input inp)->Result{ return minus(inp, [&](Input inp)->Result{ return nb_json(inp); }, [&](Input inp)->Result{ return match_cp(inp, 39); }); }});
}

// [119] NS-SINGLE-CHAR 
Result ns_single_char(Input inp) {
    return minus(inp, [&](Input inp)->Result{ return nb_single_char(inp); }, [&](Input inp)->Result{ return s_white(inp); });
}

// [120] C-SINGLE-QUOTED 
Result c_single_quoted(Input inp, int n, Ctx c) {
    return scalar(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 39); },
        [&](Input inp)->Result{ return nb_single_text(inp, n, c); },
        [&](Input inp)->Result{ return match_cp(inp, 39); }}); });
}

// [121] NB-SINGLE-TEXT 
Result nb_single_text(Input inp, int n, Ctx c) {
    return [&]()->Result{ if(ctx_eq(c,"FLOW-OUT")) return nb_single_multi_line(inp, n); if(ctx_eq(c,"FLOW-IN")) return nb_single_multi_line(inp, n); if(ctx_eq(c,"BLOCK-KEY")) return nb_single_one_line(inp); if(ctx_eq(c,"FLOW-KEY")) return nb_single_one_line(inp); return fail(inp, "no case"); }();
}

// [122] NB-SINGLE-ONE-LINE 
Result nb_single_one_line(Input inp) {
    return star(inp, [&](Input inp)->Result{ return nb_single_char(inp); });
}

// [123] NS-SINGLE-IN-LINE 
Result ns_single_in_line(Input inp) {
    return star(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); },
        [&](Input inp)->Result{ return ns_single_char(inp); }}); });
}

// [124] S-SINGLE-NEXT-LINE 
Result s_single_next_line(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_flow_folded(inp, n); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return ns_single_char(inp); },
            [&](Input inp)->Result{ return ns_single_in_line(inp); },
            [&](Input inp)->Result{ return alt(inp, {
                [&](Input inp)->Result{ return s_single_next_line(inp, n); },
                [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); }}); }}); }); }});
}

// [125] NB-SINGLE-MULTI-LINE 
Result nb_single_multi_line(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_single_in_line(inp); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return s_single_next_line(inp, n); },
            [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); }}); }});
}

// [126] NS-PLAIN-FIRST 
Result ns_plain_first(Input inp, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return minus(inp, [&](Input inp)->Result{ return ns_char(inp); }, [&](Input inp)->Result{ return c_indicator(inp); }); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return alt(inp, {
                [&](Input inp)->Result{ return match_cp(inp, 63); },
                [&](Input inp)->Result{ return match_cp(inp, 58); },
                [&](Input inp)->Result{ return match_cp(inp, 45); }}); },
            [&](Input inp)->Result{ return ahead(inp, [&](Input inp)->Result{ return ns_plain_safe(inp, c); }); }}); }});
}

// [127] NS-PLAIN-SAFE 
Result ns_plain_safe(Input inp, Ctx c) {
    return [&]()->Result{ if(ctx_eq(c,"FLOW-OUT")) return ns_plain_safe_out(inp); if(ctx_eq(c,"FLOW-IN")) return ns_plain_safe_in(inp); if(ctx_eq(c,"BLOCK-KEY")) return ns_plain_safe_out(inp); if(ctx_eq(c,"FLOW-KEY")) return ns_plain_safe_in(inp); return fail(inp, "no case"); }();
}

// [128] NS-PLAIN-SAFE-OUT 
Result ns_plain_safe_out(Input inp) {
    return ns_char(inp);
}

// [129] NS-PLAIN-SAFE-IN 
Result ns_plain_safe_in(Input inp) {
    return minus(inp, [&](Input inp)->Result{ return ns_char(inp); }, [&](Input inp)->Result{ return c_flow_indicator(inp); });
}

// [130] NS-PLAIN-CHAR 
Result ns_plain_char(Input inp, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return minus(inp, [&](Input inp)->Result{ return ns_plain_safe(inp, c); }, [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 58); },
            [&](Input inp)->Result{ return match_cp(inp, 35); }}); }); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return behind(inp, [&](Input inp)->Result{ return ns_char(inp); }); },
            [&](Input inp)->Result{ return match_cp(inp, 35); }}); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 58); },
            [&](Input inp)->Result{ return ahead(inp, [&](Input inp)->Result{ return ns_plain_safe(inp, c); }); }}); }});
}

// [131] NS-PLAIN 
Result ns_plain(Input inp, int n, Ctx c) {
    return scalar(inp, [&](Input inp)->Result{ return [&]()->Result{ if(ctx_eq(c,"FLOW-OUT")) return ns_plain_multi_line(inp, n, c); if(ctx_eq(c,"FLOW-IN")) return ns_plain_multi_line(inp, n, c); if(ctx_eq(c,"BLOCK-KEY")) return ns_plain_one_line(inp, c); if(ctx_eq(c,"FLOW-KEY")) return ns_plain_one_line(inp, c); return fail(inp, "no case"); }(); });
}

// [132] NB-NS-PLAIN-IN-LINE 
Result nb_ns_plain_in_line(Input inp, Ctx c) {
    return star(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_white(inp); }); },
        [&](Input inp)->Result{ return ns_plain_char(inp, c); }}); });
}

// [133] NS-PLAIN-ONE-LINE 
Result ns_plain_one_line(Input inp, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_plain_first(inp, c); },
        [&](Input inp)->Result{ return nb_ns_plain_in_line(inp, c); }});
}

// [134] S-NS-PLAIN-NEXT-LINE 
Result s_ns_plain_next_line(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_flow_folded(inp, n); },
        [&](Input inp)->Result{ return neg(inp, [&](Input inp)->Result{ return c_forbidden(inp); }); },
        [&](Input inp)->Result{ return ns_plain_char(inp, c); },
        [&](Input inp)->Result{ return nb_ns_plain_in_line(inp, c); }});
}

// [135] NS-PLAIN-MULTI-LINE 
Result ns_plain_multi_line(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_plain_one_line(inp, c); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return s_ns_plain_next_line(inp, n, c); }); }});
}

// [137] C-FLOW-SEQUENCE 
Result c_flow_sequence(Input inp, int n, Ctx c) {
    return build(inp, "SEQUENCE", [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 91); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return collect(inp, [&](Input inp)->Result{ return ns_s_flow_seq_entries(inp, n, in_flow(c)); }); }); },
        [&](Input inp)->Result{ return match_cp(inp, 93); }}); });
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
Result ns_s_flow_seq_entries(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_flow_seq_entry(inp, n, c); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 44); },
            [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
            [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return ns_s_flow_seq_entries(inp, n, c); }); }}); }); }});
}

// [139] NS-FLOW-SEQ-ENTRY 
Result ns_flow_seq_entry(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return ns_flow_pair(inp, n, c); },
        [&](Input inp)->Result{ return ns_flow_node(inp, n, c); }});
}

// [140] C-FLOW-MAPPING 
Result c_flow_mapping(Input inp, int n, Ctx c) {
    return build(inp, "MAPPING", [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 123); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return collect(inp, [&](Input inp)->Result{ return ns_s_flow_map_entries(inp, n, in_flow(c)); }); }); },
        [&](Input inp)->Result{ return match_cp(inp, 125); }}); });
}

// [141] NS-S-FLOW-MAP-ENTRIES 
Result ns_s_flow_map_entries(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_flow_map_entry(inp, n, c); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 44); },
            [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
            [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return ns_s_flow_map_entries(inp, n, c); }); }}); }); }});
}

// [142] NS-FLOW-MAP-ENTRY 
Result ns_flow_map_entry(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 63); },
            [&](Input inp)->Result{ return s_separate(inp, n, c); },
            [&](Input inp)->Result{ return ns_flow_map_explicit_entry(inp, n, c); }}); },
        [&](Input inp)->Result{ return ns_flow_map_implicit_entry(inp, n, c); }});
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
Result ns_flow_map_explicit_entry(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return ns_flow_map_implicit_entry(inp, n, c); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return e_node(inp); },
            [&](Input inp)->Result{ return e_node(inp); }}); }});
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
Result ns_flow_map_implicit_entry(Input inp, int n, Ctx c) {
    return build(inp, "PAIR", [&](Input inp)->Result{ return alt(inp, {
        [&](Input inp)->Result{ return ns_flow_map_yaml_key_entry(inp, n, c); },
        [&](Input inp)->Result{ return c_ns_flow_map_empty_key_entry(inp, n, c); },
        [&](Input inp)->Result{ return c_ns_flow_map_json_key_entry(inp, n, c); }}); });
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
Result ns_flow_map_yaml_key_entry(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_flow_yaml_node(inp, n, c); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
                [&](Input inp)->Result{ return c_ns_flow_map_separate_value(inp, n, c); }}); },
            [&](Input inp)->Result{ return e_node(inp); }}); }});
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
Result c_ns_flow_map_empty_key_entry(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return e_node(inp); },
        [&](Input inp)->Result{ return c_ns_flow_map_separate_value(inp, n, c); }});
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
Result c_ns_flow_map_separate_value(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 58); },
        [&](Input inp)->Result{ return neg(inp, [&](Input inp)->Result{ return ns_plain_safe(inp, c); }); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return s_separate(inp, n, c); },
                [&](Input inp)->Result{ return ns_flow_node(inp, n, c); }}); },
            [&](Input inp)->Result{ return e_node(inp); }}); }});
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
Result c_ns_flow_map_json_key_entry(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_flow_json_node(inp, n, c); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
                [&](Input inp)->Result{ return c_ns_flow_map_adjacent_value(inp, n, c); }}); },
            [&](Input inp)->Result{ return e_node(inp); }}); }});
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
Result c_ns_flow_map_adjacent_value(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 58); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate(inp, n, c); }); },
                [&](Input inp)->Result{ return ns_flow_node(inp, n, c); }}); },
            [&](Input inp)->Result{ return e_node(inp); }}); }});
}

// [150] NS-FLOW-PAIR 
Result ns_flow_pair(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return match_cp(inp, 63); },
            [&](Input inp)->Result{ return s_separate(inp, n, c); },
            [&](Input inp)->Result{ return ns_flow_map_explicit_entry(inp, n, c); }}); },
        [&](Input inp)->Result{ return ns_flow_pair_entry(inp, n, c); }});
}

// [151] NS-FLOW-PAIR-ENTRY 
Result ns_flow_pair_entry(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return ns_flow_pair_yaml_key_entry(inp, n, c); },
        [&](Input inp)->Result{ return c_ns_flow_map_empty_key_entry(inp, n, c); },
        [&](Input inp)->Result{ return c_ns_flow_pair_json_key_entry(inp, n, c); }});
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
Result ns_flow_pair_yaml_key_entry(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_s_implicit_yaml_key(inp, "FLOW-KEY"); },
        [&](Input inp)->Result{ return c_ns_flow_map_separate_value(inp, n, c); }});
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
Result c_ns_flow_pair_json_key_entry(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_s_implicit_json_key(inp, "FLOW-KEY"); },
        [&](Input inp)->Result{ return c_ns_flow_map_adjacent_value(inp, n, c); }});
}

// [154] NS-S-IMPLICIT-YAML-KEY 
Result ns_s_implicit_yaml_key(Input inp, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_flow_yaml_node(inp, 0, c); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate_in_line(inp); }); }});
}

// [155] C-S-IMPLICIT-JSON-KEY 
Result c_s_implicit_json_key(Input inp, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_flow_json_node(inp, 0, c); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return s_separate_in_line(inp); }); }});
}

// [156] NS-FLOW-YAML-CONTENT 
Result ns_flow_yaml_content(Input inp, int n, Ctx c) {
    return ns_plain(inp, n, c);
}

// [157] C-FLOW-JSON-CONTENT 
Result c_flow_json_content(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_flow_sequence(inp, n, c); },
        [&](Input inp)->Result{ return c_flow_mapping(inp, n, c); },
        [&](Input inp)->Result{ return c_single_quoted(inp, n, c); },
        [&](Input inp)->Result{ return c_double_quoted(inp, n, c); }});
}

// [158] NS-FLOW-CONTENT 
Result ns_flow_content(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return ns_flow_yaml_content(inp, n, c); },
        [&](Input inp)->Result{ return c_flow_json_content(inp, n, c); }});
}

// [159] NS-FLOW-YAML-NODE 
Result ns_flow_yaml_node(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_ns_alias_node(inp); },
        [&](Input inp)->Result{ return ns_flow_yaml_content(inp, n, c); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return c_ns_properties(inp, n, c); },
            [&](Input inp)->Result{ return alt(inp, {
                [&](Input inp)->Result{ return seq(inp, {
                    [&](Input inp)->Result{ return s_separate(inp, n, c); },
                    [&](Input inp)->Result{ return ns_flow_yaml_content(inp, n, c); }}); },
                [&](Input inp)->Result{ return e_scalar(inp); }}); }}); }});
}

// [160] C-FLOW-JSON-NODE 
Result c_flow_json_node(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return c_ns_properties(inp, n, c); },
            [&](Input inp)->Result{ return s_separate(inp, n, c); }}); }); },
        [&](Input inp)->Result{ return c_flow_json_content(inp, n, c); }});
}

// [161] NS-FLOW-NODE 
Result ns_flow_node(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_ns_alias_node(inp); },
        [&](Input inp)->Result{ return ns_flow_content(inp, n, c); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return c_ns_properties(inp, n, c); },
            [&](Input inp)->Result{ return alt(inp, {
                [&](Input inp)->Result{ return seq(inp, {
                    [&](Input inp)->Result{ return s_separate(inp, n, c); },
                    [&](Input inp)->Result{ return ns_flow_content(inp, n, c); }}); },
                [&](Input inp)->Result{ return e_scalar(inp); }}); }}); }});
}

// [162] C-B-BLOCK-HEADER 
Result c_b_block_header(Input inp, int n) {
    return alt(inp, {
        [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_int(inp, [&](Input inp)->Result{ return ns_dec_digit(inp); }); },
            [&](Input inp)->Result{ return detect_indent(inp, n); }}); if(r.fail) return r; int m=r.tag_int; return [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 45); }, "STRIP"); },
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 43); }, "KEEP"); },
            [&](Input inp)->Result{ return val(inp, "CLIP"); }}); if(r.fail) return r; Ctx t=r.tag; return [&](Input inp)->Result{ return s_b_comment(inp); }(r.rest); }(); }(r.rest); }(); },
        [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 45); }, "STRIP"); },
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 43); }, "KEEP"); },
            [&](Input inp)->Result{ return val(inp, "CLIP"); }}); if(r.fail) return r; Ctx t=r.tag; return [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_int(inp, [&](Input inp)->Result{ return ns_dec_digit(inp); }); },
            [&](Input inp)->Result{ return detect_indent(inp, n); }}); if(r.fail) return r; int m=r.tag_int; return [&](Input inp)->Result{ return s_b_comment(inp); }(r.rest); }(); }(r.rest); }(); }});
}

// [163] C-INDENTATION-INDICATOR 
Result c_indentation_indicator(Input inp, int n) {
    return alt(inp, {
        [&](Input inp)->Result{ return ns_dec_digit(inp); },
        [&](Input inp)->Result{ return ok(inp); }});
}

// [164] C-CHOMPING-INDICATOR 
Result c_chomping_indicator(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 45); },
        [&](Input inp)->Result{ return match_cp(inp, 43); },
        [&](Input inp)->Result{ return ok(inp); }});
}

// [165] B-CHOMPED-LAST 
Result b_chomped_last(Input inp, Ctx t) {
    return [&]()->Result{ if(ctx_eq(t,"STRIP")) return b_non_content(inp); if(ctx_eq(t,"CLIP")) return b_as_line_feed(inp); if(ctx_eq(t,"KEEP")) return b_as_line_feed(inp); return fail(inp, "no case"); }();
}

// [166] L-CHOMPED-EMPTY 
Result l_chomped_empty(Input inp, int n, Ctx t) {
    return [&]()->Result{ if(ctx_eq(t,"STRIP")) return l_strip_empty(inp, n); if(ctx_eq(t,"CLIP")) return l_strip_empty(inp, n); if(ctx_eq(t,"KEEP")) return l_keep_empty(inp, n); return fail(inp, "no case"); }();
}

// [167] L-STRIP-EMPTY 
Result l_strip_empty(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_indent_le(inp, n); },
            [&](Input inp)->Result{ return b_non_content(inp); }}); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return l_trail_comments(inp, n); }); }});
}

// [168] L-KEEP-EMPTY 
Result l_keep_empty(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_empty(inp, n, "BLOCK-IN"); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return l_trail_comments(inp, n); }); }});
}

// [169] L-TRAIL-COMMENTS 
Result l_trail_comments(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_indent_lt(inp, n); },
        [&](Input inp)->Result{ return c_nb_comment_text(inp); },
        [&](Input inp)->Result{ return b_comment(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_comment(inp); }); }});
}

// [170] C-L+LITERAL 
Result c_lliteral(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 124); },
        [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_int(inp, [&](Input inp)->Result{ return ns_dec_digit(inp); }); },
            [&](Input inp)->Result{ return detect_indent(inp, n); }}); if(r.fail) return r; int m=r.tag_int; return [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 45); }, "STRIP"); },
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 43); }, "KEEP"); },
            [&](Input inp)->Result{ return val(inp, "CLIP"); }}); if(r.fail) return r; Ctx t=r.tag; return [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_b_comment(inp); },
            [&](Input inp)->Result{ return l_literal_content(inp, (n + m), t); }}); }(r.rest); }(); }(r.rest); }(); }});
}

// [171] L-NB-LITERAL-TEXT 
Result l_nb_literal_text(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_empty(inp, n, "BLOCK-IN"); }); },
        [&](Input inp)->Result{ return s_indent(inp, n); },
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return nb_char(inp); }); }});
}

// [172] B-NB-LITERAL-NEXT 
Result b_nb_literal_next(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return b_as_line_feed(inp); },
        [&](Input inp)->Result{ return l_nb_literal_text(inp, n); }});
}

// [173] L-LITERAL-CONTENT 
Result l_literal_content(Input inp, int n, Ctx t) {
    return scalar(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return l_nb_literal_text(inp, n); },
            [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return b_nb_literal_next(inp, n); }); },
            [&](Input inp)->Result{ return b_chomped_last(inp, t); }}); }); },
        [&](Input inp)->Result{ return l_chomped_empty(inp, n, t); }}); });
}

// [174] C-L+FOLDED 
Result c_lfolded(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 62); },
        [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_int(inp, [&](Input inp)->Result{ return ns_dec_digit(inp); }); },
            [&](Input inp)->Result{ return detect_indent(inp, n); }}); if(r.fail) return r; int m=r.tag_int; return [&](Input inp)->Result{ return [&]()->Result{ auto r=alt(inp, {
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 45); }, "STRIP"); },
            [&](Input inp)->Result{ return parse_sym(inp, [&](Input inp)->Result{ return match_cp(inp, 43); }, "KEEP"); },
            [&](Input inp)->Result{ return val(inp, "CLIP"); }}); if(r.fail) return r; Ctx t=r.tag; return [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_b_comment(inp); },
            [&](Input inp)->Result{ return l_folded_content(inp, (n + m), t); }}); }(r.rest); }(); }(r.rest); }(); }});
}

// [175] S-NB-FOLDED-TEXT 
Result s_nb_folded_text(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_indent(inp, n); },
        [&](Input inp)->Result{ return ns_char(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return nb_char(inp); }); }});
}

// [176] L-NB-FOLDED-LINES 
Result l_nb_folded_lines(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_nb_folded_text(inp, n); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return b_l_folded(inp, n, "BLOCK-IN"); },
            [&](Input inp)->Result{ return s_nb_folded_text(inp, n); }}); }); }});
}

// [177] S-NB-SPACED-TEXT 
Result s_nb_spaced_text(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_indent(inp, n); },
        [&](Input inp)->Result{ return s_white(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return nb_char(inp); }); }});
}

// [178] B-L-SPACED 
Result b_l_spaced(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return b_as_line_feed(inp); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_empty(inp, n, "BLOCK-IN"); }); }});
}

// [179] L-NB-SPACED-LINES 
Result l_nb_spaced_lines(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_nb_spaced_text(inp, n); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return b_l_spaced(inp, n); },
            [&](Input inp)->Result{ return s_nb_spaced_text(inp, n); }}); }); }});
}

// [180] L-NB-SAME-LINES 
Result l_nb_same_lines(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_empty(inp, n, "BLOCK-IN"); }); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return l_nb_folded_lines(inp, n); },
            [&](Input inp)->Result{ return l_nb_spaced_lines(inp, n); }}); }});
}

// [181] L-NB-DIFF-LINES 
Result l_nb_diff_lines(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return l_nb_same_lines(inp, n); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return b_as_line_feed(inp); },
            [&](Input inp)->Result{ return l_nb_same_lines(inp, n); }}); }); }});
}

// [182] L-FOLDED-CONTENT 
Result l_folded_content(Input inp, int n, Ctx t) {
    return scalar(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return l_nb_diff_lines(inp, n); },
            [&](Input inp)->Result{ return b_chomped_last(inp, t); }}); }); },
        [&](Input inp)->Result{ return l_chomped_empty(inp, n, t); }}); });
}

// [183] L+BLOCK-SEQUENCE 
Result lblock_sequence(Input inp, int n) {
    return build(inp, "SEQUENCE", [&](Input inp)->Result{ return [&]()->Result{ auto r=detect_indent(inp, n); if(r.fail) return r; int m=r.tag_int; return [&](Input inp)->Result{ return collect(inp, [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return s_indent(inp, (n + m)); },
        [&](Input inp)->Result{ return c_l_block_seq_entry(inp, (n + m)); }}); }); }); }(r.rest); }(); });
}

// [184] C-L-BLOCK-SEQ-ENTRY 
Result c_l_block_seq_entry(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 45); },
        [&](Input inp)->Result{ return neg(inp, [&](Input inp)->Result{ return ns_char(inp); }); },
        [&](Input inp)->Result{ return s_lblock_indented(inp, n, "BLOCK-IN"); }});
}

// [185] S-L+BLOCK-INDENTED 
Result s_lblock_indented(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return [&]()->Result{ auto r=detect_indent(inp, 0); if(r.fail) return r; int m=r.tag_int; return [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_indent(inp, m); },
            [&](Input inp)->Result{ return alt(inp, {
                [&](Input inp)->Result{ return ns_l_compact_sequence(inp, (n + 1 + m)); },
                [&](Input inp)->Result{ return ns_l_compact_mapping(inp, (n + 1 + m)); }}); }}); }(r.rest); }(); },
        [&](Input inp)->Result{ return s_lblock_node(inp, n, c); },
        [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return e_node(inp); },
            [&](Input inp)->Result{ return s_l_comments(inp); }}); }});
}

// [186] NS-L-COMPACT-SEQUENCE 
Result ns_l_compact_sequence(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_l_block_seq_entry(inp, n); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_indent(inp, n); },
            [&](Input inp)->Result{ return c_l_block_seq_entry(inp, n); }}); }); }});
}

// [187] L+BLOCK-MAPPING 
Result lblock_mapping(Input inp, int n) {
    return build(inp, "MAPPING", [&](Input inp)->Result{ return [&]()->Result{ auto r=detect_indent(inp, n); if(r.fail) return r; int m=r.tag_int; return [&](Input inp)->Result{ return collect(inp, [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return s_indent(inp, (n + m)); },
        [&](Input inp)->Result{ return ns_l_block_map_entry(inp, (n + m)); }}); }); }); }(r.rest); }(); });
}

// [188] NS-L-BLOCK-MAP-ENTRY 
Result ns_l_block_map_entry(Input inp, int n) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_l_block_map_explicit_entry(inp, n); },
        [&](Input inp)->Result{ return ns_l_block_map_implicit_entry(inp, n); }});
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
Result c_l_block_map_explicit_entry(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_l_block_map_explicit_key(inp, n); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return l_block_map_explicit_value(inp, n); },
            [&](Input inp)->Result{ return e_node(inp); }}); }});
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
Result c_l_block_map_explicit_key(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 63); },
        [&](Input inp)->Result{ return s_lblock_indented(inp, n, "BLOCK-OUT"); }});
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
Result l_block_map_explicit_value(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_indent(inp, n); },
        [&](Input inp)->Result{ return match_cp(inp, 58); },
        [&](Input inp)->Result{ return s_lblock_indented(inp, n, "BLOCK-OUT"); }});
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
Result ns_l_block_map_implicit_entry(Input inp, int n) {
    return build(inp, "PAIR", [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return scalar(inp, [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return ns_s_block_map_implicit_key(inp); },
            [&](Input inp)->Result{ return e_node(inp); }}); }); },
        [&](Input inp)->Result{ return c_l_block_map_implicit_value(inp, n); }}); });
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
Result ns_s_block_map_implicit_key(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return c_s_implicit_json_key(inp, "BLOCK-KEY"); },
        [&](Input inp)->Result{ return ns_s_implicit_yaml_key(inp, "BLOCK-KEY"); }});
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
Result c_l_block_map_implicit_value(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return match_cp(inp, 58); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return s_lblock_node(inp, n, "BLOCK-OUT"); },
            [&](Input inp)->Result{ return scalar(inp, [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return e_node(inp); },
                [&](Input inp)->Result{ return s_l_comments(inp); }}); }); }}); }});
}

// [195] NS-L-COMPACT-MAPPING 
Result ns_l_compact_mapping(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return ns_l_block_map_entry(inp, n); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_indent(inp, n); },
            [&](Input inp)->Result{ return ns_l_block_map_entry(inp, n); }}); }); }});
}

// [196] S-L+BLOCK-NODE 
Result s_lblock_node(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return s_lblock_in_block(inp, n, c); },
        [&](Input inp)->Result{ return s_lflow_in_block(inp, n); }});
}

// [197] S-L+FLOW-IN-BLOCK 
Result s_lflow_in_block(Input inp, int n) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_separate(inp, (n + 1), "FLOW-OUT"); },
        [&](Input inp)->Result{ return ns_flow_node(inp, (n + 1), "FLOW-OUT"); },
        [&](Input inp)->Result{ return s_l_comments(inp); }});
}

// [198] S-L+BLOCK-IN-BLOCK 
Result s_lblock_in_block(Input inp, int n, Ctx c) {
    return alt(inp, {
        [&](Input inp)->Result{ return s_lblock_scalar(inp, n, c); },
        [&](Input inp)->Result{ return s_lblock_collection(inp, n, c); }});
}

// [199] S-L+BLOCK-SCALAR 
Result s_lblock_scalar(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return s_separate(inp, (n + 1), c); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return c_ns_properties(inp, (n + 1), c); },
            [&](Input inp)->Result{ return s_separate(inp, (n + 1), c); }}); }); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return c_lliteral(inp, n); },
            [&](Input inp)->Result{ return c_lfolded(inp, n); }}); }});
}

// [200] S-L+BLOCK-COLLECTION 
Result s_lblock_collection(Input inp, int n, Ctx c) {
    return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return seq(inp, {
            [&](Input inp)->Result{ return s_separate(inp, (n + 1), c); },
            [&](Input inp)->Result{ return c_ns_properties(inp, (n + 1), c); }}); }); },
        [&](Input inp)->Result{ return s_l_comments(inp); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return lblock_sequence(inp, seq_spaces(n, c)); },
            [&](Input inp)->Result{ return lblock_mapping(inp, n); }}); }});
}

// [202] L-DOCUMENT-PREFIX 
Result l_document_prefix(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return c_byte_order_mark(inp); }); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_comment(inp); }); }});
}

// [203] C-DIRECTIVES-END 
Result c_directives_end(Input inp) {
    return match_str(inp, "---");
}

// [204] C-DOCUMENT-END 
Result c_document_end(Input inp) {
    return match_str(inp, "...");
}

// [205] L-DOCUMENT-SUFFIX 
Result l_document_suffix(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return c_document_end(inp); },
        [&](Input inp)->Result{ return s_l_comments(inp); }});
}

// [206] C-FORBIDDEN 
Result c_forbidden(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return sol(inp); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return c_directives_end(inp); },
            [&](Input inp)->Result{ return c_document_end(inp); }}); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return b_char(inp); },
            [&](Input inp)->Result{ return s_white(inp); },
            [&](Input inp)->Result{ return eof_ok(inp); }}); }});
}

// [207] L-BARE-DOCUMENT 
Result l_bare_document(Input inp) {
    return build(inp, "DOC", [&](Input inp)->Result{ return s_lblock_node(inp, -1, "BLOCK-IN"); });
}

// [208] L-EXPLICIT-DOCUMENT 
Result l_explicit_document(Input inp) {
    return build(inp, "DOC", [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return c_directives_end(inp); },
        [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return l_bare_document(inp); },
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return e_node(inp); },
                [&](Input inp)->Result{ return s_l_comments(inp); }}); }}); }}); });
}

// [209] L-DIRECTIVE-DOCUMENT 
Result l_directive_document(Input inp) {
    return seq(inp, {
        [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return l_directive(inp); }); },
        [&](Input inp)->Result{ return l_explicit_document(inp); }});
}

// [210] L-ANY-DOCUMENT 
Result l_any_document(Input inp) {
    return alt(inp, {
        [&](Input inp)->Result{ return l_directive_document(inp); },
        [&](Input inp)->Result{ return l_explicit_document(inp); },
        [&](Input inp)->Result{ return l_bare_document(inp); }});
}

// [211] L-YAML-STREAM 
Result l_yaml_stream(Input inp) {
    return build(inp, "STREAM", [&](Input inp)->Result{ return seq(inp, {
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_document_prefix(inp); }); },
        [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return l_any_document(inp); }); },
        [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return alt(inp, {
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return plus_(inp, [&](Input inp)->Result{ return l_document_suffix(inp); }); },
                [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_document_prefix(inp); }); },
                [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return l_any_document(inp); }); }}); },
            [&](Input inp)->Result{ return seq(inp, {
                [&](Input inp)->Result{ return star(inp, [&](Input inp)->Result{ return l_document_prefix(inp); }); },
                [&](Input inp)->Result{ return opt(inp, [&](Input inp)->Result{ return l_explicit_document(inp); }); }}); }}); }); }}); });
}

// ── Public API ──

struct ParseResult { bool success; AST ast; int pos; std::string error; };

ParseResult parse(const std::string& text) {
    Input inp={&text,0,1,0};
    auto r=l_yaml_stream(inp);
    if(!r.fail) return {true, r.ast, r.rest.pos, ""};
    return {false, nullptr, r.rest.pos, r.err};
}

void print_ast(const AST& n, int d=0) {
    if(!n) return;
    for(int i=0;i<d;i++) std::cout<<"  ";
    if(n->type=="SCALAR") std::cout<<"SCALAR: \""<<n->text<<"\""<<std::endl;
    else { std::cout<<n->type<<std::endl; for(auto& c:n->children) print_ast(c,d+1); }
}

// ── Native Value Type ──

struct YamlValue;
using YamlMap = std::map<std::string, YamlValue>;
using YamlSeq = std::vector<YamlValue>;

struct YamlValue {
    enum Tag { Null, Bool, Int, Float, Str, Map, Seq } tag = Null;
    bool    b = false;
    int64_t i = 0;
    double  f = 0.0;
    std::string s;
    YamlMap m;
    YamlSeq v;

    static YamlValue null_val()         { return {}; }
    static YamlValue from_bool(bool x)  { YamlValue y; y.tag=Bool;  y.b=x; return y; }
    static YamlValue from_int(int64_t x){ YamlValue y; y.tag=Int;   y.i=x; return y; }
    static YamlValue from_float(double x){YamlValue y; y.tag=Float; y.f=x; return y; }
    static YamlValue from_str(const std::string& x) { YamlValue y; y.tag=Str; y.s=x; return y; }
    static YamlValue from_map(YamlMap x){ YamlValue y; y.tag=Map;   y.m=std::move(x); return y; }
    static YamlValue from_seq(YamlSeq x){ YamlValue y; y.tag=Seq;   y.v=std::move(x); return y; }

    // Subscript for maps
    YamlValue& operator[](const std::string& key) { return m[key]; }
    const YamlValue& operator[](const std::string& key) const {
        static YamlValue empty;
        auto it = m.find(key); return it != m.end() ? it->second : empty;
    }
    // Subscript for sequences
    YamlValue& operator[](size_t idx) { return v[idx]; }
    const YamlValue& operator[](size_t idx) const { return v[idx]; }

    // Convenience
    size_t size() const { return tag==Map ? m.size() : tag==Seq ? v.size() : 0; }
    bool empty() const { return tag==Null; }
    explicit operator bool() const { return tag!=Null; }
    std::string str() const { return tag==Str ? s : ""; }
};

// ── Schema Coercion (YAML Core Schema) ──

YamlValue coerce_scalar(const std::string& s) {
    // Null
    if (s=="null" || s=="Null" || s=="NULL" || s=="~" || s.empty())
        return YamlValue::null_val();
    // Bool
    if (s=="true"  || s=="True"  || s=="TRUE")  return YamlValue::from_bool(true);
    if (s=="false" || s=="False" || s=="FALSE") return YamlValue::from_bool(false);
    // Int (decimal, octal 0o, hex 0x)
    if (!s.empty() && (std::isdigit(s[0]) || s[0]=='+' || s[0]=='-')) {
        char* end = nullptr;
        if (s.size()>2 && s[0]=='0' && s[1]=='x') {
            int64_t v = std::strtoll(s.c_str(), &end, 16);
            if (end && *end=='\0') return YamlValue::from_int(v);
        } else if (s.size()>2 && s[0]=='0' && s[1]=='o') {
            int64_t v = std::strtoll(s.c_str()+2, &end, 8);
            if (end && *end=='\0') return YamlValue::from_int(v);
        } else if (s.find('.')==std::string::npos && s.find('e')==std::string::npos && s.find('E')==std::string::npos) {
            int64_t v = std::strtoll(s.c_str(), &end, 10);
            if (end && *end=='\0') return YamlValue::from_int(v);
        } else {
            double v = std::strtod(s.c_str(), &end);
            if (end && *end=='\0') return YamlValue::from_float(v);
        }
    }
    // .inf, .nan
    if (s==".inf" || s==".Inf" || s==".INF" || s=="+.inf") return YamlValue::from_float(1.0/0.0);
    if (s=="-.inf" || s=="-.Inf" || s=="-.INF") return YamlValue::from_float(-1.0/0.0);
    if (s==".nan" || s==".NaN" || s==".NAN") return YamlValue::from_float(0.0/0.0);
    // String (default)
    return YamlValue::from_str(s);
}

// ── AST → Native Conversion with Anchor Resolution ──

struct Converter {
    std::unordered_map<std::string, YamlValue> anchors;

    YamlValue convert(const AST& node) {
        if (!node) return YamlValue::null_val();
        const auto& t = node->type;

        if (t=="SCALAR") return coerce_scalar(node->text);

        if (t=="ANCHOR") {
            // First child is anchor name, rest is the value
            std::string name;
            YamlValue val = YamlValue::null_val();
            for (auto& c : node->children) {
                if (c->type=="SCALAR" && name.empty()) name = c->text;
                else val = convert(c);
            }
            if (!name.empty()) anchors[name] = val;
            return val;
        }

        if (t=="ALIAS") {
            for (auto& c : node->children) {
                if (c->type=="SCALAR") {
                    auto it = anchors.find(c->text);
                    if (it != anchors.end()) return it->second;
                }
            }
            return YamlValue::null_val();
        }

        if (t=="MAPPING") {
            YamlMap m;
            for (auto& c : node->children) {
                if (c->type=="PAIR" && c->children.size()>=2) {
                    auto key = convert(c->children[0]);
                    auto val = convert(c->children[1]);
                    // Merge key support (<<)
                    if (key.tag==YamlValue::Str && key.s=="<<" && val.tag==YamlValue::Map) {
                        for (auto& [mk,mv] : val.m) {
                            if (m.find(mk)==m.end()) m[mk] = mv;
                        }
                    } else {
                        m[key.str()] = val;
                    }
                }
            }
            return YamlValue::from_map(std::move(m));
        }

        if (t=="SEQUENCE") {
            YamlSeq seq;
            for (auto& c : node->children) seq.push_back(convert(c));
            return YamlValue::from_seq(std::move(seq));
        }

        if (t=="DOC") {
            if (node->children.size()==1) return convert(node->children[0]);
            YamlSeq docs;
            for (auto& c : node->children) docs.push_back(convert(c));
            return docs.size()==1 ? docs[0] : YamlValue::from_seq(std::move(docs));
        }

        if (t=="STREAM") {
            if (node->children.size()==1) return convert(node->children[0]);
            YamlSeq docs;
            for (auto& c : node->children) docs.push_back(convert(c));
            return docs.size()==1 ? docs[0] : YamlValue::from_seq(std::move(docs));
        }

        if (t=="PAIR") {
            // Bare pair outside mapping — return value
            if (node->children.size()>=2) return convert(node->children[1]);
        }

        // Fallback: recurse into children
        if (node->children.size()==1) return convert(node->children[0]);
        YamlSeq items;
        for (auto& c : node->children) items.push_back(convert(c));
        return YamlValue::from_seq(std::move(items));
    }
};

// ── Public API ──

YamlValue load(const std::string& text) {
    auto result = parse(text);
    if (!result.success) return YamlValue::null_val();
    Converter conv;
    return conv.convert(result.ast);
}

YamlValue load_file(const std::string& path) {
    std::ifstream f(path);
    if (!f) return YamlValue::null_val();
    std::ostringstream ss; ss << f.rdbuf();
    return load(ss.str());
}

} // namespace yaml

int main(int argc, char* argv[]) {
    std::string text;
    if(argc>1) { std::ifstream f(argv[1]); if(!f){std::cerr<<"Cannot open "<<argv[1]<<std::endl;return 1;}
        std::ostringstream ss; ss<<f.rdbuf(); text=ss.str(); }
    else { std::ostringstream ss; ss<<std::cin.rdbuf(); text=ss.str(); }
    auto result=yaml::parse(text);
    if(!result.success) { std::cerr<<"FAIL @"<<result.pos<<": "<<result.error<<std::endl; return 1; }
    std::cout<<"OK: "<<result.pos<<" chars"<<std::endl;
    yaml::print_ast(result.ast);
    // Also demonstrate native API
    auto val = yaml::load(text);
    std::cout<<"\n── Native API ──"<<std::endl;
    if (val.tag == yaml::YamlValue::Map) {
        for (auto& [k,v] : val.m) {
            std::cout << k << ": ";
            switch(v.tag) {
                case yaml::YamlValue::Null:  std::cout<<"null"; break;
                case yaml::YamlValue::Bool:  std::cout<<(v.b?"true":"false"); break;
                case yaml::YamlValue::Int:   std::cout<<v.i; break;
                case yaml::YamlValue::Float: std::cout<<v.f; break;
                case yaml::YamlValue::Str:   std::cout<<'"'<<v.s<<'"'; break;
                case yaml::YamlValue::Map:   std::cout<<"{map, "<<v.m.size()<<" keys}"; break;
                case yaml::YamlValue::Seq:   std::cout<<"[seq, "<<v.v.size()<<" items]"; break;
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
