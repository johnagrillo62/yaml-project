// ════════════════════════════════════════════════════════════════
// YAMLReader.m — YAML 1.2 parser, projected from yaml-grammar.scm
// ════════════════════════════════════════════════════════════════
// Generated. DO NOT EDIT — regenerate from the grammar.
//
// macOS:  clang -fobjc-arc -framework Foundation -o yaml_objc YAMLReader.m
// Linux:  clang -fblocks $(gnustep-config --objc-flags) -I<objc-include> \
//           -o yaml_objc YAMLReader.m $(gnustep-config --base-libs) -lBlocksRuntime
// ════════════════════════════════════════════════════════════════

#import <Foundation/Foundation.h>

// ── Input ──

typedef struct {
    __unsafe_unretained NSString *src;
    int pos;
    int line;
    int col;
} Input;

static Input mkInput(NSString *s) { return (Input){s, 0, 1, 0}; }
static BOOL atEof(Input i) { return i.pos >= (int)i.src.length; }
static int peekCp(Input i) {
    if (atEof(i)) return -1;
    return (int)[i.src characterAtIndex:i.pos];
}
static Input adv(Input i) {
    if (atEof(i)) return i;
    unichar c = [i.src characterAtIndex:i.pos];
    return (Input){i.src, i.pos+1, c=='\n' ? i.line+1 : i.line, c=='\n' ? 0 : i.col+1};
}

// ── AST ──

@interface YAMLNode : NSObject { @public
    NSString *type; NSString *text; NSMutableArray *children; BOOL isLeaf;
}
+ (instancetype)branch:(NSString *)t;
+ (instancetype)leaf:(NSString *)t;
@end

@implementation YAMLNode
+ (instancetype)branch:(NSString *)t {
    YAMLNode *n = [[YAMLNode alloc] init]; n->type = t; n->children = [NSMutableArray array]; return n;
}
+ (instancetype)leaf:(NSString *)t {
    YAMLNode *n = [[YAMLNode alloc] init]; n->type = @"SCALAR"; n->text = t; n->isLeaf = YES; return n;
}
@end

// ── Result ──

@interface PResult : NSObject { @public
    BOOL failed; NSString *val; Input rest; NSString *tag; int tagInt;
    YAMLNode *ast; NSMutableArray *astList; NSString *err;
} @end
@implementation PResult @end
typedef PResult * Res;

static Res peg_ok(Input i) { Res r=[PResult new]; r->val=@""; r->rest=i; r->tag=@""; return r; }
static Res peg_okv(Input i, NSString *v) { Res r=[PResult new]; r->val=v; r->rest=i; r->tag=@""; return r; }
static Res peg_fail(Input i, NSString *m) { Res r=[PResult new]; r->failed=YES; r->val=@""; r->rest=i; r->tag=@""; r->err=m; return r; }
static Res ok(Input i) { return peg_ok(i); }

// ── Context ──

static NSString *in_flow(NSString *c) {
    if ([c isEqualToString:@"FLOW-OUT"]||[c isEqualToString:@"FLOW-IN"]) return @"FLOW-IN";
    return @"FLOW-KEY";
}
static int seq_spaces(int n, NSString *c) { return [c isEqualToString:@"BLOCK-OUT"] ? n-1 : n; }
static BOOL ctx_eq(NSString *a, NSString *b) { return [a isEqualToString:b]; }

// ── Combinators ──

typedef Res (^PFn)(Input);

static Res match_cp(Input inp, int cp) {
    int c=peekCp(inp);
    if(c==cp){unichar u=(unichar)c; return peg_okv(adv(inp),[NSString stringWithCharacters:&u length:1]);}
    return peg_fail(inp,@"cp");
}
static Res match_range(Input inp, int lo, int hi) {
    int c=peekCp(inp);
    if(c>=lo&&c<=hi){unichar u=(unichar)c; return peg_okv(adv(inp),[NSString stringWithCharacters:&u length:1]);}
    return peg_fail(inp,@"rng");
}
static Res match_str(Input inp, const char *ct) {
    NSString *t=[NSString stringWithUTF8String:ct]; int n=(int)t.length;
    if(inp.pos+n>(int)inp.src.length) return peg_fail(inp,@"str");
    if(![[inp.src substringWithRange:NSMakeRange(inp.pos,n)] isEqualToString:t]) return peg_fail(inp,@"str");
    Input cur=inp; int i; for(i=0;i<n;i++) cur=adv(cur); return peg_okv(cur,t);
}
static void mergeAsts(NSMutableArray *dst, Res r) {
    if(r->ast)[dst addObject:r->ast];
    if(r->astList.count>0)[dst addObjectsFromArray:r->astList];
}
static Res peg_seq(Input inp, int cnt, PFn fns[]) {
    Input cur=inp; NSMutableString *acc=[NSMutableString string]; NSMutableArray *asts=[NSMutableArray array];
    int fi; for(fi=0;fi<cnt;fi++){
        Res r=fns[fi](cur); if(r->failed) return r;
        [acc appendString:r->val?:@""]; mergeAsts(asts,r); cur=r->rest;}
    Res res=peg_okv(cur,acc);
    if(asts.count==1) res->ast=[asts objectAtIndex:0]; else if(asts.count>1) res->astList=asts;
    return res;
}
static Res peg_alt(Input inp, int cnt, PFn fns[]) {
    int fi; for(fi=0;fi<cnt;fi++){
        Res r=fns[fi](inp); if(!r->failed) return r;}
    return peg_fail(inp,@"alt");
}
static Res star(Input inp, PFn f) {
    Input cur=inp; NSMutableString *acc=[NSMutableString string]; NSMutableArray *asts=[NSMutableArray array];
    for(;;){Res r=f(cur); if(r->failed||r->rest.pos<=cur.pos) break;
        [acc appendString:r->val?:@""]; mergeAsts(asts,r); cur=r->rest;}
    Res res=peg_okv(cur,acc); if(asts.count>0) res->astList=asts; return res;
}
static Res plus_(Input inp, PFn f) {
    Res first=f(inp); if(first->failed) return first;
    Res rest=star(first->rest,f);
    NSMutableString *v=[NSMutableString stringWithString:first->val?:@""];
    [v appendString:rest->val?:@""];
    Res res=peg_okv(rest->rest,v);
    NSMutableArray *asts=[NSMutableArray array]; mergeAsts(asts,first); mergeAsts(asts,rest);
    if(asts.count>0) res->astList=asts; return res;
}
static Res opt(Input inp, PFn f){Res r=f(inp); return r->failed?peg_ok(inp):r;}
static Res neg(Input inp, PFn f){Res r=f(inp); return r->failed?peg_ok(inp):peg_fail(inp,@"neg");}
static Res minus(Input inp, PFn fa, PFn fb){
    Res ra=fa(inp); if(ra->failed) return ra;
    Res rb=fb(inp); return(!rb->failed&&rb->rest.pos==ra->rest.pos)?peg_fail(inp,@"excl"):ra;
}
static Res rep(Input inp, int n, PFn f){
    Input cur=inp; NSMutableString *acc=[NSMutableString string];
    int i; for(i=0;i<n;i++){Res r=f(cur); if(r->failed) return r; [acc appendString:r->val?:@""]; cur=r->rest;}
    return peg_okv(cur,acc);
}
static Res ahead(Input inp, PFn f){Res r=f(inp); return r->failed?r:peg_ok(inp);}
static Res behind(Input inp, PFn f){
    if(inp.pos==0) return peg_fail(inp,@"bh");
    Input t=(Input){inp.src,inp.pos-1,inp.line,MAX(0,inp.col-1)};
    Res r=f(t); return r->failed?peg_fail(inp,@"bh"):peg_ok(inp);
}
static Res sol(Input inp){return inp.col==0?peg_ok(inp):peg_fail(inp,@"sol");}
static Res eof_ok(Input inp){return atEof(inp)?peg_ok(inp):peg_fail(inp,@"eof");}

// ── YAML extensions ──

static Res build(Input inp, const char *ct, PFn f){
    NSString *type=[NSString stringWithUTF8String:ct];
    Res r=f(inp); if(r->failed) return r;
    YAMLNode *node=[YAMLNode branch:type];
    if(r->ast)[node->children addObject:r->ast];
    if(r->astList.count>0)[node->children addObjectsFromArray:r->astList];
    r->ast=node; r->astList=nil; return r;
}
static Res scalar(Input inp, PFn f){
    Res r=f(inp); if(r->failed) return r; r->ast=[YAMLNode leaf:r->val]; return r;
}
static Res collect(Input inp, PFn f){return f(inp);}
static Res detect_indent(Input inp, int n){
    NSString *s=inp.src; int len=(int)s.length; int i=inp.pos;
    int sp=0; while(i+sp<len&&[s characterAtIndex:i+sp]==' ') sp++;
    if(i+sp<len&&[s characterAtIndex:i+sp]!='\n'){Res r=peg_ok(inp); r->tagInt=MAX(1,sp-n); return r;}
    int j=i; while(j<len&&[s characterAtIndex:j]!='\n') j++;
    while(j<len){
        if([s characterAtIndex:j]=='\n') j++; if(j>=len) break;
        sp=0; while(j+sp<len&&[s characterAtIndex:j+sp]==' ') sp++;
        int nx=j+sp; if(nx>=len||[s characterAtIndex:nx]=='\n'){j=nx;continue;}
        Res r=peg_ok(inp); r->tagInt=MAX(1,sp-n); return r;
    }
    Res r=peg_ok(inp); r->tagInt=1; return r;
}
static Res parse_int(Input inp, PFn f){
    Res r=f(inp); if(r->failed) return r;
    int v=0; int pi; for(pi=0;pi<(int)r->val.length;pi++){
        unichar c=[r->val characterAtIndex:pi]; if(c>='0'&&c<='9') v=v*10+(c-'0');}
    r->tagInt=v; return r;
}
static Res parse_sym(Input inp, PFn f, const char *cs){
    Res r=f(inp); if(r->failed) return r; r->tag=[NSString stringWithUTF8String:cs]; return r;
}
static Res val(Input inp, const char *cv){Res r=peg_ok(inp); r->tag=[NSString stringWithUTF8String:cv]; return r;}

Res c_printable(Input inp);
Res nb_json(Input inp);
Res c_byte_order_mark(Input inp);
Res c_sequence_entry(Input inp);
Res c_mapping_key(Input inp);
Res c_mapping_value(Input inp);
Res c_collect_entry(Input inp);
Res c_sequence_start(Input inp);
Res c_sequence_end(Input inp);
Res c_mapping_start(Input inp);
Res c_mapping_end(Input inp);
Res c_comment(Input inp);
Res c_anchor(Input inp);
Res c_alias(Input inp);
Res c_tag(Input inp);
Res c_literal(Input inp);
Res c_folded(Input inp);
Res c_single_quote(Input inp);
Res c_double_quote(Input inp);
Res c_directive(Input inp);
Res c_reserved(Input inp);
Res c_indicator(Input inp);
Res c_flow_indicator(Input inp);
Res b_line_feed(Input inp);
Res b_carriage_return(Input inp);
Res b_char(Input inp);
Res nb_char(Input inp);
Res b_break(Input inp);
Res b_as_line_feed(Input inp);
Res b_non_content(Input inp);
Res s_space(Input inp);
Res s_tab(Input inp);
Res s_white(Input inp);
Res ns_char(Input inp);
Res ns_dec_digit(Input inp);
Res ns_hex_digit(Input inp);
Res ns_ascii_letter(Input inp);
Res ns_word_char(Input inp);
Res ns_uri_char(Input inp);
Res ns_tag_char(Input inp);
Res c_escape(Input inp);
Res ns_esc_null(Input inp);
Res ns_esc_bell(Input inp);
Res ns_esc_backspace(Input inp);
Res ns_esc_horizontal_tab(Input inp);
Res ns_esc_line_feed(Input inp);
Res ns_esc_vertical_tab(Input inp);
Res ns_esc_form_feed(Input inp);
Res ns_esc_carriage_return(Input inp);
Res ns_esc_escape(Input inp);
Res ns_esc_space(Input inp);
Res ns_esc_double_quote(Input inp);
Res ns_esc_slash(Input inp);
Res ns_esc_backslash(Input inp);
Res ns_esc_next_line(Input inp);
Res ns_esc_non_breaking_space(Input inp);
Res ns_esc_line_separator(Input inp);
Res ns_esc_paragraph_separator(Input inp);
Res ns_esc_8_bit(Input inp);
Res ns_esc_16_bit(Input inp);
Res ns_esc_32_bit(Input inp);
Res c_ns_esc_char(Input inp);
Res s_indent(Input inp, int n);
Res s_indent_lt(Input inp, int n);
Res s_indent_le(Input inp, int n);
Res s_separate_in_line(Input inp);
Res s_line_prefix(Input inp, int n, NSString *c);
Res s_block_line_prefix(Input inp, int n);
Res s_flow_line_prefix(Input inp, int n);
Res l_empty(Input inp, int n, NSString *c);
Res b_l_trimmed(Input inp, int n, NSString *c);
Res b_as_space(Input inp);
Res b_l_folded(Input inp, int n, NSString *c);
Res s_flow_folded(Input inp, int n);
Res c_nb_comment_text(Input inp);
Res b_comment(Input inp);
Res s_b_comment(Input inp);
Res l_comment(Input inp);
Res s_l_comments(Input inp);
Res s_separate(Input inp, int n, NSString *c);
Res s_separate_lines(Input inp, int n);
Res l_directive(Input inp);
Res ns_reserved_directive(Input inp);
Res ns_directive_name(Input inp);
Res ns_directive_parameter(Input inp);
Res ns_yaml_directive(Input inp);
Res ns_yaml_version(Input inp);
Res ns_tag_directive(Input inp);
Res c_tag_handle(Input inp);
Res c_primary_tag_handle(Input inp);
Res c_secondary_tag_handle(Input inp);
Res c_named_tag_handle(Input inp);
Res ns_tag_prefix(Input inp);
Res c_ns_local_tag_prefix(Input inp);
Res ns_global_tag_prefix(Input inp);
Res c_ns_properties(Input inp, int n, NSString *c);
Res c_ns_tag_property(Input inp);
Res c_verbatim_tag(Input inp);
Res c_ns_shorthand_tag(Input inp);
Res c_non_specific_tag(Input inp);
Res c_ns_anchor_property(Input inp);
Res ns_anchor_char(Input inp);
Res ns_anchor_name(Input inp);
Res c_ns_alias_node(Input inp);
Res e_scalar(Input inp);
Res e_node(Input inp);
Res nb_double_char(Input inp);
Res ns_double_char(Input inp);
Res c_double_quoted(Input inp, int n, NSString *c);
Res nb_double_text(Input inp, int n, NSString *c);
Res nb_double_one_line(Input inp);
Res s_double_escaped(Input inp, int n);
Res s_double_break(Input inp, int n);
Res nb_ns_double_in_line(Input inp);
Res s_double_next_line(Input inp, int n);
Res nb_double_multi_line(Input inp, int n);
Res c_quoted_quote(Input inp);
Res nb_single_char(Input inp);
Res ns_single_char(Input inp);
Res c_single_quoted(Input inp, int n, NSString *c);
Res nb_single_text(Input inp, int n, NSString *c);
Res nb_single_one_line(Input inp);
Res ns_single_in_line(Input inp);
Res s_single_next_line(Input inp, int n);
Res nb_single_multi_line(Input inp, int n);
Res ns_plain_first(Input inp, NSString *c);
Res ns_plain_safe(Input inp, NSString *c);
Res ns_plain_safe_out(Input inp);
Res ns_plain_safe_in(Input inp);
Res ns_plain_char(Input inp, NSString *c);
Res ns_plain(Input inp, int n, NSString *c);
Res nb_ns_plain_in_line(Input inp, NSString *c);
Res ns_plain_one_line(Input inp, NSString *c);
Res s_ns_plain_next_line(Input inp, int n, NSString *c);
Res ns_plain_multi_line(Input inp, int n, NSString *c);
Res c_flow_sequence(Input inp, int n, NSString *c);
Res ns_s_flow_seq_entries(Input inp, int n, NSString *c);
Res ns_flow_seq_entry(Input inp, int n, NSString *c);
Res c_flow_mapping(Input inp, int n, NSString *c);
Res ns_s_flow_map_entries(Input inp, int n, NSString *c);
Res ns_flow_map_entry(Input inp, int n, NSString *c);
Res ns_flow_map_explicit_entry(Input inp, int n, NSString *c);
Res ns_flow_map_implicit_entry(Input inp, int n, NSString *c);
Res ns_flow_map_yaml_key_entry(Input inp, int n, NSString *c);
Res c_ns_flow_map_empty_key_entry(Input inp, int n, NSString *c);
Res c_ns_flow_map_separate_value(Input inp, int n, NSString *c);
Res c_ns_flow_map_json_key_entry(Input inp, int n, NSString *c);
Res c_ns_flow_map_adjacent_value(Input inp, int n, NSString *c);
Res ns_flow_pair(Input inp, int n, NSString *c);
Res ns_flow_pair_entry(Input inp, int n, NSString *c);
Res ns_flow_pair_yaml_key_entry(Input inp, int n, NSString *c);
Res c_ns_flow_pair_json_key_entry(Input inp, int n, NSString *c);
Res ns_s_implicit_yaml_key(Input inp, NSString *c);
Res c_s_implicit_json_key(Input inp, NSString *c);
Res ns_flow_yaml_content(Input inp, int n, NSString *c);
Res c_flow_json_content(Input inp, int n, NSString *c);
Res ns_flow_content(Input inp, int n, NSString *c);
Res ns_flow_yaml_node(Input inp, int n, NSString *c);
Res c_flow_json_node(Input inp, int n, NSString *c);
Res ns_flow_node(Input inp, int n, NSString *c);
Res c_b_block_header(Input inp, int n);
Res c_indentation_indicator(Input inp, int n);
Res c_chomping_indicator(Input inp);
Res b_chomped_last(Input inp, NSString *t);
Res l_chomped_empty(Input inp, int n, NSString *t);
Res l_strip_empty(Input inp, int n);
Res l_keep_empty(Input inp, int n);
Res l_trail_comments(Input inp, int n);
Res c_lliteral(Input inp, int n);
Res l_nb_literal_text(Input inp, int n);
Res b_nb_literal_next(Input inp, int n);
Res l_literal_content(Input inp, int n, NSString *t);
Res c_lfolded(Input inp, int n);
Res s_nb_folded_text(Input inp, int n);
Res l_nb_folded_lines(Input inp, int n);
Res s_nb_spaced_text(Input inp, int n);
Res b_l_spaced(Input inp, int n);
Res l_nb_spaced_lines(Input inp, int n);
Res l_nb_same_lines(Input inp, int n);
Res l_nb_diff_lines(Input inp, int n);
Res l_folded_content(Input inp, int n, NSString *t);
Res lblock_sequence(Input inp, int n);
Res c_l_block_seq_entry(Input inp, int n);
Res s_lblock_indented(Input inp, int n, NSString *c);
Res ns_l_compact_sequence(Input inp, int n);
Res lblock_mapping(Input inp, int n);
Res ns_l_block_map_entry(Input inp, int n);
Res c_l_block_map_explicit_entry(Input inp, int n);
Res c_l_block_map_explicit_key(Input inp, int n);
Res l_block_map_explicit_value(Input inp, int n);
Res ns_l_block_map_implicit_entry(Input inp, int n);
Res ns_s_block_map_implicit_key(Input inp);
Res c_l_block_map_implicit_value(Input inp, int n);
Res ns_l_compact_mapping(Input inp, int n);
Res s_lblock_node(Input inp, int n, NSString *c);
Res s_lflow_in_block(Input inp, int n);
Res s_lblock_in_block(Input inp, int n, NSString *c);
Res s_lblock_scalar(Input inp, int n, NSString *c);
Res s_lblock_collection(Input inp, int n, NSString *c);
Res l_document_prefix(Input inp);
Res c_directives_end(Input inp);
Res c_document_end(Input inp);
Res l_document_suffix(Input inp);
Res c_forbidden(Input inp);
Res l_bare_document(Input inp);
Res l_explicit_document(Input inp);
Res l_directive_document(Input inp);
Res l_any_document(Input inp);
Res l_yaml_stream(Input inp);

// ════════════════════════════════════════════════════════════════
// YAML 1.2 Grammar — 211 rules
// ════════════════════════════════════════════════════════════════

// [1] C-PRINTABLE
Res c_printable(Input inp) {
    return peg_alt(inp, 8, (PFn[]){^Res(Input inp){ return match_cp(inp, 0x9); }, ^Res(Input inp){ return match_cp(inp, 0x0A); }, ^Res(Input inp){ return match_cp(inp, 0x0D); }, ^Res(Input inp){ return match_range(inp, 0x20, 0x7E); }, ^Res(Input inp){ return match_cp(inp, 0x85); }, ^Res(Input inp){ return match_range(inp, 0xA0, 0xD7FF); }, ^Res(Input inp){ return match_range(inp, 0xE000, 0xFFFD); }, ^Res(Input inp){ return match_range(inp, 0x10000, 0x10FFFF); }});
}

// [2] NB-JSON
Res nb_json(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 0x9); }, ^Res(Input inp){ return match_range(inp, 0x20, 0x10FFFF); }});
}

// [3] C-BYTE-ORDER-MARK
Res c_byte_order_mark(Input inp) {
    return match_cp(inp, 0xFEFF);
}

// [4] C-SEQUENCE-ENTRY
Res c_sequence_entry(Input inp) {
    return match_cp(inp, 45);
}

// [5] C-MAPPING-KEY
Res c_mapping_key(Input inp) {
    return match_cp(inp, 63);
}

// [6] C-MAPPING-VALUE
Res c_mapping_value(Input inp) {
    return match_cp(inp, 58);
}

// [7] C-COLLECT-ENTRY
Res c_collect_entry(Input inp) {
    return match_cp(inp, 44);
}

// [8] C-SEQUENCE-START
Res c_sequence_start(Input inp) {
    return match_cp(inp, 91);
}

// [9] C-SEQUENCE-END
Res c_sequence_end(Input inp) {
    return match_cp(inp, 93);
}

// [10] C-MAPPING-START
Res c_mapping_start(Input inp) {
    return match_cp(inp, 123);
}

// [11] C-MAPPING-END
Res c_mapping_end(Input inp) {
    return match_cp(inp, 125);
}

// [12] C-COMMENT
Res c_comment(Input inp) {
    return match_cp(inp, 35);
}

// [13] C-ANCHOR
Res c_anchor(Input inp) {
    return match_cp(inp, 38);
}

// [14] C-ALIAS
Res c_alias(Input inp) {
    return match_cp(inp, 42);
}

// [15] C-TAG
Res c_tag(Input inp) {
    return match_cp(inp, 33);
}

// [16] C-LITERAL
Res c_literal(Input inp) {
    return match_cp(inp, 124);
}

// [17] C-FOLDED
Res c_folded(Input inp) {
    return match_cp(inp, 62);
}

// [18] C-SINGLE-QUOTE
Res c_single_quote(Input inp) {
    return match_cp(inp, 39);
}

// [19] C-DOUBLE-QUOTE
Res c_double_quote(Input inp) {
    return match_cp(inp, 34);
}

// [20] C-DIRECTIVE
Res c_directive(Input inp) {
    return match_cp(inp, 37);
}

// [21] C-RESERVED
Res c_reserved(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 64); }, ^Res(Input inp){ return match_cp(inp, 96); }});
}

// [22] C-INDICATOR
Res c_indicator(Input inp) {
    return peg_alt(inp, 18, (PFn[]){^Res(Input inp){ return c_sequence_entry(inp); }, ^Res(Input inp){ return c_mapping_key(inp); }, ^Res(Input inp){ return c_mapping_value(inp); }, ^Res(Input inp){ return c_collect_entry(inp); }, ^Res(Input inp){ return c_sequence_start(inp); }, ^Res(Input inp){ return c_sequence_end(inp); }, ^Res(Input inp){ return c_mapping_start(inp); }, ^Res(Input inp){ return c_mapping_end(inp); }, ^Res(Input inp){ return c_comment(inp); }, ^Res(Input inp){ return c_anchor(inp); }, ^Res(Input inp){ return c_alias(inp); }, ^Res(Input inp){ return c_tag(inp); }, ^Res(Input inp){ return c_literal(inp); }, ^Res(Input inp){ return c_folded(inp); }, ^Res(Input inp){ return c_single_quote(inp); }, ^Res(Input inp){ return c_double_quote(inp); }, ^Res(Input inp){ return c_directive(inp); }, ^Res(Input inp){ return c_reserved(inp); }});
}

// [23] C-FLOW-INDICATOR
Res c_flow_indicator(Input inp) {
    return peg_alt(inp, 5, (PFn[]){^Res(Input inp){ return c_collect_entry(inp); }, ^Res(Input inp){ return c_sequence_start(inp); }, ^Res(Input inp){ return c_sequence_end(inp); }, ^Res(Input inp){ return c_mapping_start(inp); }, ^Res(Input inp){ return c_mapping_end(inp); }});
}

// [24] B-LINE-FEED
Res b_line_feed(Input inp) {
    return match_cp(inp, 0x0A);
}

// [25] B-CARRIAGE-RETURN
Res b_carriage_return(Input inp) {
    return match_cp(inp, 0x0D);
}

// [26] B-CHAR
Res b_char(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return b_line_feed(inp); }, ^Res(Input inp){ return b_carriage_return(inp); }});
}

// [27] NB-CHAR
Res nb_char(Input inp) {
    return minus(inp, ^Res(Input inp){ return c_printable(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return b_char(inp); }, ^Res(Input inp){ return c_byte_order_mark(inp); }}); });
}

// [28] B-BREAK
Res b_break(Input inp) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return b_carriage_return(inp); }, ^Res(Input inp){ return b_line_feed(inp); }}); }, ^Res(Input inp){ return b_carriage_return(inp); }, ^Res(Input inp){ return b_line_feed(inp); }});
}

// [29] B-AS-LINE-FEED
Res b_as_line_feed(Input inp) {
    return b_break(inp);
}

// [30] B-NON-CONTENT
Res b_non_content(Input inp) {
    return b_break(inp);
}

// [31] S-SPACE
Res s_space(Input inp) {
    return match_cp(inp, 0x20);
}

// [32] S-TAB
Res s_tab(Input inp) {
    return match_cp(inp, 0x9);
}

// [33] S-WHITE
Res s_white(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_space(inp); }, ^Res(Input inp){ return s_tab(inp); }});
}

// [34] NS-CHAR
Res ns_char(Input inp) {
    return minus(inp, ^Res(Input inp){ return nb_char(inp); }, ^Res(Input inp){ return s_white(inp); });
}

// [35] NS-DEC-DIGIT
Res ns_dec_digit(Input inp) {
    return match_range(inp, 0x30, 0x39);
}

// [36] NS-HEX-DIGIT
Res ns_hex_digit(Input inp) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return ns_dec_digit(inp); }, ^Res(Input inp){ return match_range(inp, 0x41, 0x46); }, ^Res(Input inp){ return match_range(inp, 0x61, 0x66); }});
}

// [37] NS-ASCII-LETTER
Res ns_ascii_letter(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return match_range(inp, 0x41, 0x5A); }, ^Res(Input inp){ return match_range(inp, 0x61, 0x7A); }});
}

// [38] NS-WORD-CHAR
Res ns_word_char(Input inp) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return ns_dec_digit(inp); }, ^Res(Input inp){ return ns_ascii_letter(inp); }, ^Res(Input inp){ return match_cp(inp, 45); }});
}

// [39] NS-URI-CHAR
Res ns_uri_char(Input inp) {
    return peg_alt(inp, 23, (PFn[]){^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 37); }, ^Res(Input inp){ return ns_hex_digit(inp); }, ^Res(Input inp){ return ns_hex_digit(inp); }}); }, ^Res(Input inp){ return ns_word_char(inp); }, ^Res(Input inp){ return match_cp(inp, 35); }, ^Res(Input inp){ return match_cp(inp, 59); }, ^Res(Input inp){ return match_cp(inp, 47); }, ^Res(Input inp){ return match_cp(inp, 63); }, ^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return match_cp(inp, 64); }, ^Res(Input inp){ return match_cp(inp, 38); }, ^Res(Input inp){ return match_cp(inp, 61); }, ^Res(Input inp){ return match_cp(inp, 43); }, ^Res(Input inp){ return match_cp(inp, 36); }, ^Res(Input inp){ return match_cp(inp, 44); }, ^Res(Input inp){ return match_cp(inp, 95); }, ^Res(Input inp){ return match_cp(inp, 46); }, ^Res(Input inp){ return match_cp(inp, 33); }, ^Res(Input inp){ return match_cp(inp, 126); }, ^Res(Input inp){ return match_cp(inp, 42); }, ^Res(Input inp){ return match_cp(inp, 39); }, ^Res(Input inp){ return match_cp(inp, 40); }, ^Res(Input inp){ return match_cp(inp, 41); }, ^Res(Input inp){ return match_cp(inp, 91); }, ^Res(Input inp){ return match_cp(inp, 93); }});
}

// [40] NS-TAG-CHAR
Res ns_tag_char(Input inp) {
    return minus(inp, ^Res(Input inp){ return ns_uri_char(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_tag(inp); }, ^Res(Input inp){ return c_flow_indicator(inp); }}); });
}

// [41] C-ESCAPE
Res c_escape(Input inp) {
    return match_cp(inp, 92);
}

// [42] NS-ESC-NULL
Res ns_esc_null(Input inp) {
    return match_cp(inp, 48);
}

// [43] NS-ESC-BELL
Res ns_esc_bell(Input inp) {
    return match_cp(inp, 97);
}

// [44] NS-ESC-BACKSPACE
Res ns_esc_backspace(Input inp) {
    return match_cp(inp, 98);
}

// [45] NS-ESC-HORIZONTAL-TAB
Res ns_esc_horizontal_tab(Input inp) {
    return match_cp(inp, 116);
}

// [46] NS-ESC-LINE-FEED
Res ns_esc_line_feed(Input inp) {
    return match_cp(inp, 110);
}

// [47] NS-ESC-VERTICAL-TAB
Res ns_esc_vertical_tab(Input inp) {
    return match_cp(inp, 118);
}

// [48] NS-ESC-FORM-FEED
Res ns_esc_form_feed(Input inp) {
    return match_cp(inp, 102);
}

// [49] NS-ESC-CARRIAGE-RETURN
Res ns_esc_carriage_return(Input inp) {
    return match_cp(inp, 114);
}

// [50] NS-ESC-ESCAPE
Res ns_esc_escape(Input inp) {
    return match_cp(inp, 101);
}

// [51] NS-ESC-SPACE
Res ns_esc_space(Input inp) {
    return match_cp(inp, 0x20);
}

// [52] NS-ESC-DOUBLE-QUOTE
Res ns_esc_double_quote(Input inp) {
    return match_cp(inp, 34);
}

// [53] NS-ESC-SLASH
Res ns_esc_slash(Input inp) {
    return match_cp(inp, 47);
}

// [54] NS-ESC-BACKSLASH
Res ns_esc_backslash(Input inp) {
    return match_cp(inp, 92);
}

// [55] NS-ESC-NEXT-LINE
Res ns_esc_next_line(Input inp) {
    return match_cp(inp, 78);
}

// [56] NS-ESC-NON-BREAKING-SPACE
Res ns_esc_non_breaking_space(Input inp) {
    return match_cp(inp, 95);
}

// [57] NS-ESC-LINE-SEPARATOR
Res ns_esc_line_separator(Input inp) {
    return match_cp(inp, 76);
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR
Res ns_esc_paragraph_separator(Input inp) {
    return match_cp(inp, 80);
}

// [59] NS-ESC-8-BIT
Res ns_esc_8_bit(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 120); }, ^Res(Input inp){ return rep(inp, 2, ^Res(Input inp){ return ns_hex_digit(inp); }); }});
}

// [60] NS-ESC-16-BIT
Res ns_esc_16_bit(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 117); }, ^Res(Input inp){ return rep(inp, 4, ^Res(Input inp){ return ns_hex_digit(inp); }); }});
}

// [61] NS-ESC-32-BIT
Res ns_esc_32_bit(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 85); }, ^Res(Input inp){ return rep(inp, 8, ^Res(Input inp){ return ns_hex_digit(inp); }); }});
}

// [62] C-NS-ESC-CHAR
Res c_ns_esc_char(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_escape(inp); }, ^Res(Input inp){ return peg_alt(inp, 20, (PFn[]){^Res(Input inp){ return ns_esc_null(inp); }, ^Res(Input inp){ return ns_esc_bell(inp); }, ^Res(Input inp){ return ns_esc_backspace(inp); }, ^Res(Input inp){ return ns_esc_horizontal_tab(inp); }, ^Res(Input inp){ return ns_esc_line_feed(inp); }, ^Res(Input inp){ return ns_esc_vertical_tab(inp); }, ^Res(Input inp){ return ns_esc_form_feed(inp); }, ^Res(Input inp){ return ns_esc_carriage_return(inp); }, ^Res(Input inp){ return ns_esc_escape(inp); }, ^Res(Input inp){ return ns_esc_space(inp); }, ^Res(Input inp){ return ns_esc_double_quote(inp); }, ^Res(Input inp){ return ns_esc_slash(inp); }, ^Res(Input inp){ return ns_esc_backslash(inp); }, ^Res(Input inp){ return ns_esc_next_line(inp); }, ^Res(Input inp){ return ns_esc_non_breaking_space(inp); }, ^Res(Input inp){ return ns_esc_line_separator(inp); }, ^Res(Input inp){ return ns_esc_paragraph_separator(inp); }, ^Res(Input inp){ return ns_esc_8_bit(inp); }, ^Res(Input inp){ return ns_esc_16_bit(inp); }, ^Res(Input inp){ return ns_esc_32_bit(inp); }}); }});
}

// [63] S-INDENT
Res s_indent(Input inp, int n) {
    return rep(inp, n, ^Res(Input inp){ return s_space(inp); });
}

// [64] S-INDENT-LT
Res s_indent_lt(Input inp, int n) {
    return star(inp, ^Res(Input inp){ return s_space(inp); });
}

// [65] S-INDENT-LE
Res s_indent_le(Input inp, int n) {
    return star(inp, ^Res(Input inp){ return s_space(inp); });
}

// [66] S-SEPARATE-IN-LINE
Res s_separate_in_line(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return s_white(inp); }); }, ^Res(Input inp){ return ok(inp); }});
}

// [67] S-LINE-PREFIX
Res s_line_prefix(Input inp, int n, NSString *c) {
    return (^Res{ if([c isEqualToString:@"BLOCK-IN"]) return s_block_line_prefix(inp, n); if([c isEqualToString:@"BLOCK-OUT"]) return s_block_line_prefix(inp, n); if([c isEqualToString:@"FLOW-IN"]) return s_flow_line_prefix(inp, n); if([c isEqualToString:@"FLOW-OUT"]) return s_flow_line_prefix(inp, n); return peg_fail(inp, @"no case"); })();
}

// [68] S-BLOCK-LINE-PREFIX
Res s_block_line_prefix(Input inp, int n) {
    return s_indent(inp, n);
}

// [69] S-FLOW-LINE-PREFIX
Res s_flow_line_prefix(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_indent(inp, n); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate_in_line(inp); }); }});
}

// [70] L-EMPTY
Res l_empty(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_line_prefix(inp, n, c); }, ^Res(Input inp){ return s_indent_lt(inp, n); }}); }, ^Res(Input inp){ return b_as_line_feed(inp); }});
}

// [71] B-L-TRIMMED
Res b_l_trimmed(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return b_non_content(inp); }, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return l_empty(inp, n, c); }); }});
}

// [72] B-AS-SPACE
Res b_as_space(Input inp) {
    return b_break(inp);
}

// [73] B-L-FOLDED
Res b_l_folded(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return b_l_trimmed(inp, n, c); }, ^Res(Input inp){ return b_as_space(inp); }});
}

// [74] S-FLOW-FOLDED
Res s_flow_folded(Input inp, int n) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate_in_line(inp); }); }, ^Res(Input inp){ return b_l_folded(inp, n, @"FLOW-IN"); }, ^Res(Input inp){ return s_flow_line_prefix(inp, n); }});
}

// [75] C-NB-COMMENT-TEXT
Res c_nb_comment_text(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_comment(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return nb_char(inp); }); }});
}

// [76] B-COMMENT
Res b_comment(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return b_non_content(inp); }, ^Res(Input inp){ return ok(inp); }});
}

// [77] S-B-COMMENT
Res s_b_comment(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate_in_line(inp); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return c_nb_comment_text(inp); }); }}); }); }, ^Res(Input inp){ return b_comment(inp); }});
}

// [78] L-COMMENT
Res l_comment(Input inp) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return s_separate_in_line(inp); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return c_nb_comment_text(inp); }); }, ^Res(Input inp){ return b_non_content(inp); }});
}

// [79] S-L-COMMENTS
Res s_l_comments(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_b_comment(inp); }, ^Res(Input inp){ return ok(inp); }}); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_comment(inp); }); }});
}

// [80] S-SEPARATE
Res s_separate(Input inp, int n, NSString *c) {
    return (^Res{ if([c isEqualToString:@"BLOCK-OUT"]) return s_separate_lines(inp, n); if([c isEqualToString:@"BLOCK-IN"]) return s_separate_lines(inp, n); if([c isEqualToString:@"FLOW-OUT"]) return s_separate_lines(inp, n); if([c isEqualToString:@"FLOW-IN"]) return s_separate_lines(inp, n); if([c isEqualToString:@"BLOCK-KEY"]) return s_separate_in_line(inp); if([c isEqualToString:@"FLOW-KEY"]) return s_separate_in_line(inp); return peg_fail(inp, @"no case"); })();
}

// [81] S-SEPARATE-LINES
Res s_separate_lines(Input inp, int n) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_l_comments(inp); }, ^Res(Input inp){ return s_flow_line_prefix(inp, n); }}); }, ^Res(Input inp){ return s_separate_in_line(inp); }});
}

// [82] L-DIRECTIVE
Res l_directive(Input inp) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return c_directive(inp); }, ^Res(Input inp){ return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return ns_yaml_directive(inp); }, ^Res(Input inp){ return ns_tag_directive(inp); }, ^Res(Input inp){ return ns_reserved_directive(inp); }}); }, ^Res(Input inp){ return s_l_comments(inp); }});
}

// [83] NS-RESERVED-DIRECTIVE
Res ns_reserved_directive(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_directive_name(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate_in_line(inp); }, ^Res(Input inp){ return ns_directive_parameter(inp); }}); }); }});
}

// [84] NS-DIRECTIVE-NAME
Res ns_directive_name(Input inp) {
    return plus_(inp, ^Res(Input inp){ return ns_char(inp); });
}

// [85] NS-DIRECTIVE-PARAMETER
Res ns_directive_parameter(Input inp) {
    return plus_(inp, ^Res(Input inp){ return ns_char(inp); });
}

// [86] NS-YAML-DIRECTIVE
Res ns_yaml_directive(Input inp) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_str(inp, "YAML"); }, ^Res(Input inp){ return s_separate_in_line(inp); }, ^Res(Input inp){ return ns_yaml_version(inp); }});
}

// [87] NS-YAML-VERSION
Res ns_yaml_version(Input inp) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return ns_dec_digit(inp); }); }, ^Res(Input inp){ return match_cp(inp, 46); }, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return ns_dec_digit(inp); }); }});
}

// [88] NS-TAG-DIRECTIVE
Res ns_tag_directive(Input inp) {
    return peg_seq(inp, 5, (PFn[]){^Res(Input inp){ return match_str(inp, "TAG"); }, ^Res(Input inp){ return s_separate_in_line(inp); }, ^Res(Input inp){ return c_tag_handle(inp); }, ^Res(Input inp){ return s_separate_in_line(inp); }, ^Res(Input inp){ return ns_tag_prefix(inp); }});
}

// [89] C-TAG-HANDLE
Res c_tag_handle(Input inp) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return c_named_tag_handle(inp); }, ^Res(Input inp){ return c_secondary_tag_handle(inp); }, ^Res(Input inp){ return c_primary_tag_handle(inp); }});
}

// [90] C-PRIMARY-TAG-HANDLE
Res c_primary_tag_handle(Input inp) {
    return match_cp(inp, 33);
}

// [91] C-SECONDARY-TAG-HANDLE
Res c_secondary_tag_handle(Input inp) {
    return match_str(inp, "!!");
}

// [92] C-NAMED-TAG-HANDLE
Res c_named_tag_handle(Input inp) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 33); }, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return ns_word_char(inp); }); }, ^Res(Input inp){ return match_cp(inp, 33); }});
}

// [93] NS-TAG-PREFIX
Res ns_tag_prefix(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_local_tag_prefix(inp); }, ^Res(Input inp){ return ns_global_tag_prefix(inp); }});
}

// [94] C-NS-LOCAL-TAG-PREFIX
Res c_ns_local_tag_prefix(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 33); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return ns_uri_char(inp); }); }});
}

// [95] NS-GLOBAL-TAG-PREFIX
Res ns_global_tag_prefix(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_tag_char(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return ns_uri_char(inp); }); }});
}

// [96] C-NS-PROPERTIES
Res c_ns_properties(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_tag_property(inp); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate(inp, n, c); }, ^Res(Input inp){ return c_ns_anchor_property(inp); }}); }); }}); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_anchor_property(inp); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate(inp, n, c); }, ^Res(Input inp){ return c_ns_tag_property(inp); }}); }); }}); }});
}

// [97] C-NS-TAG-PROPERTY
Res c_ns_tag_property(Input inp) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return c_verbatim_tag(inp); }, ^Res(Input inp){ return c_ns_shorthand_tag(inp); }, ^Res(Input inp){ return c_non_specific_tag(inp); }});
}

// [98] C-VERBATIM-TAG
Res c_verbatim_tag(Input inp) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_str(inp, "!<"); }, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return ns_uri_char(inp); }); }, ^Res(Input inp){ return match_cp(inp, 62); }});
}

// [99] C-NS-SHORTHAND-TAG
Res c_ns_shorthand_tag(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_tag_handle(inp); }, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return ns_tag_char(inp); }); }});
}

// [100] C-NON-SPECIFIC-TAG
Res c_non_specific_tag(Input inp) {
    return match_cp(inp, 33);
}

// [101] C-NS-ANCHOR-PROPERTY
Res c_ns_anchor_property(Input inp) {
    return build(inp, "ANCHOR", ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_anchor(inp); }, ^Res(Input inp){ return scalar(inp, ^Res(Input inp){ return ns_anchor_name(inp); }); }}); });
}

// [102] NS-ANCHOR-CHAR
Res ns_anchor_char(Input inp) {
    return minus(inp, ^Res(Input inp){ return ns_char(inp); }, ^Res(Input inp){ return c_flow_indicator(inp); });
}

// [103] NS-ANCHOR-NAME
Res ns_anchor_name(Input inp) {
    return plus_(inp, ^Res(Input inp){ return ns_anchor_char(inp); });
}

// [104] C-NS-ALIAS-NODE
Res c_ns_alias_node(Input inp) {
    return build(inp, "ALIAS", ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_alias(inp); }, ^Res(Input inp){ return scalar(inp, ^Res(Input inp){ return ns_anchor_name(inp); }); }}); });
}

// [105] E-SCALAR
Res e_scalar(Input inp) {
    return ok(inp);
}

// [106] E-NODE
Res e_node(Input inp) {
    return e_scalar(inp);
}

// [107] NB-DOUBLE-CHAR
Res nb_double_char(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_esc_char(inp); }, ^Res(Input inp){ return minus(inp, ^Res(Input inp){ return nb_json(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 92); }, ^Res(Input inp){ return match_cp(inp, 34); }}); }); }});
}

// [108] NS-DOUBLE-CHAR
Res ns_double_char(Input inp) {
    return minus(inp, ^Res(Input inp){ return nb_double_char(inp); }, ^Res(Input inp){ return s_white(inp); });
}

// [109] C-DOUBLE-QUOTED
Res c_double_quoted(Input inp, int n, NSString *c) {
    return scalar(inp, ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 34); }, ^Res(Input inp){ return nb_double_text(inp, n, c); }, ^Res(Input inp){ return match_cp(inp, 34); }}); });
}

// [110] NB-DOUBLE-TEXT
Res nb_double_text(Input inp, int n, NSString *c) {
    return (^Res{ if([c isEqualToString:@"FLOW-OUT"]) return nb_double_multi_line(inp, n); if([c isEqualToString:@"FLOW-IN"]) return nb_double_multi_line(inp, n); if([c isEqualToString:@"BLOCK-KEY"]) return nb_double_one_line(inp); if([c isEqualToString:@"FLOW-KEY"]) return nb_double_one_line(inp); return peg_fail(inp, @"no case"); })();
}

// [111] NB-DOUBLE-ONE-LINE
Res nb_double_one_line(Input inp) {
    return star(inp, ^Res(Input inp){ return nb_double_char(inp); });
}

// [112] S-DOUBLE-ESCAPED
Res s_double_escaped(Input inp, int n) {
    return peg_seq(inp, 5, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }, ^Res(Input inp){ return match_cp(inp, 92); }, ^Res(Input inp){ return b_non_content(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_empty(inp, n, @"FLOW-IN"); }); }, ^Res(Input inp){ return s_flow_line_prefix(inp, n); }});
}

// [113] S-DOUBLE-BREAK
Res s_double_break(Input inp, int n) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_double_escaped(inp, n); }, ^Res(Input inp){ return s_flow_folded(inp, n); }});
}

// [114] NB-NS-DOUBLE-IN-LINE
Res nb_ns_double_in_line(Input inp) {
    return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }, ^Res(Input inp){ return ns_double_char(inp); }}); });
}

// [115] S-DOUBLE-NEXT-LINE
Res s_double_next_line(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_double_break(inp, n); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return ns_double_char(inp); }, ^Res(Input inp){ return nb_ns_double_in_line(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_double_next_line(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }}); }}); }); }});
}

// [116] NB-DOUBLE-MULTI-LINE
Res nb_double_multi_line(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return nb_ns_double_in_line(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_double_next_line(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }}); }});
}

// [117] C-QUOTED-QUOTE
Res c_quoted_quote(Input inp) {
    return match_str(inp, "''");
}

// [118] NB-SINGLE-CHAR
Res nb_single_char(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_quoted_quote(inp); }, ^Res(Input inp){ return minus(inp, ^Res(Input inp){ return nb_json(inp); }, ^Res(Input inp){ return match_cp(inp, 39); }); }});
}

// [119] NS-SINGLE-CHAR
Res ns_single_char(Input inp) {
    return minus(inp, ^Res(Input inp){ return nb_single_char(inp); }, ^Res(Input inp){ return s_white(inp); });
}

// [120] C-SINGLE-QUOTED
Res c_single_quoted(Input inp, int n, NSString *c) {
    return scalar(inp, ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 39); }, ^Res(Input inp){ return nb_single_text(inp, n, c); }, ^Res(Input inp){ return match_cp(inp, 39); }}); });
}

// [121] NB-SINGLE-TEXT
Res nb_single_text(Input inp, int n, NSString *c) {
    return (^Res{ if([c isEqualToString:@"FLOW-OUT"]) return nb_single_multi_line(inp, n); if([c isEqualToString:@"FLOW-IN"]) return nb_single_multi_line(inp, n); if([c isEqualToString:@"BLOCK-KEY"]) return nb_single_one_line(inp); if([c isEqualToString:@"FLOW-KEY"]) return nb_single_one_line(inp); return peg_fail(inp, @"no case"); })();
}

// [122] NB-SINGLE-ONE-LINE
Res nb_single_one_line(Input inp) {
    return star(inp, ^Res(Input inp){ return nb_single_char(inp); });
}

// [123] NS-SINGLE-IN-LINE
Res ns_single_in_line(Input inp) {
    return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }, ^Res(Input inp){ return ns_single_char(inp); }}); });
}

// [124] S-SINGLE-NEXT-LINE
Res s_single_next_line(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_flow_folded(inp, n); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return ns_single_char(inp); }, ^Res(Input inp){ return ns_single_in_line(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_single_next_line(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }}); }}); }); }});
}

// [125] NB-SINGLE-MULTI-LINE
Res nb_single_multi_line(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_single_in_line(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_single_next_line(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }}); }});
}

// [126] NS-PLAIN-FIRST
Res ns_plain_first(Input inp, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return minus(inp, ^Res(Input inp){ return ns_char(inp); }, ^Res(Input inp){ return c_indicator(inp); }); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 63); }, ^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return match_cp(inp, 45); }}); }, ^Res(Input inp){ return ahead(inp, ^Res(Input inp){ return ns_plain_safe(inp, c); }); }}); }});
}

// [127] NS-PLAIN-SAFE
Res ns_plain_safe(Input inp, NSString *c) {
    return (^Res{ if([c isEqualToString:@"FLOW-OUT"]) return ns_plain_safe_out(inp); if([c isEqualToString:@"FLOW-IN"]) return ns_plain_safe_in(inp); if([c isEqualToString:@"BLOCK-KEY"]) return ns_plain_safe_out(inp); if([c isEqualToString:@"FLOW-KEY"]) return ns_plain_safe_in(inp); return peg_fail(inp, @"no case"); })();
}

// [128] NS-PLAIN-SAFE-OUT
Res ns_plain_safe_out(Input inp) {
    return ns_char(inp);
}

// [129] NS-PLAIN-SAFE-IN
Res ns_plain_safe_in(Input inp) {
    return minus(inp, ^Res(Input inp){ return ns_char(inp); }, ^Res(Input inp){ return c_flow_indicator(inp); });
}

// [130] NS-PLAIN-CHAR
Res ns_plain_char(Input inp, NSString *c) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return minus(inp, ^Res(Input inp){ return ns_plain_safe(inp, c); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return match_cp(inp, 35); }}); }); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return behind(inp, ^Res(Input inp){ return ns_char(inp); }); }, ^Res(Input inp){ return match_cp(inp, 35); }}); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return ahead(inp, ^Res(Input inp){ return ns_plain_safe(inp, c); }); }}); }});
}

// [131] NS-PLAIN
Res ns_plain(Input inp, int n, NSString *c) {
    return scalar(inp, ^Res(Input inp){ return (^Res{ if([c isEqualToString:@"FLOW-OUT"]) return ns_plain_multi_line(inp, n, c); if([c isEqualToString:@"FLOW-IN"]) return ns_plain_multi_line(inp, n, c); if([c isEqualToString:@"BLOCK-KEY"]) return ns_plain_one_line(inp, c); if([c isEqualToString:@"FLOW-KEY"]) return ns_plain_one_line(inp, c); return peg_fail(inp, @"no case"); })(); });
}

// [132] NB-NS-PLAIN-IN-LINE
Res nb_ns_plain_in_line(Input inp, NSString *c) {
    return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_white(inp); }); }, ^Res(Input inp){ return ns_plain_char(inp, c); }}); });
}

// [133] NS-PLAIN-ONE-LINE
Res ns_plain_one_line(Input inp, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_plain_first(inp, c); }, ^Res(Input inp){ return nb_ns_plain_in_line(inp, c); }});
}

// [134] S-NS-PLAIN-NEXT-LINE
Res s_ns_plain_next_line(Input inp, int n, NSString *c) {
    return peg_seq(inp, 4, (PFn[]){^Res(Input inp){ return s_flow_folded(inp, n); }, ^Res(Input inp){ return neg(inp, ^Res(Input inp){ return c_forbidden(inp); }); }, ^Res(Input inp){ return ns_plain_char(inp, c); }, ^Res(Input inp){ return nb_ns_plain_in_line(inp, c); }});
}

// [135] NS-PLAIN-MULTI-LINE
Res ns_plain_multi_line(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_plain_one_line(inp, c); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return s_ns_plain_next_line(inp, n, c); }); }});
}

// [137] C-FLOW-SEQUENCE
Res c_flow_sequence(Input inp, int n, NSString *c) {
    return build(inp, "SEQUENCE", ^Res(Input inp){ return peg_seq(inp, 4, (PFn[]){^Res(Input inp){ return match_cp(inp, 91); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return collect(inp, ^Res(Input inp){ return ns_s_flow_seq_entries(inp, n, in_flow(c)); }); }); }, ^Res(Input inp){ return match_cp(inp, 93); }}); });
}

// [138] NS-S-FLOW-SEQ-ENTRIES
Res ns_s_flow_seq_entries(Input inp, int n, NSString *c) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return ns_flow_seq_entry(inp, n, c); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 44); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return ns_s_flow_seq_entries(inp, n, c); }); }}); }); }});
}

// [139] NS-FLOW-SEQ-ENTRY
Res ns_flow_seq_entry(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return ns_flow_pair(inp, n, c); }, ^Res(Input inp){ return ns_flow_node(inp, n, c); }});
}

// [140] C-FLOW-MAPPING
Res c_flow_mapping(Input inp, int n, NSString *c) {
    return build(inp, "MAPPING", ^Res(Input inp){ return peg_seq(inp, 4, (PFn[]){^Res(Input inp){ return match_cp(inp, 123); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return collect(inp, ^Res(Input inp){ return ns_s_flow_map_entries(inp, n, in_flow(c)); }); }); }, ^Res(Input inp){ return match_cp(inp, 125); }}); });
}

// [141] NS-S-FLOW-MAP-ENTRIES
Res ns_s_flow_map_entries(Input inp, int n, NSString *c) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return ns_flow_map_entry(inp, n, c); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 44); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return ns_s_flow_map_entries(inp, n, c); }); }}); }); }});
}

// [142] NS-FLOW-MAP-ENTRY
Res ns_flow_map_entry(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 63); }, ^Res(Input inp){ return s_separate(inp, n, c); }, ^Res(Input inp){ return ns_flow_map_explicit_entry(inp, n, c); }}); }, ^Res(Input inp){ return ns_flow_map_implicit_entry(inp, n, c); }});
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY
Res ns_flow_map_explicit_entry(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return ns_flow_map_implicit_entry(inp, n, c); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return e_node(inp); }, ^Res(Input inp){ return e_node(inp); }}); }});
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY
Res ns_flow_map_implicit_entry(Input inp, int n, NSString *c) {
    return build(inp, "PAIR", ^Res(Input inp){ return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return ns_flow_map_yaml_key_entry(inp, n, c); }, ^Res(Input inp){ return c_ns_flow_map_empty_key_entry(inp, n, c); }, ^Res(Input inp){ return c_ns_flow_map_json_key_entry(inp, n, c); }}); });
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY
Res ns_flow_map_yaml_key_entry(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_flow_yaml_node(inp, n, c); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return c_ns_flow_map_separate_value(inp, n, c); }}); }, ^Res(Input inp){ return e_node(inp); }}); }});
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY
Res c_ns_flow_map_empty_key_entry(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return e_node(inp); }, ^Res(Input inp){ return c_ns_flow_map_separate_value(inp, n, c); }});
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE
Res c_ns_flow_map_separate_value(Input inp, int n, NSString *c) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return neg(inp, ^Res(Input inp){ return ns_plain_safe(inp, c); }); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate(inp, n, c); }, ^Res(Input inp){ return ns_flow_node(inp, n, c); }}); }, ^Res(Input inp){ return e_node(inp); }}); }});
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY
Res c_ns_flow_map_json_key_entry(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_flow_json_node(inp, n, c); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return c_ns_flow_map_adjacent_value(inp, n, c); }}); }, ^Res(Input inp){ return e_node(inp); }}); }});
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE
Res c_ns_flow_map_adjacent_value(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate(inp, n, c); }); }, ^Res(Input inp){ return ns_flow_node(inp, n, c); }}); }, ^Res(Input inp){ return e_node(inp); }}); }});
}

// [150] NS-FLOW-PAIR
Res ns_flow_pair(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 63); }, ^Res(Input inp){ return s_separate(inp, n, c); }, ^Res(Input inp){ return ns_flow_map_explicit_entry(inp, n, c); }}); }, ^Res(Input inp){ return ns_flow_pair_entry(inp, n, c); }});
}

// [151] NS-FLOW-PAIR-ENTRY
Res ns_flow_pair_entry(Input inp, int n, NSString *c) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return ns_flow_pair_yaml_key_entry(inp, n, c); }, ^Res(Input inp){ return c_ns_flow_map_empty_key_entry(inp, n, c); }, ^Res(Input inp){ return c_ns_flow_pair_json_key_entry(inp, n, c); }});
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY
Res ns_flow_pair_yaml_key_entry(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_s_implicit_yaml_key(inp, @"FLOW-KEY"); }, ^Res(Input inp){ return c_ns_flow_map_separate_value(inp, n, c); }});
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY
Res c_ns_flow_pair_json_key_entry(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_s_implicit_json_key(inp, @"FLOW-KEY"); }, ^Res(Input inp){ return c_ns_flow_map_adjacent_value(inp, n, c); }});
}

// [154] NS-S-IMPLICIT-YAML-KEY
Res ns_s_implicit_yaml_key(Input inp, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_flow_yaml_node(inp, 0, c); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate_in_line(inp); }); }});
}

// [155] C-S-IMPLICIT-JSON-KEY
Res c_s_implicit_json_key(Input inp, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_flow_json_node(inp, 0, c); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return s_separate_in_line(inp); }); }});
}

// [156] NS-FLOW-YAML-CONTENT
Res ns_flow_yaml_content(Input inp, int n, NSString *c) {
    return ns_plain(inp, n, c);
}

// [157] C-FLOW-JSON-CONTENT
Res c_flow_json_content(Input inp, int n, NSString *c) {
    return peg_alt(inp, 4, (PFn[]){^Res(Input inp){ return c_flow_sequence(inp, n, c); }, ^Res(Input inp){ return c_flow_mapping(inp, n, c); }, ^Res(Input inp){ return c_single_quoted(inp, n, c); }, ^Res(Input inp){ return c_double_quoted(inp, n, c); }});
}

// [158] NS-FLOW-CONTENT
Res ns_flow_content(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return ns_flow_yaml_content(inp, n, c); }, ^Res(Input inp){ return c_flow_json_content(inp, n, c); }});
}

// [159] NS-FLOW-YAML-NODE
Res ns_flow_yaml_node(Input inp, int n, NSString *c) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return c_ns_alias_node(inp); }, ^Res(Input inp){ return ns_flow_yaml_content(inp, n, c); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_properties(inp, n, c); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate(inp, n, c); }, ^Res(Input inp){ return ns_flow_yaml_content(inp, n, c); }}); }, ^Res(Input inp){ return e_scalar(inp); }}); }}); }});
}

// [160] C-FLOW-JSON-NODE
Res c_flow_json_node(Input inp, int n, NSString *c) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_properties(inp, n, c); }, ^Res(Input inp){ return s_separate(inp, n, c); }}); }); }, ^Res(Input inp){ return c_flow_json_content(inp, n, c); }});
}

// [161] NS-FLOW-NODE
Res ns_flow_node(Input inp, int n, NSString *c) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return c_ns_alias_node(inp); }, ^Res(Input inp){ return ns_flow_content(inp, n, c); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_properties(inp, n, c); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate(inp, n, c); }, ^Res(Input inp){ return ns_flow_content(inp, n, c); }}); }, ^Res(Input inp){ return e_scalar(inp); }}); }}); }});
}

// [162] C-B-BLOCK-HEADER
Res c_b_block_header(Input inp, int n) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return parse_int(inp, ^Res(Input inp){ return ns_dec_digit(inp); }); }, ^Res(Input inp){ return detect_indent(inp, n); }}); if(r->failed) return r; int m=r->tagInt; return (^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 45); }, "STRIP"); }, ^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 43); }, "KEEP"); }, ^Res(Input inp){ return val(inp, "CLIP"); }}); if(r->failed) return r; NSString *t=r->tag; return (^Res(Input inp){ return s_b_comment(inp); })(r->rest); })(); })(r->rest); })(); }, ^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 45); }, "STRIP"); }, ^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 43); }, "KEEP"); }, ^Res(Input inp){ return val(inp, "CLIP"); }}); if(r->failed) return r; NSString *t=r->tag; return (^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return parse_int(inp, ^Res(Input inp){ return ns_dec_digit(inp); }); }, ^Res(Input inp){ return detect_indent(inp, n); }}); if(r->failed) return r; int m=r->tagInt; return (^Res(Input inp){ return s_b_comment(inp); })(r->rest); })(); })(r->rest); })(); }});
}

// [163] C-INDENTATION-INDICATOR
Res c_indentation_indicator(Input inp, int n) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return ns_dec_digit(inp); }, ^Res(Input inp){ return ok(inp); }});
}

// [164] C-CHOMPING-INDICATOR
Res c_chomping_indicator(Input inp) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 45); }, ^Res(Input inp){ return match_cp(inp, 43); }, ^Res(Input inp){ return ok(inp); }});
}

// [165] B-CHOMPED-LAST
Res b_chomped_last(Input inp, NSString *t) {
    return (^Res{ if([t isEqualToString:@"STRIP"]) return b_non_content(inp); if([t isEqualToString:@"CLIP"]) return b_as_line_feed(inp); if([t isEqualToString:@"KEEP"]) return b_as_line_feed(inp); return peg_fail(inp, @"no case"); })();
}

// [166] L-CHOMPED-EMPTY
Res l_chomped_empty(Input inp, int n, NSString *t) {
    return (^Res{ if([t isEqualToString:@"STRIP"]) return l_strip_empty(inp, n); if([t isEqualToString:@"CLIP"]) return l_strip_empty(inp, n); if([t isEqualToString:@"KEEP"]) return l_keep_empty(inp, n); return peg_fail(inp, @"no case"); })();
}

// [167] L-STRIP-EMPTY
Res l_strip_empty(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_indent_le(inp, n); }, ^Res(Input inp){ return b_non_content(inp); }}); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return l_trail_comments(inp, n); }); }});
}

// [168] L-KEEP-EMPTY
Res l_keep_empty(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_empty(inp, n, @"BLOCK-IN"); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return l_trail_comments(inp, n); }); }});
}

// [169] L-TRAIL-COMMENTS
Res l_trail_comments(Input inp, int n) {
    return peg_seq(inp, 4, (PFn[]){^Res(Input inp){ return s_indent_lt(inp, n); }, ^Res(Input inp){ return c_nb_comment_text(inp); }, ^Res(Input inp){ return b_comment(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_comment(inp); }); }});
}

// [170] C-L+LITERAL
Res c_lliteral(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 124); }, ^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return parse_int(inp, ^Res(Input inp){ return ns_dec_digit(inp); }); }, ^Res(Input inp){ return detect_indent(inp, n); }}); if(r->failed) return r; int m=r->tagInt; return (^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 45); }, "STRIP"); }, ^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 43); }, "KEEP"); }, ^Res(Input inp){ return val(inp, "CLIP"); }}); if(r->failed) return r; NSString *t=r->tag; return (^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_b_comment(inp); }, ^Res(Input inp){ return l_literal_content(inp, (n + m), t); }}); })(r->rest); })(); })(r->rest); })(); }});
}

// [171] L-NB-LITERAL-TEXT
Res l_nb_literal_text(Input inp, int n) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_empty(inp, n, @"BLOCK-IN"); }); }, ^Res(Input inp){ return s_indent(inp, n); }, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return nb_char(inp); }); }});
}

// [172] B-NB-LITERAL-NEXT
Res b_nb_literal_next(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return b_as_line_feed(inp); }, ^Res(Input inp){ return l_nb_literal_text(inp, n); }});
}

// [173] L-LITERAL-CONTENT
Res l_literal_content(Input inp, int n, NSString *t) {
    return scalar(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return l_nb_literal_text(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return b_nb_literal_next(inp, n); }); }, ^Res(Input inp){ return b_chomped_last(inp, t); }}); }); }, ^Res(Input inp){ return l_chomped_empty(inp, n, t); }}); });
}

// [174] C-L+FOLDED
Res c_lfolded(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 62); }, ^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return parse_int(inp, ^Res(Input inp){ return ns_dec_digit(inp); }); }, ^Res(Input inp){ return detect_indent(inp, n); }}); if(r->failed) return r; int m=r->tagInt; return (^Res(Input inp){ return (^Res{ Res r=peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 45); }, "STRIP"); }, ^Res(Input inp){ return parse_sym(inp, ^Res(Input inp){ return match_cp(inp, 43); }, "KEEP"); }, ^Res(Input inp){ return val(inp, "CLIP"); }}); if(r->failed) return r; NSString *t=r->tag; return (^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_b_comment(inp); }, ^Res(Input inp){ return l_folded_content(inp, (n + m), t); }}); })(r->rest); })(); })(r->rest); })(); }});
}

// [175] S-NB-FOLDED-TEXT
Res s_nb_folded_text(Input inp, int n) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return s_indent(inp, n); }, ^Res(Input inp){ return ns_char(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return nb_char(inp); }); }});
}

// [176] L-NB-FOLDED-LINES
Res l_nb_folded_lines(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_nb_folded_text(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return b_l_folded(inp, n, @"BLOCK-IN"); }, ^Res(Input inp){ return s_nb_folded_text(inp, n); }}); }); }});
}

// [177] S-NB-SPACED-TEXT
Res s_nb_spaced_text(Input inp, int n) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return s_indent(inp, n); }, ^Res(Input inp){ return s_white(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return nb_char(inp); }); }});
}

// [178] B-L-SPACED
Res b_l_spaced(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return b_as_line_feed(inp); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_empty(inp, n, @"BLOCK-IN"); }); }});
}

// [179] L-NB-SPACED-LINES
Res l_nb_spaced_lines(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_nb_spaced_text(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return b_l_spaced(inp, n); }, ^Res(Input inp){ return s_nb_spaced_text(inp, n); }}); }); }});
}

// [180] L-NB-SAME-LINES
Res l_nb_same_lines(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_empty(inp, n, @"BLOCK-IN"); }); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return l_nb_folded_lines(inp, n); }, ^Res(Input inp){ return l_nb_spaced_lines(inp, n); }}); }});
}

// [181] L-NB-DIFF-LINES
Res l_nb_diff_lines(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return l_nb_same_lines(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return b_as_line_feed(inp); }, ^Res(Input inp){ return l_nb_same_lines(inp, n); }}); }); }});
}

// [182] L-FOLDED-CONTENT
Res l_folded_content(Input inp, int n, NSString *t) {
    return scalar(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return l_nb_diff_lines(inp, n); }, ^Res(Input inp){ return b_chomped_last(inp, t); }}); }); }, ^Res(Input inp){ return l_chomped_empty(inp, n, t); }}); });
}

// [183] L+BLOCK-SEQUENCE
Res lblock_sequence(Input inp, int n) {
    return build(inp, "SEQUENCE", ^Res(Input inp){ return (^Res{ Res r=detect_indent(inp, n); if(r->failed) return r; int m=r->tagInt; return (^Res(Input inp){ return collect(inp, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_indent(inp, (n + m)); }, ^Res(Input inp){ return c_l_block_seq_entry(inp, (n + m)); }}); }); }); })(r->rest); })(); });
}

// [184] C-L-BLOCK-SEQ-ENTRY
Res c_l_block_seq_entry(Input inp, int n) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return match_cp(inp, 45); }, ^Res(Input inp){ return neg(inp, ^Res(Input inp){ return ns_char(inp); }); }, ^Res(Input inp){ return s_lblock_indented(inp, n, @"BLOCK-IN"); }});
}

// [185] S-L+BLOCK-INDENTED
Res s_lblock_indented(Input inp, int n, NSString *c) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return (^Res{ Res r=detect_indent(inp, 0); if(r->failed) return r; int m=r->tagInt; return (^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_indent(inp, m); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return ns_l_compact_sequence(inp, (n + 1 + m)); }, ^Res(Input inp){ return ns_l_compact_mapping(inp, (n + 1 + m)); }}); }}); })(r->rest); })(); }, ^Res(Input inp){ return s_lblock_node(inp, n, c); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return e_node(inp); }, ^Res(Input inp){ return s_l_comments(inp); }}); }});
}

// [186] NS-L-COMPACT-SEQUENCE
Res ns_l_compact_sequence(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_l_block_seq_entry(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_indent(inp, n); }, ^Res(Input inp){ return c_l_block_seq_entry(inp, n); }}); }); }});
}

// [187] L+BLOCK-MAPPING
Res lblock_mapping(Input inp, int n) {
    return build(inp, "MAPPING", ^Res(Input inp){ return (^Res{ Res r=detect_indent(inp, n); if(r->failed) return r; int m=r->tagInt; return (^Res(Input inp){ return collect(inp, ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_indent(inp, (n + m)); }, ^Res(Input inp){ return ns_l_block_map_entry(inp, (n + m)); }}); }); }); })(r->rest); })(); });
}

// [188] NS-L-BLOCK-MAP-ENTRY
Res ns_l_block_map_entry(Input inp, int n) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_l_block_map_explicit_entry(inp, n); }, ^Res(Input inp){ return ns_l_block_map_implicit_entry(inp, n); }});
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY
Res c_l_block_map_explicit_entry(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_l_block_map_explicit_key(inp, n); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return l_block_map_explicit_value(inp, n); }, ^Res(Input inp){ return e_node(inp); }}); }});
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY
Res c_l_block_map_explicit_key(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 63); }, ^Res(Input inp){ return s_lblock_indented(inp, n, @"BLOCK-OUT"); }});
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE
Res l_block_map_explicit_value(Input inp, int n) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return s_indent(inp, n); }, ^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return s_lblock_indented(inp, n, @"BLOCK-OUT"); }});
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY
Res ns_l_block_map_implicit_entry(Input inp, int n) {
    return build(inp, "PAIR", ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return scalar(inp, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return ns_s_block_map_implicit_key(inp); }, ^Res(Input inp){ return e_node(inp); }}); }); }, ^Res(Input inp){ return c_l_block_map_implicit_value(inp, n); }}); });
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY
Res ns_s_block_map_implicit_key(Input inp) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_s_implicit_json_key(inp, @"BLOCK-KEY"); }, ^Res(Input inp){ return ns_s_implicit_yaml_key(inp, @"BLOCK-KEY"); }});
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE
Res c_l_block_map_implicit_value(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return match_cp(inp, 58); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_lblock_node(inp, n, @"BLOCK-OUT"); }, ^Res(Input inp){ return scalar(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return e_node(inp); }, ^Res(Input inp){ return s_l_comments(inp); }}); }); }}); }});
}

// [195] NS-L-COMPACT-MAPPING
Res ns_l_compact_mapping(Input inp, int n) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return ns_l_block_map_entry(inp, n); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_indent(inp, n); }, ^Res(Input inp){ return ns_l_block_map_entry(inp, n); }}); }); }});
}

// [196] S-L+BLOCK-NODE
Res s_lblock_node(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_lblock_in_block(inp, n, c); }, ^Res(Input inp){ return s_lflow_in_block(inp, n); }});
}

// [197] S-L+FLOW-IN-BLOCK
Res s_lflow_in_block(Input inp, int n) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return s_separate(inp, (n + 1), @"FLOW-OUT"); }, ^Res(Input inp){ return ns_flow_node(inp, (n + 1), @"FLOW-OUT"); }, ^Res(Input inp){ return s_l_comments(inp); }});
}

// [198] S-L+BLOCK-IN-BLOCK
Res s_lblock_in_block(Input inp, int n, NSString *c) {
    return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return s_lblock_scalar(inp, n, c); }, ^Res(Input inp){ return s_lblock_collection(inp, n, c); }});
}

// [199] S-L+BLOCK-SCALAR
Res s_lblock_scalar(Input inp, int n, NSString *c) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return s_separate(inp, (n + 1), c); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_ns_properties(inp, (n + 1), c); }, ^Res(Input inp){ return s_separate(inp, (n + 1), c); }}); }); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_lliteral(inp, n); }, ^Res(Input inp){ return c_lfolded(inp, n); }}); }});
}

// [200] S-L+BLOCK-COLLECTION
Res s_lblock_collection(Input inp, int n, NSString *c) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return s_separate(inp, (n + 1), c); }, ^Res(Input inp){ return c_ns_properties(inp, (n + 1), c); }}); }); }, ^Res(Input inp){ return s_l_comments(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return lblock_sequence(inp, seq_spaces(n, c)); }, ^Res(Input inp){ return lblock_mapping(inp, n); }}); }});
}

// [202] L-DOCUMENT-PREFIX
Res l_document_prefix(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return opt(inp, ^Res(Input inp){ return c_byte_order_mark(inp); }); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_comment(inp); }); }});
}

// [203] C-DIRECTIVES-END
Res c_directives_end(Input inp) {
    return match_str(inp, "---");
}

// [204] C-DOCUMENT-END
Res c_document_end(Input inp) {
    return match_str(inp, "...");
}

// [205] L-DOCUMENT-SUFFIX
Res l_document_suffix(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_document_end(inp); }, ^Res(Input inp){ return s_l_comments(inp); }});
}

// [206] C-FORBIDDEN
Res c_forbidden(Input inp) {
    return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return sol(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return c_directives_end(inp); }, ^Res(Input inp){ return c_document_end(inp); }}); }, ^Res(Input inp){ return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return b_char(inp); }, ^Res(Input inp){ return s_white(inp); }, ^Res(Input inp){ return eof_ok(inp); }}); }});
}

// [207] L-BARE-DOCUMENT
Res l_bare_document(Input inp) {
    return build(inp, "DOC", ^Res(Input inp){ return s_lblock_node(inp, -1, @"BLOCK-IN"); });
}

// [208] L-EXPLICIT-DOCUMENT
Res l_explicit_document(Input inp) {
    return build(inp, "DOC", ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return c_directives_end(inp); }, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return l_bare_document(inp); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return e_node(inp); }, ^Res(Input inp){ return s_l_comments(inp); }}); }}); }}); });
}

// [209] L-DIRECTIVE-DOCUMENT
Res l_directive_document(Input inp) {
    return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return l_directive(inp); }); }, ^Res(Input inp){ return l_explicit_document(inp); }});
}

// [210] L-ANY-DOCUMENT
Res l_any_document(Input inp) {
    return peg_alt(inp, 3, (PFn[]){^Res(Input inp){ return l_directive_document(inp); }, ^Res(Input inp){ return l_explicit_document(inp); }, ^Res(Input inp){ return l_bare_document(inp); }});
}

// [211] L-YAML-STREAM
Res l_yaml_stream(Input inp) {
    return build(inp, "STREAM", ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_document_prefix(inp); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return l_any_document(inp); }); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return l_document_suffix(inp); }); }, ^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_document_prefix(inp); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return l_any_document(inp); }); }}); }, ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){^Res(Input inp){ return star(inp, ^Res(Input inp){ return l_document_prefix(inp); }); }, ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return l_explicit_document(inp); }); }}); }}); }); }}); });
}

// ── API ──

static void printAst(YAMLNode *node, int depth) {
    NSMutableString *indent = [NSMutableString string];
    int di; for(di=0;di<depth;di++) [indent appendString:@"  "];
    if (node->isLeaf) {
        printf("%sSCALAR: \"%s\"\n", indent.UTF8String, node->text.UTF8String);
    } else {
        printf("%s%s\n", indent.UTF8String, node->type.UTF8String);
        for (YAMLNode *c in node->children) printAst(c, depth+1);
    }
}

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        NSString *text;
        if (argc > 1) {
            NSError *e;
            text = [NSString stringWithContentsOfFile:[NSString stringWithUTF8String:argv[1]]
                                            encoding:NSUTF8StringEncoding error:&e];
            if (!text) { fprintf(stderr, "Cannot open %s\n", argv[1]); return 1; }
        } else {
            NSData *d = [[NSFileHandle fileHandleWithStandardInput] readDataToEndOfFile];
            text = [[NSString alloc] initWithData:d encoding:NSUTF8StringEncoding];
        }
        Input inp = mkInput(text);
        Res r = l_yaml_stream(inp);
        if (!r->failed) {
            printf("OK: %d chars\n", r->rest.pos);
            if (r->ast) printAst(r->ast, 0);
        } else {
            fprintf(stderr, "FAIL @%d: %s\n", r->rest.pos, r->err.UTF8String);
            return 1;
        }
    }
    return 0;
}
