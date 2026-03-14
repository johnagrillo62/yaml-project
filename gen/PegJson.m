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

@interface JSONNode : NSObject { @public
    NSString *type; NSString *text; NSMutableArray *children; BOOL isLeaf;
}
+ (instancetype)branch:(NSString *)t;
+ (instancetype)leaf:(NSString *)t;
@end

@implementation JSONNode
+ (instancetype)branch:(NSString *)t {
    JSONNode *n = [[JSONNode alloc] init]; n->type = t; n->children = [NSMutableArray array]; return n;
}
+ (instancetype)leaf:(NSString *)t {
    JSONNode *n = [[JSONNode alloc] init]; n->type = @"SCALAR"; n->text = t; n->isLeaf = YES; return n;
}
@end

// ── Result ──

@interface PResult : NSObject { @public
    BOOL failed; NSString *val; Input rest; NSString *tag; int tagInt;
    JSONNode *ast; NSMutableArray *astList; NSString *err;
} @end
@implementation PResult @end
typedef PResult * Res;

static Res peg_ok(Input i) { Res r=[PResult new]; r->val=@""; r->rest=i; r->tag=@""; return r; }
static Res peg_okv(Input i, NSString *v) { Res r=[PResult new]; r->val=v; r->rest=i; r->tag=@""; return r; }
static Res peg_fail(Input i, NSString *m) { Res r=[PResult new]; r->failed=YES; r->val=@""; r->rest=i; r->tag=@""; r->err=m; return r; }
static Res ok(Input i) { return peg_ok(i); }
static int _depth = 0;

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
        PFn f=Block_copy(fns[fi]);
        Res r=f(cur); Block_release(f);
        if(r->failed) return r;
        [acc appendString:r->val?:@""]; mergeAsts(asts,r); cur=r->rest;}
    Res res=peg_okv(cur,acc);
    if(asts.count==1) res->ast=[asts objectAtIndex:0]; else if(asts.count>1) res->astList=asts;
    return res;
}
static Res peg_alt(Input inp, int cnt, PFn fns[]) {
    int fi; for(fi=0;fi<cnt;fi++){
        PFn f=Block_copy(fns[fi]);
        Res r=f(inp); Block_release(f);
        if(!r->failed) return r;}
    return peg_fail(inp,@"alt");
}
static Res star(Input inp, PFn f) {
    PFn fc=Block_copy(f);
    Input cur=inp; NSMutableString *acc=[NSMutableString string]; NSMutableArray *asts=[NSMutableArray array];
    for(;;){Res r=fc(cur); if(r->failed||r->rest.pos<=cur.pos) break;
        [acc appendString:r->val?:@""]; mergeAsts(asts,r); cur=r->rest;}
    Block_release(fc);
    Res res=peg_okv(cur,acc); if(asts.count>0) res->astList=asts; return res;
}
static Res plus_(Input inp, PFn f) {
    PFn fc=Block_copy(f);
    Res first=fc(inp); if(first->failed){Block_release(fc);return first;}
    Res rest=star(first->rest,fc); Block_release(fc);
    NSMutableString *v=[NSMutableString stringWithString:first->val?:@""];
    [v appendString:rest->val?:@""];
    Res res=peg_okv(rest->rest,v);
    NSMutableArray *asts=[NSMutableArray array]; mergeAsts(asts,first); mergeAsts(asts,rest);
    if(asts.count>0) res->astList=asts; return res;
}
static Res opt(Input inp, PFn f){PFn fc=Block_copy(f);Res r=fc(inp);Block_release(fc); return r->failed?peg_ok(inp):r;}
static Res neg(Input inp, PFn f){PFn fc=Block_copy(f);Res r=fc(inp);Block_release(fc); return r->failed?peg_ok(inp):peg_fail(inp,@"neg");}
static Res minus(Input inp, PFn fa, PFn fb){
    PFn fac=Block_copy(fa); PFn fbc=Block_copy(fb);
    Res ra=fac(inp); Block_release(fac);
    if(ra->failed){Block_release(fbc);return ra;}
    Res rb=fbc(inp); Block_release(fbc);
    return(!rb->failed&&rb->rest.pos==ra->rest.pos)?peg_fail(inp,@"excl"):ra;
}
static Res rep(Input inp, int n, PFn f){
    PFn fc=Block_copy(f);
    Input cur=inp; NSMutableString *acc=[NSMutableString string];
    int i; for(i=0;i<n;i++){Res r=fc(cur); if(r->failed){Block_release(fc);return r;} [acc appendString:r->val?:@""]; cur=r->rest;}
    Block_release(fc); return peg_okv(cur,acc);
}
static Res ahead(Input inp, PFn f){PFn fc=Block_copy(f);Res r=fc(inp);Block_release(fc); return r->failed?r:peg_ok(inp);}
static Res behind(Input inp, PFn f){
    if(inp.pos==0) return peg_fail(inp,@"bh");
    Input t=(Input){inp.src,inp.pos-1,inp.line,MAX(0,inp.col-1)};
    PFn fc=Block_copy(f); Res r=fc(t); Block_release(fc);
    return r->failed?peg_fail(inp,@"bh"):peg_ok(inp);
}
static Res sol(Input inp){return inp.col==0?peg_ok(inp):peg_fail(inp,@"sol");}
static Res eof_ok(Input inp){return atEof(inp)?peg_ok(inp):peg_fail(inp,@"eof");}

Res json_text(Input inp);
Res value(Input inp);
Res object(Input inp);
Res members(Input inp);
Res member(Input inp);
Res array(Input inp);
Res elements(Input inp);
Res string(Input inp);
Res r_char(Input inp);
Res escaped(Input inp);
Res hex4(Input inp);
Res hexdig(Input inp);
Res number(Input inp);
Res integer(Input inp);
Res fraction(Input inp);
Res exponent(Input inp);
Res ws(Input inp);

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
Res json_text(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 4, (PFn[]){
        ^Res(Input inp){ return ws(inp); },
        ^Res(Input inp){ return value(inp); },
        ^Res(Input inp){ return ws(inp); },
        ^Res(Input inp){ return eof_ok(inp); }});
    _depth--;return _r;
}

// [2] VALUE 
Res value(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_alt(inp, 7, (PFn[]){
        ^Res(Input inp){ return object(inp); },
        ^Res(Input inp){ return array(inp); },
        ^Res(Input inp){ return string(inp); },
        ^Res(Input inp){ return number(inp); },
        ^Res(Input inp){ return match_str(inp, "true"); },
        ^Res(Input inp){ return match_str(inp, "false"); },
        ^Res(Input inp){ return match_str(inp, "null"); }});
    _depth--;return _r;
}

// [3] OBJECT 
Res object(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_alt(inp, 10, (PFn[]){
        ^Res(Input inp){ return peg_seq(inp, 5, (PFn[]){
            ^Res(Input inp){ return match_cp(inp, 123); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return members(inp); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return match_cp(inp, 125); }}); },
        ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){
            ^Res(Input inp){ return match_cp(inp, 123); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return match_cp(inp, 125); }}); }});
    _depth--;return _r;
}

// [4] MEMBERS 
Res members(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 7, (PFn[]){
        ^Res(Input inp){ return member(inp); },
        ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 4, (PFn[]){
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return match_cp(inp, 44); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return member(inp); }}); }); }});
    _depth--;return _r;
}

// [5] MEMBER 
Res member(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 7, (PFn[]){
        ^Res(Input inp){ return ws(inp); },
        ^Res(Input inp){ return string(inp); },
        ^Res(Input inp){ return ws(inp); },
        ^Res(Input inp){ return match_cp(inp, 58); },
        ^Res(Input inp){ return ws(inp); },
        ^Res(Input inp){ return value(inp); },
        ^Res(Input inp){ return ws(inp); }});
    _depth--;return _r;
}

// [6] ARRAY 
Res array(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_alt(inp, 10, (PFn[]){
        ^Res(Input inp){ return peg_seq(inp, 5, (PFn[]){
            ^Res(Input inp){ return match_cp(inp, 91); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return elements(inp); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return match_cp(inp, 93); }}); },
        ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){
            ^Res(Input inp){ return match_cp(inp, 91); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return match_cp(inp, 93); }}); }});
    _depth--;return _r;
}

// [7] ELEMENTS 
Res elements(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 7, (PFn[]){
        ^Res(Input inp){ return value(inp); },
        ^Res(Input inp){ return star(inp, ^Res(Input inp){ return peg_seq(inp, 4, (PFn[]){
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return match_cp(inp, 44); },
            ^Res(Input inp){ return ws(inp); },
            ^Res(Input inp){ return value(inp); }}); }); }});
    _depth--;return _r;
}

// [8] STRING 
Res string(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 4, (PFn[]){
        ^Res(Input inp){ return match_cp(inp, 34); },
        ^Res(Input inp){ return star(inp, ^Res(Input inp){ return r_char(inp); }); },
        ^Res(Input inp){ return match_cp(inp, 34); }});
    _depth--;return _r;
}

// [9] CHAR 
Res r_char(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_alt(inp, 11, (PFn[]){
        ^Res(Input inp){ return escaped(inp); },
        ^Res(Input inp){ return peg_seq(inp, 9, (PFn[]){
            ^Res(Input inp){ return neg(inp, ^Res(Input inp){ return match_cp(inp, 34); }); },
            ^Res(Input inp){ return neg(inp, ^Res(Input inp){ return match_cp(inp, 92); }); },
            ^Res(Input inp){ return neg(inp, ^Res(Input inp){ return match_cp(inp, 0x0); }); },
            ^Res(Input inp){ return neg(inp, ^Res(Input inp){ return match_range(inp, 0x0, 0x1F); }); },
            ^Res(Input inp){ return match_range(inp, 0x20, 0x10FFFF); }}); }});
    _depth--;return _r;
}

// [10] ESCAPED 
Res escaped(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 13, (PFn[]){
        ^Res(Input inp){ return match_cp(inp, 92); },
        ^Res(Input inp){ return peg_alt(inp, 11, (PFn[]){
            ^Res(Input inp){ return match_cp(inp, 34); },
            ^Res(Input inp){ return match_cp(inp, 92); },
            ^Res(Input inp){ return match_cp(inp, 47); },
            ^Res(Input inp){ return match_cp(inp, 98); },
            ^Res(Input inp){ return match_cp(inp, 102); },
            ^Res(Input inp){ return match_cp(inp, 110); },
            ^Res(Input inp){ return match_cp(inp, 114); },
            ^Res(Input inp){ return match_cp(inp, 116); },
            ^Res(Input inp){ return peg_seq(inp, 2, (PFn[]){
                ^Res(Input inp){ return match_cp(inp, 117); },
                ^Res(Input inp){ return hex4(inp); }}); }}); }});
    _depth--;return _r;
}

// [11] HEX4 
Res hex4(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 4, (PFn[]){
        ^Res(Input inp){ return hexdig(inp); },
        ^Res(Input inp){ return hexdig(inp); },
        ^Res(Input inp){ return hexdig(inp); },
        ^Res(Input inp){ return hexdig(inp); }});
    _depth--;return _r;
}

// [12] HEXDIG 
Res hexdig(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_alt(inp, 3, (PFn[]){
        ^Res(Input inp){ return match_range(inp, 48, 57); },
        ^Res(Input inp){ return match_range(inp, 97, 102); },
        ^Res(Input inp){ return match_range(inp, 65, 70); }});
    _depth--;return _r;
}

// [13] NUMBER 
Res number(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 7, (PFn[]){
        ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return match_cp(inp, 45); }); },
        ^Res(Input inp){ return integer(inp); },
        ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return fraction(inp); }); },
        ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return exponent(inp); }); }});
    _depth--;return _r;
}

// [14] INTEGER 
Res integer(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_alt(inp, 5, (PFn[]){
        ^Res(Input inp){ return match_cp(inp, 48); },
        ^Res(Input inp){ return peg_seq(inp, 3, (PFn[]){
            ^Res(Input inp){ return match_range(inp, 49, 57); },
            ^Res(Input inp){ return star(inp, ^Res(Input inp){ return match_range(inp, 48, 57); }); }}); }});
    _depth--;return _r;
}

// [15] FRACTION 
Res fraction(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 3, (PFn[]){
        ^Res(Input inp){ return match_cp(inp, 46); },
        ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return match_range(inp, 48, 57); }); }});
    _depth--;return _r;
}

// [16] EXPONENT 
Res exponent(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=peg_seq(inp, 9, (PFn[]){
        ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){
            ^Res(Input inp){ return match_cp(inp, 101); },
            ^Res(Input inp){ return match_cp(inp, 69); }}); },
        ^Res(Input inp){ return opt(inp, ^Res(Input inp){ return peg_alt(inp, 2, (PFn[]){
            ^Res(Input inp){ return match_cp(inp, 43); },
            ^Res(Input inp){ return match_cp(inp, 45); }}); }); },
        ^Res(Input inp){ return plus_(inp, ^Res(Input inp){ return match_range(inp, 48, 57); }); }});
    _depth--;return _r;
}

// [17] WS 
Res ws(Input inp) {
    if(++_depth>500){_depth--;return peg_fail(inp,@"depth");}
    Res _r=star(inp, ^Res(Input inp){ return peg_alt(inp, 4, (PFn[]){
        ^Res(Input inp){ return match_cp(inp, 0x20); },
        ^Res(Input inp){ return match_cp(inp, 0x9); },
        ^Res(Input inp){ return match_cp(inp, 0x0A); },
        ^Res(Input inp){ return match_cp(inp, 0x0D); }}); });
    _depth--;return _r;
}

// ── API ──

static void printAst(JSONNode *node, int depth) {
    NSMutableString *indent = [NSMutableString string];
    int di; for(di=0;di<depth;di++) [indent appendString:@"  "];
    if (node->isLeaf) {
        printf("%sSCALAR: \"%s\"\n", indent.UTF8String, node->text.UTF8String);
    } else {
        printf("%s%s\n", indent.UTF8String, node->type.UTF8String);
        for (JSONNode *c in node->children) printAst(c, depth+1);
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
        Res r = json_text(inp);
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
