;;;; peg-objc.lisp — Objective-C target for emit-yaml-peg.lisp
;;;; Requires clang with -fblocks on Linux, or Xcode on macOS.

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Objective-C")
(def-tgt "default-output" "YAMLReader.m")

(def-tgt "keywords"
  '("auto" "break" "case" "char" "const" "continue" "default" "do"
    "double" "else" "enum" "extern" "float" "for" "goto" "if"
    "int" "long" "register" "return" "short" "signed" "sizeof" "static"
    "struct" "switch" "typedef" "union" "unsigned" "void" "volatile" "while"
    "id" "self" "super" "nil" "YES" "NO" "class" "protocol" "selector"
    "in" "out" "inout" "bycopy" "byref" "oneway"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "^Res(Input inp){ return ~A; }" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "^Res(Input inp){ return ~A; }" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (let ((n (length wrapped-fns)))
      (format nil "peg_seq(inp, ~D, (PFn[]){~{~A~^, ~}})" n wrapped-fns))))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (let ((n (length wrapped-fns)))
      (format nil "peg_alt(inp, ~D, (PFn[]){~{~A~^, ~}})" n wrapped-fns))))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "(^Res{~{ if([~A isEqualToString:@~S]) return ~A;~} return peg_fail(inp, @\"no case\"); })()"
            (loop for (val body) in cases
                  collect param collect val collect body))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "(^Res{ Res r=~A; if(r->failed) return r; int ~A=r->tagInt; return (^Res(Input inp){ return ~A; })(r->rest); })()"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "(^Res{ Res r=~A; if(r->failed) return r; NSString *~A=r->tag; return (^Res(Input inp){ return ~A; })(r->rest); })()"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "@~S" s)))

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
                                (format nil "NSString *~A" (peg-ident p)))))
                        params))
        (format nil "~A(Input inp)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "Res ~A {~%    return ~A;~%}" sig body)))

(def-tgt "fwd-decl"
  (lambda (name params)
    (let ((sig (funcall (tgt "fn-sig") name params)))
      (format nil "Res ~A;" sig))))

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
#import <Foundation/Foundation.h>")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Input ──

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
    return (Input){i.src, i.pos+1, c=='\\n' ? i.line+1 : i.line, c=='\\n' ? 0 : i.col+1};
}"

"// ── AST ──

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
    YAMLNode *n = [[YAMLNode alloc] init]; n->type = @\"SCALAR\"; n->text = t; n->isLeaf = YES; return n;
}
@end"

"// ── Result ──

@interface PResult : NSObject { @public
    BOOL failed; NSString *val; Input rest; NSString *tag; int tagInt;
    YAMLNode *ast; NSMutableArray *astList; NSString *err;
} @end
@implementation PResult @end
typedef PResult * Res;

static Res peg_ok(Input i) { Res r=[PResult new]; r->val=@\"\"; r->rest=i; r->tag=@\"\"; return r; }
static Res peg_okv(Input i, NSString *v) { Res r=[PResult new]; r->val=v; r->rest=i; r->tag=@\"\"; return r; }
static Res peg_fail(Input i, NSString *m) { Res r=[PResult new]; r->failed=YES; r->val=@\"\"; r->rest=i; r->tag=@\"\"; r->err=m; return r; }
static Res ok(Input i) { return peg_ok(i); }"

"// ── Context ──

static NSString *in_flow(NSString *c) {
    if ([c isEqualToString:@\"FLOW-OUT\"]||[c isEqualToString:@\"FLOW-IN\"]) return @\"FLOW-IN\";
    return @\"FLOW-KEY\";
}
static int seq_spaces(int n, NSString *c) { return [c isEqualToString:@\"BLOCK-OUT\"] ? n-1 : n; }
static BOOL ctx_eq(NSString *a, NSString *b) { return [a isEqualToString:b]; }"

"// ── Combinators ──

typedef Res (^PFn)(Input);

static Res match_cp(Input inp, int cp) {
    int c=peekCp(inp);
    if(c==cp){unichar u=(unichar)c; return peg_okv(adv(inp),[NSString stringWithCharacters:&u length:1]);}
    return peg_fail(inp,@\"cp\");
}
static Res match_range(Input inp, int lo, int hi) {
    int c=peekCp(inp);
    if(c>=lo&&c<=hi){unichar u=(unichar)c; return peg_okv(adv(inp),[NSString stringWithCharacters:&u length:1]);}
    return peg_fail(inp,@\"rng\");
}
static Res match_str(Input inp, const char *ct) {
    NSString *t=[NSString stringWithUTF8String:ct]; int n=(int)t.length;
    if(inp.pos+n>(int)inp.src.length) return peg_fail(inp,@\"str\");
    if(![[inp.src substringWithRange:NSMakeRange(inp.pos,n)] isEqualToString:t]) return peg_fail(inp,@\"str\");
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
        [acc appendString:r->val?:@\"\"]; mergeAsts(asts,r); cur=r->rest;}
    Res res=peg_okv(cur,acc);
    if(asts.count==1) res->ast=[asts objectAtIndex:0]; else if(asts.count>1) res->astList=asts;
    return res;
}
static Res peg_alt(Input inp, int cnt, PFn fns[]) {
    int fi; for(fi=0;fi<cnt;fi++){
        Res r=fns[fi](inp); if(!r->failed) return r;}
    return peg_fail(inp,@\"alt\");
}
static Res star(Input inp, PFn f) {
    Input cur=inp; NSMutableString *acc=[NSMutableString string]; NSMutableArray *asts=[NSMutableArray array];
    for(;;){Res r=f(cur); if(r->failed||r->rest.pos<=cur.pos) break;
        [acc appendString:r->val?:@\"\"]; mergeAsts(asts,r); cur=r->rest;}
    Res res=peg_okv(cur,acc); if(asts.count>0) res->astList=asts; return res;
}
static Res plus_(Input inp, PFn f) {
    Res first=f(inp); if(first->failed) return first;
    Res rest=star(first->rest,f);
    NSMutableString *v=[NSMutableString stringWithString:first->val?:@\"\"];
    [v appendString:rest->val?:@\"\"];
    Res res=peg_okv(rest->rest,v);
    NSMutableArray *asts=[NSMutableArray array]; mergeAsts(asts,first); mergeAsts(asts,rest);
    if(asts.count>0) res->astList=asts; return res;
}
static Res opt(Input inp, PFn f){Res r=f(inp); return r->failed?peg_ok(inp):r;}
static Res neg(Input inp, PFn f){Res r=f(inp); return r->failed?peg_ok(inp):peg_fail(inp,@\"neg\");}
static Res minus(Input inp, PFn fa, PFn fb){
    Res ra=fa(inp); if(ra->failed) return ra;
    Res rb=fb(inp); return(!rb->failed&&rb->rest.pos==ra->rest.pos)?peg_fail(inp,@\"excl\"):ra;
}
static Res rep(Input inp, int n, PFn f){
    Input cur=inp; NSMutableString *acc=[NSMutableString string];
    int i; for(i=0;i<n;i++){Res r=f(cur); if(r->failed) return r; [acc appendString:r->val?:@\"\"]; cur=r->rest;}
    return peg_okv(cur,acc);
}
static Res ahead(Input inp, PFn f){Res r=f(inp); return r->failed?r:peg_ok(inp);}
static Res behind(Input inp, PFn f){
    if(inp.pos==0) return peg_fail(inp,@\"bh\");
    Input t=(Input){inp.src,inp.pos-1,inp.line,MAX(0,inp.col-1)};
    Res r=f(t); return r->failed?peg_fail(inp,@\"bh\"):peg_ok(inp);
}
static Res sol(Input inp){return inp.col==0?peg_ok(inp):peg_fail(inp,@\"sol\");}
static Res eof_ok(Input inp){return atEof(inp)?peg_ok(inp):peg_fail(inp,@\"eof\");}"

"// ── YAML extensions ──

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
    if(i+sp<len&&[s characterAtIndex:i+sp]!='\\n'){Res r=peg_ok(inp); r->tagInt=MAX(1,sp-n); return r;}
    int j=i; while(j<len&&[s characterAtIndex:j]!='\\n') j++;
    while(j<len){
        if([s characterAtIndex:j]=='\\n') j++; if(j>=len) break;
        sp=0; while(j+sp<len&&[s characterAtIndex:j+sp]==' ') sp++;
        int nx=j+sp; if(nx>=len||[s characterAtIndex:nx]=='\\n'){j=nx;continue;}
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
static Res val(Input inp, const char *cv){Res r=peg_ok(inp); r->tag=[NSString stringWithUTF8String:cv]; return r;}"
))

;;; ── API ──

(def-tgt "api"
"// ── API ──

static void printAst(YAMLNode *node, int depth) {
    NSMutableString *indent = [NSMutableString string];
    int di; for(di=0;di<depth;di++) [indent appendString:@\"  \"];
    if (node->isLeaf) {
        printf(\"%sSCALAR: \\\"%s\\\"\\n\", indent.UTF8String, node->text.UTF8String);
    } else {
        printf(\"%s%s\\n\", indent.UTF8String, node->type.UTF8String);
        for (YAMLNode *c in node->children) printAst(c, depth+1);
    }
}")

;;; ── Main ──

(def-tgt "main-fn"
"int main(int argc, const char *argv[]) {
    @autoreleasepool {
        NSString *text;
        if (argc > 1) {
            NSError *e;
            text = [NSString stringWithContentsOfFile:[NSString stringWithUTF8String:argv[1]]
                                            encoding:NSUTF8StringEncoding error:&e];
            if (!text) { fprintf(stderr, \"Cannot open %s\\n\", argv[1]); return 1; }
        } else {
            NSData *d = [[NSFileHandle fileHandleWithStandardInput] readDataToEndOfFile];
            text = [[NSString alloc] initWithData:d encoding:NSUTF8StringEncoding];
        }
        Input inp = mkInput(text);
        Res r = l_yaml_stream(inp);
        if (!r->failed) {
            printf(\"OK: %d chars\\n\", r->rest.pos);
            if (r->ast) printAst(r->ast, 0);
        } else {
            fprintf(stderr, \"FAIL @%d: %s\\n\", r->rest.pos, r->err.UTF8String);
            return 1;
        }
    }
    return 0;
}")

;;; ── Concern Vocab ──

(let ((cv (make-hash-table :test 'equal)))
  (setf (gethash "value-type-decl" cv)
"typedef enum { YNull, YBool, YInt, YFloat, YStr, YMap, YSeq } YamlTag;

@interface YamlValue : NSObject { @public
    YamlTag tag;
    BOOL b;
    int64_t i;
    double f;
    NSString *s;
    NSMutableDictionary *m;
    NSMutableArray *v;
}
+ (instancetype)nullVal;
+ (instancetype)boolVal:(BOOL)b;
+ (instancetype)intVal:(int64_t)i;
+ (instancetype)floatVal:(double)f;
+ (instancetype)strVal:(NSString *)s;
+ (instancetype)mapVal:(NSMutableDictionary *)m;
+ (instancetype)seqVal:(NSMutableArray *)v;
@end

@implementation YamlValue
+ (instancetype)nullVal { YamlValue *y=[YamlValue new]; y->tag=YNull; return y; }
+ (instancetype)boolVal:(BOOL)b { YamlValue *y=[YamlValue new]; y->tag=YBool; y->b=b; return y; }
+ (instancetype)intVal:(int64_t)i { YamlValue *y=[YamlValue new]; y->tag=YInt; y->i=i; return y; }
+ (instancetype)floatVal:(double)f { YamlValue *y=[YamlValue new]; y->tag=YFloat; y->f=f; return y; }
+ (instancetype)strVal:(NSString *)s { YamlValue *y=[YamlValue new]; y->tag=YStr; y->s=s; return y; }
+ (instancetype)mapVal:(NSMutableDictionary *)m { YamlValue *y=[YamlValue new]; y->tag=YMap; y->m=m; return y; }
+ (instancetype)seqVal:(NSMutableArray *)v { YamlValue *y=[YamlValue new]; y->tag=YSeq; y->v=v; return y; }
@end")

  (setf (gethash "accessors" cv)
"static YamlValue* yGet(YamlValue *y, NSString *key) {
    if (y->tag == YMap) { YamlValue *r = y->m[key]; if (r) return r; }
    return [YamlValue nullVal];
}
static YamlValue* yAt(YamlValue *y, NSUInteger idx) {
    if (y->tag == YSeq && idx < y->v.count) return y->v[idx];
    return [YamlValue nullVal];
}
static NSString* yStr(YamlValue *y) { return y->tag == YStr ? y->s : @\"\"; }")

  (setf (gethash "coerce-fn" cv)
"static YamlValue* coerceScalar(NSString *s) {
    if ([s isEqualToString:@\"null\"]||[s isEqualToString:@\"Null\"]||[s isEqualToString:@\"NULL\"]||[s isEqualToString:@\"~\"]||s.length==0)
        return [YamlValue nullVal];
    if ([s isEqualToString:@\"true\"]||[s isEqualToString:@\"True\"]||[s isEqualToString:@\"TRUE\"])
        return [YamlValue boolVal:YES];
    if ([s isEqualToString:@\"false\"]||[s isEqualToString:@\"False\"]||[s isEqualToString:@\"FALSE\"])
        return [YamlValue boolVal:NO];
    if ([s isEqualToString:@\".inf\"]||[s isEqualToString:@\".Inf\"]||[s isEqualToString:@\".INF\"]||[s isEqualToString:@\"+.inf\"])
        return [YamlValue floatVal:INFINITY];
    if ([s isEqualToString:@\"-.inf\"]||[s isEqualToString:@\"-.Inf\"]||[s isEqualToString:@\"-.INF\"])
        return [YamlValue floatVal:-INFINITY];
    if ([s isEqualToString:@\".nan\"]||[s isEqualToString:@\".NaN\"]||[s isEqualToString:@\".NAN\"])
        return [YamlValue floatVal:NAN];
    NSScanner *sc = [NSScanner scannerWithString:s];
    long long ll; if ([sc scanLongLong:&ll] && sc.isAtEnd) return [YamlValue intVal:(int64_t)ll];
    if (s.length > 2 && [s hasPrefix:@\"0x\"]) {
        unsigned long long ull;
        sc = [NSScanner scannerWithString:s];
        if ([sc scanHexLongLong:&ull] && sc.isAtEnd) return [YamlValue intVal:(int64_t)ull];
    }
    sc = [NSScanner scannerWithString:s];
    double d; if ([sc scanDouble:&d] && sc.isAtEnd) return [YamlValue floatVal:d];
    return [YamlValue strVal:s];
}")

  (setf (gethash "converter-decl" cv)
"@interface YamlConverter : NSObject { @public
    NSMutableDictionary *anchors;
}
- (YamlValue *)convert:(YAMLNode *)node;
@end

@implementation YamlConverter
- (instancetype)init { self=[super init]; anchors=[NSMutableDictionary dictionary]; return self; }")

  (setf (gethash "convert-fn" cv)
"- (YamlValue *)convert:(YAMLNode *)node {
    if (!node) return [YamlValue nullVal];
    if (node->isLeaf) return coerceScalar(node->text);
    NSString *t = node->type;
    if ([t isEqualToString:@\"ANCHOR\"]) {
        NSString *name = nil;
        YamlValue *val = [YamlValue nullVal];
        for (YAMLNode *ch in node->children) {
            if (ch->isLeaf && !name) name = ch->text;
            else val = [self convert:ch];
        }
        if (name) anchors[name] = val;
        return val;
    }
    if ([t isEqualToString:@\"ALIAS\"]) {
        for (YAMLNode *ch in node->children) {
            if (ch->isLeaf && anchors[ch->text]) return anchors[ch->text];
        }
        return [YamlValue nullVal];
    }
    if ([t isEqualToString:@\"MAPPING\"]) {
        NSMutableDictionary *m = [NSMutableDictionary dictionary];
        for (YAMLNode *ch in node->children) {
            if ([ch->type isEqualToString:@\"PAIR\"] && ch->children.count >= 2) {
                YamlValue *key = [self convert:ch->children[0]];
                YamlValue *val = [self convert:ch->children[1]];
                if ([yStr(key) isEqualToString:@\"<<\"] && val->tag == YMap) {
                    for (NSString *mk in val->m) { if (!m[mk]) m[mk] = val->m[mk]; }
                } else { m[yStr(key)] = val; }
            }
        }
        return [YamlValue mapVal:m];
    }
    if ([t isEqualToString:@\"SEQUENCE\"]) {
        NSMutableArray *seq = [NSMutableArray array];
        for (YAMLNode *ch in node->children) [seq addObject:[self convert:ch]];
        return [YamlValue seqVal:seq];
    }
    if ([t isEqualToString:@\"DOC\"] || [t isEqualToString:@\"STREAM\"]) {
        if (node->children.count == 1) return [self convert:node->children[0]];
        NSMutableArray *docs = [NSMutableArray array];
        for (YAMLNode *ch in node->children) [docs addObject:[self convert:ch]];
        return docs.count == 1 ? docs[0] : [YamlValue seqVal:docs];
    }
    if ([t isEqualToString:@\"PAIR\"] && node->children.count >= 2)
        return [self convert:node->children[1]];
    if (node->children.count == 1) return [self convert:node->children[0]];
    NSMutableArray *items = [NSMutableArray array];
    for (YAMLNode *ch in node->children) [items addObject:[self convert:ch]];
    return [YamlValue seqVal:items];
}
@end")

  (setf (gethash "load-fn" cv)
"static YamlValue* yamlLoad(NSString *text) {
    Input inp = mkInput(text);
    Res r = l_yaml_stream(inp);
    if (r->failed) return [YamlValue nullVal];
    YamlConverter *conv = [YamlConverter new];
    return [conv convert:r->ast];
}")
  (def-tgt "cv" cv))
