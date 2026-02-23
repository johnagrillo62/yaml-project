;;;; peg-zig.lisp — Zig target for emit-yaml-peg.lisp
;;;;
;;;; Zig has no closures and no GC. Like bash, we use wrapper functions
;;;; and pass function pointers. Grammar rules become named functions.
;;;; Parameters are passed via globals (like bash) since Zig fn ptrs
;;;; can't capture.

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Zig")
(def-tgt "default-output" "yaml_reader.zig")
(def-tgt "comment-prefix" "//")
(def-tgt "call-style" "bash")  ;; wrapper-based, no closures

(def-tgt "keywords"
  '("align" "allowzero" "and" "anyframe" "anytype" "asm" "async" "await"
    "break" "callconv" "catch" "comptime" "const" "continue" "defer"
    "else" "enum" "errdefer" "error" "export" "extern" "false" "fn"
    "for" "if" "inline" "linksection" "noalias" "nosuspend" "null"
    "opaque" "or" "orelse" "packed" "pub" "resume" "return" "struct"
    "suspend" "switch" "test" "threadlocal" "true" "try" "type"
    "undefined" "union" "unreachable" "var" "volatile" "while"))
(def-tgt "keyword-prefix" "r_")

;;; ── Identifier rules ──

(def-tgt "ident-prefix" "")
(def-tgt "ident-transform" :snake)

;;; ── Closure wrapping ──
;;; Zig can't do closures. Like bash, wrappers are function names.

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    body))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    body))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "peg_seq(&[_]PFn{~{~A~^, ~}})" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "peg_alt(&[_]PFn{~{~A~^, ~}})" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (with-output-to-string (s)
      (format s "switch_ctx(~A, &[_]CtxCase{~%" param)
      (loop for (val body) in cases
            do (format s "        .{ .ctx = ~S, .fn = ~A },~%" val body))
      (format s "    })"))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "blk: { ~A; if (g.failed) break :blk; const ~A = g.rtagint; save_inp(); ~A }"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "blk: { ~A; if (g.failed) break :blk; const ~A = g.rtag; save_inp(); ~A }"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (format nil "g.~A" (peg-ident sym))))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "@intCast(~A)" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "in_flow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seq_spaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A" name)  ;; params are in globals
        (format nil "~A" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "fn ~A() void {~%    ~A;~%}" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
// yaml_reader.zig — YAML 1.2 parser
// ════════════════════════════════════════════════════════════════
const std = @import(\"std\");

const PFn = *const fn () void;")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Global State ──

const G = struct {
    src: []const u8 = &[_]u8{},
    pos: usize = 0,
    line: usize = 1,
    col: usize = 0,
    failed: bool = false,
    rval_buf: [65536]u8 = undefined,
    rval_len: usize = 0,
    rtag: []const u8 = &[_]u8{},
    rtagint: i32 = 0,
    rast: i32 = -1,
    // Parameters (passed via globals since no closures)
    n: i32 = 0,
    c: []const u8 = &[_]u8{},
    m: i32 = 0,
    t: []const u8 = &[_]u8{},
    // Save stack
    save_stack: [16384]SaveEntry = undefined,
    save_sp: usize = 0,
};

const SaveEntry = struct { pos: usize, line: usize, col: usize };

var g: G = .{};"

"// ── Input ──

fn at_eof() bool { return g.pos >= g.src.len; }

fn peek_cp() i32 {
    if (at_eof()) return -1;
    return @intCast(g.src[g.pos]);
}

fn adv_one() void {
    if (at_eof()) return;
    const c = g.src[g.pos];
    g.pos += 1;
    if (c == '\\n') { g.line += 1; g.col = 0; } else { g.col += 1; }
}

fn save_inp() void {
    g.save_stack[g.save_sp] = .{ .pos = g.pos, .line = g.line, .col = g.col };
    g.save_sp += 1;
}

fn restore_inp() void {
    g.save_sp -= 1;
    const e = g.save_stack[g.save_sp];
    g.pos = e.pos; g.line = e.line; g.col = e.col;
}"

"// ── Result helpers ──

fn ok_r() void { g.failed = false; }
fn fail_r() void { g.failed = true; }"

"// ── Combinators ──

fn match_cp(cp: i32) void {
    const c = peek_cp();
    if (c == cp) {
        g.rval_buf[g.rval_len] = @intCast(@as(u32, @bitCast(c)));
        g.rval_len += 1;
        adv_one();
        g.failed = false;
    } else {
        g.failed = true;
    }
}

fn match_range(lo: i32, hi: i32) void {
    const c = peek_cp();
    if (c >= lo and c <= hi) {
        g.rval_buf[g.rval_len] = @intCast(@as(u32, @bitCast(c)));
        g.rval_len += 1;
        adv_one();
        g.failed = false;
    } else {
        g.failed = true;
    }
}

fn match_str(t: []const u8) void {
    if (g.pos + t.len > g.src.len) { g.failed = true; return; }
    if (!std.mem.eql(u8, g.src[g.pos..g.pos + t.len], t)) { g.failed = true; return; }
    for (0..t.len) |_| adv_one();
    @memcpy(g.rval_buf[g.rval_len..g.rval_len + t.len], t);
    g.rval_len += t.len;
    g.failed = false;
}

fn peg_seq(fns: []const PFn) void {
    save_inp();
    for (fns) |f| {
        f();
        if (g.failed) { restore_inp(); return; }
    }
}

fn peg_alt(fns: []const PFn) void {
    for (fns) |f| {
        save_inp();
        f();
        if (!g.failed) return;
        restore_inp();
    }
    g.failed = true;
}

fn peg_star(f: PFn) void {
    while (true) {
        const old_pos = g.pos;
        save_inp();
        f();
        if (g.failed or g.pos <= old_pos) { restore_inp(); g.failed = false; return; }
    }
}

fn plus_(f: PFn) void {
    f();
    if (g.failed) return;
    peg_star(f);
}

fn opt(f: PFn) void {
    save_inp();
    f();
    if (g.failed) { restore_inp(); g.failed = false; }
}

fn r_neg(f: PFn) void {
    save_inp();
    f();
    if (g.failed) { restore_inp(); g.failed = false; }
    else { restore_inp(); g.failed = true; }
}

fn minus_fn(fa: PFn, fb: PFn) void {
    save_inp();
    fa();
    if (g.failed) { restore_inp(); return; }
    const a_pos = g.pos;
    restore_inp();
    save_inp();
    fb();
    if (!g.failed and g.pos == a_pos) { restore_inp(); g.failed = true; return; }
    restore_inp();
    fa();
}

fn rep_fn(count: i32, f: PFn) void {
    var i: i32 = 0;
    while (i < count) : (i += 1) {
        f();
        if (g.failed) return;
    }
}

fn ahead(f: PFn) void {
    save_inp();
    f();
    if (!g.failed) restore_inp()
    else restore_inp();
}

fn behind(f: PFn) void {
    if (g.pos == 0) { g.failed = true; return; }
    save_inp();
    g.pos -= 1;
    if (g.col > 0) g.col -= 1;
    f();
    restore_inp();
    if (g.failed) return;
    g.failed = false;
}

fn sol() void { g.failed = g.col != 0; }
fn eof_ok() void { g.failed = !at_eof(); }"

"// ── Context ──

fn in_flow(c: []const u8) []const u8 {
    if (std.mem.eql(u8, c, \"FLOW-OUT\") or std.mem.eql(u8, c, \"FLOW-IN\")) return \"FLOW-IN\";
    return \"FLOW-KEY\";
}

fn seq_spaces(n: i32, c: []const u8) i32 {
    if (std.mem.eql(u8, c, \"BLOCK-OUT\")) return n - 1;
    return n;
}"

"// ── YAML extensions ──

fn build_ast(typ: []const u8, f: PFn) void {
    f();
    if (g.failed) return;
    // AST node creation (simplified — full impl needs parallel arrays)
    _ = typ;
    g.failed = false;
}

fn scalar_fn(f: PFn) void {
    f();
    if (g.failed) return;
    // Mark result as scalar leaf
    g.failed = false;
}

fn collect_fn(f: PFn) void { f(); }

fn detect_indent(n: i32) void {
    var sp: usize = 0;
    var i = g.pos;
    while (i + sp < g.src.len and g.src[i + sp] == ' ') : (sp += 1) {}
    if (i + sp < g.src.len and g.src[i + sp] != '\\n') {
        g.rtagint = @intCast(@max(1, @as(i32, @intCast(sp)) - n));
        g.failed = false;
        return;
    }
    var j = i;
    while (j < g.src.len and g.src[j] != '\\n') : (j += 1) {}
    while (j < g.src.len) {
        if (g.src[j] == '\\n') j += 1;
        if (j >= g.src.len) break;
        sp = 0;
        while (j + sp < g.src.len and g.src[j + sp] == ' ') : (sp += 1) {}
        const nx = j + sp;
        if (nx >= g.src.len or g.src[nx] == '\\n') { j = nx; continue; }
        g.rtagint = @intCast(@max(1, @as(i32, @intCast(sp)) - n));
        g.failed = false;
        return;
    }
    g.rtagint = 1;
    g.failed = false;
}

fn parse_int_fn(f: PFn) void {
    f();
    if (g.failed) return;
    var v: i32 = 0;
    for (g.rval_buf[0..g.rval_len]) |ch| {
        if (ch >= '0' and ch <= '9') v = v * 10 + @as(i32, ch - '0');
    }
    g.rtagint = v;
}

fn parse_sym_fn(f: PFn, sym: []const u8) void {
    f();
    if (g.failed) return;
    g.rtag = sym;
}

fn val_fn(v: []const u8) void {
    g.failed = false;
    g.rtag = v;
}"

"// ── Main ──

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const text = try stdin.readToEndAlloc(std.heap.page_allocator, 1048576);
    defer std.heap.page_allocator.free(text);

    g.src = text;
    g.pos = 0;
    g.line = 1;
    g.col = 0;
    g.failed = false;
    g.rval_len = 0;
    g.save_sp = 0;

    l_yaml_stream();

    const stdout = std.io.getStdOut().writer();
    if (!g.failed) {
        try stdout.print(\"OK: {d} chars\\n\", .{g.pos});
    } else {
        const stderr = std.io.getStdErr().writer();
        try stderr.print(\"FAIL @{d}\\n\", .{g.pos});
        std.process.exit(1);
    }
}"
))

;;; ── API / Concerns ──

(def-tgt "api" "")
(def-tgt "main-fn" nil)
(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)

;;; ── Combinator name overrides ──

(def-tgt "comb-star" "peg_star")
(def-tgt "comb-neg" "r_neg")
(def-tgt "comb-minus" "minus_fn")
(def-tgt "comb-rep" "rep_fn")
(def-tgt "comb-build" "build_ast")
(def-tgt "comb-scalar" "scalar_fn")
(def-tgt "comb-collect" "collect_fn")
(def-tgt "comb-detect" "detect_indent")
(def-tgt "comb-parse-int" "parse_int_fn")
(def-tgt "comb-parse-sym" "parse_sym_fn")
(def-tgt "comb-val" "val_fn")
