// ════════════════════════════════════════════════════════════════
// json_reader.zig — JSON (RFC 8259) parser
// ════════════════════════════════════════════════════════════════
const std = @import("std");

const PFn = *const fn () void;

// ── Global State ──

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

var g: G = .{};

// ── Input ──

fn at_eof() bool { return g.pos >= g.src.len; }

fn peek_cp() i32 {
    if (at_eof()) return -1;
    return @intCast(g.src[g.pos]);
}

fn adv_one() void {
    if (at_eof()) return;
    const c = g.src[g.pos];
    g.pos += 1;
    if (c == '\n') { g.line += 1; g.col = 0; } else { g.col += 1; }
}

fn save_inp() void {
    g.save_stack[g.save_sp] = .{ .pos = g.pos, .line = g.line, .col = g.col };
    g.save_sp += 1;
}

fn restore_inp() void {
    g.save_sp -= 1;
    const e = g.save_stack[g.save_sp];
    g.pos = e.pos; g.line = e.line; g.col = e.col;
}

// ── Result helpers ──

fn ok_r() void { g.failed = false; }
fn fail_r() void { g.failed = true; }

// ── Combinators ──

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
    g.save_sp -= 1;
}

fn peg_alt(fns: []const PFn) void {
    for (fns) |f| {
        save_inp();
        f();
        if (!g.failed) return;
        restore_inp();
        g.failed = false;
    }
    g.failed = true;
}


fn peg_star(f: PFn) void {
    while (true) {
        const old_pos = g.pos;
        save_inp();
        f();
        if (g.failed or g.pos <= old_pos) { restore_inp(); g.failed = false; return; }
        g.save_sp -= 1;
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
fn eof_ok() void { g.failed = !at_eof(); }

// ── Context ──

const CtxCase = struct { ctx: []const u8, func: PFn };

fn switch_ctx(param: []const u8, cases: []const CtxCase) void {
    for (cases) |cs| {
        if (std.mem.eql(u8, param, cs.ctx)) { cs.func(); return; }
    }
    g.failed = true;
}

fn in_flow(c: []const u8) []const u8 {
    if (std.mem.eql(u8, c, "FLOW-OUT") or std.mem.eql(u8, c, "FLOW-IN")) return "FLOW-IN";
    return "FLOW-KEY";
}

fn seq_spaces(n: i32, c: []const u8) i32 {
    if (std.mem.eql(u8, c, "BLOCK-OUT")) return n - 1;
    return n;
}

// ── JSON extensions ──

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
    const i = g.pos;
    while (i + sp < g.src.len and g.src[i + sp] == ' ') : (sp += 1) {}
    if (i + sp < g.src.len and g.src[i + sp] != '\n') {
        g.rtagint = @intCast(@max(1, @as(i32, @intCast(sp)) - n));
        g.failed = false;
        return;
    }
    var j = i;
    while (j < g.src.len and g.src[j] != '\n') : (j += 1) {}
    while (j < g.src.len) {
        if (g.src[j] == '\n') j += 1;
        if (j >= g.src.len) break;
        sp = 0;
        while (j + sp < g.src.len and g.src[j + sp] == ' ') : (sp += 1) {}
        const nx = j + sp;
        if (nx >= g.src.len or g.src[nx] == '\n') { j = nx; continue; }
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
}

// ── Main ──

fn run_parse() void {
    json_text();
    const stdout = std.io.getStdOut().writer();
    if (!g.failed) {
        stdout.print("OK: {d} chars\n", .{g.pos}) catch {};
    } else {
        const stderr = std.io.getStdErr().writer();
        stderr.print("FAIL @{d}\n", .{g.pos}) catch {};
        std.process.exit(1);
    }
}

pub fn main() !void {
    const gpa = std.heap.page_allocator;
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);
    const text = if (args.len > 1)
        try std.fs.cwd().readFileAlloc(gpa, args[1], 1048576)
    else
        try std.io.getStdIn().readToEndAlloc(gpa, 1048576);
    defer gpa.free(text);
    g.src = text;
    g.pos = 0;
    g.line = 1;
    g.col = 0;
    g.failed = false;
    g.rval_len = 0;
    g.save_sp = 0;
    const thread = try std.Thread.spawn(
        .{ .stack_size = 64 * 1024 * 1024 },
        run_parse,
        .{},
    );
    thread.join();
}

// ════════════════════════════════════════════════════════════════ 
// Wrapper functions (bash has no closures) 
// ════════════════════════════════════════════════════════════════ 

fn _w1() void { ws(); }
fn _w2() void { value(); }
fn _w3() void { ws(); }
fn _w4() void { eof_ok(); }
fn _w5() void { object(); }
fn _w6() void { array(); }
fn _w7() void { r_string(); }
fn _w8() void { number(); }
fn _w9() void { match_str("true"); }
fn _w10() void { match_str("false"); }
fn _w11() void { match_str("null"); }
fn _w12() void { match_cp(123); }
fn _w13() void { ws(); }
fn _w14() void { members(); }
fn _w15() void { ws(); }
fn _w16() void { match_cp(125); }
fn _w17() void { peg_seq(&[_]PFn{_w12, _w13, _w14, _w15, _w16}); }
fn _w18() void { match_cp(123); }
fn _w19() void { ws(); }
fn _w20() void { match_cp(125); }
fn _w21() void { peg_seq(&[_]PFn{_w18, _w19, _w20}); }
fn _w22() void { member(); }
fn _w23() void { ws(); }
fn _w24() void { match_cp(44); }
fn _w25() void { ws(); }
fn _w26() void { member(); }
fn _w27() void { peg_seq(&[_]PFn{_w23, _w24, _w25, _w26}); }
fn _w28() void { peg_star(_w27); }
fn _w29() void { ws(); }
fn _w30() void { r_string(); }
fn _w31() void { ws(); }
fn _w32() void { match_cp(58); }
fn _w33() void { ws(); }
fn _w34() void { value(); }
fn _w35() void { ws(); }
fn _w36() void { match_cp(91); }
fn _w37() void { ws(); }
fn _w38() void { elements(); }
fn _w39() void { ws(); }
fn _w40() void { match_cp(93); }
fn _w41() void { peg_seq(&[_]PFn{_w36, _w37, _w38, _w39, _w40}); }
fn _w42() void { match_cp(91); }
fn _w43() void { ws(); }
fn _w44() void { match_cp(93); }
fn _w45() void { peg_seq(&[_]PFn{_w42, _w43, _w44}); }
fn _w46() void { value(); }
fn _w47() void { ws(); }
fn _w48() void { match_cp(44); }
fn _w49() void { ws(); }
fn _w50() void { value(); }
fn _w51() void { peg_seq(&[_]PFn{_w47, _w48, _w49, _w50}); }
fn _w52() void { peg_star(_w51); }
fn _w53() void { match_cp(34); }
fn _w54() void { r_char(); }
fn _w55() void { peg_star(_w54); }
fn _w56() void { match_cp(34); }
fn _w57() void { escaped(); }
fn _w58() void { match_cp(34); }
fn _w59() void { r_neg(_w58); }
fn _w60() void { match_cp(92); }
fn _w61() void { r_neg(_w60); }
fn _w62() void { match_cp(0x0); }
fn _w63() void { r_neg(_w62); }
fn _w64() void { match_range(0x0, 0x1F); }
fn _w65() void { r_neg(_w64); }
fn _w66() void { match_range(0x20, 0x10FFFF); }
fn _w67() void { peg_seq(&[_]PFn{_w59, _w61, _w63, _w65, _w66}); }
fn _w68() void { match_cp(92); }
fn _w69() void { match_cp(34); }
fn _w70() void { match_cp(92); }
fn _w71() void { match_cp(47); }
fn _w72() void { match_cp(98); }
fn _w73() void { match_cp(102); }
fn _w74() void { match_cp(110); }
fn _w75() void { match_cp(114); }
fn _w76() void { match_cp(116); }
fn _w77() void { match_cp(117); }
fn _w78() void { hex4(); }
fn _w79() void { peg_seq(&[_]PFn{_w77, _w78}); }
fn _w80() void { peg_alt(&[_]PFn{_w69, _w70, _w71, _w72, _w73, _w74, _w75, _w76, _w79}); }
fn _w81() void { hexdig(); }
fn _w82() void { hexdig(); }
fn _w83() void { hexdig(); }
fn _w84() void { hexdig(); }
fn _w85() void { match_range(48, 57); }
fn _w86() void { match_range(97, 102); }
fn _w87() void { match_range(65, 70); }
fn _w88() void { match_cp(45); }
fn _w89() void { opt(_w88); }
fn _w90() void { integer(); }
fn _w91() void { fraction(); }
fn _w92() void { opt(_w91); }
fn _w93() void { exponent(); }
fn _w94() void { opt(_w93); }
fn _w95() void { match_cp(48); }
fn _w96() void { match_range(49, 57); }
fn _w97() void { match_range(48, 57); }
fn _w98() void { peg_star(_w97); }
fn _w99() void { peg_seq(&[_]PFn{_w96, _w98}); }
fn _w100() void { match_cp(46); }
fn _w101() void { match_range(48, 57); }
fn _w102() void { plus_(_w101); }
fn _w103() void { match_cp(101); }
fn _w104() void { match_cp(69); }
fn _w105() void { peg_alt(&[_]PFn{_w103, _w104}); }
fn _w106() void { match_cp(43); }
fn _w107() void { match_cp(45); }
fn _w108() void { peg_alt(&[_]PFn{_w106, _w107}); }
fn _w109() void { opt(_w108); }
fn _w110() void { match_range(48, 57); }
fn _w111() void { plus_(_w110); }
fn _w112() void { match_cp(0x20); }
fn _w113() void { match_cp(0x9); }
fn _w114() void { match_cp(0x0A); }
fn _w115() void { match_cp(0x0D); }
fn _w116() void { peg_alt(&[_]PFn{_w112, _w113, _w114, _w115}); }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
fn json_text() void {
    peg_seq(&[_]PFn{_w1, _w2, _w3, _w4});
}

// [2] VALUE 
fn value() void {
    peg_alt(&[_]PFn{_w5, _w6, _w7, _w8, _w9, _w10, _w11});
}

// [3] OBJECT 
fn object() void {
    peg_alt(&[_]PFn{_w17, _w21});
}

// [4] MEMBERS 
fn members() void {
    peg_seq(&[_]PFn{_w22, _w28});
}

// [5] MEMBER 
fn member() void {
    peg_seq(&[_]PFn{_w29, _w30, _w31, _w32, _w33, _w34, _w35});
}

// [6] ARRAY 
fn array() void {
    peg_alt(&[_]PFn{_w41, _w45});
}

// [7] ELEMENTS 
fn elements() void {
    peg_seq(&[_]PFn{_w46, _w52});
}

// [8] STRING 
fn r_string() void {
    peg_seq(&[_]PFn{_w53, _w55, _w56});
}

// [9] CHAR 
fn r_char() void {
    peg_alt(&[_]PFn{_w57, _w67});
}

// [10] ESCAPED 
fn escaped() void {
    peg_seq(&[_]PFn{_w68, _w80});
}

// [11] HEX4 
fn hex4() void {
    peg_seq(&[_]PFn{_w81, _w82, _w83, _w84});
}

// [12] HEXDIG 
fn hexdig() void {
    peg_alt(&[_]PFn{_w85, _w86, _w87});
}

// [13] NUMBER 
fn number() void {
    peg_seq(&[_]PFn{_w89, _w90, _w92, _w94});
}

// [14] INTEGER 
fn integer() void {
    peg_alt(&[_]PFn{_w95, _w99});
}

// [15] FRACTION 
fn fraction() void {
    peg_seq(&[_]PFn{_w100, _w102});
}

// [16] EXPONENT 
fn exponent() void {
    peg_seq(&[_]PFn{_w105, _w109, _w111});
}

// [17] WS 
fn ws() void {
    peg_star(_w116);
}



