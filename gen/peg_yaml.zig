// ════════════════════════════════════════════════════════════════
// yaml_reader.zig — YAML 1.2 parser
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

// ── YAML extensions ──

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
    l_yaml_stream();
    const stdout = std.io.getStdOut().writer();
    if (!g.failed) {
        try stdout.print("OK: {d} chars\n", .{g.pos});
    } else {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("FAIL @{d}\n", .{g.pos});
        std.process.exit(1);
    }
    }

// ════════════════════════════════════════════════════════════════ 
// Wrapper functions (bash has no closures) 
// ════════════════════════════════════════════════════════════════ 

fn _w1() void { match_cp(0x9); }
fn _w2() void { match_cp(0x0A); }
fn _w3() void { match_cp(0x0D); }
fn _w4() void { match_range(0x20, 0x7E); }
fn _w5() void { match_cp(0x85); }
fn _w6() void { match_range(0xA0, 0xD7FF); }
fn _w7() void { match_range(0xE000, 0xFFFD); }
fn _w8() void { match_range(0x10000, 0x10FFFF); }
fn _w9() void { match_cp(0x9); }
fn _w10() void { match_range(0x20, 0x10FFFF); }
fn _w11() void { match_cp(64); }
fn _w12() void { match_cp(96); }
fn _w13() void { c_sequence_entry(); }
fn _w14() void { c_mapping_key(); }
fn _w15() void { c_mapping_value(); }
fn _w16() void { c_collect_entry(); }
fn _w17() void { c_sequence_start(); }
fn _w18() void { c_sequence_end(); }
fn _w19() void { c_mapping_start(); }
fn _w20() void { c_mapping_end(); }
fn _w21() void { c_comment(); }
fn _w22() void { c_anchor(); }
fn _w23() void { c_alias(); }
fn _w24() void { c_tag(); }
fn _w25() void { c_literal(); }
fn _w26() void { c_folded(); }
fn _w27() void { c_single_quote(); }
fn _w28() void { c_double_quote(); }
fn _w29() void { c_directive(); }
fn _w30() void { c_reserved(); }
fn _w31() void { c_collect_entry(); }
fn _w32() void { c_sequence_start(); }
fn _w33() void { c_sequence_end(); }
fn _w34() void { c_mapping_start(); }
fn _w35() void { c_mapping_end(); }
fn _w36() void { b_line_feed(); }
fn _w37() void { b_carriage_return(); }
fn _w38() void { c_printable(); }
fn _w39() void { b_char(); }
fn _w40() void { c_byte_order_mark(); }
fn _w41() void { peg_alt(&[_]PFn{_w39, _w40}); }
fn _w42() void { b_carriage_return(); }
fn _w43() void { b_line_feed(); }
fn _w44() void { peg_seq(&[_]PFn{_w42, _w43}); }
fn _w45() void { b_carriage_return(); }
fn _w46() void { b_line_feed(); }
fn _w47() void { s_space(); }
fn _w48() void { s_tab(); }
fn _w49() void { nb_char(); }
fn _w50() void { s_white(); }
fn _w51() void { ns_dec_digit(); }
fn _w52() void { match_range(0x41, 0x46); }
fn _w53() void { match_range(0x61, 0x66); }
fn _w54() void { match_range(0x41, 0x5A); }
fn _w55() void { match_range(0x61, 0x7A); }
fn _w56() void { ns_dec_digit(); }
fn _w57() void { ns_ascii_letter(); }
fn _w58() void { match_cp(45); }
fn _w59() void { match_cp(37); }
fn _w60() void { ns_hex_digit(); }
fn _w61() void { ns_hex_digit(); }
fn _w62() void { peg_seq(&[_]PFn{_w59, _w60, _w61}); }
fn _w63() void { ns_word_char(); }
fn _w64() void { match_cp(35); }
fn _w65() void { match_cp(59); }
fn _w66() void { match_cp(47); }
fn _w67() void { match_cp(63); }
fn _w68() void { match_cp(58); }
fn _w69() void { match_cp(64); }
fn _w70() void { match_cp(38); }
fn _w71() void { match_cp(61); }
fn _w72() void { match_cp(43); }
fn _w73() void { match_cp(36); }
fn _w74() void { match_cp(44); }
fn _w75() void { match_cp(95); }
fn _w76() void { match_cp(46); }
fn _w77() void { match_cp(33); }
fn _w78() void { match_cp(126); }
fn _w79() void { match_cp(42); }
fn _w80() void { match_cp(39); }
fn _w81() void { match_cp(40); }
fn _w82() void { match_cp(41); }
fn _w83() void { match_cp(91); }
fn _w84() void { match_cp(93); }
fn _w85() void { ns_uri_char(); }
fn _w86() void { c_tag(); }
fn _w87() void { c_flow_indicator(); }
fn _w88() void { peg_alt(&[_]PFn{_w86, _w87}); }
fn _w89() void { match_cp(120); }
fn _w90() void { ns_hex_digit(); }
fn _w91() void { rep_fn(2, _w90); }
fn _w92() void { match_cp(117); }
fn _w93() void { ns_hex_digit(); }
fn _w94() void { rep_fn(4, _w93); }
fn _w95() void { match_cp(85); }
fn _w96() void { ns_hex_digit(); }
fn _w97() void { rep_fn(8, _w96); }
fn _w98() void { c_escape(); }
fn _w99() void { ns_esc_null(); }
fn _w100() void { ns_esc_bell(); }
fn _w101() void { ns_esc_backspace(); }
fn _w102() void { ns_esc_horizontal_tab(); }
fn _w103() void { ns_esc_line_feed(); }
fn _w104() void { ns_esc_vertical_tab(); }
fn _w105() void { ns_esc_form_feed(); }
fn _w106() void { ns_esc_carriage_return(); }
fn _w107() void { ns_esc_escape(); }
fn _w108() void { ns_esc_space(); }
fn _w109() void { ns_esc_double_quote(); }
fn _w110() void { ns_esc_slash(); }
fn _w111() void { ns_esc_backslash(); }
fn _w112() void { ns_esc_next_line(); }
fn _w113() void { ns_esc_non_breaking_space(); }
fn _w114() void { ns_esc_line_separator(); }
fn _w115() void { ns_esc_paragraph_separator(); }
fn _w116() void { ns_esc_8_bit(); }
fn _w117() void { ns_esc_16_bit(); }
fn _w118() void { ns_esc_32_bit(); }
fn _w119() void { peg_alt(&[_]PFn{_w99, _w100, _w101, _w102, _w103, _w104, _w105, _w106, _w107, _w108, _w109, _w110, _w111, _w112, _w113, _w114, _w115, _w116, _w117, _w118}); }
fn _w120() void { s_space(); }
fn _w121() void { s_space(); }
fn _w122() void { s_space(); }
fn _w123() void { s_white(); }
fn _w124() void { plus_(_w123); }
fn _w125() void { ok_r(); }
fn _w126() void { g.n = g.n; s_block_line_prefix(); }
fn _w127() void { g.n = g.n; s_block_line_prefix(); }
fn _w128() void { g.n = g.n; s_flow_line_prefix(); }
fn _w129() void { g.n = g.n; s_flow_line_prefix(); }
fn _w130() void { g.n = g.n; s_indent(); }
fn _w131() void { s_separate_in_line(); }
fn _w132() void { opt(_w131); }
fn _w133() void { g.n = g.n; g.c = g.c; s_line_prefix(); }
fn _w134() void { g.n = g.n; s_indent_lt(); }
fn _w135() void { peg_alt(&[_]PFn{_w133, _w134}); }
fn _w136() void { b_as_line_feed(); }
fn _w137() void { b_non_content(); }
fn _w138() void { g.n = g.n; g.c = g.c; l_empty(); }
fn _w139() void { plus_(_w138); }
fn _w140() void { g.n = g.n; g.c = g.c; b_l_trimmed(); }
fn _w141() void { b_as_space(); }
fn _w142() void { s_separate_in_line(); }
fn _w143() void { opt(_w142); }
fn _w144() void { g.n = g.n; g.c = "FLOW-IN"; b_l_folded(); }
fn _w145() void { g.n = g.n; s_flow_line_prefix(); }
fn _w146() void { c_comment(); }
fn _w147() void { nb_char(); }
fn _w148() void { peg_star(_w147); }
fn _w149() void { b_non_content(); }
fn _w150() void { ok_r(); }
fn _w151() void { s_separate_in_line(); }
fn _w152() void { c_nb_comment_text(); }
fn _w153() void { opt(_w152); }
fn _w154() void { peg_seq(&[_]PFn{_w151, _w153}); }
fn _w155() void { opt(_w154); }
fn _w156() void { b_comment(); }
fn _w157() void { s_separate_in_line(); }
fn _w158() void { c_nb_comment_text(); }
fn _w159() void { opt(_w158); }
fn _w160() void { b_non_content(); }
fn _w161() void { s_b_comment(); }
fn _w162() void { ok_r(); }
fn _w163() void { peg_alt(&[_]PFn{_w161, _w162}); }
fn _w164() void { l_comment(); }
fn _w165() void { peg_star(_w164); }
fn _w166() void { g.n = g.n; s_separate_lines(); }
fn _w167() void { g.n = g.n; s_separate_lines(); }
fn _w168() void { g.n = g.n; s_separate_lines(); }
fn _w169() void { g.n = g.n; s_separate_lines(); }
fn _w170() void { s_separate_in_line(); }
fn _w171() void { s_separate_in_line(); }
fn _w172() void { s_l_comments(); }
fn _w173() void { g.n = g.n; s_flow_line_prefix(); }
fn _w174() void { peg_seq(&[_]PFn{_w172, _w173}); }
fn _w175() void { s_separate_in_line(); }
fn _w176() void { c_directive(); }
fn _w177() void { ns_yaml_directive(); }
fn _w178() void { ns_tag_directive(); }
fn _w179() void { ns_reserved_directive(); }
fn _w180() void { peg_alt(&[_]PFn{_w177, _w178, _w179}); }
fn _w181() void { s_l_comments(); }
fn _w182() void { ns_directive_name(); }
fn _w183() void { s_separate_in_line(); }
fn _w184() void { ns_directive_parameter(); }
fn _w185() void { peg_seq(&[_]PFn{_w183, _w184}); }
fn _w186() void { peg_star(_w185); }
fn _w187() void { ns_char(); }
fn _w188() void { ns_char(); }
fn _w189() void { match_str("YAML"); }
fn _w190() void { s_separate_in_line(); }
fn _w191() void { ns_yaml_version(); }
fn _w192() void { ns_dec_digit(); }
fn _w193() void { plus_(_w192); }
fn _w194() void { match_cp(46); }
fn _w195() void { ns_dec_digit(); }
fn _w196() void { plus_(_w195); }
fn _w197() void { match_str("TAG"); }
fn _w198() void { s_separate_in_line(); }
fn _w199() void { c_tag_handle(); }
fn _w200() void { s_separate_in_line(); }
fn _w201() void { ns_tag_prefix(); }
fn _w202() void { c_named_tag_handle(); }
fn _w203() void { c_secondary_tag_handle(); }
fn _w204() void { c_primary_tag_handle(); }
fn _w205() void { match_cp(33); }
fn _w206() void { ns_word_char(); }
fn _w207() void { plus_(_w206); }
fn _w208() void { match_cp(33); }
fn _w209() void { c_ns_local_tag_prefix(); }
fn _w210() void { ns_global_tag_prefix(); }
fn _w211() void { match_cp(33); }
fn _w212() void { ns_uri_char(); }
fn _w213() void { peg_star(_w212); }
fn _w214() void { ns_tag_char(); }
fn _w215() void { ns_uri_char(); }
fn _w216() void { peg_star(_w215); }
fn _w217() void { c_ns_tag_property(); }
fn _w218() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w219() void { c_ns_anchor_property(); }
fn _w220() void { peg_seq(&[_]PFn{_w218, _w219}); }
fn _w221() void { opt(_w220); }
fn _w222() void { peg_seq(&[_]PFn{_w217, _w221}); }
fn _w223() void { c_ns_anchor_property(); }
fn _w224() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w225() void { c_ns_tag_property(); }
fn _w226() void { peg_seq(&[_]PFn{_w224, _w225}); }
fn _w227() void { opt(_w226); }
fn _w228() void { peg_seq(&[_]PFn{_w223, _w227}); }
fn _w229() void { c_verbatim_tag(); }
fn _w230() void { c_ns_shorthand_tag(); }
fn _w231() void { c_non_specific_tag(); }
fn _w232() void { match_str("!<"); }
fn _w233() void { ns_uri_char(); }
fn _w234() void { plus_(_w233); }
fn _w235() void { match_cp(62); }
fn _w236() void { c_tag_handle(); }
fn _w237() void { ns_tag_char(); }
fn _w238() void { plus_(_w237); }
fn _w239() void { c_anchor(); }
fn _w240() void { ns_anchor_name(); }
fn _w241() void { scalar_fn(_w240); }
fn _w242() void { peg_seq(&[_]PFn{_w239, _w241}); }
fn _w243() void { ns_char(); }
fn _w244() void { c_flow_indicator(); }
fn _w245() void { ns_anchor_char(); }
fn _w246() void { c_alias(); }
fn _w247() void { ns_anchor_name(); }
fn _w248() void { scalar_fn(_w247); }
fn _w249() void { peg_seq(&[_]PFn{_w246, _w248}); }
fn _w250() void { c_ns_esc_char(); }
fn _w251() void { nb_json(); }
fn _w252() void { match_cp(92); }
fn _w253() void { match_cp(34); }
fn _w254() void { peg_alt(&[_]PFn{_w252, _w253}); }
fn _w255() void { minus_fn(_w251, _w254); }
fn _w256() void { nb_double_char(); }
fn _w257() void { s_white(); }
fn _w258() void { match_cp(34); }
fn _w259() void { g.n = g.n; g.c = g.c; nb_double_text(); }
fn _w260() void { match_cp(34); }
fn _w261() void { peg_seq(&[_]PFn{_w258, _w259, _w260}); }
fn _w262() void { g.n = g.n; nb_double_multi_line(); }
fn _w263() void { g.n = g.n; nb_double_multi_line(); }
fn _w264() void { nb_double_one_line(); }
fn _w265() void { nb_double_one_line(); }
fn _w266() void { nb_double_char(); }
fn _w267() void { s_white(); }
fn _w268() void { peg_star(_w267); }
fn _w269() void { match_cp(92); }
fn _w270() void { b_non_content(); }
fn _w271() void { g.n = g.n; g.c = "FLOW-IN"; l_empty(); }
fn _w272() void { peg_star(_w271); }
fn _w273() void { g.n = g.n; s_flow_line_prefix(); }
fn _w274() void { g.n = g.n; s_double_escaped(); }
fn _w275() void { g.n = g.n; s_flow_folded(); }
fn _w276() void { s_white(); }
fn _w277() void { peg_star(_w276); }
fn _w278() void { ns_double_char(); }
fn _w279() void { peg_seq(&[_]PFn{_w277, _w278}); }
fn _w280() void { g.n = g.n; s_double_break(); }
fn _w281() void { ns_double_char(); }
fn _w282() void { nb_ns_double_in_line(); }
fn _w283() void { g.n = g.n; s_double_next_line(); }
fn _w284() void { s_white(); }
fn _w285() void { peg_star(_w284); }
fn _w286() void { peg_alt(&[_]PFn{_w283, _w285}); }
fn _w287() void { peg_seq(&[_]PFn{_w281, _w282, _w286}); }
fn _w288() void { opt(_w287); }
fn _w289() void { nb_ns_double_in_line(); }
fn _w290() void { g.n = g.n; s_double_next_line(); }
fn _w291() void { s_white(); }
fn _w292() void { peg_star(_w291); }
fn _w293() void { peg_alt(&[_]PFn{_w290, _w292}); }
fn _w294() void { c_quoted_quote(); }
fn _w295() void { nb_json(); }
fn _w296() void { match_cp(39); }
fn _w297() void { minus_fn(_w295, _w296); }
fn _w298() void { nb_single_char(); }
fn _w299() void { s_white(); }
fn _w300() void { match_cp(39); }
fn _w301() void { g.n = g.n; g.c = g.c; nb_single_text(); }
fn _w302() void { match_cp(39); }
fn _w303() void { peg_seq(&[_]PFn{_w300, _w301, _w302}); }
fn _w304() void { g.n = g.n; nb_single_multi_line(); }
fn _w305() void { g.n = g.n; nb_single_multi_line(); }
fn _w306() void { nb_single_one_line(); }
fn _w307() void { nb_single_one_line(); }
fn _w308() void { nb_single_char(); }
fn _w309() void { s_white(); }
fn _w310() void { peg_star(_w309); }
fn _w311() void { ns_single_char(); }
fn _w312() void { peg_seq(&[_]PFn{_w310, _w311}); }
fn _w313() void { g.n = g.n; s_flow_folded(); }
fn _w314() void { ns_single_char(); }
fn _w315() void { ns_single_in_line(); }
fn _w316() void { g.n = g.n; s_single_next_line(); }
fn _w317() void { s_white(); }
fn _w318() void { peg_star(_w317); }
fn _w319() void { peg_alt(&[_]PFn{_w316, _w318}); }
fn _w320() void { peg_seq(&[_]PFn{_w314, _w315, _w319}); }
fn _w321() void { opt(_w320); }
fn _w322() void { ns_single_in_line(); }
fn _w323() void { g.n = g.n; s_single_next_line(); }
fn _w324() void { s_white(); }
fn _w325() void { peg_star(_w324); }
fn _w326() void { peg_alt(&[_]PFn{_w323, _w325}); }
fn _w327() void { ns_char(); }
fn _w328() void { c_indicator(); }
fn _w329() void { minus_fn(_w327, _w328); }
fn _w330() void { match_cp(63); }
fn _w331() void { match_cp(58); }
fn _w332() void { match_cp(45); }
fn _w333() void { peg_alt(&[_]PFn{_w330, _w331, _w332}); }
fn _w334() void { g.c = g.c; ns_plain_safe(); }
fn _w335() void { ahead(_w334); }
fn _w336() void { peg_seq(&[_]PFn{_w333, _w335}); }
fn _w337() void { ns_plain_safe_out(); }
fn _w338() void { ns_plain_safe_in(); }
fn _w339() void { ns_plain_safe_out(); }
fn _w340() void { ns_plain_safe_in(); }
fn _w341() void { ns_char(); }
fn _w342() void { c_flow_indicator(); }
fn _w343() void { g.c = g.c; ns_plain_safe(); }
fn _w344() void { match_cp(58); }
fn _w345() void { match_cp(35); }
fn _w346() void { peg_alt(&[_]PFn{_w344, _w345}); }
fn _w347() void { minus_fn(_w343, _w346); }
fn _w348() void { ns_char(); }
fn _w349() void { behind(_w348); }
fn _w350() void { match_cp(35); }
fn _w351() void { peg_seq(&[_]PFn{_w349, _w350}); }
fn _w352() void { match_cp(58); }
fn _w353() void { g.c = g.c; ns_plain_safe(); }
fn _w354() void { ahead(_w353); }
fn _w355() void { peg_seq(&[_]PFn{_w352, _w354}); }
fn _w356() void { g.n = g.n; g.c = g.c; ns_plain_multi_line(); }
fn _w357() void { g.n = g.n; g.c = g.c; ns_plain_multi_line(); }
fn _w358() void { g.c = g.c; ns_plain_one_line(); }
fn _w359() void { g.c = g.c; ns_plain_one_line(); }
fn _w360() void { switch_ctx(g.c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w356 },
        .{ .ctx = "FLOW-IN", .func = _w357 },
        .{ .ctx = "BLOCK-KEY", .func = _w358 },
        .{ .ctx = "FLOW-KEY", .func = _w359 },
    }); }
fn _w361() void { s_white(); }
fn _w362() void { peg_star(_w361); }
fn _w363() void { g.c = g.c; ns_plain_char(); }
fn _w364() void { peg_seq(&[_]PFn{_w362, _w363}); }
fn _w365() void { g.c = g.c; ns_plain_first(); }
fn _w366() void { g.c = g.c; nb_ns_plain_in_line(); }
fn _w367() void { g.n = g.n; s_flow_folded(); }
fn _w368() void { c_forbidden(); }
fn _w369() void { r_neg(_w368); }
fn _w370() void { g.c = g.c; ns_plain_char(); }
fn _w371() void { g.c = g.c; nb_ns_plain_in_line(); }
fn _w372() void { g.c = g.c; ns_plain_one_line(); }
fn _w373() void { g.n = g.n; g.c = g.c; s_ns_plain_next_line(); }
fn _w374() void { peg_star(_w373); }
fn _w375() void { match_cp(91); }
fn _w376() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w377() void { opt(_w376); }
fn _w378() void { g.n = g.n; g.c = in_flow(g.c); ns_s_flow_seq_entries(); }
fn _w379() void { collect_fn(_w378); }
fn _w380() void { opt(_w379); }
fn _w381() void { match_cp(93); }
fn _w382() void { peg_seq(&[_]PFn{_w375, _w377, _w380, _w381}); }
fn _w383() void { g.n = g.n; g.c = g.c; ns_flow_seq_entry(); }
fn _w384() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w385() void { opt(_w384); }
fn _w386() void { match_cp(44); }
fn _w387() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w388() void { opt(_w387); }
fn _w389() void { g.n = g.n; g.c = g.c; ns_s_flow_seq_entries(); }
fn _w390() void { opt(_w389); }
fn _w391() void { peg_seq(&[_]PFn{_w386, _w388, _w390}); }
fn _w392() void { opt(_w391); }
fn _w393() void { g.n = g.n; g.c = g.c; ns_flow_pair(); }
fn _w394() void { g.n = g.n; g.c = g.c; ns_flow_node(); }
fn _w395() void { match_cp(123); }
fn _w396() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w397() void { opt(_w396); }
fn _w398() void { g.n = g.n; g.c = in_flow(g.c); ns_s_flow_map_entries(); }
fn _w399() void { collect_fn(_w398); }
fn _w400() void { opt(_w399); }
fn _w401() void { match_cp(125); }
fn _w402() void { peg_seq(&[_]PFn{_w395, _w397, _w400, _w401}); }
fn _w403() void { g.n = g.n; g.c = g.c; ns_flow_map_entry(); }
fn _w404() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w405() void { opt(_w404); }
fn _w406() void { match_cp(44); }
fn _w407() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w408() void { opt(_w407); }
fn _w409() void { g.n = g.n; g.c = g.c; ns_s_flow_map_entries(); }
fn _w410() void { opt(_w409); }
fn _w411() void { peg_seq(&[_]PFn{_w406, _w408, _w410}); }
fn _w412() void { opt(_w411); }
fn _w413() void { match_cp(63); }
fn _w414() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w415() void { g.n = g.n; g.c = g.c; ns_flow_map_explicit_entry(); }
fn _w416() void { peg_seq(&[_]PFn{_w413, _w414, _w415}); }
fn _w417() void { g.n = g.n; g.c = g.c; ns_flow_map_implicit_entry(); }
fn _w418() void { g.n = g.n; g.c = g.c; ns_flow_map_implicit_entry(); }
fn _w419() void { e_node(); }
fn _w420() void { e_node(); }
fn _w421() void { peg_seq(&[_]PFn{_w419, _w420}); }
fn _w422() void { g.n = g.n; g.c = g.c; ns_flow_map_yaml_key_entry(); }
fn _w423() void { g.n = g.n; g.c = g.c; c_ns_flow_map_empty_key_entry(); }
fn _w424() void { g.n = g.n; g.c = g.c; c_ns_flow_map_json_key_entry(); }
fn _w425() void { peg_alt(&[_]PFn{_w422, _w423, _w424}); }
fn _w426() void { g.n = g.n; g.c = g.c; ns_flow_yaml_node(); }
fn _w427() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w428() void { opt(_w427); }
fn _w429() void { g.n = g.n; g.c = g.c; c_ns_flow_map_separate_value(); }
fn _w430() void { peg_seq(&[_]PFn{_w428, _w429}); }
fn _w431() void { e_node(); }
fn _w432() void { peg_alt(&[_]PFn{_w430, _w431}); }
fn _w433() void { e_node(); }
fn _w434() void { g.n = g.n; g.c = g.c; c_ns_flow_map_separate_value(); }
fn _w435() void { match_cp(58); }
fn _w436() void { g.c = g.c; ns_plain_safe(); }
fn _w437() void { r_neg(_w436); }
fn _w438() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w439() void { g.n = g.n; g.c = g.c; ns_flow_node(); }
fn _w440() void { peg_seq(&[_]PFn{_w438, _w439}); }
fn _w441() void { e_node(); }
fn _w442() void { peg_alt(&[_]PFn{_w440, _w441}); }
fn _w443() void { g.n = g.n; g.c = g.c; c_flow_json_node(); }
fn _w444() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w445() void { opt(_w444); }
fn _w446() void { g.n = g.n; g.c = g.c; c_ns_flow_map_adjacent_value(); }
fn _w447() void { peg_seq(&[_]PFn{_w445, _w446}); }
fn _w448() void { e_node(); }
fn _w449() void { peg_alt(&[_]PFn{_w447, _w448}); }
fn _w450() void { match_cp(58); }
fn _w451() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w452() void { opt(_w451); }
fn _w453() void { g.n = g.n; g.c = g.c; ns_flow_node(); }
fn _w454() void { peg_seq(&[_]PFn{_w452, _w453}); }
fn _w455() void { e_node(); }
fn _w456() void { peg_alt(&[_]PFn{_w454, _w455}); }
fn _w457() void { match_cp(63); }
fn _w458() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w459() void { g.n = g.n; g.c = g.c; ns_flow_map_explicit_entry(); }
fn _w460() void { peg_seq(&[_]PFn{_w457, _w458, _w459}); }
fn _w461() void { g.n = g.n; g.c = g.c; ns_flow_pair_entry(); }
fn _w462() void { g.n = g.n; g.c = g.c; ns_flow_pair_yaml_key_entry(); }
fn _w463() void { g.n = g.n; g.c = g.c; c_ns_flow_map_empty_key_entry(); }
fn _w464() void { g.n = g.n; g.c = g.c; c_ns_flow_pair_json_key_entry(); }
fn _w465() void { g.c = "FLOW-KEY"; ns_s_implicit_yaml_key(); }
fn _w466() void { g.n = g.n; g.c = g.c; c_ns_flow_map_separate_value(); }
fn _w467() void { g.c = "FLOW-KEY"; c_s_implicit_json_key(); }
fn _w468() void { g.n = g.n; g.c = g.c; c_ns_flow_map_adjacent_value(); }
fn _w469() void { g.n = 0; g.c = g.c; ns_flow_yaml_node(); }
fn _w470() void { s_separate_in_line(); }
fn _w471() void { opt(_w470); }
fn _w472() void { g.n = 0; g.c = g.c; c_flow_json_node(); }
fn _w473() void { s_separate_in_line(); }
fn _w474() void { opt(_w473); }
fn _w475() void { g.n = g.n; g.c = g.c; c_flow_sequence(); }
fn _w476() void { g.n = g.n; g.c = g.c; c_flow_mapping(); }
fn _w477() void { g.n = g.n; g.c = g.c; c_single_quoted(); }
fn _w478() void { g.n = g.n; g.c = g.c; c_double_quoted(); }
fn _w479() void { g.n = g.n; g.c = g.c; ns_flow_yaml_content(); }
fn _w480() void { g.n = g.n; g.c = g.c; c_flow_json_content(); }
fn _w481() void { c_ns_alias_node(); }
fn _w482() void { g.n = g.n; g.c = g.c; ns_flow_yaml_content(); }
fn _w483() void { g.n = g.n; g.c = g.c; c_ns_properties(); }
fn _w484() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w485() void { g.n = g.n; g.c = g.c; ns_flow_yaml_content(); }
fn _w486() void { peg_seq(&[_]PFn{_w484, _w485}); }
fn _w487() void { e_scalar(); }
fn _w488() void { peg_alt(&[_]PFn{_w486, _w487}); }
fn _w489() void { peg_seq(&[_]PFn{_w483, _w488}); }
fn _w490() void { g.n = g.n; g.c = g.c; c_ns_properties(); }
fn _w491() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w492() void { peg_seq(&[_]PFn{_w490, _w491}); }
fn _w493() void { opt(_w492); }
fn _w494() void { g.n = g.n; g.c = g.c; c_flow_json_content(); }
fn _w495() void { c_ns_alias_node(); }
fn _w496() void { g.n = g.n; g.c = g.c; ns_flow_content(); }
fn _w497() void { g.n = g.n; g.c = g.c; c_ns_properties(); }
fn _w498() void { g.n = g.n; g.c = g.c; s_separate(); }
fn _w499() void { g.n = g.n; g.c = g.c; ns_flow_content(); }
fn _w500() void { peg_seq(&[_]PFn{_w498, _w499}); }
fn _w501() void { e_scalar(); }
fn _w502() void { peg_alt(&[_]PFn{_w500, _w501}); }
fn _w503() void { peg_seq(&[_]PFn{_w497, _w502}); }
fn _w504() void { ns_dec_digit(); }
fn _w505() void { parse_int_fn(_w504); }
fn _w506() void { detect_indent(g.n); }
fn _w507() void { match_cp(45); }
fn _w508() void { parse_sym_fn(_w507, "STRIP"); }
fn _w509() void { match_cp(43); }
fn _w510() void { parse_sym_fn(_w509, "KEEP"); }
fn _w511() void { val_fn("CLIP"); }
fn _w512() void { blk2: { peg_alt(&[_]PFn{_w505, _w506}); if (g.failed) break :blk2; g.m = g.rtagint; save_inp(); blk1: { peg_alt(&[_]PFn{_w508, _w510, _w511}); if (g.failed) break :blk1; g.t = g.rtag; save_inp(); s_b_comment(); } } }
fn _w513() void { match_cp(45); }
fn _w514() void { parse_sym_fn(_w513, "STRIP"); }
fn _w515() void { match_cp(43); }
fn _w516() void { parse_sym_fn(_w515, "KEEP"); }
fn _w517() void { val_fn("CLIP"); }
fn _w518() void { ns_dec_digit(); }
fn _w519() void { parse_int_fn(_w518); }
fn _w520() void { detect_indent(g.n); }
fn _w521() void { blk4: { peg_alt(&[_]PFn{_w514, _w516, _w517}); if (g.failed) break :blk4; g.t = g.rtag; save_inp(); blk3: { peg_alt(&[_]PFn{_w519, _w520}); if (g.failed) break :blk3; g.m = g.rtagint; save_inp(); s_b_comment(); } } }
fn _w522() void { ns_dec_digit(); }
fn _w523() void { ok_r(); }
fn _w524() void { match_cp(45); }
fn _w525() void { match_cp(43); }
fn _w526() void { ok_r(); }
fn _w527() void { b_non_content(); }
fn _w528() void { b_as_line_feed(); }
fn _w529() void { b_as_line_feed(); }
fn _w530() void { g.n = g.n; l_strip_empty(); }
fn _w531() void { g.n = g.n; l_strip_empty(); }
fn _w532() void { g.n = g.n; l_keep_empty(); }
fn _w533() void { g.n = g.n; s_indent_le(); }
fn _w534() void { b_non_content(); }
fn _w535() void { peg_seq(&[_]PFn{_w533, _w534}); }
fn _w536() void { peg_star(_w535); }
fn _w537() void { g.n = g.n; l_trail_comments(); }
fn _w538() void { opt(_w537); }
fn _w539() void { g.n = g.n; g.c = "BLOCK-IN"; l_empty(); }
fn _w540() void { peg_star(_w539); }
fn _w541() void { g.n = g.n; l_trail_comments(); }
fn _w542() void { opt(_w541); }
fn _w543() void { g.n = g.n; s_indent_lt(); }
fn _w544() void { c_nb_comment_text(); }
fn _w545() void { b_comment(); }
fn _w546() void { l_comment(); }
fn _w547() void { peg_star(_w546); }
fn _w548() void { match_cp(124); }
fn _w549() void { ns_dec_digit(); }
fn _w550() void { parse_int_fn(_w549); }
fn _w551() void { detect_indent(g.n); }
fn _w552() void { match_cp(45); }
fn _w553() void { parse_sym_fn(_w552, "STRIP"); }
fn _w554() void { match_cp(43); }
fn _w555() void { parse_sym_fn(_w554, "KEEP"); }
fn _w556() void { val_fn("CLIP"); }
fn _w557() void { s_b_comment(); }
fn _w558() void { g.n = (g.n + g.m); g.t = g.t; l_literal_content(); }
fn _w559() void { blk6: { peg_alt(&[_]PFn{_w550, _w551}); if (g.failed) break :blk6; g.m = g.rtagint; save_inp(); blk5: { peg_alt(&[_]PFn{_w553, _w555, _w556}); if (g.failed) break :blk5; g.t = g.rtag; save_inp(); peg_seq(&[_]PFn{_w557, _w558}); } } }
fn _w560() void { g.n = g.n; g.c = "BLOCK-IN"; l_empty(); }
fn _w561() void { peg_star(_w560); }
fn _w562() void { g.n = g.n; s_indent(); }
fn _w563() void { nb_char(); }
fn _w564() void { plus_(_w563); }
fn _w565() void { b_as_line_feed(); }
fn _w566() void { g.n = g.n; l_nb_literal_text(); }
fn _w567() void { g.n = g.n; l_nb_literal_text(); }
fn _w568() void { g.n = g.n; b_nb_literal_next(); }
fn _w569() void { peg_star(_w568); }
fn _w570() void { g.t = g.t; b_chomped_last(); }
fn _w571() void { peg_seq(&[_]PFn{_w567, _w569, _w570}); }
fn _w572() void { opt(_w571); }
fn _w573() void { g.n = g.n; g.t = g.t; l_chomped_empty(); }
fn _w574() void { peg_seq(&[_]PFn{_w572, _w573}); }
fn _w575() void { match_cp(62); }
fn _w576() void { ns_dec_digit(); }
fn _w577() void { parse_int_fn(_w576); }
fn _w578() void { detect_indent(g.n); }
fn _w579() void { match_cp(45); }
fn _w580() void { parse_sym_fn(_w579, "STRIP"); }
fn _w581() void { match_cp(43); }
fn _w582() void { parse_sym_fn(_w581, "KEEP"); }
fn _w583() void { val_fn("CLIP"); }
fn _w584() void { s_b_comment(); }
fn _w585() void { g.n = (g.n + g.m); g.t = g.t; l_folded_content(); }
fn _w586() void { blk8: { peg_alt(&[_]PFn{_w577, _w578}); if (g.failed) break :blk8; g.m = g.rtagint; save_inp(); blk7: { peg_alt(&[_]PFn{_w580, _w582, _w583}); if (g.failed) break :blk7; g.t = g.rtag; save_inp(); peg_seq(&[_]PFn{_w584, _w585}); } } }
fn _w587() void { g.n = g.n; s_indent(); }
fn _w588() void { ns_char(); }
fn _w589() void { nb_char(); }
fn _w590() void { peg_star(_w589); }
fn _w591() void { g.n = g.n; s_nb_folded_text(); }
fn _w592() void { g.n = g.n; g.c = "BLOCK-IN"; b_l_folded(); }
fn _w593() void { g.n = g.n; s_nb_folded_text(); }
fn _w594() void { peg_seq(&[_]PFn{_w592, _w593}); }
fn _w595() void { peg_star(_w594); }
fn _w596() void { g.n = g.n; s_indent(); }
fn _w597() void { s_white(); }
fn _w598() void { nb_char(); }
fn _w599() void { peg_star(_w598); }
fn _w600() void { b_as_line_feed(); }
fn _w601() void { g.n = g.n; g.c = "BLOCK-IN"; l_empty(); }
fn _w602() void { peg_star(_w601); }
fn _w603() void { g.n = g.n; s_nb_spaced_text(); }
fn _w604() void { g.n = g.n; b_l_spaced(); }
fn _w605() void { g.n = g.n; s_nb_spaced_text(); }
fn _w606() void { peg_seq(&[_]PFn{_w604, _w605}); }
fn _w607() void { peg_star(_w606); }
fn _w608() void { g.n = g.n; g.c = "BLOCK-IN"; l_empty(); }
fn _w609() void { peg_star(_w608); }
fn _w610() void { g.n = g.n; l_nb_folded_lines(); }
fn _w611() void { g.n = g.n; l_nb_spaced_lines(); }
fn _w612() void { peg_alt(&[_]PFn{_w610, _w611}); }
fn _w613() void { g.n = g.n; l_nb_same_lines(); }
fn _w614() void { b_as_line_feed(); }
fn _w615() void { g.n = g.n; l_nb_same_lines(); }
fn _w616() void { peg_seq(&[_]PFn{_w614, _w615}); }
fn _w617() void { peg_star(_w616); }
fn _w618() void { g.n = g.n; l_nb_diff_lines(); }
fn _w619() void { g.t = g.t; b_chomped_last(); }
fn _w620() void { peg_seq(&[_]PFn{_w618, _w619}); }
fn _w621() void { opt(_w620); }
fn _w622() void { g.n = g.n; g.t = g.t; l_chomped_empty(); }
fn _w623() void { peg_seq(&[_]PFn{_w621, _w622}); }
fn _w624() void { g.n = (g.n + g.m); s_indent(); }
fn _w625() void { g.n = (g.n + g.m); c_l_block_seq_entry(); }
fn _w626() void { peg_seq(&[_]PFn{_w624, _w625}); }
fn _w627() void { plus_(_w626); }
fn _w628() void { blk9: { detect_indent(g.n); if (g.failed) break :blk9; g.m = g.rtagint; save_inp(); collect_fn(_w627); } }
fn _w629() void { match_cp(45); }
fn _w630() void { ns_char(); }
fn _w631() void { r_neg(_w630); }
fn _w632() void { g.n = g.n; g.c = "BLOCK-IN"; s_lblock_indented(); }
fn _w633() void { g.n = g.m; s_indent(); }
fn _w634() void { g.n = (g.n + 1 + g.m); ns_l_compact_sequence(); }
fn _w635() void { g.n = (g.n + 1 + g.m); ns_l_compact_mapping(); }
fn _w636() void { peg_alt(&[_]PFn{_w634, _w635}); }
fn _w637() void { blk10: { detect_indent(0); if (g.failed) break :blk10; g.m = g.rtagint; save_inp(); peg_seq(&[_]PFn{_w633, _w636}); } }
fn _w638() void { g.n = g.n; g.c = g.c; s_lblock_node(); }
fn _w639() void { e_node(); }
fn _w640() void { s_l_comments(); }
fn _w641() void { peg_seq(&[_]PFn{_w639, _w640}); }
fn _w642() void { g.n = g.n; c_l_block_seq_entry(); }
fn _w643() void { g.n = g.n; s_indent(); }
fn _w644() void { g.n = g.n; c_l_block_seq_entry(); }
fn _w645() void { peg_seq(&[_]PFn{_w643, _w644}); }
fn _w646() void { peg_star(_w645); }
fn _w647() void { g.n = (g.n + g.m); s_indent(); }
fn _w648() void { g.n = (g.n + g.m); ns_l_block_map_entry(); }
fn _w649() void { peg_seq(&[_]PFn{_w647, _w648}); }
fn _w650() void { plus_(_w649); }
fn _w651() void { blk11: { detect_indent(g.n); if (g.failed) break :blk11; g.m = g.rtagint; save_inp(); collect_fn(_w650); } }
fn _w652() void { g.n = g.n; c_l_block_map_explicit_entry(); }
fn _w653() void { g.n = g.n; ns_l_block_map_implicit_entry(); }
fn _w654() void { g.n = g.n; c_l_block_map_explicit_key(); }
fn _w655() void { g.n = g.n; l_block_map_explicit_value(); }
fn _w656() void { e_node(); }
fn _w657() void { peg_alt(&[_]PFn{_w655, _w656}); }
fn _w658() void { match_cp(63); }
fn _w659() void { g.n = g.n; g.c = "BLOCK-OUT"; s_lblock_indented(); }
fn _w660() void { g.n = g.n; s_indent(); }
fn _w661() void { match_cp(58); }
fn _w662() void { g.n = g.n; g.c = "BLOCK-OUT"; s_lblock_indented(); }
fn _w663() void { ns_s_block_map_implicit_key(); }
fn _w664() void { e_node(); }
fn _w665() void { peg_alt(&[_]PFn{_w663, _w664}); }
fn _w666() void { scalar_fn(_w665); }
fn _w667() void { g.n = g.n; c_l_block_map_implicit_value(); }
fn _w668() void { peg_seq(&[_]PFn{_w666, _w667}); }
fn _w669() void { g.c = "BLOCK-KEY"; c_s_implicit_json_key(); }
fn _w670() void { g.c = "BLOCK-KEY"; ns_s_implicit_yaml_key(); }
fn _w671() void { match_cp(58); }
fn _w672() void { g.n = g.n; g.c = "BLOCK-OUT"; s_lblock_node(); }
fn _w673() void { e_node(); }
fn _w674() void { s_l_comments(); }
fn _w675() void { peg_seq(&[_]PFn{_w673, _w674}); }
fn _w676() void { scalar_fn(_w675); }
fn _w677() void { peg_alt(&[_]PFn{_w672, _w676}); }
fn _w678() void { g.n = g.n; ns_l_block_map_entry(); }
fn _w679() void { g.n = g.n; s_indent(); }
fn _w680() void { g.n = g.n; ns_l_block_map_entry(); }
fn _w681() void { peg_seq(&[_]PFn{_w679, _w680}); }
fn _w682() void { peg_star(_w681); }
fn _w683() void { g.n = g.n; g.c = g.c; s_lblock_in_block(); }
fn _w684() void { g.n = g.n; s_lflow_in_block(); }
fn _w685() void { g.n = (g.n + 1); g.c = "FLOW-OUT"; s_separate(); }
fn _w686() void { g.n = (g.n + 1); g.c = "FLOW-OUT"; ns_flow_node(); }
fn _w687() void { s_l_comments(); }
fn _w688() void { g.n = g.n; g.c = g.c; s_lblock_scalar(); }
fn _w689() void { g.n = g.n; g.c = g.c; s_lblock_collection(); }
fn _w690() void { g.n = (g.n + 1); g.c = g.c; s_separate(); }
fn _w691() void { g.n = (g.n + 1); g.c = g.c; c_ns_properties(); }
fn _w692() void { g.n = (g.n + 1); g.c = g.c; s_separate(); }
fn _w693() void { peg_seq(&[_]PFn{_w691, _w692}); }
fn _w694() void { opt(_w693); }
fn _w695() void { g.n = g.n; c_lliteral(); }
fn _w696() void { g.n = g.n; c_lfolded(); }
fn _w697() void { peg_alt(&[_]PFn{_w695, _w696}); }
fn _w698() void { g.n = (g.n + 1); g.c = g.c; s_separate(); }
fn _w699() void { g.n = (g.n + 1); g.c = g.c; c_ns_properties(); }
fn _w700() void { peg_seq(&[_]PFn{_w698, _w699}); }
fn _w701() void { opt(_w700); }
fn _w702() void { s_l_comments(); }
fn _w703() void { g.n = seq_spaces(g.n, g.c); lblock_sequence(); }
fn _w704() void { g.n = g.n; lblock_mapping(); }
fn _w705() void { peg_alt(&[_]PFn{_w703, _w704}); }
fn _w706() void { c_byte_order_mark(); }
fn _w707() void { opt(_w706); }
fn _w708() void { l_comment(); }
fn _w709() void { peg_star(_w708); }
fn _w710() void { c_document_end(); }
fn _w711() void { s_l_comments(); }
fn _w712() void { sol(); }
fn _w713() void { c_directives_end(); }
fn _w714() void { c_document_end(); }
fn _w715() void { peg_alt(&[_]PFn{_w713, _w714}); }
fn _w716() void { b_char(); }
fn _w717() void { s_white(); }
fn _w718() void { eof_ok(); }
fn _w719() void { peg_alt(&[_]PFn{_w716, _w717, _w718}); }
fn _w720() void { g.n = -1; g.c = "BLOCK-IN"; s_lblock_node(); }
fn _w721() void { c_directives_end(); }
fn _w722() void { l_bare_document(); }
fn _w723() void { e_node(); }
fn _w724() void { s_l_comments(); }
fn _w725() void { peg_seq(&[_]PFn{_w723, _w724}); }
fn _w726() void { peg_alt(&[_]PFn{_w722, _w725}); }
fn _w727() void { peg_seq(&[_]PFn{_w721, _w726}); }
fn _w728() void { l_directive(); }
fn _w729() void { plus_(_w728); }
fn _w730() void { l_explicit_document(); }
fn _w731() void { l_directive_document(); }
fn _w732() void { l_explicit_document(); }
fn _w733() void { l_bare_document(); }
fn _w734() void { l_document_prefix(); }
fn _w735() void { peg_star(_w734); }
fn _w736() void { l_any_document(); }
fn _w737() void { opt(_w736); }
fn _w738() void { l_document_suffix(); }
fn _w739() void { plus_(_w738); }
fn _w740() void { l_document_prefix(); }
fn _w741() void { peg_star(_w740); }
fn _w742() void { l_any_document(); }
fn _w743() void { opt(_w742); }
fn _w744() void { peg_seq(&[_]PFn{_w739, _w741, _w743}); }
fn _w745() void { l_document_prefix(); }
fn _w746() void { peg_star(_w745); }
fn _w747() void { l_explicit_document(); }
fn _w748() void { opt(_w747); }
fn _w749() void { peg_seq(&[_]PFn{_w746, _w748}); }
fn _w750() void { peg_alt(&[_]PFn{_w744, _w749}); }
fn _w751() void { peg_star(_w750); }
fn _w752() void { peg_seq(&[_]PFn{_w735, _w737, _w751}); }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] C-PRINTABLE 
fn c_printable() void {
    peg_alt(&[_]PFn{_w1, _w2, _w3, _w4, _w5, _w6, _w7, _w8});
}

// [2] NB-JSON 
fn nb_json() void {
    peg_alt(&[_]PFn{_w9, _w10});
}

// [3] C-BYTE-ORDER-MARK 
fn c_byte_order_mark() void {
    match_cp(0xFEFF);
}

// [4] C-SEQUENCE-ENTRY 
fn c_sequence_entry() void {
    match_cp(45);
}

// [5] C-MAPPING-KEY 
fn c_mapping_key() void {
    match_cp(63);
}

// [6] C-MAPPING-VALUE 
fn c_mapping_value() void {
    match_cp(58);
}

// [7] C-COLLECT-ENTRY 
fn c_collect_entry() void {
    match_cp(44);
}

// [8] C-SEQUENCE-START 
fn c_sequence_start() void {
    match_cp(91);
}

// [9] C-SEQUENCE-END 
fn c_sequence_end() void {
    match_cp(93);
}

// [10] C-MAPPING-START 
fn c_mapping_start() void {
    match_cp(123);
}

// [11] C-MAPPING-END 
fn c_mapping_end() void {
    match_cp(125);
}

// [12] C-COMMENT 
fn c_comment() void {
    match_cp(35);
}

// [13] C-ANCHOR 
fn c_anchor() void {
    match_cp(38);
}

// [14] C-ALIAS 
fn c_alias() void {
    match_cp(42);
}

// [15] C-TAG 
fn c_tag() void {
    match_cp(33);
}

// [16] C-LITERAL 
fn c_literal() void {
    match_cp(124);
}

// [17] C-FOLDED 
fn c_folded() void {
    match_cp(62);
}

// [18] C-SINGLE-QUOTE 
fn c_single_quote() void {
    match_cp(39);
}

// [19] C-DOUBLE-QUOTE 
fn c_double_quote() void {
    match_cp(34);
}

// [20] C-DIRECTIVE 
fn c_directive() void {
    match_cp(37);
}

// [21] C-RESERVED 
fn c_reserved() void {
    peg_alt(&[_]PFn{_w11, _w12});
}

// [22] C-INDICATOR 
fn c_indicator() void {
    peg_alt(&[_]PFn{_w13, _w14, _w15, _w16, _w17, _w18, _w19, _w20, _w21, _w22, _w23, _w24, _w25, _w26, _w27, _w28, _w29, _w30});
}

// [23] C-FLOW-INDICATOR 
fn c_flow_indicator() void {
    peg_alt(&[_]PFn{_w31, _w32, _w33, _w34, _w35});
}

// [24] B-LINE-FEED 
fn b_line_feed() void {
    match_cp(0x0A);
}

// [25] B-CARRIAGE-RETURN 
fn b_carriage_return() void {
    match_cp(0x0D);
}

// [26] B-CHAR 
fn b_char() void {
    peg_alt(&[_]PFn{_w36, _w37});
}

// [27] NB-CHAR 
fn nb_char() void {
    minus_fn(_w38, _w41);
}

// [28] B-BREAK 
fn b_break() void {
    peg_alt(&[_]PFn{_w44, _w45, _w46});
}

// [29] B-AS-LINE-FEED 
fn b_as_line_feed() void {
    b_break();
}

// [30] B-NON-CONTENT 
fn b_non_content() void {
    b_break();
}

// [31] S-SPACE 
fn s_space() void {
    match_cp(0x20);
}

// [32] S-TAB 
fn s_tab() void {
    match_cp(0x9);
}

// [33] S-WHITE 
fn s_white() void {
    peg_alt(&[_]PFn{_w47, _w48});
}

// [34] NS-CHAR 
fn ns_char() void {
    minus_fn(_w49, _w50);
}

// [35] NS-DEC-DIGIT 
fn ns_dec_digit() void {
    match_range(0x30, 0x39);
}

// [36] NS-HEX-DIGIT 
fn ns_hex_digit() void {
    peg_alt(&[_]PFn{_w51, _w52, _w53});
}

// [37] NS-ASCII-LETTER 
fn ns_ascii_letter() void {
    peg_alt(&[_]PFn{_w54, _w55});
}

// [38] NS-WORD-CHAR 
fn ns_word_char() void {
    peg_alt(&[_]PFn{_w56, _w57, _w58});
}

// [39] NS-URI-CHAR 
fn ns_uri_char() void {
    peg_alt(&[_]PFn{_w62, _w63, _w64, _w65, _w66, _w67, _w68, _w69, _w70, _w71, _w72, _w73, _w74, _w75, _w76, _w77, _w78, _w79, _w80, _w81, _w82, _w83, _w84});
}

// [40] NS-TAG-CHAR 
fn ns_tag_char() void {
    minus_fn(_w85, _w88);
}

// [41] C-ESCAPE 
fn c_escape() void {
    match_cp(92);
}

// [42] NS-ESC-NULL 
fn ns_esc_null() void {
    match_cp(48);
}

// [43] NS-ESC-BELL 
fn ns_esc_bell() void {
    match_cp(97);
}

// [44] NS-ESC-BACKSPACE 
fn ns_esc_backspace() void {
    match_cp(98);
}

// [45] NS-ESC-HORIZONTAL-TAB 
fn ns_esc_horizontal_tab() void {
    match_cp(116);
}

// [46] NS-ESC-LINE-FEED 
fn ns_esc_line_feed() void {
    match_cp(110);
}

// [47] NS-ESC-VERTICAL-TAB 
fn ns_esc_vertical_tab() void {
    match_cp(118);
}

// [48] NS-ESC-FORM-FEED 
fn ns_esc_form_feed() void {
    match_cp(102);
}

// [49] NS-ESC-CARRIAGE-RETURN 
fn ns_esc_carriage_return() void {
    match_cp(114);
}

// [50] NS-ESC-ESCAPE 
fn ns_esc_escape() void {
    match_cp(101);
}

// [51] NS-ESC-SPACE 
fn ns_esc_space() void {
    match_cp(0x20);
}

// [52] NS-ESC-DOUBLE-QUOTE 
fn ns_esc_double_quote() void {
    match_cp(34);
}

// [53] NS-ESC-SLASH 
fn ns_esc_slash() void {
    match_cp(47);
}

// [54] NS-ESC-BACKSLASH 
fn ns_esc_backslash() void {
    match_cp(92);
}

// [55] NS-ESC-NEXT-LINE 
fn ns_esc_next_line() void {
    match_cp(78);
}

// [56] NS-ESC-NON-BREAKING-SPACE 
fn ns_esc_non_breaking_space() void {
    match_cp(95);
}

// [57] NS-ESC-LINE-SEPARATOR 
fn ns_esc_line_separator() void {
    match_cp(76);
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR 
fn ns_esc_paragraph_separator() void {
    match_cp(80);
}

// [59] NS-ESC-8-BIT 
fn ns_esc_8_bit() void {
    peg_seq(&[_]PFn{_w89, _w91});
}

// [60] NS-ESC-16-BIT 
fn ns_esc_16_bit() void {
    peg_seq(&[_]PFn{_w92, _w94});
}

// [61] NS-ESC-32-BIT 
fn ns_esc_32_bit() void {
    peg_seq(&[_]PFn{_w95, _w97});
}

// [62] C-NS-ESC-CHAR 
fn c_ns_esc_char() void {
    peg_seq(&[_]PFn{_w98, _w119});
}

// [63] S-INDENT 
fn s_indent() void {
    rep_fn(g.n, _w120);
}

// [64] S-INDENT-LT 
fn s_indent_lt() void {
    peg_star(_w121);
}

// [65] S-INDENT-LE 
fn s_indent_le() void {
    peg_star(_w122);
}

// [66] S-SEPARATE-IN-LINE 
fn s_separate_in_line() void {
    peg_alt(&[_]PFn{_w124, _w125});
}

// [67] S-LINE-PREFIX 
fn s_line_prefix() void {
    switch_ctx(g.c, &[_]CtxCase{
        .{ .ctx = "BLOCK-IN", .func = _w126 },
        .{ .ctx = "BLOCK-OUT", .func = _w127 },
        .{ .ctx = "FLOW-IN", .func = _w128 },
        .{ .ctx = "FLOW-OUT", .func = _w129 },
    });
}

// [68] S-BLOCK-LINE-PREFIX 
fn s_block_line_prefix() void {
    g.n = g.n; s_indent();
}

// [69] S-FLOW-LINE-PREFIX 
fn s_flow_line_prefix() void {
    peg_seq(&[_]PFn{_w130, _w132});
}

// [70] L-EMPTY 
fn l_empty() void {
    peg_seq(&[_]PFn{_w135, _w136});
}

// [71] B-L-TRIMMED 
fn b_l_trimmed() void {
    peg_seq(&[_]PFn{_w137, _w139});
}

// [72] B-AS-SPACE 
fn b_as_space() void {
    b_break();
}

// [73] B-L-FOLDED 
fn b_l_folded() void {
    peg_alt(&[_]PFn{_w140, _w141});
}

// [74] S-FLOW-FOLDED 
fn s_flow_folded() void {
    peg_seq(&[_]PFn{_w143, _w144, _w145});
}

// [75] C-NB-COMMENT-TEXT 
fn c_nb_comment_text() void {
    peg_seq(&[_]PFn{_w146, _w148});
}

// [76] B-COMMENT 
fn b_comment() void {
    peg_alt(&[_]PFn{_w149, _w150});
}

// [77] S-B-COMMENT 
fn s_b_comment() void {
    peg_seq(&[_]PFn{_w155, _w156});
}

// [78] L-COMMENT 
fn l_comment() void {
    peg_seq(&[_]PFn{_w157, _w159, _w160});
}

// [79] S-L-COMMENTS 
fn s_l_comments() void {
    peg_seq(&[_]PFn{_w163, _w165});
}

// [80] S-SEPARATE 
fn s_separate() void {
    switch_ctx(g.c, &[_]CtxCase{
        .{ .ctx = "BLOCK-OUT", .func = _w166 },
        .{ .ctx = "BLOCK-IN", .func = _w167 },
        .{ .ctx = "FLOW-OUT", .func = _w168 },
        .{ .ctx = "FLOW-IN", .func = _w169 },
        .{ .ctx = "BLOCK-KEY", .func = _w170 },
        .{ .ctx = "FLOW-KEY", .func = _w171 },
    });
}

// [81] S-SEPARATE-LINES 
fn s_separate_lines() void {
    peg_alt(&[_]PFn{_w174, _w175});
}

// [82] L-DIRECTIVE 
fn l_directive() void {
    peg_seq(&[_]PFn{_w176, _w180, _w181});
}

// [83] NS-RESERVED-DIRECTIVE 
fn ns_reserved_directive() void {
    peg_seq(&[_]PFn{_w182, _w186});
}

// [84] NS-DIRECTIVE-NAME 
fn ns_directive_name() void {
    plus_(_w187);
}

// [85] NS-DIRECTIVE-PARAMETER 
fn ns_directive_parameter() void {
    plus_(_w188);
}

// [86] NS-YAML-DIRECTIVE 
fn ns_yaml_directive() void {
    peg_seq(&[_]PFn{_w189, _w190, _w191});
}

// [87] NS-YAML-VERSION 
fn ns_yaml_version() void {
    peg_seq(&[_]PFn{_w193, _w194, _w196});
}

// [88] NS-TAG-DIRECTIVE 
fn ns_tag_directive() void {
    peg_seq(&[_]PFn{_w197, _w198, _w199, _w200, _w201});
}

// [89] C-TAG-HANDLE 
fn c_tag_handle() void {
    peg_alt(&[_]PFn{_w202, _w203, _w204});
}

// [90] C-PRIMARY-TAG-HANDLE 
fn c_primary_tag_handle() void {
    match_cp(33);
}

// [91] C-SECONDARY-TAG-HANDLE 
fn c_secondary_tag_handle() void {
    match_str("!!");
}

// [92] C-NAMED-TAG-HANDLE 
fn c_named_tag_handle() void {
    peg_seq(&[_]PFn{_w205, _w207, _w208});
}

// [93] NS-TAG-PREFIX 
fn ns_tag_prefix() void {
    peg_alt(&[_]PFn{_w209, _w210});
}

// [94] C-NS-LOCAL-TAG-PREFIX 
fn c_ns_local_tag_prefix() void {
    peg_seq(&[_]PFn{_w211, _w213});
}

// [95] NS-GLOBAL-TAG-PREFIX 
fn ns_global_tag_prefix() void {
    peg_seq(&[_]PFn{_w214, _w216});
}

// [96] C-NS-PROPERTIES 
fn c_ns_properties() void {
    peg_alt(&[_]PFn{_w222, _w228});
}

// [97] C-NS-TAG-PROPERTY 
fn c_ns_tag_property() void {
    peg_alt(&[_]PFn{_w229, _w230, _w231});
}

// [98] C-VERBATIM-TAG 
fn c_verbatim_tag() void {
    peg_seq(&[_]PFn{_w232, _w234, _w235});
}

// [99] C-NS-SHORTHAND-TAG 
fn c_ns_shorthand_tag() void {
    peg_seq(&[_]PFn{_w236, _w238});
}

// [100] C-NON-SPECIFIC-TAG 
fn c_non_specific_tag() void {
    match_cp(33);
}

// [101] C-NS-ANCHOR-PROPERTY 
fn c_ns_anchor_property() void {
    build_ast("ANCHOR", _w242);
}

// [102] NS-ANCHOR-CHAR 
fn ns_anchor_char() void {
    minus_fn(_w243, _w244);
}

// [103] NS-ANCHOR-NAME 
fn ns_anchor_name() void {
    plus_(_w245);
}

// [104] C-NS-ALIAS-NODE 
fn c_ns_alias_node() void {
    build_ast("ALIAS", _w249);
}

// [105] E-SCALAR 
fn e_scalar() void {
    ok_r();
}

// [106] E-NODE 
fn e_node() void {
    e_scalar();
}

// [107] NB-DOUBLE-CHAR 
fn nb_double_char() void {
    peg_alt(&[_]PFn{_w250, _w255});
}

// [108] NS-DOUBLE-CHAR 
fn ns_double_char() void {
    minus_fn(_w256, _w257);
}

// [109] C-DOUBLE-QUOTED 
fn c_double_quoted() void {
    scalar_fn(_w261);
}

// [110] NB-DOUBLE-TEXT 
fn nb_double_text() void {
    switch_ctx(g.c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w262 },
        .{ .ctx = "FLOW-IN", .func = _w263 },
        .{ .ctx = "BLOCK-KEY", .func = _w264 },
        .{ .ctx = "FLOW-KEY", .func = _w265 },
    });
}

// [111] NB-DOUBLE-ONE-LINE 
fn nb_double_one_line() void {
    peg_star(_w266);
}

// [112] S-DOUBLE-ESCAPED 
fn s_double_escaped() void {
    peg_seq(&[_]PFn{_w268, _w269, _w270, _w272, _w273});
}

// [113] S-DOUBLE-BREAK 
fn s_double_break() void {
    peg_alt(&[_]PFn{_w274, _w275});
}

// [114] NB-NS-DOUBLE-IN-LINE 
fn nb_ns_double_in_line() void {
    peg_star(_w279);
}

// [115] S-DOUBLE-NEXT-LINE 
fn s_double_next_line() void {
    peg_seq(&[_]PFn{_w280, _w288});
}

// [116] NB-DOUBLE-MULTI-LINE 
fn nb_double_multi_line() void {
    peg_seq(&[_]PFn{_w289, _w293});
}

// [117] C-QUOTED-QUOTE 
fn c_quoted_quote() void {
    match_str("''");
}

// [118] NB-SINGLE-CHAR 
fn nb_single_char() void {
    peg_alt(&[_]PFn{_w294, _w297});
}

// [119] NS-SINGLE-CHAR 
fn ns_single_char() void {
    minus_fn(_w298, _w299);
}

// [120] C-SINGLE-QUOTED 
fn c_single_quoted() void {
    scalar_fn(_w303);
}

// [121] NB-SINGLE-TEXT 
fn nb_single_text() void {
    switch_ctx(g.c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w304 },
        .{ .ctx = "FLOW-IN", .func = _w305 },
        .{ .ctx = "BLOCK-KEY", .func = _w306 },
        .{ .ctx = "FLOW-KEY", .func = _w307 },
    });
}

// [122] NB-SINGLE-ONE-LINE 
fn nb_single_one_line() void {
    peg_star(_w308);
}

// [123] NS-SINGLE-IN-LINE 
fn ns_single_in_line() void {
    peg_star(_w312);
}

// [124] S-SINGLE-NEXT-LINE 
fn s_single_next_line() void {
    peg_seq(&[_]PFn{_w313, _w321});
}

// [125] NB-SINGLE-MULTI-LINE 
fn nb_single_multi_line() void {
    peg_seq(&[_]PFn{_w322, _w326});
}

// [126] NS-PLAIN-FIRST 
fn ns_plain_first() void {
    peg_alt(&[_]PFn{_w329, _w336});
}

// [127] NS-PLAIN-SAFE 
fn ns_plain_safe() void {
    switch_ctx(g.c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w337 },
        .{ .ctx = "FLOW-IN", .func = _w338 },
        .{ .ctx = "BLOCK-KEY", .func = _w339 },
        .{ .ctx = "FLOW-KEY", .func = _w340 },
    });
}

// [128] NS-PLAIN-SAFE-OUT 
fn ns_plain_safe_out() void {
    ns_char();
}

// [129] NS-PLAIN-SAFE-IN 
fn ns_plain_safe_in() void {
    minus_fn(_w341, _w342);
}

// [130] NS-PLAIN-CHAR 
fn ns_plain_char() void {
    peg_alt(&[_]PFn{_w347, _w351, _w355});
}

// [131] NS-PLAIN 
fn ns_plain() void {
    scalar_fn(_w360);
}

// [132] NB-NS-PLAIN-IN-LINE 
fn nb_ns_plain_in_line() void {
    peg_star(_w364);
}

// [133] NS-PLAIN-ONE-LINE 
fn ns_plain_one_line() void {
    peg_seq(&[_]PFn{_w365, _w366});
}

// [134] S-NS-PLAIN-NEXT-LINE 
fn s_ns_plain_next_line() void {
    peg_seq(&[_]PFn{_w367, _w369, _w370, _w371});
}

// [135] NS-PLAIN-MULTI-LINE 
fn ns_plain_multi_line() void {
    peg_seq(&[_]PFn{_w372, _w374});
}

// [137] C-FLOW-SEQUENCE 
fn c_flow_sequence() void {
    build_ast("SEQUENCE", _w382);
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
fn ns_s_flow_seq_entries() void {
    peg_seq(&[_]PFn{_w383, _w385, _w392});
}

// [139] NS-FLOW-SEQ-ENTRY 
fn ns_flow_seq_entry() void {
    peg_alt(&[_]PFn{_w393, _w394});
}

// [140] C-FLOW-MAPPING 
fn c_flow_mapping() void {
    build_ast("MAPPING", _w402);
}

// [141] NS-S-FLOW-MAP-ENTRIES 
fn ns_s_flow_map_entries() void {
    peg_seq(&[_]PFn{_w403, _w405, _w412});
}

// [142] NS-FLOW-MAP-ENTRY 
fn ns_flow_map_entry() void {
    peg_alt(&[_]PFn{_w416, _w417});
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
fn ns_flow_map_explicit_entry() void {
    peg_alt(&[_]PFn{_w418, _w421});
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
fn ns_flow_map_implicit_entry() void {
    build_ast("PAIR", _w425);
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
fn ns_flow_map_yaml_key_entry() void {
    peg_seq(&[_]PFn{_w426, _w432});
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
fn c_ns_flow_map_empty_key_entry() void {
    peg_seq(&[_]PFn{_w433, _w434});
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
fn c_ns_flow_map_separate_value() void {
    peg_seq(&[_]PFn{_w435, _w437, _w442});
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
fn c_ns_flow_map_json_key_entry() void {
    peg_seq(&[_]PFn{_w443, _w449});
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
fn c_ns_flow_map_adjacent_value() void {
    peg_seq(&[_]PFn{_w450, _w456});
}

// [150] NS-FLOW-PAIR 
fn ns_flow_pair() void {
    peg_alt(&[_]PFn{_w460, _w461});
}

// [151] NS-FLOW-PAIR-ENTRY 
fn ns_flow_pair_entry() void {
    peg_alt(&[_]PFn{_w462, _w463, _w464});
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
fn ns_flow_pair_yaml_key_entry() void {
    peg_seq(&[_]PFn{_w465, _w466});
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
fn c_ns_flow_pair_json_key_entry() void {
    peg_seq(&[_]PFn{_w467, _w468});
}

// [154] NS-S-IMPLICIT-YAML-KEY 
fn ns_s_implicit_yaml_key() void {
    peg_seq(&[_]PFn{_w469, _w471});
}

// [155] C-S-IMPLICIT-JSON-KEY 
fn c_s_implicit_json_key() void {
    peg_seq(&[_]PFn{_w472, _w474});
}

// [156] NS-FLOW-YAML-CONTENT 
fn ns_flow_yaml_content() void {
    g.n = g.n; g.c = g.c; ns_plain();
}

// [157] C-FLOW-JSON-CONTENT 
fn c_flow_json_content() void {
    peg_alt(&[_]PFn{_w475, _w476, _w477, _w478});
}

// [158] NS-FLOW-CONTENT 
fn ns_flow_content() void {
    peg_alt(&[_]PFn{_w479, _w480});
}

// [159] NS-FLOW-YAML-NODE 
fn ns_flow_yaml_node() void {
    peg_alt(&[_]PFn{_w481, _w482, _w489});
}

// [160] C-FLOW-JSON-NODE 
fn c_flow_json_node() void {
    peg_seq(&[_]PFn{_w493, _w494});
}

// [161] NS-FLOW-NODE 
fn ns_flow_node() void {
    peg_alt(&[_]PFn{_w495, _w496, _w503});
}

// [162] C-B-BLOCK-HEADER 
fn c_b_block_header() void {
    peg_alt(&[_]PFn{_w512, _w521});
}

// [163] C-INDENTATION-INDICATOR 
fn c_indentation_indicator() void {
    peg_alt(&[_]PFn{_w522, _w523});
}

// [164] C-CHOMPING-INDICATOR 
fn c_chomping_indicator() void {
    peg_alt(&[_]PFn{_w524, _w525, _w526});
}

// [165] B-CHOMPED-LAST 
fn b_chomped_last() void {
    switch_ctx(g.t, &[_]CtxCase{
        .{ .ctx = "STRIP", .func = _w527 },
        .{ .ctx = "CLIP", .func = _w528 },
        .{ .ctx = "KEEP", .func = _w529 },
    });
}

// [166] L-CHOMPED-EMPTY 
fn l_chomped_empty() void {
    switch_ctx(g.t, &[_]CtxCase{
        .{ .ctx = "STRIP", .func = _w530 },
        .{ .ctx = "CLIP", .func = _w531 },
        .{ .ctx = "KEEP", .func = _w532 },
    });
}

// [167] L-STRIP-EMPTY 
fn l_strip_empty() void {
    peg_seq(&[_]PFn{_w536, _w538});
}

// [168] L-KEEP-EMPTY 
fn l_keep_empty() void {
    peg_seq(&[_]PFn{_w540, _w542});
}

// [169] L-TRAIL-COMMENTS 
fn l_trail_comments() void {
    peg_seq(&[_]PFn{_w543, _w544, _w545, _w547});
}

// [170] C-L+LITERAL 
fn c_lliteral() void {
    peg_seq(&[_]PFn{_w548, _w559});
}

// [171] L-NB-LITERAL-TEXT 
fn l_nb_literal_text() void {
    peg_seq(&[_]PFn{_w561, _w562, _w564});
}

// [172] B-NB-LITERAL-NEXT 
fn b_nb_literal_next() void {
    peg_seq(&[_]PFn{_w565, _w566});
}

// [173] L-LITERAL-CONTENT 
fn l_literal_content() void {
    scalar_fn(_w574);
}

// [174] C-L+FOLDED 
fn c_lfolded() void {
    peg_seq(&[_]PFn{_w575, _w586});
}

// [175] S-NB-FOLDED-TEXT 
fn s_nb_folded_text() void {
    peg_seq(&[_]PFn{_w587, _w588, _w590});
}

// [176] L-NB-FOLDED-LINES 
fn l_nb_folded_lines() void {
    peg_seq(&[_]PFn{_w591, _w595});
}

// [177] S-NB-SPACED-TEXT 
fn s_nb_spaced_text() void {
    peg_seq(&[_]PFn{_w596, _w597, _w599});
}

// [178] B-L-SPACED 
fn b_l_spaced() void {
    peg_seq(&[_]PFn{_w600, _w602});
}

// [179] L-NB-SPACED-LINES 
fn l_nb_spaced_lines() void {
    peg_seq(&[_]PFn{_w603, _w607});
}

// [180] L-NB-SAME-LINES 
fn l_nb_same_lines() void {
    peg_seq(&[_]PFn{_w609, _w612});
}

// [181] L-NB-DIFF-LINES 
fn l_nb_diff_lines() void {
    peg_seq(&[_]PFn{_w613, _w617});
}

// [182] L-FOLDED-CONTENT 
fn l_folded_content() void {
    scalar_fn(_w623);
}

// [183] L+BLOCK-SEQUENCE 
fn lblock_sequence() void {
    build_ast("SEQUENCE", _w628);
}

// [184] C-L-BLOCK-SEQ-ENTRY 
fn c_l_block_seq_entry() void {
    peg_seq(&[_]PFn{_w629, _w631, _w632});
}

// [185] S-L+BLOCK-INDENTED 
fn s_lblock_indented() void {
    peg_alt(&[_]PFn{_w637, _w638, _w641});
}

// [186] NS-L-COMPACT-SEQUENCE 
fn ns_l_compact_sequence() void {
    peg_seq(&[_]PFn{_w642, _w646});
}

// [187] L+BLOCK-MAPPING 
fn lblock_mapping() void {
    build_ast("MAPPING", _w651);
}

// [188] NS-L-BLOCK-MAP-ENTRY 
fn ns_l_block_map_entry() void {
    peg_alt(&[_]PFn{_w652, _w653});
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
fn c_l_block_map_explicit_entry() void {
    peg_seq(&[_]PFn{_w654, _w657});
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
fn c_l_block_map_explicit_key() void {
    peg_seq(&[_]PFn{_w658, _w659});
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
fn l_block_map_explicit_value() void {
    peg_seq(&[_]PFn{_w660, _w661, _w662});
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
fn ns_l_block_map_implicit_entry() void {
    build_ast("PAIR", _w668);
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
fn ns_s_block_map_implicit_key() void {
    peg_alt(&[_]PFn{_w669, _w670});
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
fn c_l_block_map_implicit_value() void {
    peg_seq(&[_]PFn{_w671, _w677});
}

// [195] NS-L-COMPACT-MAPPING 
fn ns_l_compact_mapping() void {
    peg_seq(&[_]PFn{_w678, _w682});
}

// [196] S-L+BLOCK-NODE 
fn s_lblock_node() void {
    peg_alt(&[_]PFn{_w683, _w684});
}

// [197] S-L+FLOW-IN-BLOCK 
fn s_lflow_in_block() void {
    peg_seq(&[_]PFn{_w685, _w686, _w687});
}

// [198] S-L+BLOCK-IN-BLOCK 
fn s_lblock_in_block() void {
    peg_alt(&[_]PFn{_w688, _w689});
}

// [199] S-L+BLOCK-SCALAR 
fn s_lblock_scalar() void {
    peg_seq(&[_]PFn{_w690, _w694, _w697});
}

// [200] S-L+BLOCK-COLLECTION 
fn s_lblock_collection() void {
    peg_seq(&[_]PFn{_w701, _w702, _w705});
}

// [202] L-DOCUMENT-PREFIX 
fn l_document_prefix() void {
    peg_seq(&[_]PFn{_w707, _w709});
}

// [203] C-DIRECTIVES-END 
fn c_directives_end() void {
    match_str("---");
}

// [204] C-DOCUMENT-END 
fn c_document_end() void {
    match_str("...");
}

// [205] L-DOCUMENT-SUFFIX 
fn l_document_suffix() void {
    peg_seq(&[_]PFn{_w710, _w711});
}

// [206] C-FORBIDDEN 
fn c_forbidden() void {
    peg_seq(&[_]PFn{_w712, _w715, _w719});
}

// [207] L-BARE-DOCUMENT 
fn l_bare_document() void {
    build_ast("DOC", _w720);
}

// [208] L-EXPLICIT-DOCUMENT 
fn l_explicit_document() void {
    build_ast("DOC", _w727);
}

// [209] L-DIRECTIVE-DOCUMENT 
fn l_directive_document() void {
    peg_seq(&[_]PFn{_w729, _w730});
}

// [210] L-ANY-DOCUMENT 
fn l_any_document() void {
    peg_alt(&[_]PFn{_w731, _w732, _w733});
}

// [211] L-YAML-STREAM 
fn l_yaml_stream() void {
    build_ast("STREAM", _w752);
}



