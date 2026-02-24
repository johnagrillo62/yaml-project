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
    var i = g.pos;
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
fn _w31() void { c_sequence_entry(); }
fn _w32() void { c_mapping_key(); }
fn _w33() void { c_mapping_value(); }
fn _w34() void { c_collect_entry(); }
fn _w35() void { c_sequence_start(); }
fn _w36() void { c_sequence_end(); }
fn _w37() void { c_mapping_start(); }
fn _w38() void { c_mapping_end(); }
fn _w39() void { c_comment(); }
fn _w40() void { c_anchor(); }
fn _w41() void { c_alias(); }
fn _w42() void { c_tag(); }
fn _w43() void { c_literal(); }
fn _w44() void { c_folded(); }
fn _w45() void { c_single_quote(); }
fn _w46() void { c_double_quote(); }
fn _w47() void { c_directive(); }
fn _w48() void { c_reserved(); }
fn _w49() void { c_collect_entry(); }
fn _w50() void { c_sequence_start(); }
fn _w51() void { c_sequence_end(); }
fn _w52() void { c_mapping_start(); }
fn _w53() void { c_mapping_end(); }
fn _w54() void { b_line_feed(); }
fn _w55() void { b_carriage_return(); }
fn _w56() void { c_printable(); }
fn _w57() void { b_char(); }
fn _w58() void { c_byte_order_mark(); }
fn _w59() void { peg_alt(&[_]PFn{_w57, _w58}); }
fn _w60() void { b_carriage_return(); }
fn _w61() void { b_line_feed(); }
fn _w62() void { peg_seq(&[_]PFn{_w60, _w61}); }
fn _w63() void { b_carriage_return(); }
fn _w64() void { b_line_feed(); }
fn _w65() void { s_space(); }
fn _w66() void { s_tab(); }
fn _w67() void { nb_char(); }
fn _w68() void { s_white(); }
fn _w69() void { ns_dec_digit(); }
fn _w70() void { match_range(0x41, 0x46); }
fn _w71() void { match_range(0x61, 0x66); }
fn _w72() void { match_range(0x41, 0x5A); }
fn _w73() void { match_range(0x61, 0x7A); }
fn _w74() void { ns_dec_digit(); }
fn _w75() void { ns_ascii_letter(); }
fn _w76() void { match_cp(45); }
fn _w77() void { match_cp(37); }
fn _w78() void { ns_hex_digit(); }
fn _w79() void { ns_hex_digit(); }
fn _w80() void { peg_seq(&[_]PFn{_w77, _w78, _w79}); }
fn _w81() void { ns_word_char(); }
fn _w82() void { match_cp(35); }
fn _w83() void { match_cp(59); }
fn _w84() void { match_cp(47); }
fn _w85() void { match_cp(63); }
fn _w86() void { match_cp(58); }
fn _w87() void { match_cp(64); }
fn _w88() void { match_cp(38); }
fn _w89() void { match_cp(61); }
fn _w90() void { match_cp(43); }
fn _w91() void { match_cp(36); }
fn _w92() void { match_cp(44); }
fn _w93() void { match_cp(95); }
fn _w94() void { match_cp(46); }
fn _w95() void { match_cp(33); }
fn _w96() void { match_cp(126); }
fn _w97() void { match_cp(42); }
fn _w98() void { match_cp(39); }
fn _w99() void { match_cp(40); }
fn _w100() void { match_cp(41); }
fn _w101() void { match_cp(91); }
fn _w102() void { match_cp(93); }
fn _w103() void { match_cp(37); }
fn _w104() void { ns_hex_digit(); }
fn _w105() void { ns_hex_digit(); }
fn _w106() void { peg_seq(&[_]PFn{_w103, _w104, _w105}); }
fn _w107() void { ns_word_char(); }
fn _w108() void { match_cp(35); }
fn _w109() void { match_cp(59); }
fn _w110() void { match_cp(47); }
fn _w111() void { match_cp(63); }
fn _w112() void { match_cp(58); }
fn _w113() void { match_cp(64); }
fn _w114() void { match_cp(38); }
fn _w115() void { match_cp(61); }
fn _w116() void { match_cp(43); }
fn _w117() void { match_cp(36); }
fn _w118() void { match_cp(44); }
fn _w119() void { match_cp(95); }
fn _w120() void { match_cp(46); }
fn _w121() void { match_cp(33); }
fn _w122() void { match_cp(126); }
fn _w123() void { match_cp(42); }
fn _w124() void { match_cp(39); }
fn _w125() void { match_cp(40); }
fn _w126() void { match_cp(41); }
fn _w127() void { match_cp(91); }
fn _w128() void { match_cp(93); }
fn _w129() void { ns_uri_char(); }
fn _w130() void { c_tag(); }
fn _w131() void { c_flow_indicator(); }
fn _w132() void { peg_alt(&[_]PFn{_w130, _w131}); }
fn _w133() void { match_cp(120); }
fn _w134() void { ns_hex_digit(); }
fn _w135() void { rep_fn(2, _w134); }
fn _w136() void { match_cp(117); }
fn _w137() void { ns_hex_digit(); }
fn _w138() void { rep_fn(4, _w137); }
fn _w139() void { match_cp(85); }
fn _w140() void { ns_hex_digit(); }
fn _w141() void { rep_fn(8, _w140); }
fn _w142() void { c_escape(); }
fn _w143() void { ns_esc_null(); }
fn _w144() void { ns_esc_bell(); }
fn _w145() void { ns_esc_backspace(); }
fn _w146() void { ns_esc_horizontal_tab(); }
fn _w147() void { ns_esc_line_feed(); }
fn _w148() void { ns_esc_vertical_tab(); }
fn _w149() void { ns_esc_form_feed(); }
fn _w150() void { ns_esc_carriage_return(); }
fn _w151() void { ns_esc_escape(); }
fn _w152() void { ns_esc_space(); }
fn _w153() void { ns_esc_double_quote(); }
fn _w154() void { ns_esc_slash(); }
fn _w155() void { ns_esc_backslash(); }
fn _w156() void { ns_esc_next_line(); }
fn _w157() void { ns_esc_non_breaking_space(); }
fn _w158() void { ns_esc_line_separator(); }
fn _w159() void { ns_esc_paragraph_separator(); }
fn _w160() void { ns_esc_8_bit(); }
fn _w161() void { ns_esc_16_bit(); }
fn _w162() void { ns_esc_32_bit(); }
fn _w163() void { ns_esc_null(); }
fn _w164() void { ns_esc_bell(); }
fn _w165() void { ns_esc_backspace(); }
fn _w166() void { ns_esc_horizontal_tab(); }
fn _w167() void { ns_esc_line_feed(); }
fn _w168() void { ns_esc_vertical_tab(); }
fn _w169() void { ns_esc_form_feed(); }
fn _w170() void { ns_esc_carriage_return(); }
fn _w171() void { ns_esc_escape(); }
fn _w172() void { ns_esc_space(); }
fn _w173() void { ns_esc_double_quote(); }
fn _w174() void { ns_esc_slash(); }
fn _w175() void { ns_esc_backslash(); }
fn _w176() void { ns_esc_next_line(); }
fn _w177() void { ns_esc_non_breaking_space(); }
fn _w178() void { ns_esc_line_separator(); }
fn _w179() void { ns_esc_paragraph_separator(); }
fn _w180() void { ns_esc_8_bit(); }
fn _w181() void { ns_esc_16_bit(); }
fn _w182() void { ns_esc_32_bit(); }
fn _w183() void { peg_alt(&[_]PFn{
        _w163,
        _w164,
        _w165,
        _w166,
        _w167,
        _w168,
        _w169,
        _w170,
        _w171,
        _w172,
        _w173,
        _w174,
        _w175,
        _w176,
        _w177,
        _w178,
        _w179,
        _w180,
        _w181,
        _w182}); }
fn _w184() void { s_space(); }
fn _w185() void { s_space(); }
fn _w186() void { s_space(); }
fn _w187() void { s_white(); }
fn _w188() void { plus_(_w187); }
fn _w189() void { ok(); }
fn _w190() void { s_block_line_prefix(g.n); }
fn _w191() void { s_block_line_prefix(g.n); }
fn _w192() void { s_flow_line_prefix(g.n); }
fn _w193() void { s_flow_line_prefix(g.n); }
fn _w194() void { s_indent(g.n); }
fn _w195() void { s_separate_in_line(); }
fn _w196() void { opt(_w195); }
fn _w197() void { s_line_prefix(g.n, g.c); }
fn _w198() void { s_indent_lt(g.n); }
fn _w199() void { peg_alt(&[_]PFn{_w197, _w198}); }
fn _w200() void { b_as_line_feed(); }
fn _w201() void { b_non_content(); }
fn _w202() void { l_empty(g.n, g.c); }
fn _w203() void { plus_(_w202); }
fn _w204() void { b_l_trimmed(g.n, g.c); }
fn _w205() void { b_as_space(); }
fn _w206() void { s_separate_in_line(); }
fn _w207() void { opt(_w206); }
fn _w208() void { b_l_folded(g.n, "FLOW-IN"); }
fn _w209() void { s_flow_line_prefix(g.n); }
fn _w210() void { c_comment(); }
fn _w211() void { nb_char(); }
fn _w212() void { peg_star(_w211); }
fn _w213() void { b_non_content(); }
fn _w214() void { ok(); }
fn _w215() void { s_separate_in_line(); }
fn _w216() void { c_nb_comment_text(); }
fn _w217() void { opt(_w216); }
fn _w218() void { peg_seq(&[_]PFn{_w215, _w217}); }
fn _w219() void { opt(_w218); }
fn _w220() void { b_comment(); }
fn _w221() void { s_separate_in_line(); }
fn _w222() void { c_nb_comment_text(); }
fn _w223() void { opt(_w222); }
fn _w224() void { b_non_content(); }
fn _w225() void { s_b_comment(); }
fn _w226() void { ok(); }
fn _w227() void { peg_alt(&[_]PFn{_w225, _w226}); }
fn _w228() void { l_comment(); }
fn _w229() void { peg_star(_w228); }
fn _w230() void { s_separate_lines(g.n); }
fn _w231() void { s_separate_lines(g.n); }
fn _w232() void { s_separate_lines(g.n); }
fn _w233() void { s_separate_lines(g.n); }
fn _w234() void { s_separate_in_line(); }
fn _w235() void { s_separate_in_line(); }
fn _w236() void { s_l_comments(); }
fn _w237() void { s_flow_line_prefix(g.n); }
fn _w238() void { peg_seq(&[_]PFn{_w236, _w237}); }
fn _w239() void { s_separate_in_line(); }
fn _w240() void { c_directive(); }
fn _w241() void { ns_yaml_directive(); }
fn _w242() void { ns_tag_directive(); }
fn _w243() void { ns_reserved_directive(); }
fn _w244() void { peg_alt(&[_]PFn{_w241, _w242, _w243}); }
fn _w245() void { s_l_comments(); }
fn _w246() void { ns_directive_name(); }
fn _w247() void { s_separate_in_line(); }
fn _w248() void { ns_directive_parameter(); }
fn _w249() void { peg_seq(&[_]PFn{_w247, _w248}); }
fn _w250() void { peg_star(_w249); }
fn _w251() void { ns_char(); }
fn _w252() void { ns_char(); }
fn _w253() void { match_str("YAML"); }
fn _w254() void { s_separate_in_line(); }
fn _w255() void { ns_yaml_version(); }
fn _w256() void { ns_dec_digit(); }
fn _w257() void { plus_(_w256); }
fn _w258() void { match_cp(46); }
fn _w259() void { ns_dec_digit(); }
fn _w260() void { plus_(_w259); }
fn _w261() void { match_str("TAG"); }
fn _w262() void { s_separate_in_line(); }
fn _w263() void { c_tag_handle(); }
fn _w264() void { s_separate_in_line(); }
fn _w265() void { ns_tag_prefix(); }
fn _w266() void { c_named_tag_handle(); }
fn _w267() void { c_secondary_tag_handle(); }
fn _w268() void { c_primary_tag_handle(); }
fn _w269() void { match_cp(33); }
fn _w270() void { ns_word_char(); }
fn _w271() void { plus_(_w270); }
fn _w272() void { match_cp(33); }
fn _w273() void { c_ns_local_tag_prefix(); }
fn _w274() void { ns_global_tag_prefix(); }
fn _w275() void { match_cp(33); }
fn _w276() void { ns_uri_char(); }
fn _w277() void { peg_star(_w276); }
fn _w278() void { ns_tag_char(); }
fn _w279() void { ns_uri_char(); }
fn _w280() void { peg_star(_w279); }
fn _w281() void { c_ns_tag_property(); }
fn _w282() void { s_separate(g.n, g.c); }
fn _w283() void { c_ns_anchor_property(); }
fn _w284() void { peg_seq(&[_]PFn{_w282, _w283}); }
fn _w285() void { opt(_w284); }
fn _w286() void { peg_seq(&[_]PFn{_w281, _w285}); }
fn _w287() void { c_ns_anchor_property(); }
fn _w288() void { s_separate(g.n, g.c); }
fn _w289() void { c_ns_tag_property(); }
fn _w290() void { peg_seq(&[_]PFn{_w288, _w289}); }
fn _w291() void { opt(_w290); }
fn _w292() void { peg_seq(&[_]PFn{_w287, _w291}); }
fn _w293() void { c_verbatim_tag(); }
fn _w294() void { c_ns_shorthand_tag(); }
fn _w295() void { c_non_specific_tag(); }
fn _w296() void { match_str("!<"); }
fn _w297() void { ns_uri_char(); }
fn _w298() void { plus_(_w297); }
fn _w299() void { match_cp(62); }
fn _w300() void { c_tag_handle(); }
fn _w301() void { ns_tag_char(); }
fn _w302() void { plus_(_w301); }
fn _w303() void { c_anchor(); }
fn _w304() void { ns_anchor_name(); }
fn _w305() void { scalar_fn(_w304); }
fn _w306() void { peg_seq(&[_]PFn{_w303, _w305}); }
fn _w307() void { ns_char(); }
fn _w308() void { c_flow_indicator(); }
fn _w309() void { ns_anchor_char(); }
fn _w310() void { c_alias(); }
fn _w311() void { ns_anchor_name(); }
fn _w312() void { scalar_fn(_w311); }
fn _w313() void { peg_seq(&[_]PFn{_w310, _w312}); }
fn _w314() void { c_ns_esc_char(); }
fn _w315() void { nb_json(); }
fn _w316() void { match_cp(92); }
fn _w317() void { match_cp(34); }
fn _w318() void { peg_alt(&[_]PFn{_w316, _w317}); }
fn _w319() void { minus_fn(_w315, _w318); }
fn _w320() void { nb_double_char(); }
fn _w321() void { s_white(); }
fn _w322() void { match_cp(34); }
fn _w323() void { nb_double_text(g.n, g.c); }
fn _w324() void { match_cp(34); }
fn _w325() void { peg_seq(&[_]PFn{_w322, _w323, _w324}); }
fn _w326() void { nb_double_multi_line(g.n); }
fn _w327() void { nb_double_multi_line(g.n); }
fn _w328() void { nb_double_one_line(); }
fn _w329() void { nb_double_one_line(); }
fn _w330() void { nb_double_char(); }
fn _w331() void { s_white(); }
fn _w332() void { peg_star(_w331); }
fn _w333() void { match_cp(92); }
fn _w334() void { b_non_content(); }
fn _w335() void { l_empty(g.n, "FLOW-IN"); }
fn _w336() void { peg_star(_w335); }
fn _w337() void { s_flow_line_prefix(g.n); }
fn _w338() void { s_double_escaped(g.n); }
fn _w339() void { s_flow_folded(g.n); }
fn _w340() void { s_white(); }
fn _w341() void { peg_star(_w340); }
fn _w342() void { ns_double_char(); }
fn _w343() void { peg_seq(&[_]PFn{_w341, _w342}); }
fn _w344() void { s_double_break(g.n); }
fn _w345() void { ns_double_char(); }
fn _w346() void { nb_ns_double_in_line(); }
fn _w347() void { s_double_next_line(g.n); }
fn _w348() void { s_white(); }
fn _w349() void { peg_star(_w348); }
fn _w350() void { peg_alt(&[_]PFn{_w347, _w349}); }
fn _w351() void { peg_seq(&[_]PFn{_w345, _w346, _w350}); }
fn _w352() void { opt(_w351); }
fn _w353() void { nb_ns_double_in_line(); }
fn _w354() void { s_double_next_line(g.n); }
fn _w355() void { s_white(); }
fn _w356() void { peg_star(_w355); }
fn _w357() void { peg_alt(&[_]PFn{_w354, _w356}); }
fn _w358() void { c_quoted_quote(); }
fn _w359() void { nb_json(); }
fn _w360() void { match_cp(39); }
fn _w361() void { minus_fn(_w359, _w360); }
fn _w362() void { nb_single_char(); }
fn _w363() void { s_white(); }
fn _w364() void { match_cp(39); }
fn _w365() void { nb_single_text(g.n, g.c); }
fn _w366() void { match_cp(39); }
fn _w367() void { peg_seq(&[_]PFn{_w364, _w365, _w366}); }
fn _w368() void { nb_single_multi_line(g.n); }
fn _w369() void { nb_single_multi_line(g.n); }
fn _w370() void { nb_single_one_line(); }
fn _w371() void { nb_single_one_line(); }
fn _w372() void { nb_single_char(); }
fn _w373() void { s_white(); }
fn _w374() void { peg_star(_w373); }
fn _w375() void { ns_single_char(); }
fn _w376() void { peg_seq(&[_]PFn{_w374, _w375}); }
fn _w377() void { s_flow_folded(g.n); }
fn _w378() void { ns_single_char(); }
fn _w379() void { ns_single_in_line(); }
fn _w380() void { s_single_next_line(g.n); }
fn _w381() void { s_white(); }
fn _w382() void { peg_star(_w381); }
fn _w383() void { peg_alt(&[_]PFn{_w380, _w382}); }
fn _w384() void { peg_seq(&[_]PFn{_w378, _w379, _w383}); }
fn _w385() void { opt(_w384); }
fn _w386() void { ns_single_in_line(); }
fn _w387() void { s_single_next_line(g.n); }
fn _w388() void { s_white(); }
fn _w389() void { peg_star(_w388); }
fn _w390() void { peg_alt(&[_]PFn{_w387, _w389}); }
fn _w391() void { ns_char(); }
fn _w392() void { c_indicator(); }
fn _w393() void { minus_fn(_w391, _w392); }
fn _w394() void { match_cp(63); }
fn _w395() void { match_cp(58); }
fn _w396() void { match_cp(45); }
fn _w397() void { peg_alt(&[_]PFn{_w394, _w395, _w396}); }
fn _w398() void { ns_plain_safe(g.c); }
fn _w399() void { ahead(_w398); }
fn _w400() void { peg_seq(&[_]PFn{_w397, _w399}); }
fn _w401() void { ns_plain_safe_out(); }
fn _w402() void { ns_plain_safe_in(); }
fn _w403() void { ns_plain_safe_out(); }
fn _w404() void { ns_plain_safe_in(); }
fn _w405() void { ns_char(); }
fn _w406() void { c_flow_indicator(); }
fn _w407() void { ns_plain_safe(g.c); }
fn _w408() void { match_cp(58); }
fn _w409() void { match_cp(35); }
fn _w410() void { peg_alt(&[_]PFn{_w408, _w409}); }
fn _w411() void { minus_fn(_w407, _w410); }
fn _w412() void { ns_char(); }
fn _w413() void { behind(_w412); }
fn _w414() void { match_cp(35); }
fn _w415() void { peg_seq(&[_]PFn{_w413, _w414}); }
fn _w416() void { match_cp(58); }
fn _w417() void { ns_plain_safe(g.c); }
fn _w418() void { ahead(_w417); }
fn _w419() void { peg_seq(&[_]PFn{_w416, _w418}); }
fn _w420() void { ns_plain_multi_line(g.n, g.c); }
fn _w421() void { ns_plain_multi_line(g.n, g.c); }
fn _w422() void { ns_plain_one_line(g.c); }
fn _w423() void { ns_plain_one_line(g.c); }
fn _w424() void { switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w420 },
        .{ .ctx = "FLOW-IN", .func = _w421 },
        .{ .ctx = "BLOCK-KEY", .func = _w422 },
        .{ .ctx = "FLOW-KEY", .func = _w423 },
    }); }
fn _w425() void { s_white(); }
fn _w426() void { peg_star(_w425); }
fn _w427() void { ns_plain_char(g.c); }
fn _w428() void { peg_seq(&[_]PFn{_w426, _w427}); }
fn _w429() void { ns_plain_first(g.c); }
fn _w430() void { nb_ns_plain_in_line(g.c); }
fn _w431() void { s_flow_folded(g.n); }
fn _w432() void { c_forbidden(); }
fn _w433() void { r_neg(_w432); }
fn _w434() void { ns_plain_char(g.c); }
fn _w435() void { nb_ns_plain_in_line(g.c); }
fn _w436() void { ns_plain_one_line(g.c); }
fn _w437() void { s_ns_plain_next_line(g.n, g.c); }
fn _w438() void { peg_star(_w437); }
fn _w439() void { match_cp(91); }
fn _w440() void { s_separate(g.n, g.c); }
fn _w441() void { opt(_w440); }
fn _w442() void { ns_s_flow_seq_entries(g.n, in_flow(g.c)); }
fn _w443() void { collect_fn(_w442); }
fn _w444() void { opt(_w443); }
fn _w445() void { match_cp(93); }
fn _w446() void { peg_seq(&[_]PFn{_w439, _w441, _w444, _w445}); }
fn _w447() void { ns_flow_seq_entry(g.n, g.c); }
fn _w448() void { s_separate(g.n, g.c); }
fn _w449() void { opt(_w448); }
fn _w450() void { match_cp(44); }
fn _w451() void { s_separate(g.n, g.c); }
fn _w452() void { opt(_w451); }
fn _w453() void { ns_s_flow_seq_entries(g.n, g.c); }
fn _w454() void { opt(_w453); }
fn _w455() void { peg_seq(&[_]PFn{_w450, _w452, _w454}); }
fn _w456() void { opt(_w455); }
fn _w457() void { ns_flow_pair(g.n, g.c); }
fn _w458() void { ns_flow_node(g.n, g.c); }
fn _w459() void { match_cp(123); }
fn _w460() void { s_separate(g.n, g.c); }
fn _w461() void { opt(_w460); }
fn _w462() void { ns_s_flow_map_entries(g.n, in_flow(g.c)); }
fn _w463() void { collect_fn(_w462); }
fn _w464() void { opt(_w463); }
fn _w465() void { match_cp(125); }
fn _w466() void { peg_seq(&[_]PFn{_w459, _w461, _w464, _w465}); }
fn _w467() void { ns_flow_map_entry(g.n, g.c); }
fn _w468() void { s_separate(g.n, g.c); }
fn _w469() void { opt(_w468); }
fn _w470() void { match_cp(44); }
fn _w471() void { s_separate(g.n, g.c); }
fn _w472() void { opt(_w471); }
fn _w473() void { ns_s_flow_map_entries(g.n, g.c); }
fn _w474() void { opt(_w473); }
fn _w475() void { peg_seq(&[_]PFn{_w470, _w472, _w474}); }
fn _w476() void { opt(_w475); }
fn _w477() void { match_cp(63); }
fn _w478() void { s_separate(g.n, g.c); }
fn _w479() void { ns_flow_map_explicit_entry(g.n, g.c); }
fn _w480() void { peg_seq(&[_]PFn{_w477, _w478, _w479}); }
fn _w481() void { ns_flow_map_implicit_entry(g.n, g.c); }
fn _w482() void { ns_flow_map_implicit_entry(g.n, g.c); }
fn _w483() void { e_node(); }
fn _w484() void { e_node(); }
fn _w485() void { peg_seq(&[_]PFn{_w483, _w484}); }
fn _w486() void { ns_flow_map_yaml_key_entry(g.n, g.c); }
fn _w487() void { c_ns_flow_map_empty_key_entry(g.n, g.c); }
fn _w488() void { c_ns_flow_map_json_key_entry(g.n, g.c); }
fn _w489() void { peg_alt(&[_]PFn{_w486, _w487, _w488}); }
fn _w490() void { ns_flow_yaml_node(g.n, g.c); }
fn _w491() void { s_separate(g.n, g.c); }
fn _w492() void { opt(_w491); }
fn _w493() void { c_ns_flow_map_separate_value(g.n, g.c); }
fn _w494() void { peg_seq(&[_]PFn{_w492, _w493}); }
fn _w495() void { e_node(); }
fn _w496() void { peg_alt(&[_]PFn{_w494, _w495}); }
fn _w497() void { e_node(); }
fn _w498() void { c_ns_flow_map_separate_value(g.n, g.c); }
fn _w499() void { match_cp(58); }
fn _w500() void { ns_plain_safe(g.c); }
fn _w501() void { r_neg(_w500); }
fn _w502() void { s_separate(g.n, g.c); }
fn _w503() void { ns_flow_node(g.n, g.c); }
fn _w504() void { peg_seq(&[_]PFn{_w502, _w503}); }
fn _w505() void { e_node(); }
fn _w506() void { peg_alt(&[_]PFn{_w504, _w505}); }
fn _w507() void { c_flow_json_node(g.n, g.c); }
fn _w508() void { s_separate(g.n, g.c); }
fn _w509() void { opt(_w508); }
fn _w510() void { c_ns_flow_map_adjacent_value(g.n, g.c); }
fn _w511() void { peg_seq(&[_]PFn{_w509, _w510}); }
fn _w512() void { e_node(); }
fn _w513() void { peg_alt(&[_]PFn{_w511, _w512}); }
fn _w514() void { match_cp(58); }
fn _w515() void { s_separate(g.n, g.c); }
fn _w516() void { opt(_w515); }
fn _w517() void { ns_flow_node(g.n, g.c); }
fn _w518() void { peg_seq(&[_]PFn{_w516, _w517}); }
fn _w519() void { e_node(); }
fn _w520() void { peg_alt(&[_]PFn{_w518, _w519}); }
fn _w521() void { match_cp(63); }
fn _w522() void { s_separate(g.n, g.c); }
fn _w523() void { ns_flow_map_explicit_entry(g.n, g.c); }
fn _w524() void { peg_seq(&[_]PFn{_w521, _w522, _w523}); }
fn _w525() void { ns_flow_pair_entry(g.n, g.c); }
fn _w526() void { ns_flow_pair_yaml_key_entry(g.n, g.c); }
fn _w527() void { c_ns_flow_map_empty_key_entry(g.n, g.c); }
fn _w528() void { c_ns_flow_pair_json_key_entry(g.n, g.c); }
fn _w529() void { ns_s_implicit_yaml_key("FLOW-KEY"); }
fn _w530() void { c_ns_flow_map_separate_value(g.n, g.c); }
fn _w531() void { c_s_implicit_json_key("FLOW-KEY"); }
fn _w532() void { c_ns_flow_map_adjacent_value(g.n, g.c); }
fn _w533() void { ns_flow_yaml_node(0, g.c); }
fn _w534() void { s_separate_in_line(); }
fn _w535() void { opt(_w534); }
fn _w536() void { c_flow_json_node(0, g.c); }
fn _w537() void { s_separate_in_line(); }
fn _w538() void { opt(_w537); }
fn _w539() void { c_flow_sequence(g.n, g.c); }
fn _w540() void { c_flow_mapping(g.n, g.c); }
fn _w541() void { c_single_quoted(g.n, g.c); }
fn _w542() void { c_double_quoted(g.n, g.c); }
fn _w543() void { ns_flow_yaml_content(g.n, g.c); }
fn _w544() void { c_flow_json_content(g.n, g.c); }
fn _w545() void { c_ns_alias_node(); }
fn _w546() void { ns_flow_yaml_content(g.n, g.c); }
fn _w547() void { c_ns_properties(g.n, g.c); }
fn _w548() void { s_separate(g.n, g.c); }
fn _w549() void { ns_flow_yaml_content(g.n, g.c); }
fn _w550() void { peg_seq(&[_]PFn{_w548, _w549}); }
fn _w551() void { e_scalar(); }
fn _w552() void { peg_alt(&[_]PFn{_w550, _w551}); }
fn _w553() void { peg_seq(&[_]PFn{_w547, _w552}); }
fn _w554() void { c_ns_properties(g.n, g.c); }
fn _w555() void { s_separate(g.n, g.c); }
fn _w556() void { peg_seq(&[_]PFn{_w554, _w555}); }
fn _w557() void { opt(_w556); }
fn _w558() void { c_flow_json_content(g.n, g.c); }
fn _w559() void { c_ns_alias_node(); }
fn _w560() void { ns_flow_content(g.n, g.c); }
fn _w561() void { c_ns_properties(g.n, g.c); }
fn _w562() void { s_separate(g.n, g.c); }
fn _w563() void { ns_flow_content(g.n, g.c); }
fn _w564() void { peg_seq(&[_]PFn{_w562, _w563}); }
fn _w565() void { e_scalar(); }
fn _w566() void { peg_alt(&[_]PFn{_w564, _w565}); }
fn _w567() void { peg_seq(&[_]PFn{_w561, _w566}); }
fn _w568() void { ns_dec_digit(); }
fn _w569() void { parse_int_fn(_w568); }
fn _w570() void { detect_indent(g.n); }
fn _w571() void { match_cp(45); }
fn _w572() void { parse_sym_fn(_w571, "STRIP"); }
fn _w573() void { match_cp(43); }
fn _w574() void { parse_sym_fn(_w573, "KEEP"); }
fn _w575() void { val_fn("CLIP"); }
fn _w576() void { blk2: { peg_alt(&[_]PFn{_w569, _w570}); if (g.failed) break :blk2; const m = g.rtagint; save_inp(); blk1: { peg_alt(&[_]PFn{_w572, _w574, _w575}); if (g.failed) break :blk1; const t = g.rtag; save_inp(); s_b_comment() } }; }
fn _w577() void { match_cp(45); }
fn _w578() void { parse_sym_fn(_w577, "STRIP"); }
fn _w579() void { match_cp(43); }
fn _w580() void { parse_sym_fn(_w579, "KEEP"); }
fn _w581() void { val_fn("CLIP"); }
fn _w582() void { ns_dec_digit(); }
fn _w583() void { parse_int_fn(_w582); }
fn _w584() void { detect_indent(g.n); }
fn _w585() void { blk4: { peg_alt(&[_]PFn{_w578, _w580, _w581}); if (g.failed) break :blk4; const t = g.rtag; save_inp(); blk3: { peg_alt(&[_]PFn{_w583, _w584}); if (g.failed) break :blk3; const m = g.rtagint; save_inp(); s_b_comment() } }; }
fn _w586() void { ns_dec_digit(); }
fn _w587() void { ok(); }
fn _w588() void { match_cp(45); }
fn _w589() void { match_cp(43); }
fn _w590() void { ok(); }
fn _w591() void { b_non_content(); }
fn _w592() void { b_as_line_feed(); }
fn _w593() void { b_as_line_feed(); }
fn _w594() void { l_strip_empty(g.n); }
fn _w595() void { l_strip_empty(g.n); }
fn _w596() void { l_keep_empty(g.n); }
fn _w597() void { s_indent_le(g.n); }
fn _w598() void { b_non_content(); }
fn _w599() void { peg_seq(&[_]PFn{_w597, _w598}); }
fn _w600() void { peg_star(_w599); }
fn _w601() void { l_trail_comments(g.n); }
fn _w602() void { opt(_w601); }
fn _w603() void { l_empty(g.n, "BLOCK-IN"); }
fn _w604() void { peg_star(_w603); }
fn _w605() void { l_trail_comments(g.n); }
fn _w606() void { opt(_w605); }
fn _w607() void { s_indent_lt(g.n); }
fn _w608() void { c_nb_comment_text(); }
fn _w609() void { b_comment(); }
fn _w610() void { l_comment(); }
fn _w611() void { peg_star(_w610); }
fn _w612() void { match_cp(124); }
fn _w613() void { ns_dec_digit(); }
fn _w614() void { parse_int_fn(_w613); }
fn _w615() void { detect_indent(g.n); }
fn _w616() void { match_cp(45); }
fn _w617() void { parse_sym_fn(_w616, "STRIP"); }
fn _w618() void { match_cp(43); }
fn _w619() void { parse_sym_fn(_w618, "KEEP"); }
fn _w620() void { val_fn("CLIP"); }
fn _w621() void { s_b_comment(); }
fn _w622() void { l_literal_content($(( g.n + g.m )), g.t); }
fn _w623() void { blk6: { peg_alt(&[_]PFn{_w614, _w615}); if (g.failed) break :blk6; const m = g.rtagint; save_inp(); blk5: { peg_alt(&[_]PFn{_w617, _w619, _w620}); if (g.failed) break :blk5; const t = g.rtag; save_inp(); peg_seq(&[_]PFn{_w621, _w622}) } }; }
fn _w624() void { l_empty(g.n, "BLOCK-IN"); }
fn _w625() void { peg_star(_w624); }
fn _w626() void { s_indent(g.n); }
fn _w627() void { nb_char(); }
fn _w628() void { plus_(_w627); }
fn _w629() void { b_as_line_feed(); }
fn _w630() void { l_nb_literal_text(g.n); }
fn _w631() void { l_nb_literal_text(g.n); }
fn _w632() void { b_nb_literal_next(g.n); }
fn _w633() void { peg_star(_w632); }
fn _w634() void { b_chomped_last(g.t); }
fn _w635() void { peg_seq(&[_]PFn{_w631, _w633, _w634}); }
fn _w636() void { opt(_w635); }
fn _w637() void { l_chomped_empty(g.n, g.t); }
fn _w638() void { peg_seq(&[_]PFn{_w636, _w637}); }
fn _w639() void { match_cp(62); }
fn _w640() void { ns_dec_digit(); }
fn _w641() void { parse_int_fn(_w640); }
fn _w642() void { detect_indent(g.n); }
fn _w643() void { match_cp(45); }
fn _w644() void { parse_sym_fn(_w643, "STRIP"); }
fn _w645() void { match_cp(43); }
fn _w646() void { parse_sym_fn(_w645, "KEEP"); }
fn _w647() void { val_fn("CLIP"); }
fn _w648() void { s_b_comment(); }
fn _w649() void { l_folded_content($(( g.n + g.m )), g.t); }
fn _w650() void { blk8: { peg_alt(&[_]PFn{_w641, _w642}); if (g.failed) break :blk8; const m = g.rtagint; save_inp(); blk7: { peg_alt(&[_]PFn{_w644, _w646, _w647}); if (g.failed) break :blk7; const t = g.rtag; save_inp(); peg_seq(&[_]PFn{_w648, _w649}) } }; }
fn _w651() void { s_indent(g.n); }
fn _w652() void { ns_char(); }
fn _w653() void { nb_char(); }
fn _w654() void { peg_star(_w653); }
fn _w655() void { s_nb_folded_text(g.n); }
fn _w656() void { b_l_folded(g.n, "BLOCK-IN"); }
fn _w657() void { s_nb_folded_text(g.n); }
fn _w658() void { peg_seq(&[_]PFn{_w656, _w657}); }
fn _w659() void { peg_star(_w658); }
fn _w660() void { s_indent(g.n); }
fn _w661() void { s_white(); }
fn _w662() void { nb_char(); }
fn _w663() void { peg_star(_w662); }
fn _w664() void { b_as_line_feed(); }
fn _w665() void { l_empty(g.n, "BLOCK-IN"); }
fn _w666() void { peg_star(_w665); }
fn _w667() void { s_nb_spaced_text(g.n); }
fn _w668() void { b_l_spaced(g.n); }
fn _w669() void { s_nb_spaced_text(g.n); }
fn _w670() void { peg_seq(&[_]PFn{_w668, _w669}); }
fn _w671() void { peg_star(_w670); }
fn _w672() void { l_empty(g.n, "BLOCK-IN"); }
fn _w673() void { peg_star(_w672); }
fn _w674() void { l_nb_folded_lines(g.n); }
fn _w675() void { l_nb_spaced_lines(g.n); }
fn _w676() void { peg_alt(&[_]PFn{_w674, _w675}); }
fn _w677() void { l_nb_same_lines(g.n); }
fn _w678() void { b_as_line_feed(); }
fn _w679() void { l_nb_same_lines(g.n); }
fn _w680() void { peg_seq(&[_]PFn{_w678, _w679}); }
fn _w681() void { peg_star(_w680); }
fn _w682() void { l_nb_diff_lines(g.n); }
fn _w683() void { b_chomped_last(g.t); }
fn _w684() void { peg_seq(&[_]PFn{_w682, _w683}); }
fn _w685() void { opt(_w684); }
fn _w686() void { l_chomped_empty(g.n, g.t); }
fn _w687() void { peg_seq(&[_]PFn{_w685, _w686}); }
fn _w688() void { s_indent($(( g.n + g.m ))); }
fn _w689() void { c_l_block_seq_entry($(( g.n + g.m ))); }
fn _w690() void { peg_seq(&[_]PFn{_w688, _w689}); }
fn _w691() void { plus_(_w690); }
fn _w692() void { blk9: { detect_indent(g.n); if (g.failed) break :blk9; const m = g.rtagint; save_inp(); collect_fn(_w691) }; }
fn _w693() void { match_cp(45); }
fn _w694() void { ns_char(); }
fn _w695() void { r_neg(_w694); }
fn _w696() void { s_lblock_indented(g.n, "BLOCK-IN"); }
fn _w697() void { s_indent(g.m); }
fn _w698() void { ns_l_compact_sequence($(( g.n + 1 + g.m ))); }
fn _w699() void { ns_l_compact_mapping($(( g.n + 1 + g.m ))); }
fn _w700() void { peg_alt(&[_]PFn{_w698, _w699}); }
fn _w701() void { blk10: { detect_indent(0); if (g.failed) break :blk10; const m = g.rtagint; save_inp(); peg_seq(&[_]PFn{_w697, _w700}) }; }
fn _w702() void { s_lblock_node(g.n, g.c); }
fn _w703() void { e_node(); }
fn _w704() void { s_l_comments(); }
fn _w705() void { peg_seq(&[_]PFn{_w703, _w704}); }
fn _w706() void { c_l_block_seq_entry(g.n); }
fn _w707() void { s_indent(g.n); }
fn _w708() void { c_l_block_seq_entry(g.n); }
fn _w709() void { peg_seq(&[_]PFn{_w707, _w708}); }
fn _w710() void { peg_star(_w709); }
fn _w711() void { s_indent($(( g.n + g.m ))); }
fn _w712() void { ns_l_block_map_entry($(( g.n + g.m ))); }
fn _w713() void { peg_seq(&[_]PFn{_w711, _w712}); }
fn _w714() void { plus_(_w713); }
fn _w715() void { blk11: { detect_indent(g.n); if (g.failed) break :blk11; const m = g.rtagint; save_inp(); collect_fn(_w714) }; }
fn _w716() void { c_l_block_map_explicit_entry(g.n); }
fn _w717() void { ns_l_block_map_implicit_entry(g.n); }
fn _w718() void { c_l_block_map_explicit_key(g.n); }
fn _w719() void { l_block_map_explicit_value(g.n); }
fn _w720() void { e_node(); }
fn _w721() void { peg_alt(&[_]PFn{_w719, _w720}); }
fn _w722() void { match_cp(63); }
fn _w723() void { s_lblock_indented(g.n, "BLOCK-OUT"); }
fn _w724() void { s_indent(g.n); }
fn _w725() void { match_cp(58); }
fn _w726() void { s_lblock_indented(g.n, "BLOCK-OUT"); }
fn _w727() void { ns_s_block_map_implicit_key(); }
fn _w728() void { e_node(); }
fn _w729() void { peg_alt(&[_]PFn{_w727, _w728}); }
fn _w730() void { scalar_fn(_w729); }
fn _w731() void { c_l_block_map_implicit_value(g.n); }
fn _w732() void { peg_seq(&[_]PFn{_w730, _w731}); }
fn _w733() void { c_s_implicit_json_key("BLOCK-KEY"); }
fn _w734() void { ns_s_implicit_yaml_key("BLOCK-KEY"); }
fn _w735() void { match_cp(58); }
fn _w736() void { s_lblock_node(g.n, "BLOCK-OUT"); }
fn _w737() void { e_node(); }
fn _w738() void { s_l_comments(); }
fn _w739() void { peg_seq(&[_]PFn{_w737, _w738}); }
fn _w740() void { scalar_fn(_w739); }
fn _w741() void { peg_alt(&[_]PFn{_w736, _w740}); }
fn _w742() void { ns_l_block_map_entry(g.n); }
fn _w743() void { s_indent(g.n); }
fn _w744() void { ns_l_block_map_entry(g.n); }
fn _w745() void { peg_seq(&[_]PFn{_w743, _w744}); }
fn _w746() void { peg_star(_w745); }
fn _w747() void { s_lblock_in_block(g.n, g.c); }
fn _w748() void { s_lflow_in_block(g.n); }
fn _w749() void { s_separate($(( g.n + 1 )), "FLOW-OUT"); }
fn _w750() void { ns_flow_node($(( g.n + 1 )), "FLOW-OUT"); }
fn _w751() void { s_l_comments(); }
fn _w752() void { s_lblock_scalar(g.n, g.c); }
fn _w753() void { s_lblock_collection(g.n, g.c); }
fn _w754() void { s_separate($(( g.n + 1 )), g.c); }
fn _w755() void { c_ns_properties($(( g.n + 1 )), g.c); }
fn _w756() void { s_separate($(( g.n + 1 )), g.c); }
fn _w757() void { peg_seq(&[_]PFn{_w755, _w756}); }
fn _w758() void { opt(_w757); }
fn _w759() void { c_lliteral(g.n); }
fn _w760() void { c_lfolded(g.n); }
fn _w761() void { peg_alt(&[_]PFn{_w759, _w760}); }
fn _w762() void { s_separate($(( g.n + 1 )), g.c); }
fn _w763() void { c_ns_properties($(( g.n + 1 )), g.c); }
fn _w764() void { peg_seq(&[_]PFn{_w762, _w763}); }
fn _w765() void { opt(_w764); }
fn _w766() void { s_l_comments(); }
fn _w767() void { lblock_sequence(seq_spaces(g.n, g.c)); }
fn _w768() void { lblock_mapping(g.n); }
fn _w769() void { peg_alt(&[_]PFn{_w767, _w768}); }
fn _w770() void { c_byte_order_mark(); }
fn _w771() void { opt(_w770); }
fn _w772() void { l_comment(); }
fn _w773() void { peg_star(_w772); }
fn _w774() void { c_document_end(); }
fn _w775() void { s_l_comments(); }
fn _w776() void { sol(); }
fn _w777() void { c_directives_end(); }
fn _w778() void { c_document_end(); }
fn _w779() void { peg_alt(&[_]PFn{_w777, _w778}); }
fn _w780() void { b_char(); }
fn _w781() void { s_white(); }
fn _w782() void { eof_ok(); }
fn _w783() void { peg_alt(&[_]PFn{_w780, _w781, _w782}); }
fn _w784() void { s_lblock_node(-1, "BLOCK-IN"); }
fn _w785() void { c_directives_end(); }
fn _w786() void { l_bare_document(); }
fn _w787() void { e_node(); }
fn _w788() void { s_l_comments(); }
fn _w789() void { peg_seq(&[_]PFn{_w787, _w788}); }
fn _w790() void { peg_alt(&[_]PFn{_w786, _w789}); }
fn _w791() void { peg_seq(&[_]PFn{_w785, _w790}); }
fn _w792() void { l_directive(); }
fn _w793() void { plus_(_w792); }
fn _w794() void { l_explicit_document(); }
fn _w795() void { l_directive_document(); }
fn _w796() void { l_explicit_document(); }
fn _w797() void { l_bare_document(); }
fn _w798() void { l_document_prefix(); }
fn _w799() void { peg_star(_w798); }
fn _w800() void { l_any_document(); }
fn _w801() void { opt(_w800); }
fn _w802() void { l_document_suffix(); }
fn _w803() void { plus_(_w802); }
fn _w804() void { l_document_prefix(); }
fn _w805() void { peg_star(_w804); }
fn _w806() void { l_any_document(); }
fn _w807() void { opt(_w806); }
fn _w808() void { peg_seq(&[_]PFn{_w803, _w805, _w807}); }
fn _w809() void { l_document_prefix(); }
fn _w810() void { peg_star(_w809); }
fn _w811() void { l_explicit_document(); }
fn _w812() void { opt(_w811); }
fn _w813() void { peg_seq(&[_]PFn{_w810, _w812}); }
fn _w814() void { peg_alt(&[_]PFn{_w808, _w813}); }
fn _w815() void { peg_star(_w814); }
fn _w816() void { peg_seq(&[_]PFn{_w799, _w801, _w815}); }

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
    peg_alt(&[_]PFn{
        _w31,
        _w32,
        _w33,
        _w34,
        _w35,
        _w36,
        _w37,
        _w38,
        _w39,
        _w40,
        _w41,
        _w42,
        _w43,
        _w44,
        _w45,
        _w46,
        _w47,
        _w48});
}

// [23] C-FLOW-INDICATOR 
fn c_flow_indicator() void {
    peg_alt(&[_]PFn{_w49, _w50, _w51, _w52, _w53});
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
    peg_alt(&[_]PFn{_w54, _w55});
}

// [27] NB-CHAR 
fn nb_char() void {
    minus_fn(_w56, _w59);
}

// [28] B-BREAK 
fn b_break() void {
    peg_alt(&[_]PFn{_w62, _w63, _w64});
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
    peg_alt(&[_]PFn{_w65, _w66});
}

// [34] NS-CHAR 
fn ns_char() void {
    minus_fn(_w67, _w68);
}

// [35] NS-DEC-DIGIT 
fn ns_dec_digit() void {
    match_range(0x30, 0x39);
}

// [36] NS-HEX-DIGIT 
fn ns_hex_digit() void {
    peg_alt(&[_]PFn{_w69, _w70, _w71});
}

// [37] NS-ASCII-LETTER 
fn ns_ascii_letter() void {
    peg_alt(&[_]PFn{_w72, _w73});
}

// [38] NS-WORD-CHAR 
fn ns_word_char() void {
    peg_alt(&[_]PFn{_w74, _w75, _w76});
}

// [39] NS-URI-CHAR 
fn ns_uri_char() void {
    peg_alt(&[_]PFn{
        _w106,
        _w107,
        _w108,
        _w109,
        _w110,
        _w111,
        _w112,
        _w113,
        _w114,
        _w115,
        _w116,
        _w117,
        _w118,
        _w119,
        _w120,
        _w121,
        _w122,
        _w123,
        _w124,
        _w125,
        _w126,
        _w127,
        _w128});
}

// [40] NS-TAG-CHAR 
fn ns_tag_char() void {
    minus_fn(_w129, _w132);
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
    peg_seq(&[_]PFn{_w133, _w135});
}

// [60] NS-ESC-16-BIT 
fn ns_esc_16_bit() void {
    peg_seq(&[_]PFn{_w136, _w138});
}

// [61] NS-ESC-32-BIT 
fn ns_esc_32_bit() void {
    peg_seq(&[_]PFn{_w139, _w141});
}

// [62] C-NS-ESC-CHAR 
fn c_ns_esc_char() void {
    peg_seq(&[_]PFn{_w142, _w183});
}

// [63] S-INDENT 
fn s_indent() void {
    rep_fn(g.n, _w184);
}

// [64] S-INDENT-LT 
fn s_indent_lt() void {
    peg_star(_w185);
}

// [65] S-INDENT-LE 
fn s_indent_le() void {
    peg_star(_w186);
}

// [66] S-SEPARATE-IN-LINE 
fn s_separate_in_line() void {
    peg_alt(&[_]PFn{_w188, _w189});
}

// [67] S-LINE-PREFIX 
fn s_line_prefix() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "BLOCK-IN", .func = _w190 },
        .{ .ctx = "BLOCK-OUT", .func = _w191 },
        .{ .ctx = "FLOW-IN", .func = _w192 },
        .{ .ctx = "FLOW-OUT", .func = _w193 },
    });
}

// [68] S-BLOCK-LINE-PREFIX 
fn s_block_line_prefix() void {
    s_indent(g.n);
}

// [69] S-FLOW-LINE-PREFIX 
fn s_flow_line_prefix() void {
    peg_seq(&[_]PFn{_w194, _w196});
}

// [70] L-EMPTY 
fn l_empty() void {
    peg_seq(&[_]PFn{_w199, _w200});
}

// [71] B-L-TRIMMED 
fn b_l_trimmed() void {
    peg_seq(&[_]PFn{_w201, _w203});
}

// [72] B-AS-SPACE 
fn b_as_space() void {
    b_break();
}

// [73] B-L-FOLDED 
fn b_l_folded() void {
    peg_alt(&[_]PFn{_w204, _w205});
}

// [74] S-FLOW-FOLDED 
fn s_flow_folded() void {
    peg_seq(&[_]PFn{_w207, _w208, _w209});
}

// [75] C-NB-COMMENT-TEXT 
fn c_nb_comment_text() void {
    peg_seq(&[_]PFn{_w210, _w212});
}

// [76] B-COMMENT 
fn b_comment() void {
    peg_alt(&[_]PFn{_w213, _w214});
}

// [77] S-B-COMMENT 
fn s_b_comment() void {
    peg_seq(&[_]PFn{_w219, _w220});
}

// [78] L-COMMENT 
fn l_comment() void {
    peg_seq(&[_]PFn{_w221, _w223, _w224});
}

// [79] S-L-COMMENTS 
fn s_l_comments() void {
    peg_seq(&[_]PFn{_w227, _w229});
}

// [80] S-SEPARATE 
fn s_separate() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "BLOCK-OUT", .func = _w230 },
        .{ .ctx = "BLOCK-IN", .func = _w231 },
        .{ .ctx = "FLOW-OUT", .func = _w232 },
        .{ .ctx = "FLOW-IN", .func = _w233 },
        .{ .ctx = "BLOCK-KEY", .func = _w234 },
        .{ .ctx = "FLOW-KEY", .func = _w235 },
    });
}

// [81] S-SEPARATE-LINES 
fn s_separate_lines() void {
    peg_alt(&[_]PFn{_w238, _w239});
}

// [82] L-DIRECTIVE 
fn l_directive() void {
    peg_seq(&[_]PFn{_w240, _w244, _w245});
}

// [83] NS-RESERVED-DIRECTIVE 
fn ns_reserved_directive() void {
    peg_seq(&[_]PFn{_w246, _w250});
}

// [84] NS-DIRECTIVE-NAME 
fn ns_directive_name() void {
    plus_(_w251);
}

// [85] NS-DIRECTIVE-PARAMETER 
fn ns_directive_parameter() void {
    plus_(_w252);
}

// [86] NS-YAML-DIRECTIVE 
fn ns_yaml_directive() void {
    peg_seq(&[_]PFn{_w253, _w254, _w255});
}

// [87] NS-YAML-VERSION 
fn ns_yaml_version() void {
    peg_seq(&[_]PFn{_w257, _w258, _w260});
}

// [88] NS-TAG-DIRECTIVE 
fn ns_tag_directive() void {
    peg_seq(&[_]PFn{_w261, _w262, _w263, _w264, _w265});
}

// [89] C-TAG-HANDLE 
fn c_tag_handle() void {
    peg_alt(&[_]PFn{_w266, _w267, _w268});
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
    peg_seq(&[_]PFn{_w269, _w271, _w272});
}

// [93] NS-TAG-PREFIX 
fn ns_tag_prefix() void {
    peg_alt(&[_]PFn{_w273, _w274});
}

// [94] C-NS-LOCAL-TAG-PREFIX 
fn c_ns_local_tag_prefix() void {
    peg_seq(&[_]PFn{_w275, _w277});
}

// [95] NS-GLOBAL-TAG-PREFIX 
fn ns_global_tag_prefix() void {
    peg_seq(&[_]PFn{_w278, _w280});
}

// [96] C-NS-PROPERTIES 
fn c_ns_properties() void {
    peg_alt(&[_]PFn{_w286, _w292});
}

// [97] C-NS-TAG-PROPERTY 
fn c_ns_tag_property() void {
    peg_alt(&[_]PFn{_w293, _w294, _w295});
}

// [98] C-VERBATIM-TAG 
fn c_verbatim_tag() void {
    peg_seq(&[_]PFn{_w296, _w298, _w299});
}

// [99] C-NS-SHORTHAND-TAG 
fn c_ns_shorthand_tag() void {
    peg_seq(&[_]PFn{_w300, _w302});
}

// [100] C-NON-SPECIFIC-TAG 
fn c_non_specific_tag() void {
    match_cp(33);
}

// [101] C-NS-ANCHOR-PROPERTY 
fn c_ns_anchor_property() void {
    build_ast("ANCHOR", _w306);
}

// [102] NS-ANCHOR-CHAR 
fn ns_anchor_char() void {
    minus_fn(_w307, _w308);
}

// [103] NS-ANCHOR-NAME 
fn ns_anchor_name() void {
    plus_(_w309);
}

// [104] C-NS-ALIAS-NODE 
fn c_ns_alias_node() void {
    build_ast("ALIAS", _w313);
}

// [105] E-SCALAR 
fn e_scalar() void {
    ok();
}

// [106] E-NODE 
fn e_node() void {
    e_scalar();
}

// [107] NB-DOUBLE-CHAR 
fn nb_double_char() void {
    peg_alt(&[_]PFn{_w314, _w319});
}

// [108] NS-DOUBLE-CHAR 
fn ns_double_char() void {
    minus_fn(_w320, _w321);
}

// [109] C-DOUBLE-QUOTED 
fn c_double_quoted() void {
    scalar_fn(_w325);
}

// [110] NB-DOUBLE-TEXT 
fn nb_double_text() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w326 },
        .{ .ctx = "FLOW-IN", .func = _w327 },
        .{ .ctx = "BLOCK-KEY", .func = _w328 },
        .{ .ctx = "FLOW-KEY", .func = _w329 },
    });
}

// [111] NB-DOUBLE-ONE-LINE 
fn nb_double_one_line() void {
    peg_star(_w330);
}

// [112] S-DOUBLE-ESCAPED 
fn s_double_escaped() void {
    peg_seq(&[_]PFn{_w332, _w333, _w334, _w336, _w337});
}

// [113] S-DOUBLE-BREAK 
fn s_double_break() void {
    peg_alt(&[_]PFn{_w338, _w339});
}

// [114] NB-NS-DOUBLE-IN-LINE 
fn nb_ns_double_in_line() void {
    peg_star(_w343);
}

// [115] S-DOUBLE-NEXT-LINE 
fn s_double_next_line() void {
    peg_seq(&[_]PFn{_w344, _w352});
}

// [116] NB-DOUBLE-MULTI-LINE 
fn nb_double_multi_line() void {
    peg_seq(&[_]PFn{_w353, _w357});
}

// [117] C-QUOTED-QUOTE 
fn c_quoted_quote() void {
    match_str("''");
}

// [118] NB-SINGLE-CHAR 
fn nb_single_char() void {
    peg_alt(&[_]PFn{_w358, _w361});
}

// [119] NS-SINGLE-CHAR 
fn ns_single_char() void {
    minus_fn(_w362, _w363);
}

// [120] C-SINGLE-QUOTED 
fn c_single_quoted() void {
    scalar_fn(_w367);
}

// [121] NB-SINGLE-TEXT 
fn nb_single_text() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w368 },
        .{ .ctx = "FLOW-IN", .func = _w369 },
        .{ .ctx = "BLOCK-KEY", .func = _w370 },
        .{ .ctx = "FLOW-KEY", .func = _w371 },
    });
}

// [122] NB-SINGLE-ONE-LINE 
fn nb_single_one_line() void {
    peg_star(_w372);
}

// [123] NS-SINGLE-IN-LINE 
fn ns_single_in_line() void {
    peg_star(_w376);
}

// [124] S-SINGLE-NEXT-LINE 
fn s_single_next_line() void {
    peg_seq(&[_]PFn{_w377, _w385});
}

// [125] NB-SINGLE-MULTI-LINE 
fn nb_single_multi_line() void {
    peg_seq(&[_]PFn{_w386, _w390});
}

// [126] NS-PLAIN-FIRST 
fn ns_plain_first() void {
    peg_alt(&[_]PFn{_w393, _w400});
}

// [127] NS-PLAIN-SAFE 
fn ns_plain_safe() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .func = _w401 },
        .{ .ctx = "FLOW-IN", .func = _w402 },
        .{ .ctx = "BLOCK-KEY", .func = _w403 },
        .{ .ctx = "FLOW-KEY", .func = _w404 },
    });
}

// [128] NS-PLAIN-SAFE-OUT 
fn ns_plain_safe_out() void {
    ns_char();
}

// [129] NS-PLAIN-SAFE-IN 
fn ns_plain_safe_in() void {
    minus_fn(_w405, _w406);
}

// [130] NS-PLAIN-CHAR 
fn ns_plain_char() void {
    peg_alt(&[_]PFn{_w411, _w415, _w419});
}

// [131] NS-PLAIN 
fn ns_plain() void {
    scalar_fn(_w424);
}

// [132] NB-NS-PLAIN-IN-LINE 
fn nb_ns_plain_in_line() void {
    peg_star(_w428);
}

// [133] NS-PLAIN-ONE-LINE 
fn ns_plain_one_line() void {
    peg_seq(&[_]PFn{_w429, _w430});
}

// [134] S-NS-PLAIN-NEXT-LINE 
fn s_ns_plain_next_line() void {
    peg_seq(&[_]PFn{_w431, _w433, _w434, _w435});
}

// [135] NS-PLAIN-MULTI-LINE 
fn ns_plain_multi_line() void {
    peg_seq(&[_]PFn{_w436, _w438});
}

// [137] C-FLOW-SEQUENCE 
fn c_flow_sequence() void {
    build_ast("SEQUENCE", _w446);
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
fn ns_s_flow_seq_entries() void {
    peg_seq(&[_]PFn{_w447, _w449, _w456});
}

// [139] NS-FLOW-SEQ-ENTRY 
fn ns_flow_seq_entry() void {
    peg_alt(&[_]PFn{_w457, _w458});
}

// [140] C-FLOW-MAPPING 
fn c_flow_mapping() void {
    build_ast("MAPPING", _w466);
}

// [141] NS-S-FLOW-MAP-ENTRIES 
fn ns_s_flow_map_entries() void {
    peg_seq(&[_]PFn{_w467, _w469, _w476});
}

// [142] NS-FLOW-MAP-ENTRY 
fn ns_flow_map_entry() void {
    peg_alt(&[_]PFn{_w480, _w481});
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
fn ns_flow_map_explicit_entry() void {
    peg_alt(&[_]PFn{_w482, _w485});
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
fn ns_flow_map_implicit_entry() void {
    build_ast("PAIR", _w489);
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
fn ns_flow_map_yaml_key_entry() void {
    peg_seq(&[_]PFn{_w490, _w496});
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
fn c_ns_flow_map_empty_key_entry() void {
    peg_seq(&[_]PFn{_w497, _w498});
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
fn c_ns_flow_map_separate_value() void {
    peg_seq(&[_]PFn{_w499, _w501, _w506});
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
fn c_ns_flow_map_json_key_entry() void {
    peg_seq(&[_]PFn{_w507, _w513});
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
fn c_ns_flow_map_adjacent_value() void {
    peg_seq(&[_]PFn{_w514, _w520});
}

// [150] NS-FLOW-PAIR 
fn ns_flow_pair() void {
    peg_alt(&[_]PFn{_w524, _w525});
}

// [151] NS-FLOW-PAIR-ENTRY 
fn ns_flow_pair_entry() void {
    peg_alt(&[_]PFn{_w526, _w527, _w528});
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
fn ns_flow_pair_yaml_key_entry() void {
    peg_seq(&[_]PFn{_w529, _w530});
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
fn c_ns_flow_pair_json_key_entry() void {
    peg_seq(&[_]PFn{_w531, _w532});
}

// [154] NS-S-IMPLICIT-YAML-KEY 
fn ns_s_implicit_yaml_key() void {
    peg_seq(&[_]PFn{_w533, _w535});
}

// [155] C-S-IMPLICIT-JSON-KEY 
fn c_s_implicit_json_key() void {
    peg_seq(&[_]PFn{_w536, _w538});
}

// [156] NS-FLOW-YAML-CONTENT 
fn ns_flow_yaml_content() void {
    ns_plain(g.n, g.c);
}

// [157] C-FLOW-JSON-CONTENT 
fn c_flow_json_content() void {
    peg_alt(&[_]PFn{_w539, _w540, _w541, _w542});
}

// [158] NS-FLOW-CONTENT 
fn ns_flow_content() void {
    peg_alt(&[_]PFn{_w543, _w544});
}

// [159] NS-FLOW-YAML-NODE 
fn ns_flow_yaml_node() void {
    peg_alt(&[_]PFn{_w545, _w546, _w553});
}

// [160] C-FLOW-JSON-NODE 
fn c_flow_json_node() void {
    peg_seq(&[_]PFn{_w557, _w558});
}

// [161] NS-FLOW-NODE 
fn ns_flow_node() void {
    peg_alt(&[_]PFn{_w559, _w560, _w567});
}

// [162] C-B-BLOCK-HEADER 
fn c_b_block_header() void {
    peg_alt(&[_]PFn{_w576, _w585});
}

// [163] C-INDENTATION-INDICATOR 
fn c_indentation_indicator() void {
    peg_alt(&[_]PFn{_w586, _w587});
}

// [164] C-CHOMPING-INDICATOR 
fn c_chomping_indicator() void {
    peg_alt(&[_]PFn{_w588, _w589, _w590});
}

// [165] B-CHOMPED-LAST 
fn b_chomped_last() void {
    switch_ctx(t, &[_]CtxCase{
        .{ .ctx = "STRIP", .func = _w591 },
        .{ .ctx = "CLIP", .func = _w592 },
        .{ .ctx = "KEEP", .func = _w593 },
    });
}

// [166] L-CHOMPED-EMPTY 
fn l_chomped_empty() void {
    switch_ctx(t, &[_]CtxCase{
        .{ .ctx = "STRIP", .func = _w594 },
        .{ .ctx = "CLIP", .func = _w595 },
        .{ .ctx = "KEEP", .func = _w596 },
    });
}

// [167] L-STRIP-EMPTY 
fn l_strip_empty() void {
    peg_seq(&[_]PFn{_w600, _w602});
}

// [168] L-KEEP-EMPTY 
fn l_keep_empty() void {
    peg_seq(&[_]PFn{_w604, _w606});
}

// [169] L-TRAIL-COMMENTS 
fn l_trail_comments() void {
    peg_seq(&[_]PFn{_w607, _w608, _w609, _w611});
}

// [170] C-L+LITERAL 
fn c_lliteral() void {
    peg_seq(&[_]PFn{_w612, _w623});
}

// [171] L-NB-LITERAL-TEXT 
fn l_nb_literal_text() void {
    peg_seq(&[_]PFn{_w625, _w626, _w628});
}

// [172] B-NB-LITERAL-NEXT 
fn b_nb_literal_next() void {
    peg_seq(&[_]PFn{_w629, _w630});
}

// [173] L-LITERAL-CONTENT 
fn l_literal_content() void {
    scalar_fn(_w638);
}

// [174] C-L+FOLDED 
fn c_lfolded() void {
    peg_seq(&[_]PFn{_w639, _w650});
}

// [175] S-NB-FOLDED-TEXT 
fn s_nb_folded_text() void {
    peg_seq(&[_]PFn{_w651, _w652, _w654});
}

// [176] L-NB-FOLDED-LINES 
fn l_nb_folded_lines() void {
    peg_seq(&[_]PFn{_w655, _w659});
}

// [177] S-NB-SPACED-TEXT 
fn s_nb_spaced_text() void {
    peg_seq(&[_]PFn{_w660, _w661, _w663});
}

// [178] B-L-SPACED 
fn b_l_spaced() void {
    peg_seq(&[_]PFn{_w664, _w666});
}

// [179] L-NB-SPACED-LINES 
fn l_nb_spaced_lines() void {
    peg_seq(&[_]PFn{_w667, _w671});
}

// [180] L-NB-SAME-LINES 
fn l_nb_same_lines() void {
    peg_seq(&[_]PFn{_w673, _w676});
}

// [181] L-NB-DIFF-LINES 
fn l_nb_diff_lines() void {
    peg_seq(&[_]PFn{_w677, _w681});
}

// [182] L-FOLDED-CONTENT 
fn l_folded_content() void {
    scalar_fn(_w687);
}

// [183] L+BLOCK-SEQUENCE 
fn lblock_sequence() void {
    build_ast("SEQUENCE", _w692);
}

// [184] C-L-BLOCK-SEQ-ENTRY 
fn c_l_block_seq_entry() void {
    peg_seq(&[_]PFn{_w693, _w695, _w696});
}

// [185] S-L+BLOCK-INDENTED 
fn s_lblock_indented() void {
    peg_alt(&[_]PFn{_w701, _w702, _w705});
}

// [186] NS-L-COMPACT-SEQUENCE 
fn ns_l_compact_sequence() void {
    peg_seq(&[_]PFn{_w706, _w710});
}

// [187] L+BLOCK-MAPPING 
fn lblock_mapping() void {
    build_ast("MAPPING", _w715);
}

// [188] NS-L-BLOCK-MAP-ENTRY 
fn ns_l_block_map_entry() void {
    peg_alt(&[_]PFn{_w716, _w717});
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
fn c_l_block_map_explicit_entry() void {
    peg_seq(&[_]PFn{_w718, _w721});
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
fn c_l_block_map_explicit_key() void {
    peg_seq(&[_]PFn{_w722, _w723});
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
fn l_block_map_explicit_value() void {
    peg_seq(&[_]PFn{_w724, _w725, _w726});
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
fn ns_l_block_map_implicit_entry() void {
    build_ast("PAIR", _w732);
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
fn ns_s_block_map_implicit_key() void {
    peg_alt(&[_]PFn{_w733, _w734});
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
fn c_l_block_map_implicit_value() void {
    peg_seq(&[_]PFn{_w735, _w741});
}

// [195] NS-L-COMPACT-MAPPING 
fn ns_l_compact_mapping() void {
    peg_seq(&[_]PFn{_w742, _w746});
}

// [196] S-L+BLOCK-NODE 
fn s_lblock_node() void {
    peg_alt(&[_]PFn{_w747, _w748});
}

// [197] S-L+FLOW-IN-BLOCK 
fn s_lflow_in_block() void {
    peg_seq(&[_]PFn{_w749, _w750, _w751});
}

// [198] S-L+BLOCK-IN-BLOCK 
fn s_lblock_in_block() void {
    peg_alt(&[_]PFn{_w752, _w753});
}

// [199] S-L+BLOCK-SCALAR 
fn s_lblock_scalar() void {
    peg_seq(&[_]PFn{_w754, _w758, _w761});
}

// [200] S-L+BLOCK-COLLECTION 
fn s_lblock_collection() void {
    peg_seq(&[_]PFn{_w765, _w766, _w769});
}

// [202] L-DOCUMENT-PREFIX 
fn l_document_prefix() void {
    peg_seq(&[_]PFn{_w771, _w773});
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
    peg_seq(&[_]PFn{_w774, _w775});
}

// [206] C-FORBIDDEN 
fn c_forbidden() void {
    peg_seq(&[_]PFn{_w776, _w779, _w783});
}

// [207] L-BARE-DOCUMENT 
fn l_bare_document() void {
    build_ast("DOC", _w784);
}

// [208] L-EXPLICIT-DOCUMENT 
fn l_explicit_document() void {
    build_ast("DOC", _w791);
}

// [209] L-DIRECTIVE-DOCUMENT 
fn l_directive_document() void {
    peg_seq(&[_]PFn{_w793, _w794});
}

// [210] L-ANY-DOCUMENT 
fn l_any_document() void {
    peg_alt(&[_]PFn{_w795, _w796, _w797});
}

// [211] L-YAML-STREAM 
fn l_yaml_stream() void {
    build_ast("STREAM", _w816);
}



