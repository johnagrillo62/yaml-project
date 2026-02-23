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

_w1() { match_cp 0x9; }
_w2() { match_cp 0x0A; }
_w3() { match_cp 0x0D; }
_w4() { match_range 0x20 0x7E; }
_w5() { match_cp 0x85; }
_w6() { match_range 0xA0 0xD7FF; }
_w7() { match_range 0xE000 0xFFFD; }
_w8() { match_range 0x10000 0x10FFFF; }
_w9() { match_cp 0x9; }
_w10() { match_range 0x20 0x10FFFF; }
_w11() { match_cp 64; }
_w12() { match_cp 96; }
_w13() { c_sequence_entry; }
_w14() { c_mapping_key; }
_w15() { c_mapping_value; }
_w16() { c_collect_entry; }
_w17() { c_sequence_start; }
_w18() { c_sequence_end; }
_w19() { c_mapping_start; }
_w20() { c_mapping_end; }
_w21() { c_comment; }
_w22() { c_anchor; }
_w23() { c_alias; }
_w24() { c_tag; }
_w25() { c_literal; }
_w26() { c_folded; }
_w27() { c_single_quote; }
_w28() { c_double_quote; }
_w29() { c_directive; }
_w30() { c_reserved; }
_w31() { c_sequence_entry; }
_w32() { c_mapping_key; }
_w33() { c_mapping_value; }
_w34() { c_collect_entry; }
_w35() { c_sequence_start; }
_w36() { c_sequence_end; }
_w37() { c_mapping_start; }
_w38() { c_mapping_end; }
_w39() { c_comment; }
_w40() { c_anchor; }
_w41() { c_alias; }
_w42() { c_tag; }
_w43() { c_literal; }
_w44() { c_folded; }
_w45() { c_single_quote; }
_w46() { c_double_quote; }
_w47() { c_directive; }
_w48() { c_reserved; }
_w49() { c_collect_entry; }
_w50() { c_sequence_start; }
_w51() { c_sequence_end; }
_w52() { c_mapping_start; }
_w53() { c_mapping_end; }
_w54() { b_line_feed; }
_w55() { b_carriage_return; }
_w56() { c_printable; }
_w57() { b_char; }
_w58() { c_byte_order_mark; }
_w59() { peg_alt(&[_]PFn{_w57, _w58}); }
_w60() { b_carriage_return; }
_w61() { b_line_feed; }
_w62() { peg_seq(&[_]PFn{_w60, _w61}); }
_w63() { b_carriage_return; }
_w64() { b_line_feed; }
_w65() { s_space; }
_w66() { s_tab; }
_w67() { nb_char; }
_w68() { s_white; }
_w69() { ns_dec_digit; }
_w70() { match_range 0x41 0x46; }
_w71() { match_range 0x61 0x66; }
_w72() { match_range 0x41 0x5A; }
_w73() { match_range 0x61 0x7A; }
_w74() { ns_dec_digit; }
_w75() { ns_ascii_letter; }
_w76() { match_cp 45; }
_w77() { match_cp 37; }
_w78() { ns_hex_digit; }
_w79() { ns_hex_digit; }
_w80() { peg_seq(&[_]PFn{_w77, _w78, _w79}); }
_w81() { ns_word_char; }
_w82() { match_cp 35; }
_w83() { match_cp 59; }
_w84() { match_cp 47; }
_w85() { match_cp 63; }
_w86() { match_cp 58; }
_w87() { match_cp 64; }
_w88() { match_cp 38; }
_w89() { match_cp 61; }
_w90() { match_cp 43; }
_w91() { match_cp 36; }
_w92() { match_cp 44; }
_w93() { match_cp 95; }
_w94() { match_cp 46; }
_w95() { match_cp 33; }
_w96() { match_cp 126; }
_w97() { match_cp 42; }
_w98() { match_cp 39; }
_w99() { match_cp 40; }
_w100() { match_cp 41; }
_w101() { match_cp 91; }
_w102() { match_cp 93; }
_w103() { match_cp 37; }
_w104() { ns_hex_digit; }
_w105() { ns_hex_digit; }
_w106() { peg_seq(&[_]PFn{_w103, _w104, _w105}); }
_w107() { ns_word_char; }
_w108() { match_cp 35; }
_w109() { match_cp 59; }
_w110() { match_cp 47; }
_w111() { match_cp 63; }
_w112() { match_cp 58; }
_w113() { match_cp 64; }
_w114() { match_cp 38; }
_w115() { match_cp 61; }
_w116() { match_cp 43; }
_w117() { match_cp 36; }
_w118() { match_cp 44; }
_w119() { match_cp 95; }
_w120() { match_cp 46; }
_w121() { match_cp 33; }
_w122() { match_cp 126; }
_w123() { match_cp 42; }
_w124() { match_cp 39; }
_w125() { match_cp 40; }
_w126() { match_cp 41; }
_w127() { match_cp 91; }
_w128() { match_cp 93; }
_w129() { ns_uri_char; }
_w130() { c_tag; }
_w131() { c_flow_indicator; }
_w132() { peg_alt(&[_]PFn{_w130, _w131}); }
_w133() { match_cp 120; }
_w134() { ns_hex_digit; }
_w135() { rep_fn 2 _w134; }
_w136() { match_cp 117; }
_w137() { ns_hex_digit; }
_w138() { rep_fn 4 _w137; }
_w139() { match_cp 85; }
_w140() { ns_hex_digit; }
_w141() { rep_fn 8 _w140; }
_w142() { c_escape; }
_w143() { ns_esc_null; }
_w144() { ns_esc_bell; }
_w145() { ns_esc_backspace; }
_w146() { ns_esc_horizontal_tab; }
_w147() { ns_esc_line_feed; }
_w148() { ns_esc_vertical_tab; }
_w149() { ns_esc_form_feed; }
_w150() { ns_esc_carriage_return; }
_w151() { ns_esc_escape; }
_w152() { ns_esc_space; }
_w153() { ns_esc_double_quote; }
_w154() { ns_esc_slash; }
_w155() { ns_esc_backslash; }
_w156() { ns_esc_next_line; }
_w157() { ns_esc_non_breaking_space; }
_w158() { ns_esc_line_separator; }
_w159() { ns_esc_paragraph_separator; }
_w160() { ns_esc_8_bit; }
_w161() { ns_esc_16_bit; }
_w162() { ns_esc_32_bit; }
_w163() { ns_esc_null; }
_w164() { ns_esc_bell; }
_w165() { ns_esc_backspace; }
_w166() { ns_esc_horizontal_tab; }
_w167() { ns_esc_line_feed; }
_w168() { ns_esc_vertical_tab; }
_w169() { ns_esc_form_feed; }
_w170() { ns_esc_carriage_return; }
_w171() { ns_esc_escape; }
_w172() { ns_esc_space; }
_w173() { ns_esc_double_quote; }
_w174() { ns_esc_slash; }
_w175() { ns_esc_backslash; }
_w176() { ns_esc_next_line; }
_w177() { ns_esc_non_breaking_space; }
_w178() { ns_esc_line_separator; }
_w179() { ns_esc_paragraph_separator; }
_w180() { ns_esc_8_bit; }
_w181() { ns_esc_16_bit; }
_w182() { ns_esc_32_bit; }
_w183() { peg_alt(&[_]PFn{
        _w163
        _w164
        _w165
        _w166
        _w167
        _w168
        _w169
        _w170
        _w171
        _w172
        _w173
        _w174
        _w175
        _w176
        _w177
        _w178
        _w179
        _w180
        _w181
        _w182}); }
_w184() { s_space; }
_w185() { s_space; }
_w186() { s_space; }
_w187() { s_white; }
_w188() { plus_ _w187; }
_w189() { ok; }
_w190() { s_indent g.n; }
_w191() { s_separate_in_line; }
_w192() { opt _w191; }
_w193() { s_line_prefix g.n g.c; }
_w194() { s_indent_lt g.n; }
_w195() { peg_alt(&[_]PFn{_w193, _w194}); }
_w196() { b_as_line_feed; }
_w197() { b_non_content; }
_w198() { l_empty g.n g.c; }
_w199() { plus_ _w198; }
_w200() { b_l_trimmed g.n g.c; }
_w201() { b_as_space; }
_w202() { s_separate_in_line; }
_w203() { opt _w202; }
_w204() { b_l_folded g.n "FLOW-IN"; }
_w205() { s_flow_line_prefix g.n; }
_w206() { c_comment; }
_w207() { nb_char; }
_w208() { peg_star _w207; }
_w209() { b_non_content; }
_w210() { ok; }
_w211() { s_separate_in_line; }
_w212() { c_nb_comment_text; }
_w213() { opt _w212; }
_w214() { peg_seq(&[_]PFn{_w211, _w213}); }
_w215() { opt _w214; }
_w216() { b_comment; }
_w217() { s_separate_in_line; }
_w218() { c_nb_comment_text; }
_w219() { opt _w218; }
_w220() { b_non_content; }
_w221() { s_b_comment; }
_w222() { ok; }
_w223() { peg_alt(&[_]PFn{_w221, _w222}); }
_w224() { l_comment; }
_w225() { peg_star _w224; }
_w226() { s_l_comments; }
_w227() { s_flow_line_prefix g.n; }
_w228() { peg_seq(&[_]PFn{_w226, _w227}); }
_w229() { s_separate_in_line; }
_w230() { c_directive; }
_w231() { ns_yaml_directive; }
_w232() { ns_tag_directive; }
_w233() { ns_reserved_directive; }
_w234() { peg_alt(&[_]PFn{_w231, _w232, _w233}); }
_w235() { s_l_comments; }
_w236() { ns_directive_name; }
_w237() { s_separate_in_line; }
_w238() { ns_directive_parameter; }
_w239() { peg_seq(&[_]PFn{_w237, _w238}); }
_w240() { peg_star _w239; }
_w241() { ns_char; }
_w242() { ns_char; }
_w243() { match_str "YAML"; }
_w244() { s_separate_in_line; }
_w245() { ns_yaml_version; }
_w246() { ns_dec_digit; }
_w247() { plus_ _w246; }
_w248() { match_cp 46; }
_w249() { ns_dec_digit; }
_w250() { plus_ _w249; }
_w251() { match_str "TAG"; }
_w252() { s_separate_in_line; }
_w253() { c_tag_handle; }
_w254() { s_separate_in_line; }
_w255() { ns_tag_prefix; }
_w256() { c_named_tag_handle; }
_w257() { c_secondary_tag_handle; }
_w258() { c_primary_tag_handle; }
_w259() { match_cp 33; }
_w260() { ns_word_char; }
_w261() { plus_ _w260; }
_w262() { match_cp 33; }
_w263() { c_ns_local_tag_prefix; }
_w264() { ns_global_tag_prefix; }
_w265() { match_cp 33; }
_w266() { ns_uri_char; }
_w267() { peg_star _w266; }
_w268() { ns_tag_char; }
_w269() { ns_uri_char; }
_w270() { peg_star _w269; }
_w271() { c_ns_tag_property; }
_w272() { s_separate g.n g.c; }
_w273() { c_ns_anchor_property; }
_w274() { peg_seq(&[_]PFn{_w272, _w273}); }
_w275() { opt _w274; }
_w276() { peg_seq(&[_]PFn{_w271, _w275}); }
_w277() { c_ns_anchor_property; }
_w278() { s_separate g.n g.c; }
_w279() { c_ns_tag_property; }
_w280() { peg_seq(&[_]PFn{_w278, _w279}); }
_w281() { opt _w280; }
_w282() { peg_seq(&[_]PFn{_w277, _w281}); }
_w283() { c_verbatim_tag; }
_w284() { c_ns_shorthand_tag; }
_w285() { c_non_specific_tag; }
_w286() { match_str "!<"; }
_w287() { ns_uri_char; }
_w288() { plus_ _w287; }
_w289() { match_cp 62; }
_w290() { c_tag_handle; }
_w291() { ns_tag_char; }
_w292() { plus_ _w291; }
_w293() { c_anchor; }
_w294() { ns_anchor_name; }
_w295() { scalar_fn _w294; }
_w296() { peg_seq(&[_]PFn{_w293, _w295}); }
_w297() { ns_char; }
_w298() { c_flow_indicator; }
_w299() { ns_anchor_char; }
_w300() { c_alias; }
_w301() { ns_anchor_name; }
_w302() { scalar_fn _w301; }
_w303() { peg_seq(&[_]PFn{_w300, _w302}); }
_w304() { c_ns_esc_char; }
_w305() { nb_json; }
_w306() { match_cp 92; }
_w307() { match_cp 34; }
_w308() { peg_alt(&[_]PFn{_w306, _w307}); }
_w309() { minus_fn _w305 _w308; }
_w310() { nb_double_char; }
_w311() { s_white; }
_w312() { match_cp 34; }
_w313() { nb_double_text g.n g.c; }
_w314() { match_cp 34; }
_w315() { peg_seq(&[_]PFn{_w312, _w313, _w314}); }
_w316() { nb_double_char; }
_w317() { s_white; }
_w318() { peg_star _w317; }
_w319() { match_cp 92; }
_w320() { b_non_content; }
_w321() { l_empty g.n "FLOW-IN"; }
_w322() { peg_star _w321; }
_w323() { s_flow_line_prefix g.n; }
_w324() { s_double_escaped g.n; }
_w325() { s_flow_folded g.n; }
_w326() { s_white; }
_w327() { peg_star _w326; }
_w328() { ns_double_char; }
_w329() { peg_seq(&[_]PFn{_w327, _w328}); }
_w330() { s_double_break g.n; }
_w331() { ns_double_char; }
_w332() { nb_ns_double_in_line; }
_w333() { s_double_next_line g.n; }
_w334() { s_white; }
_w335() { peg_star _w334; }
_w336() { peg_alt(&[_]PFn{_w333, _w335}); }
_w337() { peg_seq(&[_]PFn{_w331, _w332, _w336}); }
_w338() { opt _w337; }
_w339() { nb_ns_double_in_line; }
_w340() { s_double_next_line g.n; }
_w341() { s_white; }
_w342() { peg_star _w341; }
_w343() { peg_alt(&[_]PFn{_w340, _w342}); }
_w344() { c_quoted_quote; }
_w345() { nb_json; }
_w346() { match_cp 39; }
_w347() { minus_fn _w345 _w346; }
_w348() { nb_single_char; }
_w349() { s_white; }
_w350() { match_cp 39; }
_w351() { nb_single_text g.n g.c; }
_w352() { match_cp 39; }
_w353() { peg_seq(&[_]PFn{_w350, _w351, _w352}); }
_w354() { nb_single_char; }
_w355() { s_white; }
_w356() { peg_star _w355; }
_w357() { ns_single_char; }
_w358() { peg_seq(&[_]PFn{_w356, _w357}); }
_w359() { s_flow_folded g.n; }
_w360() { ns_single_char; }
_w361() { ns_single_in_line; }
_w362() { s_single_next_line g.n; }
_w363() { s_white; }
_w364() { peg_star _w363; }
_w365() { peg_alt(&[_]PFn{_w362, _w364}); }
_w366() { peg_seq(&[_]PFn{_w360, _w361, _w365}); }
_w367() { opt _w366; }
_w368() { ns_single_in_line; }
_w369() { s_single_next_line g.n; }
_w370() { s_white; }
_w371() { peg_star _w370; }
_w372() { peg_alt(&[_]PFn{_w369, _w371}); }
_w373() { ns_char; }
_w374() { c_indicator; }
_w375() { minus_fn _w373 _w374; }
_w376() { match_cp 63; }
_w377() { match_cp 58; }
_w378() { match_cp 45; }
_w379() { peg_alt(&[_]PFn{_w376, _w377, _w378}); }
_w380() { ns_plain_safe g.c; }
_w381() { ahead _w380; }
_w382() { peg_seq(&[_]PFn{_w379, _w381}); }
_w383() { ns_char; }
_w384() { c_flow_indicator; }
_w385() { ns_plain_safe g.c; }
_w386() { match_cp 58; }
_w387() { match_cp 35; }
_w388() { peg_alt(&[_]PFn{_w386, _w387}); }
_w389() { minus_fn _w385 _w388; }
_w390() { ns_char; }
_w391() { behind _w390; }
_w392() { match_cp 35; }
_w393() { peg_seq(&[_]PFn{_w391, _w392}); }
_w394() { match_cp 58; }
_w395() { ns_plain_safe g.c; }
_w396() { ahead _w395; }
_w397() { peg_seq(&[_]PFn{_w394, _w396}); }
_w398() { switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .fn = ns_plain_multi_line g.n g.c },
        .{ .ctx = "FLOW-IN", .fn = ns_plain_multi_line g.n g.c },
        .{ .ctx = "BLOCK-KEY", .fn = ns_plain_one_line g.c },
        .{ .ctx = "FLOW-KEY", .fn = ns_plain_one_line g.c },
    }); }
_w399() { s_white; }
_w400() { peg_star _w399; }
_w401() { ns_plain_char g.c; }
_w402() { peg_seq(&[_]PFn{_w400, _w401}); }
_w403() { ns_plain_first g.c; }
_w404() { nb_ns_plain_in_line g.c; }
_w405() { s_flow_folded g.n; }
_w406() { c_forbidden; }
_w407() { r_neg _w406; }
_w408() { ns_plain_char g.c; }
_w409() { nb_ns_plain_in_line g.c; }
_w410() { ns_plain_one_line g.c; }
_w411() { s_ns_plain_next_line g.n g.c; }
_w412() { peg_star _w411; }
_w413() { match_cp 91; }
_w414() { s_separate g.n g.c; }
_w415() { opt _w414; }
_w416() { ns_s_flow_seq_entries g.n in_flow(g.c); }
_w417() { collect_fn _w416; }
_w418() { opt _w417; }
_w419() { match_cp 93; }
_w420() { peg_seq(&[_]PFn{_w413, _w415, _w418, _w419}); }
_w421() { ns_flow_seq_entry g.n g.c; }
_w422() { s_separate g.n g.c; }
_w423() { opt _w422; }
_w424() { match_cp 44; }
_w425() { s_separate g.n g.c; }
_w426() { opt _w425; }
_w427() { ns_s_flow_seq_entries g.n g.c; }
_w428() { opt _w427; }
_w429() { peg_seq(&[_]PFn{_w424, _w426, _w428}); }
_w430() { opt _w429; }
_w431() { ns_flow_pair g.n g.c; }
_w432() { ns_flow_node g.n g.c; }
_w433() { match_cp 123; }
_w434() { s_separate g.n g.c; }
_w435() { opt _w434; }
_w436() { ns_s_flow_map_entries g.n in_flow(g.c); }
_w437() { collect_fn _w436; }
_w438() { opt _w437; }
_w439() { match_cp 125; }
_w440() { peg_seq(&[_]PFn{_w433, _w435, _w438, _w439}); }
_w441() { ns_flow_map_entry g.n g.c; }
_w442() { s_separate g.n g.c; }
_w443() { opt _w442; }
_w444() { match_cp 44; }
_w445() { s_separate g.n g.c; }
_w446() { opt _w445; }
_w447() { ns_s_flow_map_entries g.n g.c; }
_w448() { opt _w447; }
_w449() { peg_seq(&[_]PFn{_w444, _w446, _w448}); }
_w450() { opt _w449; }
_w451() { match_cp 63; }
_w452() { s_separate g.n g.c; }
_w453() { ns_flow_map_explicit_entry g.n g.c; }
_w454() { peg_seq(&[_]PFn{_w451, _w452, _w453}); }
_w455() { ns_flow_map_implicit_entry g.n g.c; }
_w456() { ns_flow_map_implicit_entry g.n g.c; }
_w457() { e_node; }
_w458() { e_node; }
_w459() { peg_seq(&[_]PFn{_w457, _w458}); }
_w460() { ns_flow_map_yaml_key_entry g.n g.c; }
_w461() { c_ns_flow_map_empty_key_entry g.n g.c; }
_w462() { c_ns_flow_map_json_key_entry g.n g.c; }
_w463() { peg_alt(&[_]PFn{_w460, _w461, _w462}); }
_w464() { ns_flow_yaml_node g.n g.c; }
_w465() { s_separate g.n g.c; }
_w466() { opt _w465; }
_w467() { c_ns_flow_map_separate_value g.n g.c; }
_w468() { peg_seq(&[_]PFn{_w466, _w467}); }
_w469() { e_node; }
_w470() { peg_alt(&[_]PFn{_w468, _w469}); }
_w471() { e_node; }
_w472() { c_ns_flow_map_separate_value g.n g.c; }
_w473() { match_cp 58; }
_w474() { ns_plain_safe g.c; }
_w475() { r_neg _w474; }
_w476() { s_separate g.n g.c; }
_w477() { ns_flow_node g.n g.c; }
_w478() { peg_seq(&[_]PFn{_w476, _w477}); }
_w479() { e_node; }
_w480() { peg_alt(&[_]PFn{_w478, _w479}); }
_w481() { c_flow_json_node g.n g.c; }
_w482() { s_separate g.n g.c; }
_w483() { opt _w482; }
_w484() { c_ns_flow_map_adjacent_value g.n g.c; }
_w485() { peg_seq(&[_]PFn{_w483, _w484}); }
_w486() { e_node; }
_w487() { peg_alt(&[_]PFn{_w485, _w486}); }
_w488() { match_cp 58; }
_w489() { s_separate g.n g.c; }
_w490() { opt _w489; }
_w491() { ns_flow_node g.n g.c; }
_w492() { peg_seq(&[_]PFn{_w490, _w491}); }
_w493() { e_node; }
_w494() { peg_alt(&[_]PFn{_w492, _w493}); }
_w495() { match_cp 63; }
_w496() { s_separate g.n g.c; }
_w497() { ns_flow_map_explicit_entry g.n g.c; }
_w498() { peg_seq(&[_]PFn{_w495, _w496, _w497}); }
_w499() { ns_flow_pair_entry g.n g.c; }
_w500() { ns_flow_pair_yaml_key_entry g.n g.c; }
_w501() { c_ns_flow_map_empty_key_entry g.n g.c; }
_w502() { c_ns_flow_pair_json_key_entry g.n g.c; }
_w503() { ns_s_implicit_yaml_key "FLOW-KEY"; }
_w504() { c_ns_flow_map_separate_value g.n g.c; }
_w505() { c_s_implicit_json_key "FLOW-KEY"; }
_w506() { c_ns_flow_map_adjacent_value g.n g.c; }
_w507() { ns_flow_yaml_node 0 g.c; }
_w508() { s_separate_in_line; }
_w509() { opt _w508; }
_w510() { c_flow_json_node 0 g.c; }
_w511() { s_separate_in_line; }
_w512() { opt _w511; }
_w513() { c_flow_sequence g.n g.c; }
_w514() { c_flow_mapping g.n g.c; }
_w515() { c_single_quoted g.n g.c; }
_w516() { c_double_quoted g.n g.c; }
_w517() { ns_flow_yaml_content g.n g.c; }
_w518() { c_flow_json_content g.n g.c; }
_w519() { c_ns_alias_node; }
_w520() { ns_flow_yaml_content g.n g.c; }
_w521() { c_ns_properties g.n g.c; }
_w522() { s_separate g.n g.c; }
_w523() { ns_flow_yaml_content g.n g.c; }
_w524() { peg_seq(&[_]PFn{_w522, _w523}); }
_w525() { e_scalar; }
_w526() { peg_alt(&[_]PFn{_w524, _w525}); }
_w527() { peg_seq(&[_]PFn{_w521, _w526}); }
_w528() { c_ns_properties g.n g.c; }
_w529() { s_separate g.n g.c; }
_w530() { peg_seq(&[_]PFn{_w528, _w529}); }
_w531() { opt _w530; }
_w532() { c_flow_json_content g.n g.c; }
_w533() { c_ns_alias_node; }
_w534() { ns_flow_content g.n g.c; }
_w535() { c_ns_properties g.n g.c; }
_w536() { s_separate g.n g.c; }
_w537() { ns_flow_content g.n g.c; }
_w538() { peg_seq(&[_]PFn{_w536, _w537}); }
_w539() { e_scalar; }
_w540() { peg_alt(&[_]PFn{_w538, _w539}); }
_w541() { peg_seq(&[_]PFn{_w535, _w540}); }
_w542() { ns_dec_digit; }
_w543() { parse_int_fn _w542; }
_w544() { detect_indent g.n; }
_w545() { match_cp 45; }
_w546() { parse_sym_fn _w545 "STRIP"; }
_w547() { match_cp 43; }
_w548() { parse_sym_fn _w547 "KEEP"; }
_w549() { val_fn "CLIP"; }
_w550() { blk: { peg_alt(&[_]PFn{_w543, _w544}); if (g.failed) break :blk; const m = g.rtagint; save_inp(); blk: { peg_alt(&[_]PFn{_w546, _w548, _w549}); if (g.failed) break :blk; const t = g.rtag; save_inp(); s_b_comment } }; }
_w551() { match_cp 45; }
_w552() { parse_sym_fn _w551 "STRIP"; }
_w553() { match_cp 43; }
_w554() { parse_sym_fn _w553 "KEEP"; }
_w555() { val_fn "CLIP"; }
_w556() { ns_dec_digit; }
_w557() { parse_int_fn _w556; }
_w558() { detect_indent g.n; }
_w559() { blk: { peg_alt(&[_]PFn{_w552, _w554, _w555}); if (g.failed) break :blk; const t = g.rtag; save_inp(); blk: { peg_alt(&[_]PFn{_w557, _w558}); if (g.failed) break :blk; const m = g.rtagint; save_inp(); s_b_comment } }; }
_w560() { ns_dec_digit; }
_w561() { ok; }
_w562() { match_cp 45; }
_w563() { match_cp 43; }
_w564() { ok; }
_w565() { s_indent_le g.n; }
_w566() { b_non_content; }
_w567() { peg_seq(&[_]PFn{_w565, _w566}); }
_w568() { peg_star _w567; }
_w569() { l_trail_comments g.n; }
_w570() { opt _w569; }
_w571() { l_empty g.n "BLOCK-IN"; }
_w572() { peg_star _w571; }
_w573() { l_trail_comments g.n; }
_w574() { opt _w573; }
_w575() { s_indent_lt g.n; }
_w576() { c_nb_comment_text; }
_w577() { b_comment; }
_w578() { l_comment; }
_w579() { peg_star _w578; }
_w580() { match_cp 124; }
_w581() { ns_dec_digit; }
_w582() { parse_int_fn _w581; }
_w583() { detect_indent g.n; }
_w584() { match_cp 45; }
_w585() { parse_sym_fn _w584 "STRIP"; }
_w586() { match_cp 43; }
_w587() { parse_sym_fn _w586 "KEEP"; }
_w588() { val_fn "CLIP"; }
_w589() { s_b_comment; }
_w590() { l_literal_content $(( g.n + g.m )) g.t; }
_w591() { blk: { peg_alt(&[_]PFn{_w582, _w583}); if (g.failed) break :blk; const m = g.rtagint; save_inp(); blk: { peg_alt(&[_]PFn{_w585, _w587, _w588}); if (g.failed) break :blk; const t = g.rtag; save_inp(); peg_seq(&[_]PFn{_w589, _w590}) } }; }
_w592() { l_empty g.n "BLOCK-IN"; }
_w593() { peg_star _w592; }
_w594() { s_indent g.n; }
_w595() { nb_char; }
_w596() { plus_ _w595; }
_w597() { b_as_line_feed; }
_w598() { l_nb_literal_text g.n; }
_w599() { l_nb_literal_text g.n; }
_w600() { b_nb_literal_next g.n; }
_w601() { peg_star _w600; }
_w602() { b_chomped_last g.t; }
_w603() { peg_seq(&[_]PFn{_w599, _w601, _w602}); }
_w604() { opt _w603; }
_w605() { l_chomped_empty g.n g.t; }
_w606() { peg_seq(&[_]PFn{_w604, _w605}); }
_w607() { match_cp 62; }
_w608() { ns_dec_digit; }
_w609() { parse_int_fn _w608; }
_w610() { detect_indent g.n; }
_w611() { match_cp 45; }
_w612() { parse_sym_fn _w611 "STRIP"; }
_w613() { match_cp 43; }
_w614() { parse_sym_fn _w613 "KEEP"; }
_w615() { val_fn "CLIP"; }
_w616() { s_b_comment; }
_w617() { l_folded_content $(( g.n + g.m )) g.t; }
_w618() { blk: { peg_alt(&[_]PFn{_w609, _w610}); if (g.failed) break :blk; const m = g.rtagint; save_inp(); blk: { peg_alt(&[_]PFn{_w612, _w614, _w615}); if (g.failed) break :blk; const t = g.rtag; save_inp(); peg_seq(&[_]PFn{_w616, _w617}) } }; }
_w619() { s_indent g.n; }
_w620() { ns_char; }
_w621() { nb_char; }
_w622() { peg_star _w621; }
_w623() { s_nb_folded_text g.n; }
_w624() { b_l_folded g.n "BLOCK-IN"; }
_w625() { s_nb_folded_text g.n; }
_w626() { peg_seq(&[_]PFn{_w624, _w625}); }
_w627() { peg_star _w626; }
_w628() { s_indent g.n; }
_w629() { s_white; }
_w630() { nb_char; }
_w631() { peg_star _w630; }
_w632() { b_as_line_feed; }
_w633() { l_empty g.n "BLOCK-IN"; }
_w634() { peg_star _w633; }
_w635() { s_nb_spaced_text g.n; }
_w636() { b_l_spaced g.n; }
_w637() { s_nb_spaced_text g.n; }
_w638() { peg_seq(&[_]PFn{_w636, _w637}); }
_w639() { peg_star _w638; }
_w640() { l_empty g.n "BLOCK-IN"; }
_w641() { peg_star _w640; }
_w642() { l_nb_folded_lines g.n; }
_w643() { l_nb_spaced_lines g.n; }
_w644() { peg_alt(&[_]PFn{_w642, _w643}); }
_w645() { l_nb_same_lines g.n; }
_w646() { b_as_line_feed; }
_w647() { l_nb_same_lines g.n; }
_w648() { peg_seq(&[_]PFn{_w646, _w647}); }
_w649() { peg_star _w648; }
_w650() { l_nb_diff_lines g.n; }
_w651() { b_chomped_last g.t; }
_w652() { peg_seq(&[_]PFn{_w650, _w651}); }
_w653() { opt _w652; }
_w654() { l_chomped_empty g.n g.t; }
_w655() { peg_seq(&[_]PFn{_w653, _w654}); }
_w656() { s_indent $(( g.n + g.m )); }
_w657() { c_l_block_seq_entry $(( g.n + g.m )); }
_w658() { peg_seq(&[_]PFn{_w656, _w657}); }
_w659() { plus_ _w658; }
_w660() { blk: { detect_indent g.n; if (g.failed) break :blk; const m = g.rtagint; save_inp(); collect_fn _w659 }; }
_w661() { match_cp 45; }
_w662() { ns_char; }
_w663() { r_neg _w662; }
_w664() { s_lblock_indented g.n "BLOCK-IN"; }
_w665() { s_indent g.m; }
_w666() { ns_l_compact_sequence $(( g.n + 1 + g.m )); }
_w667() { ns_l_compact_mapping $(( g.n + 1 + g.m )); }
_w668() { peg_alt(&[_]PFn{_w666, _w667}); }
_w669() { blk: { detect_indent 0; if (g.failed) break :blk; const m = g.rtagint; save_inp(); peg_seq(&[_]PFn{_w665, _w668}) }; }
_w670() { s_lblock_node g.n g.c; }
_w671() { e_node; }
_w672() { s_l_comments; }
_w673() { peg_seq(&[_]PFn{_w671, _w672}); }
_w674() { c_l_block_seq_entry g.n; }
_w675() { s_indent g.n; }
_w676() { c_l_block_seq_entry g.n; }
_w677() { peg_seq(&[_]PFn{_w675, _w676}); }
_w678() { peg_star _w677; }
_w679() { s_indent $(( g.n + g.m )); }
_w680() { ns_l_block_map_entry $(( g.n + g.m )); }
_w681() { peg_seq(&[_]PFn{_w679, _w680}); }
_w682() { plus_ _w681; }
_w683() { blk: { detect_indent g.n; if (g.failed) break :blk; const m = g.rtagint; save_inp(); collect_fn _w682 }; }
_w684() { c_l_block_map_explicit_entry g.n; }
_w685() { ns_l_block_map_implicit_entry g.n; }
_w686() { c_l_block_map_explicit_key g.n; }
_w687() { l_block_map_explicit_value g.n; }
_w688() { e_node; }
_w689() { peg_alt(&[_]PFn{_w687, _w688}); }
_w690() { match_cp 63; }
_w691() { s_lblock_indented g.n "BLOCK-OUT"; }
_w692() { s_indent g.n; }
_w693() { match_cp 58; }
_w694() { s_lblock_indented g.n "BLOCK-OUT"; }
_w695() { ns_s_block_map_implicit_key; }
_w696() { e_node; }
_w697() { peg_alt(&[_]PFn{_w695, _w696}); }
_w698() { scalar_fn _w697; }
_w699() { c_l_block_map_implicit_value g.n; }
_w700() { peg_seq(&[_]PFn{_w698, _w699}); }
_w701() { c_s_implicit_json_key "BLOCK-KEY"; }
_w702() { ns_s_implicit_yaml_key "BLOCK-KEY"; }
_w703() { match_cp 58; }
_w704() { s_lblock_node g.n "BLOCK-OUT"; }
_w705() { e_node; }
_w706() { s_l_comments; }
_w707() { peg_seq(&[_]PFn{_w705, _w706}); }
_w708() { scalar_fn _w707; }
_w709() { peg_alt(&[_]PFn{_w704, _w708}); }
_w710() { ns_l_block_map_entry g.n; }
_w711() { s_indent g.n; }
_w712() { ns_l_block_map_entry g.n; }
_w713() { peg_seq(&[_]PFn{_w711, _w712}); }
_w714() { peg_star _w713; }
_w715() { s_lblock_in_block g.n g.c; }
_w716() { s_lflow_in_block g.n; }
_w717() { s_separate $(( g.n + 1 )) "FLOW-OUT"; }
_w718() { ns_flow_node $(( g.n + 1 )) "FLOW-OUT"; }
_w719() { s_l_comments; }
_w720() { s_lblock_scalar g.n g.c; }
_w721() { s_lblock_collection g.n g.c; }
_w722() { s_separate $(( g.n + 1 )) g.c; }
_w723() { c_ns_properties $(( g.n + 1 )) g.c; }
_w724() { s_separate $(( g.n + 1 )) g.c; }
_w725() { peg_seq(&[_]PFn{_w723, _w724}); }
_w726() { opt _w725; }
_w727() { c_lliteral g.n; }
_w728() { c_lfolded g.n; }
_w729() { peg_alt(&[_]PFn{_w727, _w728}); }
_w730() { s_separate $(( g.n + 1 )) g.c; }
_w731() { c_ns_properties $(( g.n + 1 )) g.c; }
_w732() { peg_seq(&[_]PFn{_w730, _w731}); }
_w733() { opt _w732; }
_w734() { s_l_comments; }
_w735() { lblock_sequence seq_spaces(g.n, g.c); }
_w736() { lblock_mapping g.n; }
_w737() { peg_alt(&[_]PFn{_w735, _w736}); }
_w738() { c_byte_order_mark; }
_w739() { opt _w738; }
_w740() { l_comment; }
_w741() { peg_star _w740; }
_w742() { c_document_end; }
_w743() { s_l_comments; }
_w744() { sol; }
_w745() { c_directives_end; }
_w746() { c_document_end; }
_w747() { peg_alt(&[_]PFn{_w745, _w746}); }
_w748() { b_char; }
_w749() { s_white; }
_w750() { eof_ok; }
_w751() { peg_alt(&[_]PFn{_w748, _w749, _w750}); }
_w752() { s_lblock_node -1 "BLOCK-IN"; }
_w753() { c_directives_end; }
_w754() { l_bare_document; }
_w755() { e_node; }
_w756() { s_l_comments; }
_w757() { peg_seq(&[_]PFn{_w755, _w756}); }
_w758() { peg_alt(&[_]PFn{_w754, _w757}); }
_w759() { peg_seq(&[_]PFn{_w753, _w758}); }
_w760() { l_directive; }
_w761() { plus_ _w760; }
_w762() { l_explicit_document; }
_w763() { l_directive_document; }
_w764() { l_explicit_document; }
_w765() { l_bare_document; }
_w766() { l_document_prefix; }
_w767() { peg_star _w766; }
_w768() { l_any_document; }
_w769() { opt _w768; }
_w770() { l_document_suffix; }
_w771() { plus_ _w770; }
_w772() { l_document_prefix; }
_w773() { peg_star _w772; }
_w774() { l_any_document; }
_w775() { opt _w774; }
_w776() { peg_seq(&[_]PFn{_w771, _w773, _w775}); }
_w777() { l_document_prefix; }
_w778() { peg_star _w777; }
_w779() { l_explicit_document; }
_w780() { opt _w779; }
_w781() { peg_seq(&[_]PFn{_w778, _w780}); }
_w782() { peg_alt(&[_]PFn{_w776, _w781}); }
_w783() { peg_star _w782; }
_w784() { peg_seq(&[_]PFn{_w767, _w769, _w783}); }

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
    match_cp 0xFEFF;
}

// [4] C-SEQUENCE-ENTRY 
fn c_sequence_entry() void {
    match_cp 45;
}

// [5] C-MAPPING-KEY 
fn c_mapping_key() void {
    match_cp 63;
}

// [6] C-MAPPING-VALUE 
fn c_mapping_value() void {
    match_cp 58;
}

// [7] C-COLLECT-ENTRY 
fn c_collect_entry() void {
    match_cp 44;
}

// [8] C-SEQUENCE-START 
fn c_sequence_start() void {
    match_cp 91;
}

// [9] C-SEQUENCE-END 
fn c_sequence_end() void {
    match_cp 93;
}

// [10] C-MAPPING-START 
fn c_mapping_start() void {
    match_cp 123;
}

// [11] C-MAPPING-END 
fn c_mapping_end() void {
    match_cp 125;
}

// [12] C-COMMENT 
fn c_comment() void {
    match_cp 35;
}

// [13] C-ANCHOR 
fn c_anchor() void {
    match_cp 38;
}

// [14] C-ALIAS 
fn c_alias() void {
    match_cp 42;
}

// [15] C-TAG 
fn c_tag() void {
    match_cp 33;
}

// [16] C-LITERAL 
fn c_literal() void {
    match_cp 124;
}

// [17] C-FOLDED 
fn c_folded() void {
    match_cp 62;
}

// [18] C-SINGLE-QUOTE 
fn c_single_quote() void {
    match_cp 39;
}

// [19] C-DOUBLE-QUOTE 
fn c_double_quote() void {
    match_cp 34;
}

// [20] C-DIRECTIVE 
fn c_directive() void {
    match_cp 37;
}

// [21] C-RESERVED 
fn c_reserved() void {
    peg_alt(&[_]PFn{_w11, _w12});
}

// [22] C-INDICATOR 
fn c_indicator() void {
    peg_alt(&[_]PFn{
        _w31
        _w32
        _w33
        _w34
        _w35
        _w36
        _w37
        _w38
        _w39
        _w40
        _w41
        _w42
        _w43
        _w44
        _w45
        _w46
        _w47
        _w48});
}

// [23] C-FLOW-INDICATOR 
fn c_flow_indicator() void {
    peg_alt(&[_]PFn{_w49, _w50, _w51, _w52, _w53});
}

// [24] B-LINE-FEED 
fn b_line_feed() void {
    match_cp 0x0A;
}

// [25] B-CARRIAGE-RETURN 
fn b_carriage_return() void {
    match_cp 0x0D;
}

// [26] B-CHAR 
fn b_char() void {
    peg_alt(&[_]PFn{_w54, _w55});
}

// [27] NB-CHAR 
fn nb_char() void {
    minus_fn _w56 _w59;
}

// [28] B-BREAK 
fn b_break() void {
    peg_alt(&[_]PFn{_w62, _w63, _w64});
}

// [29] B-AS-LINE-FEED 
fn b_as_line_feed() void {
    b_break;
}

// [30] B-NON-CONTENT 
fn b_non_content() void {
    b_break;
}

// [31] S-SPACE 
fn s_space() void {
    match_cp 0x20;
}

// [32] S-TAB 
fn s_tab() void {
    match_cp 0x9;
}

// [33] S-WHITE 
fn s_white() void {
    peg_alt(&[_]PFn{_w65, _w66});
}

// [34] NS-CHAR 
fn ns_char() void {
    minus_fn _w67 _w68;
}

// [35] NS-DEC-DIGIT 
fn ns_dec_digit() void {
    match_range 0x30 0x39;
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
        _w106
        _w107
        _w108
        _w109
        _w110
        _w111
        _w112
        _w113
        _w114
        _w115
        _w116
        _w117
        _w118
        _w119
        _w120
        _w121
        _w122
        _w123
        _w124
        _w125
        _w126
        _w127
        _w128});
}

// [40] NS-TAG-CHAR 
fn ns_tag_char() void {
    minus_fn _w129 _w132;
}

// [41] C-ESCAPE 
fn c_escape() void {
    match_cp 92;
}

// [42] NS-ESC-NULL 
fn ns_esc_null() void {
    match_cp 48;
}

// [43] NS-ESC-BELL 
fn ns_esc_bell() void {
    match_cp 97;
}

// [44] NS-ESC-BACKSPACE 
fn ns_esc_backspace() void {
    match_cp 98;
}

// [45] NS-ESC-HORIZONTAL-TAB 
fn ns_esc_horizontal_tab() void {
    match_cp 116;
}

// [46] NS-ESC-LINE-FEED 
fn ns_esc_line_feed() void {
    match_cp 110;
}

// [47] NS-ESC-VERTICAL-TAB 
fn ns_esc_vertical_tab() void {
    match_cp 118;
}

// [48] NS-ESC-FORM-FEED 
fn ns_esc_form_feed() void {
    match_cp 102;
}

// [49] NS-ESC-CARRIAGE-RETURN 
fn ns_esc_carriage_return() void {
    match_cp 114;
}

// [50] NS-ESC-ESCAPE 
fn ns_esc_escape() void {
    match_cp 101;
}

// [51] NS-ESC-SPACE 
fn ns_esc_space() void {
    match_cp 0x20;
}

// [52] NS-ESC-DOUBLE-QUOTE 
fn ns_esc_double_quote() void {
    match_cp 34;
}

// [53] NS-ESC-SLASH 
fn ns_esc_slash() void {
    match_cp 47;
}

// [54] NS-ESC-BACKSLASH 
fn ns_esc_backslash() void {
    match_cp 92;
}

// [55] NS-ESC-NEXT-LINE 
fn ns_esc_next_line() void {
    match_cp 78;
}

// [56] NS-ESC-NON-BREAKING-SPACE 
fn ns_esc_non_breaking_space() void {
    match_cp 95;
}

// [57] NS-ESC-LINE-SEPARATOR 
fn ns_esc_line_separator() void {
    match_cp 76;
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR 
fn ns_esc_paragraph_separator() void {
    match_cp 80;
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
    rep_fn g.n _w184;
}

// [64] S-INDENT-LT 
fn s_indent_lt() void {
    peg_star _w185;
}

// [65] S-INDENT-LE 
fn s_indent_le() void {
    peg_star _w186;
}

// [66] S-SEPARATE-IN-LINE 
fn s_separate_in_line() void {
    peg_alt(&[_]PFn{_w188, _w189});
}

// [67] S-LINE-PREFIX 
fn s_line_prefix() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "BLOCK-IN", .fn = s_block_line_prefix g.n },
        .{ .ctx = "BLOCK-OUT", .fn = s_block_line_prefix g.n },
        .{ .ctx = "FLOW-IN", .fn = s_flow_line_prefix g.n },
        .{ .ctx = "FLOW-OUT", .fn = s_flow_line_prefix g.n },
    });
}

// [68] S-BLOCK-LINE-PREFIX 
fn s_block_line_prefix() void {
    s_indent g.n;
}

// [69] S-FLOW-LINE-PREFIX 
fn s_flow_line_prefix() void {
    peg_seq(&[_]PFn{_w190, _w192});
}

// [70] L-EMPTY 
fn l_empty() void {
    peg_seq(&[_]PFn{_w195, _w196});
}

// [71] B-L-TRIMMED 
fn b_l_trimmed() void {
    peg_seq(&[_]PFn{_w197, _w199});
}

// [72] B-AS-SPACE 
fn b_as_space() void {
    b_break;
}

// [73] B-L-FOLDED 
fn b_l_folded() void {
    peg_alt(&[_]PFn{_w200, _w201});
}

// [74] S-FLOW-FOLDED 
fn s_flow_folded() void {
    peg_seq(&[_]PFn{_w203, _w204, _w205});
}

// [75] C-NB-COMMENT-TEXT 
fn c_nb_comment_text() void {
    peg_seq(&[_]PFn{_w206, _w208});
}

// [76] B-COMMENT 
fn b_comment() void {
    peg_alt(&[_]PFn{_w209, _w210});
}

// [77] S-B-COMMENT 
fn s_b_comment() void {
    peg_seq(&[_]PFn{_w215, _w216});
}

// [78] L-COMMENT 
fn l_comment() void {
    peg_seq(&[_]PFn{_w217, _w219, _w220});
}

// [79] S-L-COMMENTS 
fn s_l_comments() void {
    peg_seq(&[_]PFn{_w223, _w225});
}

// [80] S-SEPARATE 
fn s_separate() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "BLOCK-OUT", .fn = s_separate_lines g.n },
        .{ .ctx = "BLOCK-IN", .fn = s_separate_lines g.n },
        .{ .ctx = "FLOW-OUT", .fn = s_separate_lines g.n },
        .{ .ctx = "FLOW-IN", .fn = s_separate_lines g.n },
        .{ .ctx = "BLOCK-KEY", .fn = s_separate_in_line },
        .{ .ctx = "FLOW-KEY", .fn = s_separate_in_line },
    });
}

// [81] S-SEPARATE-LINES 
fn s_separate_lines() void {
    peg_alt(&[_]PFn{_w228, _w229});
}

// [82] L-DIRECTIVE 
fn l_directive() void {
    peg_seq(&[_]PFn{_w230, _w234, _w235});
}

// [83] NS-RESERVED-DIRECTIVE 
fn ns_reserved_directive() void {
    peg_seq(&[_]PFn{_w236, _w240});
}

// [84] NS-DIRECTIVE-NAME 
fn ns_directive_name() void {
    plus_ _w241;
}

// [85] NS-DIRECTIVE-PARAMETER 
fn ns_directive_parameter() void {
    plus_ _w242;
}

// [86] NS-YAML-DIRECTIVE 
fn ns_yaml_directive() void {
    peg_seq(&[_]PFn{_w243, _w244, _w245});
}

// [87] NS-YAML-VERSION 
fn ns_yaml_version() void {
    peg_seq(&[_]PFn{_w247, _w248, _w250});
}

// [88] NS-TAG-DIRECTIVE 
fn ns_tag_directive() void {
    peg_seq(&[_]PFn{_w251, _w252, _w253, _w254, _w255});
}

// [89] C-TAG-HANDLE 
fn c_tag_handle() void {
    peg_alt(&[_]PFn{_w256, _w257, _w258});
}

// [90] C-PRIMARY-TAG-HANDLE 
fn c_primary_tag_handle() void {
    match_cp 33;
}

// [91] C-SECONDARY-TAG-HANDLE 
fn c_secondary_tag_handle() void {
    match_str "!!";
}

// [92] C-NAMED-TAG-HANDLE 
fn c_named_tag_handle() void {
    peg_seq(&[_]PFn{_w259, _w261, _w262});
}

// [93] NS-TAG-PREFIX 
fn ns_tag_prefix() void {
    peg_alt(&[_]PFn{_w263, _w264});
}

// [94] C-NS-LOCAL-TAG-PREFIX 
fn c_ns_local_tag_prefix() void {
    peg_seq(&[_]PFn{_w265, _w267});
}

// [95] NS-GLOBAL-TAG-PREFIX 
fn ns_global_tag_prefix() void {
    peg_seq(&[_]PFn{_w268, _w270});
}

// [96] C-NS-PROPERTIES 
fn c_ns_properties() void {
    peg_alt(&[_]PFn{_w276, _w282});
}

// [97] C-NS-TAG-PROPERTY 
fn c_ns_tag_property() void {
    peg_alt(&[_]PFn{_w283, _w284, _w285});
}

// [98] C-VERBATIM-TAG 
fn c_verbatim_tag() void {
    peg_seq(&[_]PFn{_w286, _w288, _w289});
}

// [99] C-NS-SHORTHAND-TAG 
fn c_ns_shorthand_tag() void {
    peg_seq(&[_]PFn{_w290, _w292});
}

// [100] C-NON-SPECIFIC-TAG 
fn c_non_specific_tag() void {
    match_cp 33;
}

// [101] C-NS-ANCHOR-PROPERTY 
fn c_ns_anchor_property() void {
    build_ast "ANCHOR" _w296;
}

// [102] NS-ANCHOR-CHAR 
fn ns_anchor_char() void {
    minus_fn _w297 _w298;
}

// [103] NS-ANCHOR-NAME 
fn ns_anchor_name() void {
    plus_ _w299;
}

// [104] C-NS-ALIAS-NODE 
fn c_ns_alias_node() void {
    build_ast "ALIAS" _w303;
}

// [105] E-SCALAR 
fn e_scalar() void {
    ok;
}

// [106] E-NODE 
fn e_node() void {
    e_scalar;
}

// [107] NB-DOUBLE-CHAR 
fn nb_double_char() void {
    peg_alt(&[_]PFn{_w304, _w309});
}

// [108] NS-DOUBLE-CHAR 
fn ns_double_char() void {
    minus_fn _w310 _w311;
}

// [109] C-DOUBLE-QUOTED 
fn c_double_quoted() void {
    scalar_fn _w315;
}

// [110] NB-DOUBLE-TEXT 
fn nb_double_text() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .fn = nb_double_multi_line g.n },
        .{ .ctx = "FLOW-IN", .fn = nb_double_multi_line g.n },
        .{ .ctx = "BLOCK-KEY", .fn = nb_double_one_line },
        .{ .ctx = "FLOW-KEY", .fn = nb_double_one_line },
    });
}

// [111] NB-DOUBLE-ONE-LINE 
fn nb_double_one_line() void {
    peg_star _w316;
}

// [112] S-DOUBLE-ESCAPED 
fn s_double_escaped() void {
    peg_seq(&[_]PFn{_w318, _w319, _w320, _w322, _w323});
}

// [113] S-DOUBLE-BREAK 
fn s_double_break() void {
    peg_alt(&[_]PFn{_w324, _w325});
}

// [114] NB-NS-DOUBLE-IN-LINE 
fn nb_ns_double_in_line() void {
    peg_star _w329;
}

// [115] S-DOUBLE-NEXT-LINE 
fn s_double_next_line() void {
    peg_seq(&[_]PFn{_w330, _w338});
}

// [116] NB-DOUBLE-MULTI-LINE 
fn nb_double_multi_line() void {
    peg_seq(&[_]PFn{_w339, _w343});
}

// [117] C-QUOTED-QUOTE 
fn c_quoted_quote() void {
    match_str "''";
}

// [118] NB-SINGLE-CHAR 
fn nb_single_char() void {
    peg_alt(&[_]PFn{_w344, _w347});
}

// [119] NS-SINGLE-CHAR 
fn ns_single_char() void {
    minus_fn _w348 _w349;
}

// [120] C-SINGLE-QUOTED 
fn c_single_quoted() void {
    scalar_fn _w353;
}

// [121] NB-SINGLE-TEXT 
fn nb_single_text() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .fn = nb_single_multi_line g.n },
        .{ .ctx = "FLOW-IN", .fn = nb_single_multi_line g.n },
        .{ .ctx = "BLOCK-KEY", .fn = nb_single_one_line },
        .{ .ctx = "FLOW-KEY", .fn = nb_single_one_line },
    });
}

// [122] NB-SINGLE-ONE-LINE 
fn nb_single_one_line() void {
    peg_star _w354;
}

// [123] NS-SINGLE-IN-LINE 
fn ns_single_in_line() void {
    peg_star _w358;
}

// [124] S-SINGLE-NEXT-LINE 
fn s_single_next_line() void {
    peg_seq(&[_]PFn{_w359, _w367});
}

// [125] NB-SINGLE-MULTI-LINE 
fn nb_single_multi_line() void {
    peg_seq(&[_]PFn{_w368, _w372});
}

// [126] NS-PLAIN-FIRST 
fn ns_plain_first() void {
    peg_alt(&[_]PFn{_w375, _w382});
}

// [127] NS-PLAIN-SAFE 
fn ns_plain_safe() void {
    switch_ctx(c, &[_]CtxCase{
        .{ .ctx = "FLOW-OUT", .fn = ns_plain_safe_out },
        .{ .ctx = "FLOW-IN", .fn = ns_plain_safe_in },
        .{ .ctx = "BLOCK-KEY", .fn = ns_plain_safe_out },
        .{ .ctx = "FLOW-KEY", .fn = ns_plain_safe_in },
    });
}

// [128] NS-PLAIN-SAFE-OUT 
fn ns_plain_safe_out() void {
    ns_char;
}

// [129] NS-PLAIN-SAFE-IN 
fn ns_plain_safe_in() void {
    minus_fn _w383 _w384;
}

// [130] NS-PLAIN-CHAR 
fn ns_plain_char() void {
    peg_alt(&[_]PFn{_w389, _w393, _w397});
}

// [131] NS-PLAIN 
fn ns_plain() void {
    scalar_fn _w398;
}

// [132] NB-NS-PLAIN-IN-LINE 
fn nb_ns_plain_in_line() void {
    peg_star _w402;
}

// [133] NS-PLAIN-ONE-LINE 
fn ns_plain_one_line() void {
    peg_seq(&[_]PFn{_w403, _w404});
}

// [134] S-NS-PLAIN-NEXT-LINE 
fn s_ns_plain_next_line() void {
    peg_seq(&[_]PFn{_w405, _w407, _w408, _w409});
}

// [135] NS-PLAIN-MULTI-LINE 
fn ns_plain_multi_line() void {
    peg_seq(&[_]PFn{_w410, _w412});
}

// [137] C-FLOW-SEQUENCE 
fn c_flow_sequence() void {
    build_ast "SEQUENCE" _w420;
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
fn ns_s_flow_seq_entries() void {
    peg_seq(&[_]PFn{_w421, _w423, _w430});
}

// [139] NS-FLOW-SEQ-ENTRY 
fn ns_flow_seq_entry() void {
    peg_alt(&[_]PFn{_w431, _w432});
}

// [140] C-FLOW-MAPPING 
fn c_flow_mapping() void {
    build_ast "MAPPING" _w440;
}

// [141] NS-S-FLOW-MAP-ENTRIES 
fn ns_s_flow_map_entries() void {
    peg_seq(&[_]PFn{_w441, _w443, _w450});
}

// [142] NS-FLOW-MAP-ENTRY 
fn ns_flow_map_entry() void {
    peg_alt(&[_]PFn{_w454, _w455});
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
fn ns_flow_map_explicit_entry() void {
    peg_alt(&[_]PFn{_w456, _w459});
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
fn ns_flow_map_implicit_entry() void {
    build_ast "PAIR" _w463;
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
fn ns_flow_map_yaml_key_entry() void {
    peg_seq(&[_]PFn{_w464, _w470});
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
fn c_ns_flow_map_empty_key_entry() void {
    peg_seq(&[_]PFn{_w471, _w472});
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
fn c_ns_flow_map_separate_value() void {
    peg_seq(&[_]PFn{_w473, _w475, _w480});
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
fn c_ns_flow_map_json_key_entry() void {
    peg_seq(&[_]PFn{_w481, _w487});
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
fn c_ns_flow_map_adjacent_value() void {
    peg_seq(&[_]PFn{_w488, _w494});
}

// [150] NS-FLOW-PAIR 
fn ns_flow_pair() void {
    peg_alt(&[_]PFn{_w498, _w499});
}

// [151] NS-FLOW-PAIR-ENTRY 
fn ns_flow_pair_entry() void {
    peg_alt(&[_]PFn{_w500, _w501, _w502});
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
fn ns_flow_pair_yaml_key_entry() void {
    peg_seq(&[_]PFn{_w503, _w504});
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
fn c_ns_flow_pair_json_key_entry() void {
    peg_seq(&[_]PFn{_w505, _w506});
}

// [154] NS-S-IMPLICIT-YAML-KEY 
fn ns_s_implicit_yaml_key() void {
    peg_seq(&[_]PFn{_w507, _w509});
}

// [155] C-S-IMPLICIT-JSON-KEY 
fn c_s_implicit_json_key() void {
    peg_seq(&[_]PFn{_w510, _w512});
}

// [156] NS-FLOW-YAML-CONTENT 
fn ns_flow_yaml_content() void {
    ns_plain g.n g.c;
}

// [157] C-FLOW-JSON-CONTENT 
fn c_flow_json_content() void {
    peg_alt(&[_]PFn{_w513, _w514, _w515, _w516});
}

// [158] NS-FLOW-CONTENT 
fn ns_flow_content() void {
    peg_alt(&[_]PFn{_w517, _w518});
}

// [159] NS-FLOW-YAML-NODE 
fn ns_flow_yaml_node() void {
    peg_alt(&[_]PFn{_w519, _w520, _w527});
}

// [160] C-FLOW-JSON-NODE 
fn c_flow_json_node() void {
    peg_seq(&[_]PFn{_w531, _w532});
}

// [161] NS-FLOW-NODE 
fn ns_flow_node() void {
    peg_alt(&[_]PFn{_w533, _w534, _w541});
}

// [162] C-B-BLOCK-HEADER 
fn c_b_block_header() void {
    peg_alt(&[_]PFn{_w550, _w559});
}

// [163] C-INDENTATION-INDICATOR 
fn c_indentation_indicator() void {
    peg_alt(&[_]PFn{_w560, _w561});
}

// [164] C-CHOMPING-INDICATOR 
fn c_chomping_indicator() void {
    peg_alt(&[_]PFn{_w562, _w563, _w564});
}

// [165] B-CHOMPED-LAST 
fn b_chomped_last() void {
    switch_ctx(t, &[_]CtxCase{
        .{ .ctx = "STRIP", .fn = b_non_content },
        .{ .ctx = "CLIP", .fn = b_as_line_feed },
        .{ .ctx = "KEEP", .fn = b_as_line_feed },
    });
}

// [166] L-CHOMPED-EMPTY 
fn l_chomped_empty() void {
    switch_ctx(t, &[_]CtxCase{
        .{ .ctx = "STRIP", .fn = l_strip_empty g.n },
        .{ .ctx = "CLIP", .fn = l_strip_empty g.n },
        .{ .ctx = "KEEP", .fn = l_keep_empty g.n },
    });
}

// [167] L-STRIP-EMPTY 
fn l_strip_empty() void {
    peg_seq(&[_]PFn{_w568, _w570});
}

// [168] L-KEEP-EMPTY 
fn l_keep_empty() void {
    peg_seq(&[_]PFn{_w572, _w574});
}

// [169] L-TRAIL-COMMENTS 
fn l_trail_comments() void {
    peg_seq(&[_]PFn{_w575, _w576, _w577, _w579});
}

// [170] C-L+LITERAL 
fn c_lliteral() void {
    peg_seq(&[_]PFn{_w580, _w591});
}

// [171] L-NB-LITERAL-TEXT 
fn l_nb_literal_text() void {
    peg_seq(&[_]PFn{_w593, _w594, _w596});
}

// [172] B-NB-LITERAL-NEXT 
fn b_nb_literal_next() void {
    peg_seq(&[_]PFn{_w597, _w598});
}

// [173] L-LITERAL-CONTENT 
fn l_literal_content() void {
    scalar_fn _w606;
}

// [174] C-L+FOLDED 
fn c_lfolded() void {
    peg_seq(&[_]PFn{_w607, _w618});
}

// [175] S-NB-FOLDED-TEXT 
fn s_nb_folded_text() void {
    peg_seq(&[_]PFn{_w619, _w620, _w622});
}

// [176] L-NB-FOLDED-LINES 
fn l_nb_folded_lines() void {
    peg_seq(&[_]PFn{_w623, _w627});
}

// [177] S-NB-SPACED-TEXT 
fn s_nb_spaced_text() void {
    peg_seq(&[_]PFn{_w628, _w629, _w631});
}

// [178] B-L-SPACED 
fn b_l_spaced() void {
    peg_seq(&[_]PFn{_w632, _w634});
}

// [179] L-NB-SPACED-LINES 
fn l_nb_spaced_lines() void {
    peg_seq(&[_]PFn{_w635, _w639});
}

// [180] L-NB-SAME-LINES 
fn l_nb_same_lines() void {
    peg_seq(&[_]PFn{_w641, _w644});
}

// [181] L-NB-DIFF-LINES 
fn l_nb_diff_lines() void {
    peg_seq(&[_]PFn{_w645, _w649});
}

// [182] L-FOLDED-CONTENT 
fn l_folded_content() void {
    scalar_fn _w655;
}

// [183] L+BLOCK-SEQUENCE 
fn lblock_sequence() void {
    build_ast "SEQUENCE" _w660;
}

// [184] C-L-BLOCK-SEQ-ENTRY 
fn c_l_block_seq_entry() void {
    peg_seq(&[_]PFn{_w661, _w663, _w664});
}

// [185] S-L+BLOCK-INDENTED 
fn s_lblock_indented() void {
    peg_alt(&[_]PFn{_w669, _w670, _w673});
}

// [186] NS-L-COMPACT-SEQUENCE 
fn ns_l_compact_sequence() void {
    peg_seq(&[_]PFn{_w674, _w678});
}

// [187] L+BLOCK-MAPPING 
fn lblock_mapping() void {
    build_ast "MAPPING" _w683;
}

// [188] NS-L-BLOCK-MAP-ENTRY 
fn ns_l_block_map_entry() void {
    peg_alt(&[_]PFn{_w684, _w685});
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
fn c_l_block_map_explicit_entry() void {
    peg_seq(&[_]PFn{_w686, _w689});
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
fn c_l_block_map_explicit_key() void {
    peg_seq(&[_]PFn{_w690, _w691});
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
fn l_block_map_explicit_value() void {
    peg_seq(&[_]PFn{_w692, _w693, _w694});
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
fn ns_l_block_map_implicit_entry() void {
    build_ast "PAIR" _w700;
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
fn ns_s_block_map_implicit_key() void {
    peg_alt(&[_]PFn{_w701, _w702});
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
fn c_l_block_map_implicit_value() void {
    peg_seq(&[_]PFn{_w703, _w709});
}

// [195] NS-L-COMPACT-MAPPING 
fn ns_l_compact_mapping() void {
    peg_seq(&[_]PFn{_w710, _w714});
}

// [196] S-L+BLOCK-NODE 
fn s_lblock_node() void {
    peg_alt(&[_]PFn{_w715, _w716});
}

// [197] S-L+FLOW-IN-BLOCK 
fn s_lflow_in_block() void {
    peg_seq(&[_]PFn{_w717, _w718, _w719});
}

// [198] S-L+BLOCK-IN-BLOCK 
fn s_lblock_in_block() void {
    peg_alt(&[_]PFn{_w720, _w721});
}

// [199] S-L+BLOCK-SCALAR 
fn s_lblock_scalar() void {
    peg_seq(&[_]PFn{_w722, _w726, _w729});
}

// [200] S-L+BLOCK-COLLECTION 
fn s_lblock_collection() void {
    peg_seq(&[_]PFn{_w733, _w734, _w737});
}

// [202] L-DOCUMENT-PREFIX 
fn l_document_prefix() void {
    peg_seq(&[_]PFn{_w739, _w741});
}

// [203] C-DIRECTIVES-END 
fn c_directives_end() void {
    match_str "---";
}

// [204] C-DOCUMENT-END 
fn c_document_end() void {
    match_str "...";
}

// [205] L-DOCUMENT-SUFFIX 
fn l_document_suffix() void {
    peg_seq(&[_]PFn{_w742, _w743});
}

// [206] C-FORBIDDEN 
fn c_forbidden() void {
    peg_seq(&[_]PFn{_w744, _w747, _w751});
}

// [207] L-BARE-DOCUMENT 
fn l_bare_document() void {
    build_ast "DOC" _w752;
}

// [208] L-EXPLICIT-DOCUMENT 
fn l_explicit_document() void {
    build_ast "DOC" _w759;
}

// [209] L-DIRECTIVE-DOCUMENT 
fn l_directive_document() void {
    peg_seq(&[_]PFn{_w761, _w762});
}

// [210] L-ANY-DOCUMENT 
fn l_any_document() void {
    peg_alt(&[_]PFn{_w763, _w764, _w765});
}

// [211] L-YAML-STREAM 
fn l_yaml_stream() void {
    build_ast "STREAM" _w784;
}



