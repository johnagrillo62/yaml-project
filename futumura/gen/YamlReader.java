// ════════════════════════════════════════════════════════════════
// YamlReader.java — YAML 1.2 parser, projected from yaml-grammar.scm
// ════════════════════════════════════════════════════════════════
// Generated. DO NOT EDIT — regenerate from the grammar.
// ════════════════════════════════════════════════════════════════

import java.io.*;
import java.nio.file.*;
import java.util.*;

@SuppressWarnings("all")
public class YamlReader {

  @FunctionalInterface
  interface PFn {
    Result apply(Input inp);
  }
  @FunctionalInterface
  interface Sup {
    Result get();
  }

  static final class Input {
    final String src;
    final int pos, line, col;
    Input(String src, int pos, int line, int col) {
      this.src = src;
      this.pos = pos;
      this.line = line;
      this.col = col;
    }
    static Input of(String s) { return new Input(s, 0, 1, 0); }
    boolean atEof() { return pos >= src.length(); }
    int peek() {
      if (atEof())
        return -1;
      return src.codePointAt(pos);
    }
    Input adv() {
      if (atEof())
        return this;
      int cp = src.codePointAt(pos);
      int npos = pos + Character.charCount(cp);
      boolean nl = cp == '\n';
      return new Input(src, npos, nl ? line + 1 : line, nl ? 0 : col + 1);
    }
  }

  static class Ast {
    String tag, text;
    List<Ast> children;
    boolean isLeaf;
    static Ast branch(String tag) {
      Ast a = new Ast();
      a.tag = tag;
      a.children = new ArrayList<>();
      return a;
    }
    static Ast leaf(String text) {
      Ast a = new Ast();
      a.text = text;
      a.isLeaf = true;
      return a;
    }
  }

  static final class Result {
    boolean fail;
    String val = "";
    Input rest;
    String tag = "";
    int tagInt;
    Ast ast;
    List<Ast> astList;
    String err = "";
    Result() { astList = new ArrayList<>(); }
  }

  static Result ok(Input inp) {
    Result r = new Result();
    r.rest = inp;
    return r;
  }
  static Result okV(Input inp, String v) {
    Result r = ok(inp);
    r.val = v;
    return r;
  }
  static Result fail(Input inp, String m) {
    Result r = new Result();
    r.fail = true;
    r.rest = inp;
    r.err = m;
    return r;
  }

  static String inFlow(String c) {
    return c.equals("FLOW-OUT") || c.equals("FLOW-IN") ? "FLOW-IN" : "FLOW-KEY";
  }
  static int seqSpaces(int n, String c) {
    return c.equals("BLOCK-OUT") ? n - 1 : n;
  }

  static Result matchCp(Input inp, int cp) {
    int c = inp.peek();
    if (c == cp) {
      String s = new String(Character.toChars(c));
      Input nx = inp;
      for (int i = 0; i < s.length(); i++)
        nx = nx.adv();
      return okV(nx, s);
    }
    return fail(inp, "cp");
  }

  static Result matchRange(Input inp, int lo, int hi) {
    int c = inp.peek();
    if (c >= lo && c <= hi) {
      String s = new String(Character.toChars(c));
      Input nx = inp;
      for (int i = 0; i < s.length(); i++)
        nx = nx.adv();
      return okV(nx, s);
    }
    return fail(inp, "rng");
  }

  static Result matchStr(Input inp, String t) {
    int n = t.length();
    if (inp.pos + n > inp.src.length())
      return fail(inp, "str");
    if (!inp.src.substring(inp.pos, inp.pos + n).equals(t))
      return fail(inp, "str");
    Input cur = inp;
    for (int i = 0; i < n; i++)
      cur = cur.adv();
    return okV(cur, t);
  }

  static void mergeAsts(List<Ast> dst, Result r) {
    if (r.ast != null)
      dst.add(r.ast);
    if (r.astList != null && !r.astList.isEmpty())
      dst.addAll(r.astList);
  }

  static Result seq(Input inp, PFn[] fns) {
    Input cur = inp;
    StringBuilder acc = new StringBuilder();
    List<Ast> asts = new ArrayList<>();
    for (PFn f : fns) {
      Result r = f.apply(cur);
      if (r.fail)
        return r;
      acc.append(r.val);
      mergeAsts(asts, r);
      cur = r.rest;
    }
    Result res = okV(cur, acc.toString());
    if (asts.size() == 1)
      res.ast = asts.get(0);
    else if (asts.size() > 1)
      res.astList = asts;
    return res;
  }

  static Result alt(Input inp, PFn[] fns) {
    for (PFn f : fns) {
      Result r = f.apply(inp);
      if (!r.fail)
        return r;
    }
    return fail(inp, "alt");
  }

  static Result star(Input inp, PFn f) {
    Input cur = inp;
    StringBuilder acc = new StringBuilder();
    List<Ast> asts = new ArrayList<>();
    while (true) {
      Result r = f.apply(cur);
      if (r.fail || r.rest.pos <= cur.pos)
        break;
      acc.append(r.val);
      mergeAsts(asts, r);
      cur = r.rest;
    }
    Result res = okV(cur, acc.toString());
    if (!asts.isEmpty())
      res.astList = asts;
    return res;
  }

  static Result plus_(Input inp, PFn f) {
    Result first = f.apply(inp);
    if (first.fail)
      return first;
    Result rest = star(first.rest, f);
    Result res = okV(rest.rest, first.val + rest.val);
    List<Ast> asts = new ArrayList<>();
    mergeAsts(asts, first);
    mergeAsts(asts, rest);
    if (!asts.isEmpty())
      res.astList = asts;
    return res;
  }

  static Result opt(Input inp, PFn f) {
    Result r = f.apply(inp);
    return r.fail ? ok(inp) : r;
  }
  static Result neg(Input inp, PFn f) {
    Result r = f.apply(inp);
    return r.fail ? ok(inp) : fail(inp, "neg");
  }
  static Result minus(Input inp, PFn fa, PFn fb) {
    Result ra = fa.apply(inp);
    if (ra.fail)
      return ra;
    Result rb = fb.apply(inp);
    return (!rb.fail && rb.rest.pos == ra.rest.pos) ? fail(inp, "excl") : ra;
  }
  static Result rep(Input inp, int n, PFn f) {
    Input cur = inp;
    StringBuilder acc = new StringBuilder();
    for (int i = 0; i < n; i++) {
      Result r = f.apply(cur);
      if (r.fail)
        return r;
      acc.append(r.val);
      cur = r.rest;
    }
    return okV(cur, acc.toString());
  }
  static Result ahead(Input inp, PFn f) {
    Result r = f.apply(inp);
    return r.fail ? r : ok(inp);
  }
  static Result behind(Input inp, PFn f) {
    if (inp.pos == 0)
      return fail(inp, "bh");
    Input t =
        new Input(inp.src, inp.pos - 1, inp.line, Math.max(0, inp.col - 1));
    Result r = f.apply(t);
    return r.fail ? fail(inp, "bh") : ok(inp);
  }
  static Result sol(Input inp) {
    return inp.col == 0 ? ok(inp) : fail(inp, "sol");
  }
  static Result eofOk(Input inp) {
    return inp.atEof() ? ok(inp) : fail(inp, "eof");
  }

  static Result build(Input inp, String typ, PFn f) {
    Result r = f.apply(inp);
    if (r.fail)
      return r;
    Ast node = Ast.branch(typ);
    if (r.ast != null)
      node.children.add(r.ast);
    if (r.astList != null && !r.astList.isEmpty())
      node.children.addAll(r.astList);
    r.ast = node;
    r.astList = new ArrayList<>();
    return r;
  }

  static Result scalar(Input inp, PFn f) {
    Result r = f.apply(inp);
    if (r.fail)
      return r;
    r.ast = Ast.leaf(r.val);
    return r;
  }

  static Result collect(Input inp, PFn f) { return f.apply(inp); }

  static Result detectIndent(Input inp, int n) {
    String s = inp.src;
    int len = s.length();
    int i = inp.pos;
    int sp = 0;
    while (i + sp < len && s.charAt(i + sp) == ' ')
      sp++;
    if (i + sp < len && s.charAt(i + sp) != '\n') {
      Result r = ok(inp);
      r.tagInt = Math.max(1, sp - n);
      return r;
    }
    int j = i;
    while (j < len && s.charAt(j) != '\n')
      j++;
    while (j < len) {
      if (s.charAt(j) == '\n')
        j++;
      if (j >= len)
        break;
      sp = 0;
      while (j + sp < len && s.charAt(j + sp) == ' ')
        sp++;
      int nx = j + sp;
      if (nx >= len || s.charAt(nx) == '\n') {
        j = nx;
        continue;
      }
      Result r = ok(inp);
      r.tagInt = Math.max(1, sp - n);
      return r;
    }
    Result r = ok(inp);
    r.tagInt = 1;
    return r;
  }

  static Result parseInt_(Input inp, PFn f) {
    Result r = f.apply(inp);
    if (r.fail)
      return r;
    int v = 0;
    for (char c : r.val.toCharArray())
      if (c >= '0' && c <= '9')
        v = v * 10 + (c - '0');
    r.tagInt = v;
    return r;
  }
  static Result parseSym(Input inp, PFn f, String sym) {
    Result r = f.apply(inp);
    if (r.fail)
      return r;
    r.tag = sym;
    return r;
  }
  static Result val(Input inp, String v) {
    Result r = ok(inp);
    r.tag = v;
    return r;
  }

  // ════════════════════════════════════════════════════════════════
  // YAML 1.2 Grammar — 211 rules
  // ════════════════════════════════════════════════════════════════

  // [1] C-PRINTABLE
  static Result c_printable(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 0x9);
  }
}
, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 0x0A); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 0x0D); }
}, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0x20, 0x7E); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 0x85); }
}, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0xA0, 0xD7FF); }
}, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0xE000, 0xFFFD); }
}, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0x10000, 0x10FFFF); }
}
});
}

// [2] NB-JSON
static Result nb_json(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 0x9);
}
}
, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0x20, 0x10FFFF); }
}
});
}

// [3] C-BYTE-ORDER-MARK
static Result c_byte_order_mark(Input inp) { return matchCp(inp, 0xFEFF); }

// [4] C-SEQUENCE-ENTRY
static Result c_sequence_entry(Input inp) { return matchCp(inp, 45); }

// [5] C-MAPPING-KEY
static Result c_mapping_key(Input inp) { return matchCp(inp, 63); }

// [6] C-MAPPING-VALUE
static Result c_mapping_value(Input inp) { return matchCp(inp, 58); }

// [7] C-COLLECT-ENTRY
static Result c_collect_entry(Input inp) { return matchCp(inp, 44); }

// [8] C-SEQUENCE-START
static Result c_sequence_start(Input inp) { return matchCp(inp, 91); }

// [9] C-SEQUENCE-END
static Result c_sequence_end(Input inp) { return matchCp(inp, 93); }

// [10] C-MAPPING-START
static Result c_mapping_start(Input inp) { return matchCp(inp, 123); }

// [11] C-MAPPING-END
static Result c_mapping_end(Input inp) { return matchCp(inp, 125); }

// [12] C-COMMENT
static Result c_comment(Input inp) { return matchCp(inp, 35); }

// [13] C-ANCHOR
static Result c_anchor(Input inp) { return matchCp(inp, 38); }

// [14] C-ALIAS
static Result c_alias(Input inp) { return matchCp(inp, 42); }

// [15] C-TAG
static Result c_tag(Input inp) { return matchCp(inp, 33); }

// [16] C-LITERAL
static Result c_literal(Input inp) { return matchCp(inp, 124); }

// [17] C-FOLDED
static Result c_folded(Input inp) { return matchCp(inp, 62); }

// [18] C-SINGLE-QUOTE
static Result c_single_quote(Input inp) { return matchCp(inp, 39); }

// [19] C-DOUBLE-QUOTE
static Result c_double_quote(Input inp) { return matchCp(inp, 34); }

// [20] C-DIRECTIVE
static Result c_directive(Input inp) { return matchCp(inp, 37); }

// [21] C-RESERVED
static Result c_reserved(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 64);
}
}
, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 96); }
}
});
}

// [22] C-INDICATOR
static Result c_indicator(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_sequence_entry(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return c_mapping_key(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_mapping_value(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_collect_entry(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_sequence_start(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_sequence_end(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_mapping_start(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_mapping_end(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_comment(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_anchor(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_alias(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_tag(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_literal(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_folded(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_single_quote(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_double_quote(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_directive(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_reserved(inp); }
}
});
}

// [23] C-FLOW-INDICATOR
static Result c_flow_indicator(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_collect_entry(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return c_sequence_start(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_sequence_end(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_mapping_start(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_mapping_end(inp); }
}
});
}

// [24] B-LINE-FEED
static Result b_line_feed(Input inp) { return matchCp(inp, 0x0A); }

// [25] B-CARRIAGE-RETURN
static Result b_carriage_return(Input inp) { return matchCp(inp, 0x0D); }

// [26] B-CHAR
static Result b_char(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return b_line_feed(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return b_carriage_return(inp); }
}
});
}

// [27] NB-CHAR
static Result nb_char(Input inp) {
        return minus(inp, new PFn() {
    public Result apply(Input inp) { return c_printable(inp); } }, new PFn() {
    public Result apply(Input inp) { return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return b_char(inp);
    } },
        new PFn() {
    public Result apply(Input inp) { return c_byte_order_mark(inp); } }
});
}
});
}

// [28] B-BREAK
static Result b_break(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return b_carriage_return(inp); } },
            new PFn() { public Result apply(Input inp) { return b_line_feed(inp);
}
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return b_carriage_return(inp); }
}, new PFn() {
  public Result apply(Input inp) { return b_line_feed(inp); }
}
});
}

// [29] B-AS-LINE-FEED
static Result b_as_line_feed(Input inp) { return b_break(inp); }

// [30] B-NON-CONTENT
static Result b_non_content(Input inp) { return b_break(inp); }

// [31] S-SPACE
static Result s_space(Input inp) { return matchCp(inp, 0x20); }

// [32] S-TAB
static Result s_tab(Input inp) { return matchCp(inp, 0x9); }

// [33] S-WHITE
static Result s_white(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_space(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return s_tab(inp); }
}
});
}

// [34] NS-CHAR
static Result ns_char(Input inp) {
  return minus(inp,
               new PFn() {
                 public Result apply(Input inp) { return nb_char(inp); }
               },
               new PFn() {
                 public Result apply(Input inp) { return s_white(inp); }
               });
}

// [35] NS-DEC-DIGIT
static Result ns_dec_digit(Input inp) { return matchRange(inp, 0x30, 0x39); }

// [36] NS-HEX-DIGIT
static Result ns_hex_digit(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_dec_digit(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0x41, 0x46); }
}, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0x61, 0x66); }
}
});
}

// [37] NS-ASCII-LETTER
static Result ns_ascii_letter(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchRange(inp, 0x41, 0x5A);
}
}
, new PFn() {
  public Result apply(Input inp) { return matchRange(inp, 0x61, 0x7A); }
}
});
}

// [38] NS-WORD-CHAR
static Result ns_word_char(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_dec_digit(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_ascii_letter(inp); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 45); }
}
});
}

// [39] NS-URI-CHAR
static Result ns_uri_char(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 37); } },
            new PFn() { public Result apply(Input inp) { return ns_hex_digit(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_hex_digit(inp); }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_word_char(inp); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 35); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 59); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 47); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 63); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 58); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 64); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 38); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 61); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 43); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 36); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 44); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 95); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 46); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 33); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 126); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 42); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 39); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 40); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 41); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 91); }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 93); }
}
});
}

// [40] NS-TAG-CHAR
static Result ns_tag_char(Input inp) {
        return minus(inp, new PFn() {
    public Result apply(Input inp) { return ns_uri_char(inp); } }, new PFn() {
    public Result apply(Input inp) { return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_tag(inp);
    } },
        new PFn() {
    public Result apply(Input inp) { return c_flow_indicator(inp); } }
});
}
});
}

// [41] C-ESCAPE
static Result c_escape(Input inp) { return matchCp(inp, 92); }

// [42] NS-ESC-NULL
static Result ns_esc_null(Input inp) { return matchCp(inp, 48); }

// [43] NS-ESC-BELL
static Result ns_esc_bell(Input inp) { return matchCp(inp, 97); }

// [44] NS-ESC-BACKSPACE
static Result ns_esc_backspace(Input inp) { return matchCp(inp, 98); }

// [45] NS-ESC-HORIZONTAL-TAB
static Result ns_esc_horizontal_tab(Input inp) { return matchCp(inp, 116); }

// [46] NS-ESC-LINE-FEED
static Result ns_esc_line_feed(Input inp) { return matchCp(inp, 110); }

// [47] NS-ESC-VERTICAL-TAB
static Result ns_esc_vertical_tab(Input inp) { return matchCp(inp, 118); }

// [48] NS-ESC-FORM-FEED
static Result ns_esc_form_feed(Input inp) { return matchCp(inp, 102); }

// [49] NS-ESC-CARRIAGE-RETURN
static Result ns_esc_carriage_return(Input inp) { return matchCp(inp, 114); }

// [50] NS-ESC-ESCAPE
static Result ns_esc_escape(Input inp) { return matchCp(inp, 101); }

// [51] NS-ESC-SPACE
static Result ns_esc_space(Input inp) { return matchCp(inp, 0x20); }

// [52] NS-ESC-DOUBLE-QUOTE
static Result ns_esc_double_quote(Input inp) { return matchCp(inp, 34); }

// [53] NS-ESC-SLASH
static Result ns_esc_slash(Input inp) { return matchCp(inp, 47); }

// [54] NS-ESC-BACKSLASH
static Result ns_esc_backslash(Input inp) { return matchCp(inp, 92); }

// [55] NS-ESC-NEXT-LINE
static Result ns_esc_next_line(Input inp) { return matchCp(inp, 78); }

// [56] NS-ESC-NON-BREAKING-SPACE
static Result ns_esc_non_breaking_space(Input inp) { return matchCp(inp, 95); }

// [57] NS-ESC-LINE-SEPARATOR
static Result ns_esc_line_separator(Input inp) { return matchCp(inp, 76); }

// [58] NS-ESC-PARAGRAPH-SEPARATOR
static Result ns_esc_paragraph_separator(Input inp) { return matchCp(inp, 80); }

// [59] NS-ESC-8-BIT
static Result ns_esc_8_bit(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 120);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return rep(inp, 2, new PFn() {
      public Result apply(Input inp) { return ns_hex_digit(inp); }
    });
  }
}
});
}

// [60] NS-ESC-16-BIT
static Result ns_esc_16_bit(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 117);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return rep(inp, 4, new PFn() {
      public Result apply(Input inp) { return ns_hex_digit(inp); }
    });
  }
}
});
}

// [61] NS-ESC-32-BIT
static Result ns_esc_32_bit(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 85);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return rep(inp, 8, new PFn() {
      public Result apply(Input inp) { return ns_hex_digit(inp); }
    });
  }
}
});
}

// [62] C-NS-ESC-CHAR
static Result c_ns_esc_char(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_escape(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return ns_esc_null(inp);
  }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_bell(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_backspace(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_horizontal_tab(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_line_feed(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_vertical_tab(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_form_feed(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_carriage_return(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_escape(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_space(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_double_quote(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_slash(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_backslash(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_next_line(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_non_breaking_space(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_line_separator(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_paragraph_separator(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_8_bit(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_16_bit(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_esc_32_bit(inp); }
}
});
}
}
});
}

// [63] S-INDENT
static Result s_indent(Input inp, final int n) {
  return rep(inp, n, new PFn() {
    public Result apply(Input inp) { return s_space(inp); }
  });
}

// [64] S-INDENT-LT
static Result s_indent_lt(Input inp, final int n) {
  return star(inp, new PFn() {
    public Result apply(Input inp) { return s_space(inp); }
  });
}

// [65] S-INDENT-LE
static Result s_indent_le(Input inp, final int n) {
  return star(inp, new PFn() {
    public Result apply(Input inp) { return s_space(inp); }
  });
}

// [66] S-SEPARATE-IN-LINE
static Result s_separate_in_line(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return plus_(inp, new PFn() {
    public Result apply(Input inp) { return s_white(inp); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return ok(inp); }
}
});
}

// [67] S-LINE-PREFIX
static Result s_line_prefix(Input inp, final int n, final String c) {
  return ((Sup)(() -> {
           if (c.equals("BLOCK-IN"))
             return s_block_line_prefix(inp, n);
           if (c.equals("BLOCK-OUT"))
             return s_block_line_prefix(inp, n);
           if (c.equals("FLOW-IN"))
             return s_flow_line_prefix(inp, n);
           if (c.equals("FLOW-OUT"))
             return s_flow_line_prefix(inp, n);
           return fail(inp, "no case");
         }))
      .get();
}

// [68] S-BLOCK-LINE-PREFIX
static Result s_block_line_prefix(Input inp, final int n) {
  return s_indent(inp, n);
}

// [69] S-FLOW-LINE-PREFIX
static Result s_flow_line_prefix(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_indent(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate_in_line(inp); }
    });
  }
}
});
}

// [70] L-EMPTY
static Result l_empty(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_line_prefix(inp, n, c); } },
            new PFn() { public Result apply(Input inp) { return s_indent_lt(inp, n);
}
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return b_as_line_feed(inp); }
}
});
}

// [71] B-L-TRIMMED
static Result b_l_trimmed(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return b_non_content(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return plus_(inp, new PFn() {
      public Result apply(Input inp) { return l_empty(inp, n, c); }
    });
  }
}
});
}

// [72] B-AS-SPACE
static Result b_as_space(Input inp) { return b_break(inp); }

// [73] B-L-FOLDED
static Result b_l_folded(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return b_l_trimmed(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return b_as_space(inp); }
}
});
}

// [74] S-FLOW-FOLDED
static Result s_flow_folded(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
    public Result apply(Input inp) { return s_separate_in_line(inp); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return b_l_folded(inp, n, "FLOW-IN"); }
}, new PFn() {
  public Result apply(Input inp) { return s_flow_line_prefix(inp, n); }
}
});
}

// [75] C-NB-COMMENT-TEXT
static Result c_nb_comment_text(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_comment(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return nb_char(inp); }
    });
  }
}
});
}

// [76] B-COMMENT
static Result b_comment(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return b_non_content(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return ok(inp); }
}
});
}

// [77] S-B-COMMENT
static Result s_b_comment(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_separate_in_line(inp);
    } },
            new PFn() {
    public Result apply(Input inp) {
      return opt(inp, new PFn() {
        public Result apply(Input inp) { return c_nb_comment_text(inp); }
      });
    } }});
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return b_comment(inp); }
}
});
}

// [78] L-COMMENT
static Result l_comment(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_separate_in_line(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return c_nb_comment_text(inp); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return b_non_content(inp); }
}
});
}

// [79] S-L-COMMENTS
static Result s_l_comments(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_b_comment(inp); } },
            new PFn() { public Result apply(Input inp) { return ok(inp);
}
}
});
}
}
, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return l_comment(inp); }
    });
  }
}
});
}

// [80] S-SEPARATE
static Result s_separate(Input inp, final int n, final String c) {
  return ((Sup)(() -> {
           if (c.equals("BLOCK-OUT"))
             return s_separate_lines(inp, n);
           if (c.equals("BLOCK-IN"))
             return s_separate_lines(inp, n);
           if (c.equals("FLOW-OUT"))
             return s_separate_lines(inp, n);
           if (c.equals("FLOW-IN"))
             return s_separate_lines(inp, n);
           if (c.equals("BLOCK-KEY"))
             return s_separate_in_line(inp);
           if (c.equals("FLOW-KEY"))
             return s_separate_in_line(inp);
           return fail(inp, "no case");
         }))
      .get();
}

// [81] S-SEPARATE-LINES
static Result s_separate_lines(Input inp, final int n) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_l_comments(inp); } },
            new PFn() { public Result apply(Input inp) { return s_flow_line_prefix(inp, n);
}
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return s_separate_in_line(inp); }
}
});
}

// [82] L-DIRECTIVE
static Result l_directive(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_directive(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return ns_yaml_directive(inp);
  }
}, new PFn() {
  public Result apply(Input inp) { return ns_tag_directive(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_reserved_directive(inp); }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return s_l_comments(inp); }
}
});
}

// [83] NS-RESERVED-DIRECTIVE
static Result ns_reserved_directive(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_directive_name(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return star(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_separate_in_line(inp);
      } },
            new PFn() {
      public Result apply(Input inp) { return ns_directive_parameter(inp); } }
  });
}
});
}
}
});
}

// [84] NS-DIRECTIVE-NAME
static Result ns_directive_name(Input inp) {
  return plus_(inp, new PFn() {
    public Result apply(Input inp) { return ns_char(inp); }
  });
}

// [85] NS-DIRECTIVE-PARAMETER
static Result ns_directive_parameter(Input inp) {
  return plus_(inp, new PFn() {
    public Result apply(Input inp) { return ns_char(inp); }
  });
}

// [86] NS-YAML-DIRECTIVE
static Result ns_yaml_directive(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchStr(inp, "YAML");
}
}
, new PFn() {
  public Result apply(Input inp) { return s_separate_in_line(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_yaml_version(inp); }
}
});
}

// [87] NS-YAML-VERSION
static Result ns_yaml_version(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return plus_(inp, new PFn() {
    public Result apply(Input inp) { return ns_dec_digit(inp); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 46); }
}, new PFn() {
  public Result apply(Input inp) {
    return plus_(inp, new PFn() {
      public Result apply(Input inp) { return ns_dec_digit(inp); }
    });
  }
}
});
}

// [88] NS-TAG-DIRECTIVE
static Result ns_tag_directive(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchStr(inp, "TAG");
}
}
, new PFn() {
  public Result apply(Input inp) { return s_separate_in_line(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_tag_handle(inp); }
}, new PFn() {
  public Result apply(Input inp) { return s_separate_in_line(inp); }
}, new PFn() {
  public Result apply(Input inp) { return ns_tag_prefix(inp); }
}
});
}

// [89] C-TAG-HANDLE
static Result c_tag_handle(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_named_tag_handle(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return c_secondary_tag_handle(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_primary_tag_handle(inp); }
}
});
}

// [90] C-PRIMARY-TAG-HANDLE
static Result c_primary_tag_handle(Input inp) { return matchCp(inp, 33); }

// [91] C-SECONDARY-TAG-HANDLE
static Result c_secondary_tag_handle(Input inp) { return matchStr(inp, "!!"); }

// [92] C-NAMED-TAG-HANDLE
static Result c_named_tag_handle(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 33);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return plus_(inp, new PFn() {
      public Result apply(Input inp) { return ns_word_char(inp); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 33); }
}
});
}

// [93] NS-TAG-PREFIX
static Result ns_tag_prefix(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_ns_local_tag_prefix(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_global_tag_prefix(inp); }
}
});
}

// [94] C-NS-LOCAL-TAG-PREFIX
static Result c_ns_local_tag_prefix(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 33);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return ns_uri_char(inp); }
    });
  }
}
});
}

// [95] NS-GLOBAL-TAG-PREFIX
static Result ns_global_tag_prefix(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_tag_char(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return ns_uri_char(inp); }
    });
  }
}
});
}

// [96] C-NS-PROPERTIES
static Result c_ns_properties(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_ns_tag_property(inp); } },
            new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return s_separate(inp, n, c);
    } },
                new PFn() {
    public Result apply(Input inp) { return c_ns_anchor_property(inp); } }});
}
});
}
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_ns_anchor_property(inp);
  }
}, new PFn() {
  public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return s_separate(inp, n, c);
      } },
                new PFn() {
      public Result apply(Input inp) { return c_ns_tag_property(inp); } }
  });
}
});
}
}
});
}
}
});
}

// [97] C-NS-TAG-PROPERTY
static Result c_ns_tag_property(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_verbatim_tag(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return c_ns_shorthand_tag(inp); }
}, new PFn() {
  public Result apply(Input inp) { return c_non_specific_tag(inp); }
}
});
}

// [98] C-VERBATIM-TAG
static Result c_verbatim_tag(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchStr(inp, "!<");
}
}
, new PFn() {
  public Result apply(Input inp) {
    return plus_(inp, new PFn() {
      public Result apply(Input inp) { return ns_uri_char(inp); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 62); }
}
});
}

// [99] C-NS-SHORTHAND-TAG
static Result c_ns_shorthand_tag(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_tag_handle(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return plus_(inp, new PFn() {
      public Result apply(Input inp) { return ns_tag_char(inp); }
    });
  }
}
});
}

// [100] C-NON-SPECIFIC-TAG
static Result c_non_specific_tag(Input inp) { return matchCp(inp, 33); }

// [101] C-NS-ANCHOR-PROPERTY
static Result c_ns_anchor_property(Input inp) {
        return build(inp, "ANCHOR", new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_anchor(inp);
    } },
        new PFn() {
    public Result apply(Input inp) {
      return scalar(inp, new PFn() {
        public Result apply(Input inp) { return ns_anchor_name(inp); }
      });
    } }
});
}
});
}

// [102] NS-ANCHOR-CHAR
static Result ns_anchor_char(Input inp) {
  return minus(inp,
               new PFn() {
                 public Result apply(Input inp) { return ns_char(inp); }
               },
               new PFn() {
                 public Result apply(Input inp) {
                   return c_flow_indicator(inp);
                 }
               });
}

// [103] NS-ANCHOR-NAME
static Result ns_anchor_name(Input inp) {
  return plus_(inp, new PFn() {
    public Result apply(Input inp) { return ns_anchor_char(inp); }
  });
}

// [104] C-NS-ALIAS-NODE
static Result c_ns_alias_node(Input inp) {
        return build(inp, "ALIAS", new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_alias(inp);
    } },
        new PFn() {
    public Result apply(Input inp) {
      return scalar(inp, new PFn() {
        public Result apply(Input inp) { return ns_anchor_name(inp); }
      });
    } }
});
}
});
}

// [105] E-SCALAR
static Result e_scalar(Input inp) { return ok(inp); }

// [106] E-NODE
static Result e_node(Input inp) { return e_scalar(inp); }

// [107] NB-DOUBLE-CHAR
static Result nb_double_char(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_ns_esc_char(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return minus(inp, new PFn() {
      public Result apply(Input inp) { return nb_json(inp); } }, new PFn() {
      public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 92);
      } },
            new PFn() {
      public Result apply(Input inp) { return matchCp(inp, 34); } }
  });
}
});
}
}
});
}

// [108] NS-DOUBLE-CHAR
static Result ns_double_char(Input inp) {
  return minus(inp,
               new PFn() {
                 public Result apply(Input inp) { return nb_double_char(inp); }
               },
               new PFn() {
                 public Result apply(Input inp) { return s_white(inp); }
               });
}

// [109] C-DOUBLE-QUOTED
static Result c_double_quoted(Input inp, final int n, final String c) {
        return scalar(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 34);
    } },
        new PFn() {
    public Result apply(Input inp) { return nb_double_text(inp, n, c); } },
        new PFn() {
    public Result apply(Input inp) { return matchCp(inp, 34); } }
});
}
});
}

// [110] NB-DOUBLE-TEXT
static Result nb_double_text(Input inp, final int n, final String c) {
  return ((Sup)(() -> {
           if (c.equals("FLOW-OUT"))
             return nb_double_multi_line(inp, n);
           if (c.equals("FLOW-IN"))
             return nb_double_multi_line(inp, n);
           if (c.equals("BLOCK-KEY"))
             return nb_double_one_line(inp);
           if (c.equals("FLOW-KEY"))
             return nb_double_one_line(inp);
           return fail(inp, "no case");
         }))
      .get();
}

// [111] NB-DOUBLE-ONE-LINE
static Result nb_double_one_line(Input inp) {
  return star(inp, new PFn() {
    public Result apply(Input inp) { return nb_double_char(inp); }
  });
}

// [112] S-DOUBLE-ESCAPED
static Result s_double_escaped(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
    public Result apply(Input inp) { return s_white(inp); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 92); }
}, new PFn() {
  public Result apply(Input inp) { return b_non_content(inp); }
}, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return l_empty(inp, n, "FLOW-IN"); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return s_flow_line_prefix(inp, n); }
}
});
}

// [113] S-DOUBLE-BREAK
static Result s_double_break(Input inp, final int n) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_double_escaped(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return s_flow_folded(inp, n); }
}
});
}

// [114] NB-NS-DOUBLE-IN-LINE
static Result nb_ns_double_in_line(Input inp) {
        return star(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
        public Result apply(Input inp) { return s_white(inp); } });
    } },
        new PFn() {
    public Result apply(Input inp) { return ns_double_char(inp); } }
});
}
});
}

// [115] S-DOUBLE-NEXT-LINE
static Result s_double_next_line(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_double_break(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return ns_double_char(inp);
      } },
            new PFn() {
      public Result apply(Input inp) { return nb_ns_double_in_line(inp); } },
            new PFn() {
      public Result apply(Input inp) { return alt(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return s_double_next_line(inp, n);
      } },
                new PFn() {
      public Result apply(Input inp) {
        return star(inp, new PFn() {
          public Result apply(Input inp) { return s_white(inp); }
        });
      } }
  });
}
}
});
}
});
}
}
});
}

// [116] NB-DOUBLE-MULTI-LINE
static Result nb_double_multi_line(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return nb_ns_double_in_line(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_double_next_line(inp, n);
  }
}, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return s_white(inp); }
    });
  }
}
});
}
}
});
}

// [117] C-QUOTED-QUOTE
static Result c_quoted_quote(Input inp) { return matchStr(inp, "''"); }

// [118] NB-SINGLE-CHAR
static Result nb_single_char(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_quoted_quote(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return minus(inp,
                 new PFn() {
                   public Result apply(Input inp) { return nb_json(inp); }
                 },
                 new PFn() {
                   public Result apply(Input inp) { return matchCp(inp, 39); }
                 });
  }
}
});
}

// [119] NS-SINGLE-CHAR
static Result ns_single_char(Input inp) {
  return minus(inp,
               new PFn() {
                 public Result apply(Input inp) { return nb_single_char(inp); }
               },
               new PFn() {
                 public Result apply(Input inp) { return s_white(inp); }
               });
}

// [120] C-SINGLE-QUOTED
static Result c_single_quoted(Input inp, final int n, final String c) {
        return scalar(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 39);
    } },
        new PFn() {
    public Result apply(Input inp) { return nb_single_text(inp, n, c); } },
        new PFn() {
    public Result apply(Input inp) { return matchCp(inp, 39); } }
});
}
});
}

// [121] NB-SINGLE-TEXT
static Result nb_single_text(Input inp, final int n, final String c) {
  return ((Sup)(() -> {
           if (c.equals("FLOW-OUT"))
             return nb_single_multi_line(inp, n);
           if (c.equals("FLOW-IN"))
             return nb_single_multi_line(inp, n);
           if (c.equals("BLOCK-KEY"))
             return nb_single_one_line(inp);
           if (c.equals("FLOW-KEY"))
             return nb_single_one_line(inp);
           return fail(inp, "no case");
         }))
      .get();
}

// [122] NB-SINGLE-ONE-LINE
static Result nb_single_one_line(Input inp) {
  return star(inp, new PFn() {
    public Result apply(Input inp) { return nb_single_char(inp); }
  });
}

// [123] NS-SINGLE-IN-LINE
static Result ns_single_in_line(Input inp) {
        return star(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
        public Result apply(Input inp) { return s_white(inp); } });
    } },
        new PFn() {
    public Result apply(Input inp) { return ns_single_char(inp); } }
});
}
});
}

// [124] S-SINGLE-NEXT-LINE
static Result s_single_next_line(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_flow_folded(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return ns_single_char(inp);
      } },
            new PFn() {
      public Result apply(Input inp) { return ns_single_in_line(inp); } },
            new PFn() {
      public Result apply(Input inp) { return alt(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return s_single_next_line(inp, n);
      } },
                new PFn() {
      public Result apply(Input inp) {
        return star(inp, new PFn() {
          public Result apply(Input inp) { return s_white(inp); }
        });
      } }
  });
}
}
});
}
});
}
}
});
}

// [125] NB-SINGLE-MULTI-LINE
static Result nb_single_multi_line(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_single_in_line(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_single_next_line(inp, n);
  }
}, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return s_white(inp); }
    });
  }
}
});
}
}
});
}

// [126] NS-PLAIN-FIRST
static Result ns_plain_first(Input inp, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return minus(inp, new PFn() {
    public Result apply(Input inp) { return ns_char(inp); } }, new PFn() {
    public Result apply(Input inp) { return c_indicator(inp); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return alt(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return matchCp(inp, 63); } },
                new PFn() { public Result apply(Input inp) { return matchCp(inp, 58);
  }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 45); }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) {
    return ahead(inp, new PFn() {
      public Result apply(Input inp) { return ns_plain_safe(inp, c); }
    });
  }
}
});
}
}
});
}

// [127] NS-PLAIN-SAFE
static Result ns_plain_safe(Input inp, final String c) {
  return ((Sup)(() -> {
           if (c.equals("FLOW-OUT"))
             return ns_plain_safe_out(inp);
           if (c.equals("FLOW-IN"))
             return ns_plain_safe_in(inp);
           if (c.equals("BLOCK-KEY"))
             return ns_plain_safe_out(inp);
           if (c.equals("FLOW-KEY"))
             return ns_plain_safe_in(inp);
           return fail(inp, "no case");
         }))
      .get();
}

// [128] NS-PLAIN-SAFE-OUT
static Result ns_plain_safe_out(Input inp) { return ns_char(inp); }

// [129] NS-PLAIN-SAFE-IN
static Result ns_plain_safe_in(Input inp) {
  return minus(inp,
               new PFn() {
                 public Result apply(Input inp) { return ns_char(inp); }
               },
               new PFn() {
                 public Result apply(Input inp) {
                   return c_flow_indicator(inp);
                 }
               });
}

// [130] NS-PLAIN-CHAR
static Result ns_plain_char(Input inp, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return minus(inp, new PFn() {
    public Result apply(Input inp) { return ns_plain_safe(inp, c); } }, new PFn() {
    public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 58);
    } },
            new PFn() {
    public Result apply(Input inp) { return matchCp(inp, 35); } }});
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return behind(inp, new PFn() {
      public Result apply(Input inp) { return ns_char(inp); } });
  }
}, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 35); }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 58);
  }
}, new PFn() {
  public Result apply(Input inp) {
    return ahead(inp, new PFn() {
      public Result apply(Input inp) { return ns_plain_safe(inp, c); }
    });
  }
}
});
}
}
});
}

// [131] NS-PLAIN
static Result ns_plain(Input inp, final int n, final String c) {
  return scalar(inp, new PFn() {
    public Result apply(Input inp) {
      return ((Sup)(() -> {
               if (c.equals("FLOW-OUT"))
                 return ns_plain_multi_line(inp, n, c);
               if (c.equals("FLOW-IN"))
                 return ns_plain_multi_line(inp, n, c);
               if (c.equals("BLOCK-KEY"))
                 return ns_plain_one_line(inp, c);
               if (c.equals("FLOW-KEY"))
                 return ns_plain_one_line(inp, c);
               return fail(inp, "no case");
             }))
          .get();
    }
  });
}

// [132] NB-NS-PLAIN-IN-LINE
static Result nb_ns_plain_in_line(Input inp, final String c) {
        return star(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
        public Result apply(Input inp) { return s_white(inp); } });
    } },
        new PFn() {
    public Result apply(Input inp) { return ns_plain_char(inp, c); } }
});
}
});
}

// [133] NS-PLAIN-ONE-LINE
static Result ns_plain_one_line(Input inp, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_plain_first(inp, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return nb_ns_plain_in_line(inp, c); }
}
});
}

// [134] S-NS-PLAIN-NEXT-LINE
static Result s_ns_plain_next_line(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_flow_folded(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return neg(inp, new PFn() {
      public Result apply(Input inp) { return c_forbidden(inp); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return ns_plain_char(inp, c); }
}, new PFn() {
  public Result apply(Input inp) { return nb_ns_plain_in_line(inp, c); }
}
});
}

// [135] NS-PLAIN-MULTI-LINE
static Result ns_plain_multi_line(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_plain_one_line(inp, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return s_ns_plain_next_line(inp, n, c); }
    });
  }
}
});
}

// [137] C-FLOW-SEQUENCE
static Result c_flow_sequence(Input inp, final int n, final String c) {
        return build(inp, "SEQUENCE", new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 91);
    } },
        new PFn() {
    public Result apply(Input inp) {
      return opt(inp, new PFn() {
        public Result apply(Input inp) { return s_separate(inp, n, c); }
      });
    } },
        new PFn() {
    public Result apply(Input inp) {
      return opt(inp, new PFn() {
        public Result apply(Input inp) {
          return collect(inp, new PFn() {
            public Result apply(Input inp) {
              return ns_s_flow_seq_entries(inp, n, inFlow(c));
            }
          });
        }
      });
    } },
        new PFn() {
    public Result apply(Input inp) { return matchCp(inp, 93); } }
});
}
});
}

// [138] NS-S-FLOW-SEQ-ENTRIES
static Result ns_s_flow_seq_entries(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_seq_entry(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate(inp, n, c); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 44);
      } },
            new PFn() {
      public Result apply(Input inp) {
        return opt(inp, new PFn() {
          public Result apply(Input inp) { return s_separate(inp, n, c); }
        });
      } },
            new PFn() {
      public Result apply(Input inp) {
        return opt(inp, new PFn() {
          public Result apply(Input inp) {
            return ns_s_flow_seq_entries(inp, n, c);
          }
        });
      } }
  });
}
});
}
}
});
}

// [139] NS-FLOW-SEQ-ENTRY
static Result ns_flow_seq_entry(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_pair(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_flow_node(inp, n, c); }
}
});
}

// [140] C-FLOW-MAPPING
static Result c_flow_mapping(Input inp, final int n, final String c) {
        return build(inp, "MAPPING", new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 123);
    } },
        new PFn() {
    public Result apply(Input inp) {
      return opt(inp, new PFn() {
        public Result apply(Input inp) { return s_separate(inp, n, c); }
      });
    } },
        new PFn() {
    public Result apply(Input inp) {
      return opt(inp, new PFn() {
        public Result apply(Input inp) {
          return collect(inp, new PFn() {
            public Result apply(Input inp) {
              return ns_s_flow_map_entries(inp, n, inFlow(c));
            }
          });
        }
      });
    } },
        new PFn() {
    public Result apply(Input inp) { return matchCp(inp, 125); } }
});
}
});
}

// [141] NS-S-FLOW-MAP-ENTRIES
static Result ns_s_flow_map_entries(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_map_entry(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate(inp, n, c); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 44);
      } },
            new PFn() {
      public Result apply(Input inp) {
        return opt(inp, new PFn() {
          public Result apply(Input inp) { return s_separate(inp, n, c); }
        });
      } },
            new PFn() {
      public Result apply(Input inp) {
        return opt(inp, new PFn() {
          public Result apply(Input inp) {
            return ns_s_flow_map_entries(inp, n, c);
          }
        });
      } }
  });
}
});
}
}
});
}

// [142] NS-FLOW-MAP-ENTRY
static Result ns_flow_map_entry(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 63); } },
            new PFn() { public Result apply(Input inp) { return s_separate(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return ns_flow_map_explicit_entry(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) {
    return ns_flow_map_implicit_entry(inp, n, c);
  }
}
});
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY
static Result ns_flow_map_explicit_entry(Input inp, final int n,
                                         final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_map_implicit_entry(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return e_node(inp);
  }
}, new PFn() {
  public Result apply(Input inp) { return e_node(inp); }
}
});
}
}
});
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY
static Result ns_flow_map_implicit_entry(Input inp, final int n,
                                         final String c) {
        return build(inp, "PAIR", new PFn() {
    public Result apply(Input inp) { return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_map_yaml_key_entry(inp, n, c);
    } },
        new PFn() {
    public Result apply(Input inp) {
      return c_ns_flow_map_empty_key_entry(inp, n, c);
    } },
        new PFn() {
    public Result apply(Input inp) {
      return c_ns_flow_map_json_key_entry(inp, n, c);
    } }
});
}
});
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY
static Result ns_flow_map_yaml_key_entry(Input inp, final int n,
                                         final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_yaml_node(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate(inp, n, c); } }); } },
                new PFn() { public Result apply(Input inp) { return c_ns_flow_map_separate_value(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return e_node(inp); }
}
});
}
}
});
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY
static Result c_ns_flow_map_empty_key_entry(Input inp, final int n,
                                            final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return e_node(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return c_ns_flow_map_separate_value(inp, n, c);
  }
}
});
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE
static Result c_ns_flow_map_separate_value(Input inp, final int n,
                                           final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 58);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return neg(inp, new PFn() {
      public Result apply(Input inp) { return ns_plain_safe(inp, c); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return s_separate(inp, n, c); } },
                new PFn() { public Result apply(Input inp) { return ns_flow_node(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return e_node(inp); }
}
});
}
}
});
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY
static Result c_ns_flow_map_json_key_entry(Input inp, final int n,
                                           final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_flow_json_node(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate(inp, n, c); } }); } },
                new PFn() { public Result apply(Input inp) { return c_ns_flow_map_adjacent_value(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return e_node(inp); }
}
});
}
}
});
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE
static Result c_ns_flow_map_adjacent_value(Input inp, final int n,
                                           final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 58);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate(inp, n, c); } }); } },
                new PFn() { public Result apply(Input inp) { return ns_flow_node(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return e_node(inp); }
}
});
}
}
});
}

// [150] NS-FLOW-PAIR
static Result ns_flow_pair(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return matchCp(inp, 63); } },
            new PFn() { public Result apply(Input inp) { return s_separate(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return ns_flow_map_explicit_entry(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_flow_pair_entry(inp, n, c); }
}
});
}

// [151] NS-FLOW-PAIR-ENTRY
static Result ns_flow_pair_entry(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_pair_yaml_key_entry(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return c_ns_flow_map_empty_key_entry(inp, n, c);
  }
}, new PFn() {
  public Result apply(Input inp) {
    return c_ns_flow_pair_json_key_entry(inp, n, c);
  }
}
});
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY
static Result ns_flow_pair_yaml_key_entry(Input inp, final int n,
                                          final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_s_implicit_yaml_key(inp, "FLOW-KEY");
}
}
, new PFn() {
  public Result apply(Input inp) {
    return c_ns_flow_map_separate_value(inp, n, c);
  }
}
});
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY
static Result c_ns_flow_pair_json_key_entry(Input inp, final int n,
                                            final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_s_implicit_json_key(inp, "FLOW-KEY");
}
}
, new PFn() {
  public Result apply(Input inp) {
    return c_ns_flow_map_adjacent_value(inp, n, c);
  }
}
});
}

// [154] NS-S-IMPLICIT-YAML-KEY
static Result ns_s_implicit_yaml_key(Input inp, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_yaml_node(inp, 0, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate_in_line(inp); }
    });
  }
}
});
}

// [155] C-S-IMPLICIT-JSON-KEY
static Result c_s_implicit_json_key(Input inp, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_flow_json_node(inp, 0, c);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return s_separate_in_line(inp); }
    });
  }
}
});
}

// [156] NS-FLOW-YAML-CONTENT
static Result ns_flow_yaml_content(Input inp, final int n, final String c) {
  return ns_plain(inp, n, c);
}

// [157] C-FLOW-JSON-CONTENT
static Result c_flow_json_content(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_flow_sequence(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return c_flow_mapping(inp, n, c); }
}, new PFn() {
  public Result apply(Input inp) { return c_single_quoted(inp, n, c); }
}, new PFn() {
  public Result apply(Input inp) { return c_double_quoted(inp, n, c); }
}
});
}

// [158] NS-FLOW-CONTENT
static Result ns_flow_content(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_flow_yaml_content(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return c_flow_json_content(inp, n, c); }
}
});
}

// [159] NS-FLOW-YAML-NODE
static Result ns_flow_yaml_node(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_ns_alias_node(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_flow_yaml_content(inp, n, c); }
}, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_ns_properties(inp, n, c);
  }
}, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
                    new PFn() { public Result apply(Input inp) { return s_separate(inp, n, c); } },
                    new PFn() { public Result apply(Input inp) { return ns_flow_yaml_content(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return e_scalar(inp); }
}
});
}
}
});
}
}
});
}

// [160] C-FLOW-JSON-NODE
static Result c_flow_json_node(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_ns_properties(inp, n, c);
    } },
            new PFn() {
    public Result apply(Input inp) { return s_separate(inp, n, c); } }});
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return c_flow_json_content(inp, n, c); }
}
});
}

// [161] NS-FLOW-NODE
static Result ns_flow_node(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_ns_alias_node(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_flow_content(inp, n, c); }
}, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_ns_properties(inp, n, c);
  }
}, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
                    new PFn() { public Result apply(Input inp) { return s_separate(inp, n, c); } },
                    new PFn() { public Result apply(Input inp) { return ns_flow_content(inp, n, c);
  }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return e_scalar(inp); }
}
});
}
}
});
}
}
});
}

// [162] C-B-BLOCK-HEADER
static Result c_b_block_header(Input inp, final int n) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ((Sup)(() -> { Result r5_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseInt_(inp, new PFn() {
      public Result apply(Input inp) { return ns_dec_digit(inp); } }); } },
            new PFn() { public Result apply(Input inp) { return detectIndent(inp, n);
}
}
});
if (r5_.fail)
  return r5_;
int m = r5_.tagInt;
final Input inp5_ = r5_.rest; return new PFn() {
  public Result apply(Input inp) { return ((Sup)(() -> { Result r6_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 45); } }, "STRIP"); }
  }
  , new PFn() {
    public Result apply(Input inp) {
      return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 43); }
      }, "KEEP");
    }
  }, new PFn() {
    public Result apply(Input inp) { return val(inp, "CLIP"); }
  }});
if (r6_.fail)
  return r6_;
String t = r6_.tag;
final Input inp6_ = r6_.rest;
return new PFn() {
  public Result apply(Input inp) { return s_b_comment(inp); }
}.apply(inp6_);
})).get();
}
}
.apply(inp5_);
})).get();
}
},
        new PFn() {
  public Result apply(Input inp) { return ((Sup)(() -> { Result r7_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 45); } }, "STRIP"); }
  }
  , new PFn() {
    public Result apply(Input inp) {
      return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 43); }
      }, "KEEP");
    }
  }, new PFn() {
    public Result apply(Input inp) { return val(inp, "CLIP"); }
  }});
if (r7_.fail)
  return r7_;
String t = r7_.tag;
final Input inp7_ = r7_.rest; return new PFn() {
  public Result apply(Input inp) { return ((Sup)(() -> { Result r8_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseInt_(inp, new PFn() {
        public Result apply(Input inp) { return ns_dec_digit(inp); } }); }
  }
  , new PFn() {
    public Result apply(Input inp) { return detectIndent(inp, n); }
  }});
if (r8_.fail)
  return r8_;
int m = r8_.tagInt;
final Input inp8_ = r8_.rest;
return new PFn() {
  public Result apply(Input inp) { return s_b_comment(inp); }
}.apply(inp8_);
})).get();
}
}
.apply(inp7_);
})).get();
}
}
});
}

// [163] C-INDENTATION-INDICATOR
static Result c_indentation_indicator(Input inp, final int n) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_dec_digit(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return ok(inp); }
}
});
}

// [164] C-CHOMPING-INDICATOR
static Result c_chomping_indicator(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 45);
}
}
, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 43); }
}, new PFn() {
  public Result apply(Input inp) { return ok(inp); }
}
});
}

// [165] B-CHOMPED-LAST
static Result b_chomped_last(Input inp, final String t) {
  return ((Sup)(() -> {
           if (t.equals("STRIP"))
             return b_non_content(inp);
           if (t.equals("CLIP"))
             return b_as_line_feed(inp);
           if (t.equals("KEEP"))
             return b_as_line_feed(inp);
           return fail(inp, "no case");
         }))
      .get();
}

// [166] L-CHOMPED-EMPTY
static Result l_chomped_empty(Input inp, final int n, final String t) {
  return ((Sup)(() -> {
           if (t.equals("STRIP"))
             return l_strip_empty(inp, n);
           if (t.equals("CLIP"))
             return l_strip_empty(inp, n);
           if (t.equals("KEEP"))
             return l_keep_empty(inp, n);
           return fail(inp, "no case");
         }))
      .get();
}

// [167] L-STRIP-EMPTY
static Result l_strip_empty(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_indent_le(inp, n);
    } },
            new PFn() {
    public Result apply(Input inp) { return b_non_content(inp); } }});
}
});
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return l_trail_comments(inp, n); }
    });
  }
}
});
}

// [168] L-KEEP-EMPTY
static Result l_keep_empty(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
    public Result apply(Input inp) { return l_empty(inp, n, "BLOCK-IN"); } });
}
}
, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return l_trail_comments(inp, n); }
    });
  }
}
});
}

// [169] L-TRAIL-COMMENTS
static Result l_trail_comments(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_indent_lt(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return c_nb_comment_text(inp); }
}, new PFn() {
  public Result apply(Input inp) { return b_comment(inp); }
}, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return l_comment(inp); }
    });
  }
}
});
}

// [170] C-L+LITERAL
static Result c_lliteral(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 124);
}
},
        new PFn() {
  public Result apply(Input inp) { return ((Sup)(() -> { Result r11_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseInt_(inp, new PFn() {
        public Result apply(Input inp) { return ns_dec_digit(inp); } }); }
  }
  , new PFn() {
    public Result apply(Input inp) { return detectIndent(inp, n); }
  }});
if (r11_.fail)
  return r11_;
int m = r11_.tagInt;
final Input inp11_ = r11_.rest; return new PFn() {
  public Result apply(Input inp) { return ((Sup)(() -> { Result r12_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 45); } }, "STRIP"); }
  }
  , new PFn() {
    public Result apply(Input inp) {
      return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 43); }
      }, "KEEP");
    }
  }, new PFn() {
    public Result apply(Input inp) { return val(inp, "CLIP"); }
  }});
if (r12_.fail)
  return r12_;
String t = r12_.tag;
final Input inp12_ = r12_.rest;
return new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_b_comment(inp);
  }
},
    new PFn() {
      public Result apply(Input inp) {
        return l_literal_content(inp, (n + m), t);
      }
    }
});
}
}
.apply(inp12_);
})).get();
}
}
.apply(inp11_);
})).get();
}
}
});
}

// [171] L-NB-LITERAL-TEXT
static Result l_nb_literal_text(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
    public Result apply(Input inp) { return l_empty(inp, n, "BLOCK-IN"); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return s_indent(inp, n); }
}, new PFn() {
  public Result apply(Input inp) {
    return plus_(inp, new PFn() {
      public Result apply(Input inp) { return nb_char(inp); }
    });
  }
}
});
}

// [172] B-NB-LITERAL-NEXT
static Result b_nb_literal_next(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return b_as_line_feed(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return l_nb_literal_text(inp, n); }
}
});
}

// [173] L-LITERAL-CONTENT
static Result l_literal_content(Input inp, final int n, final String t) {
  return scalar(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
        public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return l_nb_literal_text(inp, n);
        } },
            new PFn() {
        public Result apply(Input inp) {
          return star(inp, new PFn() {
            public Result apply(Input inp) { return b_nb_literal_next(inp, n); }
          });
        } },
            new PFn() {
        public Result apply(Input inp) { return b_chomped_last(inp, t); } }});
    }
  });
}
}
, new PFn() {
  public Result apply(Input inp) { return l_chomped_empty(inp, n, t); }
}
});
}
});
}

// [174] C-L+FOLDED
static Result c_lfolded(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 62);
}
},
        new PFn() {
  public Result apply(Input inp) { return ((Sup)(() -> { Result r15_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseInt_(inp, new PFn() {
        public Result apply(Input inp) { return ns_dec_digit(inp); } }); }
  }
  , new PFn() {
    public Result apply(Input inp) { return detectIndent(inp, n); }
  }});
if (r15_.fail)
  return r15_;
int m = r15_.tagInt;
final Input inp15_ = r15_.rest; return new PFn() {
  public Result apply(Input inp) { return ((Sup)(() -> { Result r16_ = alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 45); } }, "STRIP"); }
  }
  , new PFn() {
    public Result apply(Input inp) {
      return parseSym(inp, new PFn() {
        public Result apply(Input inp) { return matchCp(inp, 43); }
      }, "KEEP");
    }
  }, new PFn() {
    public Result apply(Input inp) { return val(inp, "CLIP"); }
  }});
if (r16_.fail)
  return r16_;
String t = r16_.tag;
final Input inp16_ = r16_.rest;
return new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_b_comment(inp);
  }
},
    new PFn() {
      public Result apply(Input inp) {
        return l_folded_content(inp, (n + m), t);
      }
    }
});
}
}
.apply(inp16_);
})).get();
}
}
.apply(inp15_);
})).get();
}
}
});
}

// [175] S-NB-FOLDED-TEXT
static Result s_nb_folded_text(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_indent(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return ns_char(inp); }
}, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return nb_char(inp); }
    });
  }
}
});
}

// [176] L-NB-FOLDED-LINES
static Result l_nb_folded_lines(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_nb_folded_text(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return star(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return b_l_folded(inp, n, "BLOCK-IN");
      } },
            new PFn() {
      public Result apply(Input inp) { return s_nb_folded_text(inp, n); } }
  });
}
});
}
}
});
}

// [177] S-NB-SPACED-TEXT
static Result s_nb_spaced_text(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_indent(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return s_white(inp); }
}, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return nb_char(inp); }
    });
  }
}
});
}

// [178] B-L-SPACED
static Result b_l_spaced(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return b_as_line_feed(inp);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return l_empty(inp, n, "BLOCK-IN"); }
    });
  }
}
});
}

// [179] L-NB-SPACED-LINES
static Result l_nb_spaced_lines(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_nb_spaced_text(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return star(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return b_l_spaced(inp, n);
      } },
            new PFn() {
      public Result apply(Input inp) { return s_nb_spaced_text(inp, n); } }
  });
}
});
}
}
});
}

// [180] L-NB-SAME-LINES
static Result l_nb_same_lines(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
    public Result apply(Input inp) { return l_empty(inp, n, "BLOCK-IN"); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return l_nb_folded_lines(inp, n);
  }
}, new PFn() {
  public Result apply(Input inp) { return l_nb_spaced_lines(inp, n); }
}
});
}
}
});
}

// [181] L-NB-DIFF-LINES
static Result l_nb_diff_lines(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return l_nb_same_lines(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return star(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return b_as_line_feed(inp);
      } },
            new PFn() {
      public Result apply(Input inp) { return l_nb_same_lines(inp, n); } }
  });
}
});
}
}
});
}

// [182] L-FOLDED-CONTENT
static Result l_folded_content(Input inp, final int n, final String t) {
  return scalar(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
        public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return l_nb_diff_lines(inp, n);
        } },
            new PFn() {
        public Result apply(Input inp) { return b_chomped_last(inp, t); } }});
    }
  });
}
}
, new PFn() {
  public Result apply(Input inp) { return l_chomped_empty(inp, n, t); }
}
});
}
});
}

// [183] L+BLOCK-SEQUENCE
static Result lblock_sequence(Input inp, final int n) {
        return build(inp, "SEQUENCE", new PFn() {
    public Result apply(Input inp) { return ((Sup)(() -> {
        Result r17_ = detectIndent(inp, n);
        if (r17_.fail)
          return r17_;
        int m = r17_.tagInt;
        final Input inp17_ = r17_.rest;
        return new PFn() {
          public Result apply(Input inp) { return collect(inp, new PFn() {
              public Result apply(Input inp) { return plus_(inp, new PFn() {
                  public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_indent(inp, (n + m));
                  } },
        new PFn() {
                  public Result apply(Input inp) {
                    return c_l_block_seq_entry(inp, (n + m));
                  } }
              }); }
          });
        } });
    } }.apply(inp17_);
})).get();
}
});
}

// [184] C-L-BLOCK-SEQ-ENTRY
static Result c_l_block_seq_entry(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 45);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return neg(inp, new PFn() {
      public Result apply(Input inp) { return ns_char(inp); }
    });
  }
}, new PFn() {
  public Result apply(Input inp) {
    return s_lblock_indented(inp, n, "BLOCK-IN");
  }
}
});
}

// [185] S-L+BLOCK-INDENTED
static Result s_lblock_indented(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ((Sup)(() -> {
    Result r19_ = detectIndent(inp, 0);
    if (r19_.fail)
      return r19_;
    int m = r19_.tagInt;
    final Input inp19_ = r19_.rest;
    return new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_indent(inp, m);
      }
    },
        new PFn() {
          public Result apply(Input inp) { return alt(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return ns_l_compact_sequence(inp, (n + 1 + m));
          }
        },
        new PFn() {
          public Result apply(Input inp) {
            return ns_l_compact_mapping(inp, (n + 1 + m));
          }
        }}); } }});
}
}
.apply(inp19_);
})).get();
}
}
, new PFn() {
  public Result apply(Input inp) { return s_lblock_node(inp, n, c); }
}, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return e_node(inp);
  }
}, new PFn() {
  public Result apply(Input inp) { return s_l_comments(inp); }
}
});
}
}
});
}

// [186] NS-L-COMPACT-SEQUENCE
static Result ns_l_compact_sequence(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_l_block_seq_entry(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return star(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_indent(inp, n);
      } },
            new PFn() {
      public Result apply(Input inp) { return c_l_block_seq_entry(inp, n); } }
  });
}
});
}
}
});
}

// [187] L+BLOCK-MAPPING
static Result lblock_mapping(Input inp, final int n) {
        return build(inp, "MAPPING", new PFn() {
    public Result apply(Input inp) { return ((Sup)(() -> {
        Result r20_ = detectIndent(inp, n);
        if (r20_.fail)
          return r20_;
        int m = r20_.tagInt;
        final Input inp20_ = r20_.rest;
        return new PFn() {
          public Result apply(Input inp) { return collect(inp, new PFn() {
              public Result apply(Input inp) { return plus_(inp, new PFn() {
                  public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_indent(inp, (n + m));
                  } },
        new PFn() {
                  public Result apply(Input inp) {
                    return ns_l_block_map_entry(inp, (n + m));
                  } }
              }); }
          });
        } });
    } }.apply(inp20_);
})).get();
}
});
}

// [188] NS-L-BLOCK-MAP-ENTRY
static Result ns_l_block_map_entry(Input inp, final int n) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_l_block_map_explicit_entry(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return ns_l_block_map_implicit_entry(inp, n);
  }
}
});
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY
static Result c_l_block_map_explicit_entry(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_l_block_map_explicit_key(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return l_block_map_explicit_value(inp, n);
  }
}, new PFn() {
  public Result apply(Input inp) { return e_node(inp); }
}
});
}
}
});
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY
static Result c_l_block_map_explicit_key(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 63);
}
}
, new PFn() {
  public Result apply(Input inp) {
    return s_lblock_indented(inp, n, "BLOCK-OUT");
  }
}
});
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE
static Result l_block_map_explicit_value(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_indent(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return matchCp(inp, 58); }
}, new PFn() {
  public Result apply(Input inp) {
    return s_lblock_indented(inp, n, "BLOCK-OUT");
  }
}
});
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY
static Result ns_l_block_map_implicit_entry(Input inp, final int n) {
  return build(inp, "PAIR", new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return scalar(inp, new PFn() {
        public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return ns_s_block_map_implicit_key(inp);
        } },
            new PFn() {
        public Result apply(Input inp) { return e_node(inp); } }});
    }
  });
}
}
, new PFn() {
  public Result apply(Input inp) {
    return c_l_block_map_implicit_value(inp, n);
  }
}
});
}
});
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY
static Result ns_s_block_map_implicit_key(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_s_implicit_json_key(inp, "BLOCK-KEY");
}
}
, new PFn() {
  public Result apply(Input inp) {
    return ns_s_implicit_yaml_key(inp, "BLOCK-KEY");
  }
}
});
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE
static Result c_l_block_map_implicit_value(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return matchCp(inp, 58);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_lblock_node(inp, n, "BLOCK-OUT");
  }
}, new PFn() {
  public Result apply(Input inp) { return scalar(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return e_node(inp);
      } },
                new PFn() {
      public Result apply(Input inp) { return s_l_comments(inp); } }
  });
}
});
}
}
});
}
}
});
}

// [195] NS-L-COMPACT-MAPPING
static Result ns_l_compact_mapping(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return ns_l_block_map_entry(inp, n);
}
}
, new PFn() {
  public Result apply(Input inp) { return star(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_indent(inp, n);
      } },
            new PFn() {
      public Result apply(Input inp) { return ns_l_block_map_entry(inp, n); } }
  });
}
});
}
}
});
}

// [196] S-L+BLOCK-NODE
static Result s_lblock_node(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_lblock_in_block(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return s_lflow_in_block(inp, n); }
}
});
}

// [197] S-L+FLOW-IN-BLOCK
static Result s_lflow_in_block(Input inp, final int n) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_separate(inp, (n + 1), "FLOW-OUT");
}
}
, new PFn() {
  public Result apply(Input inp) {
    return ns_flow_node(inp, (n + 1), "FLOW-OUT");
  }
}, new PFn() {
  public Result apply(Input inp) { return s_l_comments(inp); }
}
});
}

// [198] S-L+BLOCK-IN-BLOCK
static Result s_lblock_in_block(Input inp, final int n, final String c) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_lblock_scalar(inp, n, c);
}
}
, new PFn() {
  public Result apply(Input inp) { return s_lblock_collection(inp, n, c); }
}
});
}

// [199] S-L+BLOCK-SCALAR
static Result s_lblock_scalar(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return s_separate(inp, (n + 1), c);
}
}
, new PFn() {
  public Result apply(Input inp) { return opt(inp, new PFn() {
      public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_ns_properties(inp, (n + 1), c);
      } },
            new PFn() {
      public Result apply(Input inp) { return s_separate(inp, (n + 1), c); } }
  });
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_lliteral(inp, n);
  }
}, new PFn() {
  public Result apply(Input inp) { return c_lfolded(inp, n); }
}
});
}
}
});
}

// [200] S-L+BLOCK-COLLECTION
static Result s_lblock_collection(Input inp, final int n, final String c) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return s_separate(inp, (n + 1), c);
    } },
            new PFn() {
    public Result apply(Input inp) { return c_ns_properties(inp, (n + 1), c); } }});
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return s_l_comments(inp); }
}, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return lblock_sequence(inp, seqSpaces(n, c));
  }
}, new PFn() {
  public Result apply(Input inp) { return lblock_mapping(inp, n); }
}
});
}
}
});
}

// [202] L-DOCUMENT-PREFIX
static Result l_document_prefix(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return opt(inp, new PFn() {
    public Result apply(Input inp) { return c_byte_order_mark(inp); } });
}
}
, new PFn() {
  public Result apply(Input inp) {
    return star(inp, new PFn() {
      public Result apply(Input inp) { return l_comment(inp); }
    });
  }
}
});
}

// [203] C-DIRECTIVES-END
static Result c_directives_end(Input inp) { return matchStr(inp, "---"); }

// [204] C-DOCUMENT-END
static Result c_document_end(Input inp) { return matchStr(inp, "..."); }

// [205] L-DOCUMENT-SUFFIX
static Result l_document_suffix(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_document_end(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return s_l_comments(inp); }
}
});
}

// [206] C-FORBIDDEN
static Result c_forbidden(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return sol(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return c_directives_end(inp);
  }
}, new PFn() {
  public Result apply(Input inp) { return c_document_end(inp); }
}
});
}
}
, new PFn() {
  public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return b_char(inp);
  }
}, new PFn() {
  public Result apply(Input inp) { return s_white(inp); }
}, new PFn() {
  public Result apply(Input inp) { return eofOk(inp); }
}
});
}
}
});
}

// [207] L-BARE-DOCUMENT
static Result l_bare_document(Input inp) {
  return build(inp, "DOC", new PFn() {
    public Result apply(Input inp) {
      return s_lblock_node(inp, -1, "BLOCK-IN");
    }
  });
}

// [208] L-EXPLICIT-DOCUMENT
static Result l_explicit_document(Input inp) {
        return build(inp, "DOC", new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return c_directives_end(inp);
    } },
        new PFn() {
    public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return l_bare_document(inp);
    } },
            new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return e_node(inp);
    } },
                new PFn() {
    public Result apply(Input inp) { return s_l_comments(inp); } }
});
}
}
});
}
}
});
}
});
}

// [209] L-DIRECTIVE-DOCUMENT
static Result l_directive_document(Input inp) {
        return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return plus_(inp, new PFn() {
    public Result apply(Input inp) { return l_directive(inp); } });
}
}
, new PFn() {
  public Result apply(Input inp) { return l_explicit_document(inp); }
}
});
}

// [210] L-ANY-DOCUMENT
static Result l_any_document(Input inp) {
        return alt(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return l_directive_document(inp);
}
}
, new PFn() {
  public Result apply(Input inp) { return l_explicit_document(inp); }
}, new PFn() {
  public Result apply(Input inp) { return l_bare_document(inp); }
}
});
}

// [211] L-YAML-STREAM
static Result l_yaml_stream(Input inp) {
        return build(inp, "STREAM", new PFn() {
    public Result apply(Input inp) { return seq(inp, new PFn[]{
        new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
        public Result apply(Input inp) { return l_document_prefix(inp); } });
    } },
        new PFn() {
    public Result apply(Input inp) {
      return opt(inp, new PFn() {
        public Result apply(Input inp) { return l_any_document(inp); }
      });
    } },
        new PFn() {
    public Result apply(Input inp) { return star(inp, new PFn() {
        public Result apply(Input inp) { return alt(inp, new PFn[]{
            new PFn() { public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return plus_(inp, new PFn() {
            public Result apply(Input inp) { return l_document_suffix(inp); } }); } },
                new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
            public Result apply(Input inp) { return l_document_prefix(inp); } });
        } },
                new PFn() {
        public Result apply(Input inp) {
          return opt(inp, new PFn() {
            public Result apply(Input inp) { return l_any_document(inp); }
          });
        } }
    }); }
}
, new PFn() {
  public Result apply(Input inp) { return seq(inp, new PFn[]{
                new PFn() { public Result apply(Input inp) { return star(inp, new PFn() {
      public Result apply(Input inp) { return l_document_prefix(inp); } });
  }
}, new PFn() {
  public Result apply(Input inp) {
    return opt(inp, new PFn() {
      public Result apply(Input inp) { return l_explicit_document(inp); }
    });
  }
}
});
}
}
});
}
});
}
}
});
}
});
}

static void printAst(Ast node, int depth) {
  String indent = "  ".repeat(depth);
  if (node.isLeaf)
    System.out.println(indent + "SCALAR: \"" + node.text + "\"");
  else {
    System.out.println(indent + node.tag);
    if (node.children != null)
      for (Ast c : node.children)
        printAst(c, depth + 1);
  }
}

public static void main(String[] args) throws Exception {
  String text;
  if (args.length > 0)
    text = new String(Files.readAllBytes(Paths.get(args[0])));
  else {
    text = new String(System.in.readAllBytes());
  }
  Input inp = Input.of(text);
  Result r = l_yaml_stream(inp);
  if (!r.fail) {
    System.out.println("OK: " + r.rest.pos + " chars");
    if (r.ast != null)
      printAst(r.ast, 0);
  } else {
    System.err.println("FAIL @" + r.rest.pos + ": " + r.err);
    System.exit(1);
  }
}
}
