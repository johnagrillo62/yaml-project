%% ════════════════════════════════════════════════════════════════
%% peg_yaml.erl — YAML 1.2 parser
%% ════════════════════════════════════════════════════════════════
-module(peg_yaml).
-export([main/1, parse/1]).


%% ── Input ──
%%
%% Input is {Src, Pos, Line, Col} where Src is a binary.

mk_input(Src) -> {Src, 0, 1, 0}.

at_eof({Src, Pos, _, _}) -> Pos >= byte_size(Src).

peek_cp({Src, Pos, _, _}) ->
    case Pos >= byte_size(Src) of
        true -> -1;
        false -> binary:at(Src, Pos)
    end.

adv({Src, Pos, Line, Col}) ->
    case Pos >= byte_size(Src) of
        true -> {Src, Pos, Line, Col};
        false ->
            C = binary:at(Src, Pos),
            case C of
                $\n -> {Src, Pos + 1, Line + 1, 0};
                _ -> {Src, Pos + 1, Line, Col + 1}
            end
    end.

%% ── Result ──
%%
%% Result is {Failed, Rval, Ast, Rest, Tag, TagInt, AstList, Err}

ok_r(Inp) -> {false, <<>>, none, Inp, <<>>, 0, [], <<>>}.
ok_v(Inp, V) -> {false, V, none, Inp, <<>>, 0, [], <<>>}.
fail_r(Inp, M) -> {true, <<>>, none, Inp, <<>>, 0, [], M}.

r_failed({F, _, _, _, _, _, _, _}) -> F.
r_rval({_, V, _, _, _, _, _, _}) -> V.
r_ast({_, _, A, _, _, _, _, _}) -> A.
r_rest({_, _, _, R, _, _, _, _}) -> R.
r_tag({_, _, _, _, T, _, _, _}) -> T.
r_tag_int({_, _, _, _, _, I, _, _}) -> I.
r_ast_list({_, _, _, _, _, _, L, _}) -> L.
r_set_ast(R, A) -> setelement(3, R, A).
r_set_ast_list(R, L) -> setelement(7, R, L).
r_set_tag(R, T) -> setelement(5, R, T).
r_set_tag_int(R, I) -> setelement(6, R, I).

%% ── Context ──

in_flow(C) ->
    case C of
        <<"FLOW-OUT">> -> <<"FLOW-IN">>;
        <<"FLOW-IN">> -> <<"FLOW-IN">>;
        _ -> <<"FLOW-KEY">>
    end.

seq_spaces(N, C) ->
    case C of
        <<"BLOCK-OUT">> -> N - 1;
        _ -> N
    end.

%% ── Combinators ──

match_cp(Inp, Cp) ->
    case peek_cp(Inp) of
        Cp -> ok_v(adv(Inp), <<Cp>>);
        _ -> fail_r(Inp, <<"cp">>)
    end.

match_range(Inp, Lo, Hi) ->
    C = peek_cp(Inp),
    case C >= Lo andalso C =< Hi of
        true -> ok_v(adv(Inp), <<C>>);
        false -> fail_r(Inp, <<"rng">>)
    end.

match_str(Inp, T) ->
    {_, Pos, _, _} = Inp,
    TLen = byte_size(T),
    {Src, _, _, _} = Inp,
    case Pos + TLen =< byte_size(Src) of
        false -> fail_r(Inp, <<"str">>);
        true ->
            Sub = binary:part(Src, Pos, TLen),
            case Sub =:= T of
                false -> fail_r(Inp, <<"str">>);
                true ->
                    Inp2 = lists:foldl(fun(_, Acc) -> adv(Acc) end, Inp, lists:seq(1, TLen)),
                    ok_v(Inp2, T)
            end
    end.

peg_seq(Inp, []) -> ok_r(Inp);
peg_seq(Inp, Fns) -> peg_seq_go(Fns, Inp, <<>>, []).

peg_seq_go([], Cur, Acc, Asts) ->
    R = ok_v(Cur, Acc),
    case Asts of
        [A] -> r_set_ast(R, A);
        [] -> R;
        _ -> r_set_ast_list(R, Asts)
    end;
peg_seq_go([F|Rest], Cur, Acc, Asts) ->
    R = F(Cur),
    case r_failed(R) of
        true -> R;
        false ->
            NewAsts = merge_asts(Asts, R),
            peg_seq_go(Rest, r_rest(R), <<Acc/binary, (r_rval(R))/binary>>, NewAsts)
    end.

merge_asts(Acc, R) ->
    A = case r_ast(R) of none -> []; X -> [X] end,
    Acc ++ A ++ r_ast_list(R).

peg_alt(Inp, []) -> fail_r(Inp, <<"alt">>);
peg_alt(Inp, [F|Rest]) ->
    R = F(Inp),
    case r_failed(R) of
        true -> peg_alt(Inp, Rest);
        false -> R
    end.

star(Inp, F) -> star_go(F, Inp, <<>>, []).
star_go(F, Cur, Acc, Asts) ->
    R = F(Cur),
    {_, CurPos, _, _} = Cur,
    {_, _, _, Rest, _, _, _, _} = R,
    {_, RestPos, _, _} = Rest,
    case r_failed(R) orelse RestPos =< CurPos of
        true ->
            Res = ok_v(Cur, Acc),
            case Asts of [] -> Res; _ -> r_set_ast_list(Res, Asts) end;
        false ->
            star_go(F, r_rest(R), <<Acc/binary, (r_rval(R))/binary>>, merge_asts(Asts, R))
    end.

plus_(Inp, F) ->
    R = F(Inp),
    case r_failed(R) of
        true -> R;
        false ->
            R2 = star(r_rest(R), F),
            Asts = merge_asts(merge_asts([], R), R2),
            Res = ok_v(r_rest(R2), <<(r_rval(R))/binary, (r_rval(R2))/binary>>),
            case Asts of [] -> Res; _ -> r_set_ast_list(Res, Asts) end
    end.

opt(Inp, F) ->
    R = F(Inp),
    case r_failed(R) of true -> ok_r(Inp); false -> R end.

neg(Inp, F) ->
    R = F(Inp),
    case r_failed(R) of true -> ok_r(Inp); false -> fail_r(Inp, <<"neg">>) end.

minus_fn(Inp, Fa, Fb) ->
    Ra = Fa(Inp),
    case r_failed(Ra) of
        true -> Ra;
        false ->
            Rb = Fb(Inp),
            {_, _, _, RestA, _, _, _, _} = Ra,
            {_, PosA, _, _} = RestA,
            {_, _, _, RestB, _, _, _, _} = Rb,
            {_, PosB, _, _} = RestB,
            case (not r_failed(Rb)) andalso PosB =:= PosA of
                true -> fail_r(Inp, <<"excl">>);
                false -> Ra
            end
    end.

rep_fn(Inp, 0, _F) -> ok_v(Inp, <<>>);
rep_fn(Inp, Count, F) ->
    R = F(Inp),
    case r_failed(R) of
        true -> R;
        false ->
            R2 = rep_fn(r_rest(R), Count - 1, F),
            case r_failed(R2) of
                true -> R2;
                false -> ok_v(r_rest(R2), <<(r_rval(R))/binary, (r_rval(R2))/binary>>)
            end
    end.

ahead(Inp, F) ->
    R = F(Inp),
    case r_failed(R) of
        true -> R;
        false -> Res = ok_r(Inp), {false, r_rval(R), none, Inp, <<>>, 0, [], <<>>}
    end.

behind(Inp, F) ->
    {Src, Pos, Line, Col} = Inp,
    case Pos of
        0 -> fail_r(Inp, <<"behind">>);
        _ ->
            Prev = {Src, Pos - 1, Line, max(0, Col - 1)},
            R = F(Prev),
            case r_failed(R) of
                true -> fail_r(Inp, <<"behind">>);
                false -> ok_r(Inp)
            end
    end.

sol(Inp) ->
    {_, _, _, Col} = Inp,
    case Col of 0 -> ok_r(Inp); _ -> fail_r(Inp, <<"sol">>) end.

eof_ok(Inp) ->
    case at_eof(Inp) of true -> ok_r(Inp); false -> fail_r(Inp, <<"eof">>) end.

%% ── YAML extensions ──

build_ast(Inp, Typ, F) ->
    R = F(Inp),
    case r_failed(R) of
        true -> R;
        false ->
            Children = case r_ast(R) of none -> []; A -> [A] end ++ r_ast_list(R),
            R2 = r_set_ast(R, {branch, Typ, Children}),
            r_set_ast_list(R2, [])
    end.

scalar_fn(Inp, F) ->
    R = F(Inp),
    case r_failed(R) of
        true -> R;
        false -> r_set_ast(R, {leaf, r_rval(R)})
    end.

collect_fn(Inp, F) -> F(Inp).

detect_indent(Inp, N) ->
    {Src, _, _, _} = Inp,
    {_, Pos, _, _} = Inp,
    Len = byte_size(Src),
    Sp = count_spaces(Src, Pos, Len),
    case (Pos + Sp < Len) andalso (binary:at(Src, Pos + Sp) =/= $\n) of
        true -> r_set_tag_int(ok_r(Inp), max(1, Sp - N));
        false -> scan_lines(Src, Len, Pos + Sp, N, Inp)
    end.

count_spaces(Src, I, Len) ->
    case I < Len andalso binary:at(Src, I) =:= $\s of
        true -> 1 + count_spaces(Src, I + 1, Len);
        false -> 0
    end.

scan_lines(Src, Len, J, N, Inp) ->
    case J >= Len of
        true -> r_set_tag_int(ok_r(Inp), 1);
        false ->
            case binary:at(Src, J) of
                $\n ->
                    J1 = J + 1,
                    case J1 >= Len of
                        true -> r_set_tag_int(ok_r(Inp), 1);
                        false ->
                            Sp = count_spaces(Src, J1, Len),
                            Nx = J1 + Sp,
                            case (Nx >= Len) orelse (binary:at(Src, Nx) =:= $\n) of
                                true -> scan_lines(Src, Len, Nx, N, Inp);
                                false -> r_set_tag_int(ok_r(Inp), max(1, Sp - N))
                            end
                    end;
                _ -> r_set_tag_int(ok_r(Inp), 1)
            end
    end.

parse_int_fn(Inp, F) ->
    R = F(Inp),
    case r_failed(R) of
        true -> R;
        false ->
            V = binary_to_integer_safe(r_rval(R)),
            r_set_tag_int(R, V)
    end.

binary_to_integer_safe(B) ->
    Digits = << <<C>> || <<C>> <= B, C >= $0, C =< $9 >>,
    case byte_size(Digits) of
        0 -> 0;
        _ -> binary_to_integer(Digits)
    end.

parse_sym_fn(Inp, F, Sym) ->
    R = F(Inp),
    case r_failed(R) of
        true -> R;
        false -> r_set_tag(R, Sym)
    end.

val_fn(Inp, V) ->
    r_set_tag(ok_r(Inp), V).

% ════════════════════════════════════════════════════════════════ 
% YAML 1.2 Grammar — 211 rules 
% ════════════════════════════════════════════════════════════════ 

% [1] C-PRINTABLE 
c_printable(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> match_cp(Inp, 16#9) end,
        fun(Inp) -> match_cp(Inp, 16#0A) end,
        fun(Inp) -> match_cp(Inp, 16#0D) end,
        fun(Inp) -> match_range(Inp, 16#20, 16#7E) end,
        fun(Inp) -> match_cp(Inp, 16#85) end,
        fun(Inp) -> match_range(Inp, 16#A0, 16#D7FF) end,
        fun(Inp) -> match_range(Inp, 16#E000, 16#FFFD) end,
        fun(Inp) -> match_range(Inp, 16#10000, 16#10FFFF) end]).

% [2] NB-JSON 
nb_json(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> match_cp(Inp, 16#9) end,
        fun(Inp) -> match_range(Inp, 16#20, 16#10FFFF) end]).

% [3] C-BYTE-ORDER-MARK 
c_byte_order_mark(Inp) ->
    match_cp(Inp, 16#FEFF).

% [4] C-SEQUENCE-ENTRY 
c_sequence_entry(Inp) ->
    match_cp(Inp, 45).

% [5] C-MAPPING-KEY 
c_mapping_key(Inp) ->
    match_cp(Inp, 63).

% [6] C-MAPPING-VALUE 
c_mapping_value(Inp) ->
    match_cp(Inp, 58).

% [7] C-COLLECT-ENTRY 
c_collect_entry(Inp) ->
    match_cp(Inp, 44).

% [8] C-SEQUENCE-START 
c_sequence_start(Inp) ->
    match_cp(Inp, 91).

% [9] C-SEQUENCE-END 
c_sequence_end(Inp) ->
    match_cp(Inp, 93).

% [10] C-MAPPING-START 
c_mapping_start(Inp) ->
    match_cp(Inp, 123).

% [11] C-MAPPING-END 
c_mapping_end(Inp) ->
    match_cp(Inp, 125).

% [12] C-COMMENT 
c_comment(Inp) ->
    match_cp(Inp, 35).

% [13] C-ANCHOR 
c_anchor(Inp) ->
    match_cp(Inp, 38).

% [14] C-ALIAS 
c_alias(Inp) ->
    match_cp(Inp, 42).

% [15] C-TAG 
c_tag(Inp) ->
    match_cp(Inp, 33).

% [16] C-LITERAL 
c_literal(Inp) ->
    match_cp(Inp, 124).

% [17] C-FOLDED 
c_folded(Inp) ->
    match_cp(Inp, 62).

% [18] C-SINGLE-QUOTE 
c_single_quote(Inp) ->
    match_cp(Inp, 39).

% [19] C-DOUBLE-QUOTE 
c_double_quote(Inp) ->
    match_cp(Inp, 34).

% [20] C-DIRECTIVE 
c_directive(Inp) ->
    match_cp(Inp, 37).

% [21] C-RESERVED 
c_reserved(Inp) ->
    peg_alt(Inp, [fun(Inp) -> match_cp(Inp, 64) end, fun(Inp) -> match_cp(Inp, 96) end]).

% [22] C-INDICATOR 
c_indicator(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_sequence_entry(Inp) end,
        fun(Inp) -> c_mapping_key(Inp) end,
        fun(Inp) -> c_mapping_value(Inp) end,
        fun(Inp) -> c_collect_entry(Inp) end,
        fun(Inp) -> c_sequence_start(Inp) end,
        fun(Inp) -> c_sequence_end(Inp) end,
        fun(Inp) -> c_mapping_start(Inp) end,
        fun(Inp) -> c_mapping_end(Inp) end,
        fun(Inp) -> c_comment(Inp) end,
        fun(Inp) -> c_anchor(Inp) end,
        fun(Inp) -> c_alias(Inp) end,
        fun(Inp) -> c_tag(Inp) end,
        fun(Inp) -> c_literal(Inp) end,
        fun(Inp) -> c_folded(Inp) end,
        fun(Inp) -> c_single_quote(Inp) end,
        fun(Inp) -> c_double_quote(Inp) end,
        fun(Inp) -> c_directive(Inp) end,
        fun(Inp) -> c_reserved(Inp) end]).

% [23] C-FLOW-INDICATOR 
c_flow_indicator(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_collect_entry(Inp) end,
        fun(Inp) -> c_sequence_start(Inp) end,
        fun(Inp) -> c_sequence_end(Inp) end,
        fun(Inp) -> c_mapping_start(Inp) end,
        fun(Inp) -> c_mapping_end(Inp) end]).

% [24] B-LINE-FEED 
b_line_feed(Inp) ->
    match_cp(Inp, 16#0A).

% [25] B-CARRIAGE-RETURN 
b_carriage_return(Inp) ->
    match_cp(Inp, 16#0D).

% [26] B-CHAR 
b_char(Inp) ->
    peg_alt(Inp, [fun(Inp) -> b_line_feed(Inp) end, fun(Inp) -> b_carriage_return(Inp) end]).

% [27] NB-CHAR 
nb_char(Inp) ->
    minus_fn(Inp, fun(Inp) -> c_printable(Inp) end, fun(Inp) -> peg_alt(Inp, [fun(Inp) -> b_char(Inp) end, fun(Inp) -> c_byte_order_mark(Inp) end]) end).

% [28] B-BREAK 
b_break(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [fun(Inp) -> b_carriage_return(Inp) end, fun(Inp) -> b_line_feed(Inp) end]) end,
        fun(Inp) -> b_carriage_return(Inp) end,
        fun(Inp) -> b_line_feed(Inp) end]).

% [29] B-AS-LINE-FEED 
b_as_line_feed(Inp) ->
    b_break(Inp).

% [30] B-NON-CONTENT 
b_non_content(Inp) ->
    b_break(Inp).

% [31] S-SPACE 
s_space(Inp) ->
    match_cp(Inp, 16#20).

% [32] S-TAB 
s_tab(Inp) ->
    match_cp(Inp, 16#9).

% [33] S-WHITE 
s_white(Inp) ->
    peg_alt(Inp, [fun(Inp) -> s_space(Inp) end, fun(Inp) -> s_tab(Inp) end]).

% [34] NS-CHAR 
ns_char(Inp) ->
    minus_fn(Inp, fun(Inp) -> nb_char(Inp) end, fun(Inp) -> s_white(Inp) end).

% [35] NS-DEC-DIGIT 
ns_dec_digit(Inp) ->
    match_range(Inp, 16#30, 16#39).

% [36] NS-HEX-DIGIT 
ns_hex_digit(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> ns_dec_digit(Inp) end,
        fun(Inp) -> match_range(Inp, 16#41, 16#46) end,
        fun(Inp) -> match_range(Inp, 16#61, 16#66) end]).

% [37] NS-ASCII-LETTER 
ns_ascii_letter(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> match_range(Inp, 16#41, 16#5A) end,
        fun(Inp) -> match_range(Inp, 16#61, 16#7A) end]).

% [38] NS-WORD-CHAR 
ns_word_char(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> ns_dec_digit(Inp) end,
        fun(Inp) -> ns_ascii_letter(Inp) end,
        fun(Inp) -> match_cp(Inp, 45) end]).

% [39] NS-URI-CHAR 
ns_uri_char(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 37) end,
            fun(Inp) -> ns_hex_digit(Inp) end,
            fun(Inp) -> ns_hex_digit(Inp) end]) end,
        fun(Inp) -> ns_word_char(Inp) end,
        fun(Inp) -> match_cp(Inp, 35) end,
        fun(Inp) -> match_cp(Inp, 59) end,
        fun(Inp) -> match_cp(Inp, 47) end,
        fun(Inp) -> match_cp(Inp, 63) end,
        fun(Inp) -> match_cp(Inp, 58) end,
        fun(Inp) -> match_cp(Inp, 64) end,
        fun(Inp) -> match_cp(Inp, 38) end,
        fun(Inp) -> match_cp(Inp, 61) end,
        fun(Inp) -> match_cp(Inp, 43) end,
        fun(Inp) -> match_cp(Inp, 36) end,
        fun(Inp) -> match_cp(Inp, 44) end,
        fun(Inp) -> match_cp(Inp, 95) end,
        fun(Inp) -> match_cp(Inp, 46) end,
        fun(Inp) -> match_cp(Inp, 33) end,
        fun(Inp) -> match_cp(Inp, 126) end,
        fun(Inp) -> match_cp(Inp, 42) end,
        fun(Inp) -> match_cp(Inp, 39) end,
        fun(Inp) -> match_cp(Inp, 40) end,
        fun(Inp) -> match_cp(Inp, 41) end,
        fun(Inp) -> match_cp(Inp, 91) end,
        fun(Inp) -> match_cp(Inp, 93) end]).

% [40] NS-TAG-CHAR 
ns_tag_char(Inp) ->
    minus_fn(Inp, fun(Inp) -> ns_uri_char(Inp) end, fun(Inp) -> peg_alt(Inp, [fun(Inp) -> c_tag(Inp) end, fun(Inp) -> c_flow_indicator(Inp) end]) end).

% [41] C-ESCAPE 
c_escape(Inp) ->
    match_cp(Inp, 92).

% [42] NS-ESC-NULL 
ns_esc_null(Inp) ->
    match_cp(Inp, 48).

% [43] NS-ESC-BELL 
ns_esc_bell(Inp) ->
    match_cp(Inp, 97).

% [44] NS-ESC-BACKSPACE 
ns_esc_backspace(Inp) ->
    match_cp(Inp, 98).

% [45] NS-ESC-HORIZONTAL-TAB 
ns_esc_horizontal_tab(Inp) ->
    match_cp(Inp, 116).

% [46] NS-ESC-LINE-FEED 
ns_esc_line_feed(Inp) ->
    match_cp(Inp, 110).

% [47] NS-ESC-VERTICAL-TAB 
ns_esc_vertical_tab(Inp) ->
    match_cp(Inp, 118).

% [48] NS-ESC-FORM-FEED 
ns_esc_form_feed(Inp) ->
    match_cp(Inp, 102).

% [49] NS-ESC-CARRIAGE-RETURN 
ns_esc_carriage_return(Inp) ->
    match_cp(Inp, 114).

% [50] NS-ESC-ESCAPE 
ns_esc_escape(Inp) ->
    match_cp(Inp, 101).

% [51] NS-ESC-SPACE 
ns_esc_space(Inp) ->
    match_cp(Inp, 16#20).

% [52] NS-ESC-DOUBLE-QUOTE 
ns_esc_double_quote(Inp) ->
    match_cp(Inp, 34).

% [53] NS-ESC-SLASH 
ns_esc_slash(Inp) ->
    match_cp(Inp, 47).

% [54] NS-ESC-BACKSLASH 
ns_esc_backslash(Inp) ->
    match_cp(Inp, 92).

% [55] NS-ESC-NEXT-LINE 
ns_esc_next_line(Inp) ->
    match_cp(Inp, 78).

% [56] NS-ESC-NON-BREAKING-SPACE 
ns_esc_non_breaking_space(Inp) ->
    match_cp(Inp, 95).

% [57] NS-ESC-LINE-SEPARATOR 
ns_esc_line_separator(Inp) ->
    match_cp(Inp, 76).

% [58] NS-ESC-PARAGRAPH-SEPARATOR 
ns_esc_paragraph_separator(Inp) ->
    match_cp(Inp, 80).

% [59] NS-ESC-8-BIT 
ns_esc_8_bit(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 120) end,
        fun(Inp) -> rep_fn(Inp, 2, fun(Inp) -> ns_hex_digit(Inp) end) end]).

% [60] NS-ESC-16-BIT 
ns_esc_16_bit(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 117) end,
        fun(Inp) -> rep_fn(Inp, 4, fun(Inp) -> ns_hex_digit(Inp) end) end]).

% [61] NS-ESC-32-BIT 
ns_esc_32_bit(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 85) end,
        fun(Inp) -> rep_fn(Inp, 8, fun(Inp) -> ns_hex_digit(Inp) end) end]).

% [62] C-NS-ESC-CHAR 
c_ns_esc_char(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> c_escape(Inp) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> ns_esc_null(Inp) end,
            fun(Inp) -> ns_esc_bell(Inp) end,
            fun(Inp) -> ns_esc_backspace(Inp) end,
            fun(Inp) -> ns_esc_horizontal_tab(Inp) end,
            fun(Inp) -> ns_esc_line_feed(Inp) end,
            fun(Inp) -> ns_esc_vertical_tab(Inp) end,
            fun(Inp) -> ns_esc_form_feed(Inp) end,
            fun(Inp) -> ns_esc_carriage_return(Inp) end,
            fun(Inp) -> ns_esc_escape(Inp) end,
            fun(Inp) -> ns_esc_space(Inp) end,
            fun(Inp) -> ns_esc_double_quote(Inp) end,
            fun(Inp) -> ns_esc_slash(Inp) end,
            fun(Inp) -> ns_esc_backslash(Inp) end,
            fun(Inp) -> ns_esc_next_line(Inp) end,
            fun(Inp) -> ns_esc_non_breaking_space(Inp) end,
            fun(Inp) -> ns_esc_line_separator(Inp) end,
            fun(Inp) -> ns_esc_paragraph_separator(Inp) end,
            fun(Inp) -> ns_esc_8_bit(Inp) end,
            fun(Inp) -> ns_esc_16_bit(Inp) end,
            fun(Inp) -> ns_esc_32_bit(Inp) end]) end]).

% [63] S-INDENT 
s_indent(Inp, N) ->
    rep_fn(Inp, N, fun(Inp) -> s_space(Inp) end).

% [64] S-INDENT-LT 
s_indent_lt(Inp, N) ->
    star(Inp, fun(Inp) -> s_space(Inp) end).

% [65] S-INDENT-LE 
s_indent_le(Inp, N) ->
    star(Inp, fun(Inp) -> s_space(Inp) end).

% [66] S-SEPARATE-IN-LINE 
s_separate_in_line(Inp) ->
    peg_alt(Inp, [fun(Inp) -> plus_(Inp, fun(Inp) -> s_white(Inp) end) end, fun(Inp) -> ok_r(Inp) end]).

% [67] S-LINE-PREFIX 
s_line_prefix(Inp, N, C) ->
    case C of <<"BLOCK-IN">> -> s_block_line_prefix(Inp, N); <<"BLOCK-OUT">> -> s_block_line_prefix(Inp, N); <<"FLOW-IN">> -> s_flow_line_prefix(Inp, N); <<"FLOW-OUT">> -> s_flow_line_prefix(Inp, N); _ -> fail_r(Inp, <<"no case">>) end.

% [68] S-BLOCK-LINE-PREFIX 
s_block_line_prefix(Inp, N) ->
    s_indent(Inp, N).

% [69] S-FLOW-LINE-PREFIX 
s_flow_line_prefix(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_indent(Inp, N) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate_in_line(Inp) end) end]).

% [70] L-EMPTY 
l_empty(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> peg_alt(Inp, [fun(Inp) -> s_line_prefix(Inp, N, C) end, fun(Inp) -> s_indent_lt(Inp, N) end]) end,
        fun(Inp) -> b_as_line_feed(Inp) end]).

% [71] B-L-TRIMMED 
b_l_trimmed(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> b_non_content(Inp) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> l_empty(Inp, N, C) end) end]).

% [72] B-AS-SPACE 
b_as_space(Inp) ->
    b_break(Inp).

% [73] B-L-FOLDED 
b_l_folded(Inp, N, C) ->
    peg_alt(Inp, [fun(Inp) -> b_l_trimmed(Inp, N, C) end, fun(Inp) -> b_as_space(Inp) end]).

% [74] S-FLOW-FOLDED 
s_flow_folded(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate_in_line(Inp) end) end,
        fun(Inp) -> b_l_folded(Inp, N, <<"FLOW-IN">>) end,
        fun(Inp) -> s_flow_line_prefix(Inp, N) end]).

% [75] C-NB-COMMENT-TEXT 
c_nb_comment_text(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> c_comment(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> nb_char(Inp) end) end]).

% [76] B-COMMENT 
b_comment(Inp) ->
    peg_alt(Inp, [fun(Inp) -> b_non_content(Inp) end, fun(Inp) -> ok_r(Inp) end]).

% [77] S-B-COMMENT 
s_b_comment(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> s_separate_in_line(Inp) end,
            fun(Inp) -> opt(Inp, fun(Inp) -> c_nb_comment_text(Inp) end) end]) end) end,
        fun(Inp) -> b_comment(Inp) end]).

% [78] L-COMMENT 
l_comment(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> s_separate_in_line(Inp) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> c_nb_comment_text(Inp) end) end,
        fun(Inp) -> b_non_content(Inp) end]).

% [79] S-L-COMMENTS 
s_l_comments(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> peg_alt(Inp, [fun(Inp) -> s_b_comment(Inp) end, fun(Inp) -> ok_r(Inp) end]) end,
        fun(Inp) -> star(Inp, fun(Inp) -> l_comment(Inp) end) end]).

% [80] S-SEPARATE 
s_separate(Inp, N, C) ->
    case C of <<"BLOCK-OUT">> -> s_separate_lines(Inp, N); <<"BLOCK-IN">> -> s_separate_lines(Inp, N); <<"FLOW-OUT">> -> s_separate_lines(Inp, N); <<"FLOW-IN">> -> s_separate_lines(Inp, N); <<"BLOCK-KEY">> -> s_separate_in_line(Inp); <<"FLOW-KEY">> -> s_separate_in_line(Inp); _ -> fail_r(Inp, <<"no case">>) end.

% [81] S-SEPARATE-LINES 
s_separate_lines(Inp, N) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_l_comments(Inp) end, fun(Inp) -> s_flow_line_prefix(Inp, N) end]) end,
        fun(Inp) -> s_separate_in_line(Inp) end]).

% [82] L-DIRECTIVE 
l_directive(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> c_directive(Inp) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> ns_yaml_directive(Inp) end,
            fun(Inp) -> ns_tag_directive(Inp) end,
            fun(Inp) -> ns_reserved_directive(Inp) end]) end,
        fun(Inp) -> s_l_comments(Inp) end]).

% [83] NS-RESERVED-DIRECTIVE 
ns_reserved_directive(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_directive_name(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_separate_in_line(Inp) end, fun(Inp) -> ns_directive_parameter(Inp) end]) end) end]).

% [84] NS-DIRECTIVE-NAME 
ns_directive_name(Inp) ->
    plus_(Inp, fun(Inp) -> ns_char(Inp) end).

% [85] NS-DIRECTIVE-PARAMETER 
ns_directive_parameter(Inp) ->
    plus_(Inp, fun(Inp) -> ns_char(Inp) end).

% [86] NS-YAML-DIRECTIVE 
ns_yaml_directive(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_str(Inp, <<"YAML">>) end,
        fun(Inp) -> s_separate_in_line(Inp) end,
        fun(Inp) -> ns_yaml_version(Inp) end]).

% [87] NS-YAML-VERSION 
ns_yaml_version(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> plus_(Inp, fun(Inp) -> ns_dec_digit(Inp) end) end,
        fun(Inp) -> match_cp(Inp, 46) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> ns_dec_digit(Inp) end) end]).

% [88] NS-TAG-DIRECTIVE 
ns_tag_directive(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_str(Inp, <<"TAG">>) end,
        fun(Inp) -> s_separate_in_line(Inp) end,
        fun(Inp) -> c_tag_handle(Inp) end,
        fun(Inp) -> s_separate_in_line(Inp) end,
        fun(Inp) -> ns_tag_prefix(Inp) end]).

% [89] C-TAG-HANDLE 
c_tag_handle(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_named_tag_handle(Inp) end,
        fun(Inp) -> c_secondary_tag_handle(Inp) end,
        fun(Inp) -> c_primary_tag_handle(Inp) end]).

% [90] C-PRIMARY-TAG-HANDLE 
c_primary_tag_handle(Inp) ->
    match_cp(Inp, 33).

% [91] C-SECONDARY-TAG-HANDLE 
c_secondary_tag_handle(Inp) ->
    match_str(Inp, <<"!!">>).

% [92] C-NAMED-TAG-HANDLE 
c_named_tag_handle(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 33) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> ns_word_char(Inp) end) end,
        fun(Inp) -> match_cp(Inp, 33) end]).

% [93] NS-TAG-PREFIX 
ns_tag_prefix(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_ns_local_tag_prefix(Inp) end,
        fun(Inp) -> ns_global_tag_prefix(Inp) end]).

% [94] C-NS-LOCAL-TAG-PREFIX 
c_ns_local_tag_prefix(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 33) end,
        fun(Inp) -> star(Inp, fun(Inp) -> ns_uri_char(Inp) end) end]).

% [95] NS-GLOBAL-TAG-PREFIX 
ns_global_tag_prefix(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_tag_char(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> ns_uri_char(Inp) end) end]).

% [96] C-NS-PROPERTIES 
c_ns_properties(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> c_ns_tag_property(Inp) end,
            fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_separate(Inp, N, C) end, fun(Inp) -> c_ns_anchor_property(Inp) end]) end) end]) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> c_ns_anchor_property(Inp) end,
            fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_separate(Inp, N, C) end, fun(Inp) -> c_ns_tag_property(Inp) end]) end) end]) end]).

% [97] C-NS-TAG-PROPERTY 
c_ns_tag_property(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_verbatim_tag(Inp) end,
        fun(Inp) -> c_ns_shorthand_tag(Inp) end,
        fun(Inp) -> c_non_specific_tag(Inp) end]).

% [98] C-VERBATIM-TAG 
c_verbatim_tag(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_str(Inp, <<"!<">>) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> ns_uri_char(Inp) end) end,
        fun(Inp) -> match_cp(Inp, 62) end]).

% [99] C-NS-SHORTHAND-TAG 
c_ns_shorthand_tag(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> c_tag_handle(Inp) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> ns_tag_char(Inp) end) end]).

% [100] C-NON-SPECIFIC-TAG 
c_non_specific_tag(Inp) ->
    match_cp(Inp, 33).

% [101] C-NS-ANCHOR-PROPERTY 
c_ns_anchor_property(Inp) ->
    build_ast(Inp, <<"ANCHOR">>, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> c_anchor(Inp) end,
        fun(Inp) -> scalar_fn(Inp, fun(Inp) -> ns_anchor_name(Inp) end) end]) end).

% [102] NS-ANCHOR-CHAR 
ns_anchor_char(Inp) ->
    minus_fn(Inp, fun(Inp) -> ns_char(Inp) end, fun(Inp) -> c_flow_indicator(Inp) end).

% [103] NS-ANCHOR-NAME 
ns_anchor_name(Inp) ->
    plus_(Inp, fun(Inp) -> ns_anchor_char(Inp) end).

% [104] C-NS-ALIAS-NODE 
c_ns_alias_node(Inp) ->
    build_ast(Inp, <<"ALIAS">>, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> c_alias(Inp) end,
        fun(Inp) -> scalar_fn(Inp, fun(Inp) -> ns_anchor_name(Inp) end) end]) end).

% [105] E-SCALAR 
e_scalar(Inp) ->
    ok_r(Inp).

% [106] E-NODE 
e_node(Inp) ->
    e_scalar(Inp).

% [107] NB-DOUBLE-CHAR 
nb_double_char(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_ns_esc_char(Inp) end,
        fun(Inp) -> minus_fn(Inp, fun(Inp) -> nb_json(Inp) end, fun(Inp) -> peg_alt(Inp, [fun(Inp) -> match_cp(Inp, 92) end, fun(Inp) -> match_cp(Inp, 34) end]) end) end]).

% [108] NS-DOUBLE-CHAR 
ns_double_char(Inp) ->
    minus_fn(Inp, fun(Inp) -> nb_double_char(Inp) end, fun(Inp) -> s_white(Inp) end).

% [109] C-DOUBLE-QUOTED 
c_double_quoted(Inp, N, C) ->
    scalar_fn(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 34) end,
        fun(Inp) -> nb_double_text(Inp, N, C) end,
        fun(Inp) -> match_cp(Inp, 34) end]) end).

% [110] NB-DOUBLE-TEXT 
nb_double_text(Inp, N, C) ->
    case C of <<"FLOW-OUT">> -> nb_double_multi_line(Inp, N); <<"FLOW-IN">> -> nb_double_multi_line(Inp, N); <<"BLOCK-KEY">> -> nb_double_one_line(Inp); <<"FLOW-KEY">> -> nb_double_one_line(Inp); _ -> fail_r(Inp, <<"no case">>) end.

% [111] NB-DOUBLE-ONE-LINE 
nb_double_one_line(Inp) ->
    star(Inp, fun(Inp) -> nb_double_char(Inp) end).

% [112] S-DOUBLE-ESCAPED 
s_double_escaped(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end,
        fun(Inp) -> match_cp(Inp, 92) end,
        fun(Inp) -> b_non_content(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> l_empty(Inp, N, <<"FLOW-IN">>) end) end,
        fun(Inp) -> s_flow_line_prefix(Inp, N) end]).

% [113] S-DOUBLE-BREAK 
s_double_break(Inp, N) ->
    peg_alt(Inp, [fun(Inp) -> s_double_escaped(Inp, N) end, fun(Inp) -> s_flow_folded(Inp, N) end]).

% [114] NB-NS-DOUBLE-IN-LINE 
nb_ns_double_in_line(Inp) ->
    star(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end,
        fun(Inp) -> ns_double_char(Inp) end]) end).

% [115] S-DOUBLE-NEXT-LINE 
s_double_next_line(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_double_break(Inp, N) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> ns_double_char(Inp) end,
            fun(Inp) -> nb_ns_double_in_line(Inp) end,
            fun(Inp) -> peg_alt(Inp, [
                fun(Inp) -> s_double_next_line(Inp, N) end,
                fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end]) end]) end) end]).

% [116] NB-DOUBLE-MULTI-LINE 
nb_double_multi_line(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> nb_ns_double_in_line(Inp) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> s_double_next_line(Inp, N) end,
            fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end]) end]).

% [117] C-QUOTED-QUOTE 
c_quoted_quote(Inp) ->
    match_str(Inp, <<"''">>).

% [118] NB-SINGLE-CHAR 
nb_single_char(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_quoted_quote(Inp) end,
        fun(Inp) -> minus_fn(Inp, fun(Inp) -> nb_json(Inp) end, fun(Inp) -> match_cp(Inp, 39) end) end]).

% [119] NS-SINGLE-CHAR 
ns_single_char(Inp) ->
    minus_fn(Inp, fun(Inp) -> nb_single_char(Inp) end, fun(Inp) -> s_white(Inp) end).

% [120] C-SINGLE-QUOTED 
c_single_quoted(Inp, N, C) ->
    scalar_fn(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 39) end,
        fun(Inp) -> nb_single_text(Inp, N, C) end,
        fun(Inp) -> match_cp(Inp, 39) end]) end).

% [121] NB-SINGLE-TEXT 
nb_single_text(Inp, N, C) ->
    case C of <<"FLOW-OUT">> -> nb_single_multi_line(Inp, N); <<"FLOW-IN">> -> nb_single_multi_line(Inp, N); <<"BLOCK-KEY">> -> nb_single_one_line(Inp); <<"FLOW-KEY">> -> nb_single_one_line(Inp); _ -> fail_r(Inp, <<"no case">>) end.

% [122] NB-SINGLE-ONE-LINE 
nb_single_one_line(Inp) ->
    star(Inp, fun(Inp) -> nb_single_char(Inp) end).

% [123] NS-SINGLE-IN-LINE 
ns_single_in_line(Inp) ->
    star(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end,
        fun(Inp) -> ns_single_char(Inp) end]) end).

% [124] S-SINGLE-NEXT-LINE 
s_single_next_line(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_flow_folded(Inp, N) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> ns_single_char(Inp) end,
            fun(Inp) -> ns_single_in_line(Inp) end,
            fun(Inp) -> peg_alt(Inp, [
                fun(Inp) -> s_single_next_line(Inp, N) end,
                fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end]) end]) end) end]).

% [125] NB-SINGLE-MULTI-LINE 
nb_single_multi_line(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_single_in_line(Inp) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> s_single_next_line(Inp, N) end,
            fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end]) end]).

% [126] NS-PLAIN-FIRST 
ns_plain_first(Inp, C) ->
    peg_alt(Inp, [
        fun(Inp) -> minus_fn(Inp, fun(Inp) -> ns_char(Inp) end, fun(Inp) -> c_indicator(Inp) end) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> peg_alt(Inp, [
                fun(Inp) -> match_cp(Inp, 63) end,
                fun(Inp) -> match_cp(Inp, 58) end,
                fun(Inp) -> match_cp(Inp, 45) end]) end,
            fun(Inp) -> ahead(Inp, fun(Inp) -> ns_plain_safe(Inp, C) end) end]) end]).

% [127] NS-PLAIN-SAFE 
ns_plain_safe(Inp, C) ->
    case C of <<"FLOW-OUT">> -> ns_plain_safe_out(Inp); <<"FLOW-IN">> -> ns_plain_safe_in(Inp); <<"BLOCK-KEY">> -> ns_plain_safe_out(Inp); <<"FLOW-KEY">> -> ns_plain_safe_in(Inp); _ -> fail_r(Inp, <<"no case">>) end.

% [128] NS-PLAIN-SAFE-OUT 
ns_plain_safe_out(Inp) ->
    ns_char(Inp).

% [129] NS-PLAIN-SAFE-IN 
ns_plain_safe_in(Inp) ->
    minus_fn(Inp, fun(Inp) -> ns_char(Inp) end, fun(Inp) -> c_flow_indicator(Inp) end).

% [130] NS-PLAIN-CHAR 
ns_plain_char(Inp, C) ->
    peg_alt(Inp, [
        fun(Inp) -> minus_fn(Inp, fun(Inp) -> ns_plain_safe(Inp, C) end, fun(Inp) -> peg_alt(Inp, [fun(Inp) -> match_cp(Inp, 58) end, fun(Inp) -> match_cp(Inp, 35) end]) end) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> behind(Inp, fun(Inp) -> ns_char(Inp) end) end,
            fun(Inp) -> match_cp(Inp, 35) end]) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 58) end,
            fun(Inp) -> ahead(Inp, fun(Inp) -> ns_plain_safe(Inp, C) end) end]) end]).

% [131] NS-PLAIN 
ns_plain(Inp, N, C) ->
    scalar_fn(Inp, fun(Inp) -> case C of <<"FLOW-OUT">> -> ns_plain_multi_line(Inp, N, C); <<"FLOW-IN">> -> ns_plain_multi_line(Inp, N, C); <<"BLOCK-KEY">> -> ns_plain_one_line(Inp, C); <<"FLOW-KEY">> -> ns_plain_one_line(Inp, C); _ -> fail_r(Inp, <<"no case">>) end end).

% [132] NB-NS-PLAIN-IN-LINE 
nb_ns_plain_in_line(Inp, C) ->
    star(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> s_white(Inp) end) end,
        fun(Inp) -> ns_plain_char(Inp, C) end]) end).

% [133] NS-PLAIN-ONE-LINE 
ns_plain_one_line(Inp, C) ->
    peg_seq(Inp, [fun(Inp) -> ns_plain_first(Inp, C) end, fun(Inp) -> nb_ns_plain_in_line(Inp, C) end]).

% [134] S-NS-PLAIN-NEXT-LINE 
s_ns_plain_next_line(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> s_flow_folded(Inp, N) end,
        fun(Inp) -> neg(Inp, fun(Inp) -> c_forbidden(Inp) end) end,
        fun(Inp) -> ns_plain_char(Inp, C) end,
        fun(Inp) -> nb_ns_plain_in_line(Inp, C) end]).

% [135] NS-PLAIN-MULTI-LINE 
ns_plain_multi_line(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_plain_one_line(Inp, C) end,
        fun(Inp) -> star(Inp, fun(Inp) -> s_ns_plain_next_line(Inp, N, C) end) end]).

% [137] C-FLOW-SEQUENCE 
c_flow_sequence(Inp, N, C) ->
    build_ast(Inp, <<"SEQUENCE">>, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 91) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> collect_fn(Inp, fun(Inp) -> ns_s_flow_seq_entries(Inp, N, in_flow(C)) end) end) end,
        fun(Inp) -> match_cp(Inp, 93) end]) end).

% [138] NS-S-FLOW-SEQ-ENTRIES 
ns_s_flow_seq_entries(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_flow_seq_entry(Inp, N, C) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 44) end,
            fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
            fun(Inp) -> opt(Inp, fun(Inp) -> ns_s_flow_seq_entries(Inp, N, C) end) end]) end) end]).

% [139] NS-FLOW-SEQ-ENTRY 
ns_flow_seq_entry(Inp, N, C) ->
    peg_alt(Inp, [fun(Inp) -> ns_flow_pair(Inp, N, C) end, fun(Inp) -> ns_flow_node(Inp, N, C) end]).

% [140] C-FLOW-MAPPING 
c_flow_mapping(Inp, N, C) ->
    build_ast(Inp, <<"MAPPING">>, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 123) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> collect_fn(Inp, fun(Inp) -> ns_s_flow_map_entries(Inp, N, in_flow(C)) end) end) end,
        fun(Inp) -> match_cp(Inp, 125) end]) end).

% [141] NS-S-FLOW-MAP-ENTRIES 
ns_s_flow_map_entries(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_flow_map_entry(Inp, N, C) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 44) end,
            fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
            fun(Inp) -> opt(Inp, fun(Inp) -> ns_s_flow_map_entries(Inp, N, C) end) end]) end) end]).

% [142] NS-FLOW-MAP-ENTRY 
ns_flow_map_entry(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 63) end,
            fun(Inp) -> s_separate(Inp, N, C) end,
            fun(Inp) -> ns_flow_map_explicit_entry(Inp, N, C) end]) end,
        fun(Inp) -> ns_flow_map_implicit_entry(Inp, N, C) end]).

% [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
ns_flow_map_explicit_entry(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> ns_flow_map_implicit_entry(Inp, N, C) end,
        fun(Inp) -> peg_seq(Inp, [fun(Inp) -> e_node(Inp) end, fun(Inp) -> e_node(Inp) end]) end]).

% [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
ns_flow_map_implicit_entry(Inp, N, C) ->
    build_ast(Inp, <<"PAIR">>, fun(Inp) -> peg_alt(Inp, [
        fun(Inp) -> ns_flow_map_yaml_key_entry(Inp, N, C) end,
        fun(Inp) -> c_ns_flow_map_empty_key_entry(Inp, N, C) end,
        fun(Inp) -> c_ns_flow_map_json_key_entry(Inp, N, C) end]) end).

% [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
ns_flow_map_yaml_key_entry(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_flow_yaml_node(Inp, N, C) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> peg_seq(Inp, [
                fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
                fun(Inp) -> c_ns_flow_map_separate_value(Inp, N, C) end]) end,
            fun(Inp) -> e_node(Inp) end]) end]).

% [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
c_ns_flow_map_empty_key_entry(Inp, N, C) ->
    peg_seq(Inp, [fun(Inp) -> e_node(Inp) end, fun(Inp) -> c_ns_flow_map_separate_value(Inp, N, C) end]).

% [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
c_ns_flow_map_separate_value(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 58) end,
        fun(Inp) -> neg(Inp, fun(Inp) -> ns_plain_safe(Inp, C) end) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_separate(Inp, N, C) end, fun(Inp) -> ns_flow_node(Inp, N, C) end]) end,
            fun(Inp) -> e_node(Inp) end]) end]).

% [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
c_ns_flow_map_json_key_entry(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> c_flow_json_node(Inp, N, C) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> peg_seq(Inp, [
                fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
                fun(Inp) -> c_ns_flow_map_adjacent_value(Inp, N, C) end]) end,
            fun(Inp) -> e_node(Inp) end]) end]).

% [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
c_ns_flow_map_adjacent_value(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 58) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> peg_seq(Inp, [
                fun(Inp) -> opt(Inp, fun(Inp) -> s_separate(Inp, N, C) end) end,
                fun(Inp) -> ns_flow_node(Inp, N, C) end]) end,
            fun(Inp) -> e_node(Inp) end]) end]).

% [150] NS-FLOW-PAIR 
ns_flow_pair(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 63) end,
            fun(Inp) -> s_separate(Inp, N, C) end,
            fun(Inp) -> ns_flow_map_explicit_entry(Inp, N, C) end]) end,
        fun(Inp) -> ns_flow_pair_entry(Inp, N, C) end]).

% [151] NS-FLOW-PAIR-ENTRY 
ns_flow_pair_entry(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> ns_flow_pair_yaml_key_entry(Inp, N, C) end,
        fun(Inp) -> c_ns_flow_map_empty_key_entry(Inp, N, C) end,
        fun(Inp) -> c_ns_flow_pair_json_key_entry(Inp, N, C) end]).

% [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
ns_flow_pair_yaml_key_entry(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_s_implicit_yaml_key(Inp, <<"FLOW-KEY">>) end,
        fun(Inp) -> c_ns_flow_map_separate_value(Inp, N, C) end]).

% [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
c_ns_flow_pair_json_key_entry(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> c_s_implicit_json_key(Inp, <<"FLOW-KEY">>) end,
        fun(Inp) -> c_ns_flow_map_adjacent_value(Inp, N, C) end]).

% [154] NS-S-IMPLICIT-YAML-KEY 
ns_s_implicit_yaml_key(Inp, C) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_flow_yaml_node(Inp, 0, C) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate_in_line(Inp) end) end]).

% [155] C-S-IMPLICIT-JSON-KEY 
c_s_implicit_json_key(Inp, C) ->
    peg_seq(Inp, [
        fun(Inp) -> c_flow_json_node(Inp, 0, C) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> s_separate_in_line(Inp) end) end]).

% [156] NS-FLOW-YAML-CONTENT 
ns_flow_yaml_content(Inp, N, C) ->
    ns_plain(Inp, N, C).

% [157] C-FLOW-JSON-CONTENT 
c_flow_json_content(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> c_flow_sequence(Inp, N, C) end,
        fun(Inp) -> c_flow_mapping(Inp, N, C) end,
        fun(Inp) -> c_single_quoted(Inp, N, C) end,
        fun(Inp) -> c_double_quoted(Inp, N, C) end]).

% [158] NS-FLOW-CONTENT 
ns_flow_content(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> ns_flow_yaml_content(Inp, N, C) end,
        fun(Inp) -> c_flow_json_content(Inp, N, C) end]).

% [159] NS-FLOW-YAML-NODE 
ns_flow_yaml_node(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> c_ns_alias_node(Inp) end,
        fun(Inp) -> ns_flow_yaml_content(Inp, N, C) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> c_ns_properties(Inp, N, C) end,
            fun(Inp) -> peg_alt(Inp, [
                fun(Inp) -> peg_seq(Inp, [
                    fun(Inp) -> s_separate(Inp, N, C) end,
                    fun(Inp) -> ns_flow_yaml_content(Inp, N, C) end]) end,
                fun(Inp) -> e_scalar(Inp) end]) end]) end]).

% [160] C-FLOW-JSON-NODE 
c_flow_json_node(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> c_ns_properties(Inp, N, C) end, fun(Inp) -> s_separate(Inp, N, C) end]) end) end,
        fun(Inp) -> c_flow_json_content(Inp, N, C) end]).

% [161] NS-FLOW-NODE 
ns_flow_node(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> c_ns_alias_node(Inp) end,
        fun(Inp) -> ns_flow_content(Inp, N, C) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> c_ns_properties(Inp, N, C) end,
            fun(Inp) -> peg_alt(Inp, [
                fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_separate(Inp, N, C) end, fun(Inp) -> ns_flow_content(Inp, N, C) end]) end,
                fun(Inp) -> e_scalar(Inp) end]) end]) end]).

% [162] C-B-BLOCK-HEADER 
c_b_block_header(Inp, N) ->
    peg_alt(Inp, [
        fun(Inp) -> (fun() -> RL6 = peg_alt(Inp, [
            fun(Inp) -> parse_int_fn(Inp, fun(Inp) -> ns_dec_digit(Inp) end) end,
            fun(Inp) -> detect_indent(Inp, N) end]), case r_failed(RL6) of true -> RL6; false -> M = r_tag_int(RL6), (fun(Inp) -> (fun() -> RL5 = peg_alt(Inp, [
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 45) end, <<"STRIP">>) end,
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 43) end, <<"KEEP">>) end,
            fun(Inp) -> val_fn(Inp, <<"CLIP">>) end]), case r_failed(RL5) of true -> RL5; false -> T = r_tag(RL5), (fun(Inp) -> s_b_comment(Inp) end)(r_rest(RL5)) end end)() end)(r_rest(RL6)) end end)() end,
        fun(Inp) -> (fun() -> RL8 = peg_alt(Inp, [
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 45) end, <<"STRIP">>) end,
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 43) end, <<"KEEP">>) end,
            fun(Inp) -> val_fn(Inp, <<"CLIP">>) end]), case r_failed(RL8) of true -> RL8; false -> T = r_tag(RL8), (fun(Inp) -> (fun() -> RL7 = peg_alt(Inp, [
            fun(Inp) -> parse_int_fn(Inp, fun(Inp) -> ns_dec_digit(Inp) end) end,
            fun(Inp) -> detect_indent(Inp, N) end]), case r_failed(RL7) of true -> RL7; false -> M = r_tag_int(RL7), (fun(Inp) -> s_b_comment(Inp) end)(r_rest(RL7)) end end)() end)(r_rest(RL8)) end end)() end]).

% [163] C-INDENTATION-INDICATOR 
c_indentation_indicator(Inp, N) ->
    peg_alt(Inp, [fun(Inp) -> ns_dec_digit(Inp) end, fun(Inp) -> ok_r(Inp) end]).

% [164] C-CHOMPING-INDICATOR 
c_chomping_indicator(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> match_cp(Inp, 45) end,
        fun(Inp) -> match_cp(Inp, 43) end,
        fun(Inp) -> ok_r(Inp) end]).

% [165] B-CHOMPED-LAST 
b_chomped_last(Inp, T) ->
    case T of <<"STRIP">> -> b_non_content(Inp); <<"CLIP">> -> b_as_line_feed(Inp); <<"KEEP">> -> b_as_line_feed(Inp); _ -> fail_r(Inp, <<"no case">>) end.

% [166] L-CHOMPED-EMPTY 
l_chomped_empty(Inp, N, T) ->
    case T of <<"STRIP">> -> l_strip_empty(Inp, N); <<"CLIP">> -> l_strip_empty(Inp, N); <<"KEEP">> -> l_keep_empty(Inp, N); _ -> fail_r(Inp, <<"no case">>) end.

% [167] L-STRIP-EMPTY 
l_strip_empty(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_indent_le(Inp, N) end, fun(Inp) -> b_non_content(Inp) end]) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> l_trail_comments(Inp, N) end) end]).

% [168] L-KEEP-EMPTY 
l_keep_empty(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> l_empty(Inp, N, <<"BLOCK-IN">>) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> l_trail_comments(Inp, N) end) end]).

% [169] L-TRAIL-COMMENTS 
l_trail_comments(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_indent_lt(Inp, N) end,
        fun(Inp) -> c_nb_comment_text(Inp) end,
        fun(Inp) -> b_comment(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> l_comment(Inp) end) end]).

% [170] C-L+LITERAL 
c_lliteral(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 124) end,
        fun(Inp) -> (fun() -> RL12 = peg_alt(Inp, [
            fun(Inp) -> parse_int_fn(Inp, fun(Inp) -> ns_dec_digit(Inp) end) end,
            fun(Inp) -> detect_indent(Inp, N) end]), case r_failed(RL12) of true -> RL12; false -> M = r_tag_int(RL12), (fun(Inp) -> (fun() -> RL11 = peg_alt(Inp, [
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 45) end, <<"STRIP">>) end,
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 43) end, <<"KEEP">>) end,
            fun(Inp) -> val_fn(Inp, <<"CLIP">>) end]), case r_failed(RL11) of true -> RL11; false -> T = r_tag(RL11), (fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_b_comment(Inp) end, fun(Inp) -> l_literal_content(Inp, (N + M), T) end]) end)(r_rest(RL11)) end end)() end)(r_rest(RL12)) end end)() end]).

% [171] L-NB-LITERAL-TEXT 
l_nb_literal_text(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> l_empty(Inp, N, <<"BLOCK-IN">>) end) end,
        fun(Inp) -> s_indent(Inp, N) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> nb_char(Inp) end) end]).

% [172] B-NB-LITERAL-NEXT 
b_nb_literal_next(Inp, N) ->
    peg_seq(Inp, [fun(Inp) -> b_as_line_feed(Inp) end, fun(Inp) -> l_nb_literal_text(Inp, N) end]).

% [173] L-LITERAL-CONTENT 
l_literal_content(Inp, N, T) ->
    scalar_fn(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> l_nb_literal_text(Inp, N) end,
            fun(Inp) -> star(Inp, fun(Inp) -> b_nb_literal_next(Inp, N) end) end,
            fun(Inp) -> b_chomped_last(Inp, T) end]) end) end,
        fun(Inp) -> l_chomped_empty(Inp, N, T) end]) end).

% [174] C-L+FOLDED 
c_lfolded(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 62) end,
        fun(Inp) -> (fun() -> RL16 = peg_alt(Inp, [
            fun(Inp) -> parse_int_fn(Inp, fun(Inp) -> ns_dec_digit(Inp) end) end,
            fun(Inp) -> detect_indent(Inp, N) end]), case r_failed(RL16) of true -> RL16; false -> M = r_tag_int(RL16), (fun(Inp) -> (fun() -> RL15 = peg_alt(Inp, [
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 45) end, <<"STRIP">>) end,
            fun(Inp) -> parse_sym_fn(Inp, fun(Inp) -> match_cp(Inp, 43) end, <<"KEEP">>) end,
            fun(Inp) -> val_fn(Inp, <<"CLIP">>) end]), case r_failed(RL15) of true -> RL15; false -> T = r_tag(RL15), (fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_b_comment(Inp) end, fun(Inp) -> l_folded_content(Inp, (N + M), T) end]) end)(r_rest(RL15)) end end)() end)(r_rest(RL16)) end end)() end]).

% [175] S-NB-FOLDED-TEXT 
s_nb_folded_text(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_indent(Inp, N) end,
        fun(Inp) -> ns_char(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> nb_char(Inp) end) end]).

% [176] L-NB-FOLDED-LINES 
l_nb_folded_lines(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_nb_folded_text(Inp, N) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> b_l_folded(Inp, N, <<"BLOCK-IN">>) end,
            fun(Inp) -> s_nb_folded_text(Inp, N) end]) end) end]).

% [177] S-NB-SPACED-TEXT 
s_nb_spaced_text(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_indent(Inp, N) end,
        fun(Inp) -> s_white(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> nb_char(Inp) end) end]).

% [178] B-L-SPACED 
b_l_spaced(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> b_as_line_feed(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> l_empty(Inp, N, <<"BLOCK-IN">>) end) end]).

% [179] L-NB-SPACED-LINES 
l_nb_spaced_lines(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_nb_spaced_text(Inp, N) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> b_l_spaced(Inp, N) end, fun(Inp) -> s_nb_spaced_text(Inp, N) end]) end) end]).

% [180] L-NB-SAME-LINES 
l_nb_same_lines(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> l_empty(Inp, N, <<"BLOCK-IN">>) end) end,
        fun(Inp) -> peg_alt(Inp, [fun(Inp) -> l_nb_folded_lines(Inp, N) end, fun(Inp) -> l_nb_spaced_lines(Inp, N) end]) end]).

% [181] L-NB-DIFF-LINES 
l_nb_diff_lines(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> l_nb_same_lines(Inp, N) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> b_as_line_feed(Inp) end, fun(Inp) -> l_nb_same_lines(Inp, N) end]) end) end]).

% [182] L-FOLDED-CONTENT 
l_folded_content(Inp, N, T) ->
    scalar_fn(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> l_nb_diff_lines(Inp, N) end, fun(Inp) -> b_chomped_last(Inp, T) end]) end) end,
        fun(Inp) -> l_chomped_empty(Inp, N, T) end]) end).

% [183] L+BLOCK-SEQUENCE 
lblock_sequence(Inp, N) ->
    build_ast(Inp, <<"SEQUENCE">>, fun(Inp) -> (fun() -> RL17 = detect_indent(Inp, N), case r_failed(RL17) of true -> RL17; false -> M = r_tag_int(RL17), (fun(Inp) -> collect_fn(Inp, fun(Inp) -> plus_(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> s_indent(Inp, (N + M)) end,
        fun(Inp) -> c_l_block_seq_entry(Inp, (N + M)) end]) end) end) end)(r_rest(RL17)) end end)() end).

% [184] C-L-BLOCK-SEQ-ENTRY 
c_l_block_seq_entry(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 45) end,
        fun(Inp) -> neg(Inp, fun(Inp) -> ns_char(Inp) end) end,
        fun(Inp) -> s_lblock_indented(Inp, N, <<"BLOCK-IN">>) end]).

% [185] S-L+BLOCK-INDENTED 
s_lblock_indented(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> (fun() -> RL19 = detect_indent(Inp, 0), case r_failed(RL19) of true -> RL19; false -> M = r_tag_int(RL19), (fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> s_indent(Inp, M) end,
            fun(Inp) -> peg_alt(Inp, [
                fun(Inp) -> ns_l_compact_sequence(Inp, (N + 1 + M)) end,
                fun(Inp) -> ns_l_compact_mapping(Inp, (N + 1 + M)) end]) end]) end)(r_rest(RL19)) end end)() end,
        fun(Inp) -> s_lblock_node(Inp, N, C) end,
        fun(Inp) -> peg_seq(Inp, [fun(Inp) -> e_node(Inp) end, fun(Inp) -> s_l_comments(Inp) end]) end]).

% [186] NS-L-COMPACT-SEQUENCE 
ns_l_compact_sequence(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> c_l_block_seq_entry(Inp, N) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_indent(Inp, N) end, fun(Inp) -> c_l_block_seq_entry(Inp, N) end]) end) end]).

% [187] L+BLOCK-MAPPING 
lblock_mapping(Inp, N) ->
    build_ast(Inp, <<"MAPPING">>, fun(Inp) -> (fun() -> RL20 = detect_indent(Inp, N), case r_failed(RL20) of true -> RL20; false -> M = r_tag_int(RL20), (fun(Inp) -> collect_fn(Inp, fun(Inp) -> plus_(Inp, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> s_indent(Inp, (N + M)) end,
        fun(Inp) -> ns_l_block_map_entry(Inp, (N + M)) end]) end) end) end)(r_rest(RL20)) end end)() end).

% [188] NS-L-BLOCK-MAP-ENTRY 
ns_l_block_map_entry(Inp, N) ->
    peg_alt(Inp, [
        fun(Inp) -> c_l_block_map_explicit_entry(Inp, N) end,
        fun(Inp) -> ns_l_block_map_implicit_entry(Inp, N) end]).

% [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
c_l_block_map_explicit_entry(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> c_l_block_map_explicit_key(Inp, N) end,
        fun(Inp) -> peg_alt(Inp, [fun(Inp) -> l_block_map_explicit_value(Inp, N) end, fun(Inp) -> e_node(Inp) end]) end]).

% [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
c_l_block_map_explicit_key(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 63) end,
        fun(Inp) -> s_lblock_indented(Inp, N, <<"BLOCK-OUT">>) end]).

% [191] L-BLOCK-MAP-EXPLICIT-VALUE 
l_block_map_explicit_value(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_indent(Inp, N) end,
        fun(Inp) -> match_cp(Inp, 58) end,
        fun(Inp) -> s_lblock_indented(Inp, N, <<"BLOCK-OUT">>) end]).

% [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
ns_l_block_map_implicit_entry(Inp, N) ->
    build_ast(Inp, <<"PAIR">>, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> scalar_fn(Inp, fun(Inp) -> peg_alt(Inp, [fun(Inp) -> ns_s_block_map_implicit_key(Inp) end, fun(Inp) -> e_node(Inp) end]) end) end,
        fun(Inp) -> c_l_block_map_implicit_value(Inp, N) end]) end).

% [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
ns_s_block_map_implicit_key(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> c_s_implicit_json_key(Inp, <<"BLOCK-KEY">>) end,
        fun(Inp) -> ns_s_implicit_yaml_key(Inp, <<"BLOCK-KEY">>) end]).

% [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
c_l_block_map_implicit_value(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 58) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> s_lblock_node(Inp, N, <<"BLOCK-OUT">>) end,
            fun(Inp) -> scalar_fn(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> e_node(Inp) end, fun(Inp) -> s_l_comments(Inp) end]) end) end]) end]).

% [195] NS-L-COMPACT-MAPPING 
ns_l_compact_mapping(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> ns_l_block_map_entry(Inp, N) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [fun(Inp) -> s_indent(Inp, N) end, fun(Inp) -> ns_l_block_map_entry(Inp, N) end]) end) end]).

% [196] S-L+BLOCK-NODE 
s_lblock_node(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> s_lblock_in_block(Inp, N, C) end,
        fun(Inp) -> s_lflow_in_block(Inp, N) end]).

% [197] S-L+FLOW-IN-BLOCK 
s_lflow_in_block(Inp, N) ->
    peg_seq(Inp, [
        fun(Inp) -> s_separate(Inp, (N + 1), <<"FLOW-OUT">>) end,
        fun(Inp) -> ns_flow_node(Inp, (N + 1), <<"FLOW-OUT">>) end,
        fun(Inp) -> s_l_comments(Inp) end]).

% [198] S-L+BLOCK-IN-BLOCK 
s_lblock_in_block(Inp, N, C) ->
    peg_alt(Inp, [
        fun(Inp) -> s_lblock_scalar(Inp, N, C) end,
        fun(Inp) -> s_lblock_collection(Inp, N, C) end]).

% [199] S-L+BLOCK-SCALAR 
s_lblock_scalar(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> s_separate(Inp, (N + 1), C) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> c_ns_properties(Inp, (N + 1), C) end,
            fun(Inp) -> s_separate(Inp, (N + 1), C) end]) end) end,
        fun(Inp) -> peg_alt(Inp, [fun(Inp) -> c_lliteral(Inp, N) end, fun(Inp) -> c_lfolded(Inp, N) end]) end]).

% [200] S-L+BLOCK-COLLECTION 
s_lblock_collection(Inp, N, C) ->
    peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> s_separate(Inp, (N + 1), C) end,
            fun(Inp) -> c_ns_properties(Inp, (N + 1), C) end]) end) end,
        fun(Inp) -> s_l_comments(Inp) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> lblock_sequence(Inp, seq_spaces(N, C)) end,
            fun(Inp) -> lblock_mapping(Inp, N) end]) end]).

% [202] L-DOCUMENT-PREFIX 
l_document_prefix(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> c_byte_order_mark(Inp) end) end,
        fun(Inp) -> star(Inp, fun(Inp) -> l_comment(Inp) end) end]).

% [203] C-DIRECTIVES-END 
c_directives_end(Inp) ->
    match_str(Inp, <<"---">>).

% [204] C-DOCUMENT-END 
c_document_end(Inp) ->
    match_str(Inp, <<"...">>).

% [205] L-DOCUMENT-SUFFIX 
l_document_suffix(Inp) ->
    peg_seq(Inp, [fun(Inp) -> c_document_end(Inp) end, fun(Inp) -> s_l_comments(Inp) end]).

% [206] C-FORBIDDEN 
c_forbidden(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> sol(Inp) end,
        fun(Inp) -> peg_alt(Inp, [fun(Inp) -> c_directives_end(Inp) end, fun(Inp) -> c_document_end(Inp) end]) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> b_char(Inp) end,
            fun(Inp) -> s_white(Inp) end,
            fun(Inp) -> eof_ok(Inp) end]) end]).

% [207] L-BARE-DOCUMENT 
l_bare_document(Inp) ->
    build_ast(Inp, <<"DOC">>, fun(Inp) -> s_lblock_node(Inp, -1, <<"BLOCK-IN">>) end).

% [208] L-EXPLICIT-DOCUMENT 
l_explicit_document(Inp) ->
    build_ast(Inp, <<"DOC">>, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> c_directives_end(Inp) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> l_bare_document(Inp) end,
            fun(Inp) -> peg_seq(Inp, [fun(Inp) -> e_node(Inp) end, fun(Inp) -> s_l_comments(Inp) end]) end]) end]) end).

% [209] L-DIRECTIVE-DOCUMENT 
l_directive_document(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> plus_(Inp, fun(Inp) -> l_directive(Inp) end) end,
        fun(Inp) -> l_explicit_document(Inp) end]).

% [210] L-ANY-DOCUMENT 
l_any_document(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> l_directive_document(Inp) end,
        fun(Inp) -> l_explicit_document(Inp) end,
        fun(Inp) -> l_bare_document(Inp) end]).

% [211] L-YAML-STREAM 
l_yaml_stream(Inp) ->
    build_ast(Inp, <<"STREAM">>, fun(Inp) -> peg_seq(Inp, [
        fun(Inp) -> star(Inp, fun(Inp) -> l_document_prefix(Inp) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> l_any_document(Inp) end) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> peg_seq(Inp, [
                fun(Inp) -> plus_(Inp, fun(Inp) -> l_document_suffix(Inp) end) end,
                fun(Inp) -> star(Inp, fun(Inp) -> l_document_prefix(Inp) end) end,
                fun(Inp) -> opt(Inp, fun(Inp) -> l_any_document(Inp) end) end]) end,
            fun(Inp) -> peg_seq(Inp, [
                fun(Inp) -> star(Inp, fun(Inp) -> l_document_prefix(Inp) end) end,
                fun(Inp) -> opt(Inp, fun(Inp) -> l_explicit_document(Inp) end) end]) end]) end) end]) end).



%% ── API ──

parse(Text) when is_binary(Text) ->
    Inp = mk_input(Text),
    l_yaml_stream(Inp);
parse(Text) when is_list(Text) ->
    parse(list_to_binary(Text)).

main(Args) ->
    case Args of
        [File|_] ->
            case file:read_file(File) of
                {ok, Text} ->
                    run_parse(Text);
                {error, _} ->
                    io:format(standard_error, "Error: cannot open '~s'~n", [File]),
                    halt(1)
            end;
        [] ->
            case is_stdin_tty() of
                true ->
                    io:format(standard_error, "Usage: peg_yaml [file]~n", []),
                    io:format(standard_error, "  Reads YAML from file or stdin.~n", []),
                    io:format(standard_error, "  If no file given and stdin is a terminal, shows this help.~n", []),
                    halt(1);
                false ->
                    Text = read_stdin(),
                    run_parse(Text)
            end
    end.

run_parse(Text) ->
    R = parse(Text),
    case r_failed(R) of
        false ->
            {_, Pos, _, _} = r_rest(R),
            io:format("OK: ~B chars~n", [Pos]);
        true ->
            {_, Pos, _, _} = r_rest(R),
            io:format(standard_error, "FAIL @~B~n", [Pos]),
            halt(1)
    end.

is_stdin_tty() ->
    case os:type() of
        {unix, _} ->
            case os:cmd("test -t 0 && echo yes || echo no") of
                "yes\n" -> true;
                _ -> false
            end;
        _ -> false
    end.

read_stdin() ->
    read_stdin(<<>>).
read_stdin(Acc) ->
    case io:get_chars('', 4096) of
        eof -> Acc;
        Data ->
            Bin = case is_list(Data) of true -> list_to_binary(Data); false -> Data end,
            read_stdin(<<Acc/binary, Bin/binary>>)
    end.
