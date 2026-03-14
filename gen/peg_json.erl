%% ════════════════════════════════════════════════════════════════
%% peg_json.erl — JSON (RFC 8259) parser
%% ════════════════════════════════════════════════════════════════
-module(peg_json).
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

% ════════════════════════════════════════════════════════════════ 
% YAML 1.2 Grammar — 211 rules 
% ════════════════════════════════════════════════════════════════ 

% [1] JSON-TEXT 
json_text(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> ws(Inp) end,
        fun(Inp) -> value(Inp) end,
        fun(Inp) -> ws(Inp) end,
        fun(Inp) -> eof_ok(Inp) end]).

% [2] VALUE 
value(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> object(Inp) end,
        fun(Inp) -> array(Inp) end,
        fun(Inp) -> string(Inp) end,
        fun(Inp) -> number(Inp) end,
        fun(Inp) -> match_str(Inp, <<"true">>) end,
        fun(Inp) -> match_str(Inp, <<"false">>) end,
        fun(Inp) -> match_str(Inp, <<"null">>) end]).

% [3] OBJECT 
object(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 123) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> members(Inp) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> match_cp(Inp, 125) end]) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 123) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> match_cp(Inp, 125) end]) end]).

% [4] MEMBERS 
members(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> member(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> match_cp(Inp, 44) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> member(Inp) end]) end) end]).

% [5] MEMBER 
member(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> ws(Inp) end,
        fun(Inp) -> string(Inp) end,
        fun(Inp) -> ws(Inp) end,
        fun(Inp) -> match_cp(Inp, 58) end,
        fun(Inp) -> ws(Inp) end,
        fun(Inp) -> value(Inp) end,
        fun(Inp) -> ws(Inp) end]).

% [6] ARRAY 
array(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 91) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> elements(Inp) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> match_cp(Inp, 93) end]) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_cp(Inp, 91) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> match_cp(Inp, 93) end]) end]).

% [7] ELEMENTS 
elements(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> value(Inp) end,
        fun(Inp) -> star(Inp, fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> match_cp(Inp, 44) end,
            fun(Inp) -> ws(Inp) end,
            fun(Inp) -> value(Inp) end]) end) end]).

% [8] STRING 
string(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 34) end,
        fun(Inp) -> star(Inp, fun(Inp) -> char(Inp) end) end,
        fun(Inp) -> match_cp(Inp, 34) end]).

% [9] CHAR 
char(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> escaped(Inp) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> neg(Inp, fun(Inp) -> match_cp(Inp, 34) end) end,
            fun(Inp) -> neg(Inp, fun(Inp) -> match_cp(Inp, 92) end) end,
            fun(Inp) -> neg(Inp, fun(Inp) -> match_cp(Inp, 16#0) end) end,
            fun(Inp) -> neg(Inp, fun(Inp) -> match_range(Inp, 16#0, 16#1F) end) end,
            fun(Inp) -> match_range(Inp, 16#20, 16#10FFFF) end]) end]).

% [10] ESCAPED 
escaped(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 92) end,
        fun(Inp) -> peg_alt(Inp, [
            fun(Inp) -> match_cp(Inp, 34) end,
            fun(Inp) -> match_cp(Inp, 92) end,
            fun(Inp) -> match_cp(Inp, 47) end,
            fun(Inp) -> match_cp(Inp, 98) end,
            fun(Inp) -> match_cp(Inp, 102) end,
            fun(Inp) -> match_cp(Inp, 110) end,
            fun(Inp) -> match_cp(Inp, 114) end,
            fun(Inp) -> match_cp(Inp, 116) end,
            fun(Inp) -> peg_seq(Inp, [fun(Inp) -> match_cp(Inp, 117) end, fun(Inp) -> hex4(Inp) end]) end]) end]).

% [11] HEX4 
hex4(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> hexdig(Inp) end,
        fun(Inp) -> hexdig(Inp) end,
        fun(Inp) -> hexdig(Inp) end,
        fun(Inp) -> hexdig(Inp) end]).

% [12] HEXDIG 
hexdig(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> match_range(Inp, 48, 57) end,
        fun(Inp) -> match_range(Inp, 97, 102) end,
        fun(Inp) -> match_range(Inp, 65, 70) end]).

% [13] NUMBER 
number(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> opt(Inp, fun(Inp) -> match_cp(Inp, 45) end) end,
        fun(Inp) -> integer(Inp) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> fraction(Inp) end) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> exponent(Inp) end) end]).

% [14] INTEGER 
integer(Inp) ->
    peg_alt(Inp, [
        fun(Inp) -> match_cp(Inp, 48) end,
        fun(Inp) -> peg_seq(Inp, [
            fun(Inp) -> match_range(Inp, 49, 57) end,
            fun(Inp) -> star(Inp, fun(Inp) -> match_range(Inp, 48, 57) end) end]) end]).

% [15] FRACTION 
fraction(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> match_cp(Inp, 46) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> match_range(Inp, 48, 57) end) end]).

% [16] EXPONENT 
exponent(Inp) ->
    peg_seq(Inp, [
        fun(Inp) -> peg_alt(Inp, [fun(Inp) -> match_cp(Inp, 101) end, fun(Inp) -> match_cp(Inp, 69) end]) end,
        fun(Inp) -> opt(Inp, fun(Inp) -> peg_alt(Inp, [fun(Inp) -> match_cp(Inp, 43) end, fun(Inp) -> match_cp(Inp, 45) end]) end) end,
        fun(Inp) -> plus_(Inp, fun(Inp) -> match_range(Inp, 48, 57) end) end]).

% [17] WS 
ws(Inp) ->
    star(Inp, fun(Inp) -> peg_alt(Inp, [
        fun(Inp) -> match_cp(Inp, 16#20) end,
        fun(Inp) -> match_cp(Inp, 16#9) end,
        fun(Inp) -> match_cp(Inp, 16#0A) end,
        fun(Inp) -> match_cp(Inp, 16#0D) end]) end).



%% ── API ──

parse(Text) when is_binary(Text) ->
    Inp = mk_input(Text),
    json_text(Inp);
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
                    io:format(standard_error, "Usage: peg_json [file]~n", []),
                    io:format(standard_error, "  Reads JSON from file or stdin.~n", []),
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
