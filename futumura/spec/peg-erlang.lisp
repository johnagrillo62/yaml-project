;;;; peg-erlang.lisp — Erlang target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Erlang")
(def-tgt "default-output" "peg_yaml.erl")
(def-tgt "comment-prefix" "%")

(def-tgt "keywords"
  '("after" "and" "andalso" "band" "begin" "bnot" "bor" "bsl" "bsr"
    "bxor" "case" "catch" "cond" "div" "end" "fun" "if" "let" "not"
    "of" "or" "orelse" "receive" "rem" "try" "when" "xor"))
(def-tgt "keyword-prefix" "r_")

;;; ── Note: Erlang uses default C-style calls but needs custom hex format.
;;; The emitter's hex output (0xFF) needs post-processing for Erlang (16#FF).
(def-tgt "hex-prefix" "16#")
(def-tgt "inp-name" "Inp")
(def-tgt "str-wrap" (lambda (s) (format nil "<<~A>>" s)))

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "fun(Inp) -> ~A end" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "fun(Inp) -> ~A end" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "peg_seq(Inp, [~{~A~^, ~}])" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "peg_alt(Inp, [~{~A~^, ~}])" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (let ((uparam (string-upcase param :end 1)))
      (format nil "case ~A of~{ <<~S>> -> ~A;~} _ -> fail_r(Inp, <<\"no case\">>) end"
              uparam
              (loop for (val body) in cases
                    collect val collect body)))))

;;; ── Let ──

(defvar *erlang-let-counter* 0)

(def-tgt "let-int"
  (lambda (vname expr rest)
    (let ((rv (format nil "RL~D" (incf *erlang-let-counter*)))
          (uv (string-upcase vname :end 1)))
      (format nil "(fun() -> ~A = ~A, case r_failed(~A) of true -> ~A; false -> ~A = r_tag_int(~A), (fun(Inp) -> ~A end)(r_rest(~A)) end end)()"
              rv expr rv rv uv rv rest rv))))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (let ((rv (format nil "RL~D" (incf *erlang-let-counter*)))
          (uv (string-upcase vname :end 1)))
      (format nil "(fun() -> ~A = ~A, case r_failed(~A) of true -> ~A; false -> ~A = r_tag(~A), (fun(Inp) -> ~A end)(r_rest(~A)) end end)()"
              rv expr rv rv uv rv rest rv))))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (let ((n (peg-ident sym)))
      (string-upcase n :end 1))))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "<<~S>>" s)))

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
        (format nil "~A(Inp~{, ~A~})" name
                (mapcar (lambda (p)
                          (let ((n (peg-ident p)))
                            (string-upcase n :end 1)))
                        params))
        (format nil "~A(Inp)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "~A ->~%    ~A." sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"%% ════════════════════════════════════════════════════════════════
%% peg_yaml.erl — YAML 1.2 parser
%% ════════════════════════════════════════════════════════════════
-module(peg_yaml).
-export([main/1, parse/1]).
")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"%% ── Input ──
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
                $\\n -> {Src, Pos + 1, Line + 1, 0};
                _ -> {Src, Pos + 1, Line, Col + 1}
            end
    end."

"%% ── Result ──
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
r_set_tag_int(R, I) -> setelement(6, R, I)."

"%% ── Context ──

in_flow(C) ->
    case C of
        <<\"FLOW-OUT\">> -> <<\"FLOW-IN\">>;
        <<\"FLOW-IN\">> -> <<\"FLOW-IN\">>;
        _ -> <<\"FLOW-KEY\">>
    end.

seq_spaces(N, C) ->
    case C of
        <<\"BLOCK-OUT\">> -> N - 1;
        _ -> N
    end."

"%% ── Combinators ──

match_cp(Inp, Cp) ->
    case peek_cp(Inp) of
        Cp -> ok_v(adv(Inp), <<Cp>>);
        _ -> fail_r(Inp, <<\"cp\">>)
    end.

match_range(Inp, Lo, Hi) ->
    C = peek_cp(Inp),
    case C >= Lo andalso C =< Hi of
        true -> ok_v(adv(Inp), <<C>>);
        false -> fail_r(Inp, <<\"rng\">>)
    end.

match_str(Inp, T) ->
    {_, Pos, _, _} = Inp,
    TLen = byte_size(T),
    {Src, _, _, _} = Inp,
    case Pos + TLen =< byte_size(Src) of
        false -> fail_r(Inp, <<\"str\">>);
        true ->
            Sub = binary:part(Src, Pos, TLen),
            case Sub =:= T of
                false -> fail_r(Inp, <<\"str\">>);
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

peg_alt(Inp, []) -> fail_r(Inp, <<\"alt\">>);
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
    case r_failed(R) of true -> ok_r(Inp); false -> fail_r(Inp, <<\"neg\">>) end.

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
                true -> fail_r(Inp, <<\"excl\">>);
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
        0 -> fail_r(Inp, <<\"behind\">>);
        _ ->
            Prev = {Src, Pos - 1, Line, max(0, Col - 1)},
            R = F(Prev),
            case r_failed(R) of
                true -> fail_r(Inp, <<\"behind\">>);
                false -> ok_r(Inp)
            end
    end.

sol(Inp) ->
    {_, _, _, Col} = Inp,
    case Col of 0 -> ok_r(Inp); _ -> fail_r(Inp, <<\"sol\">>) end.

eof_ok(Inp) ->
    case at_eof(Inp) of true -> ok_r(Inp); false -> fail_r(Inp, <<\"eof\">>) end."

"%% ── YAML extensions ──

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
    case (Pos + Sp < Len) andalso (binary:at(Src, Pos + Sp) =/= $\\n) of
        true -> r_set_tag_int(ok_r(Inp), max(1, Sp - N));
        false -> scan_lines(Src, Len, Pos + Sp, N, Inp)
    end.

count_spaces(Src, I, Len) ->
    case I < Len andalso binary:at(Src, I) =:= $\\s of
        true -> 1 + count_spaces(Src, I + 1, Len);
        false -> 0
    end.

scan_lines(Src, Len, J, N, Inp) ->
    case J >= Len of
        true -> r_set_tag_int(ok_r(Inp), 1);
        false ->
            case binary:at(Src, J) of
                $\\n ->
                    J1 = J + 1,
                    case J1 >= Len of
                        true -> r_set_tag_int(ok_r(Inp), 1);
                        false ->
                            Sp = count_spaces(Src, J1, Len),
                            Nx = J1 + Sp,
                            case (Nx >= Len) orelse (binary:at(Src, Nx) =:= $\\n) of
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
    r_set_tag(ok_r(Inp), V)."))

;;; ── Combinator name overrides ──

(def-tgt "comb-star"        "star")
(def-tgt "comb-plus"        "plus_")
(def-tgt "comb-opt"         "opt")
(def-tgt "comb-neg"         "neg")
(def-tgt "comb-rep"         "rep_fn")
(def-tgt "comb-ahead"       "ahead")
(def-tgt "comb-behind"      "behind")
(def-tgt "comb-minus"       "minus_fn")
(def-tgt "comb-build"       "build_ast")
(def-tgt "comb-scalar"      "scalar_fn")
(def-tgt "comb-collect"     "collect_fn")
(def-tgt "comb-sol"         "sol")
(def-tgt "comb-eof"         "eof_ok")
(def-tgt "comb-ok"          "ok_r")
(def-tgt "comb-detect"      "detect_indent")
(def-tgt "comb-parse-int"   "parse_int_fn")
(def-tgt "comb-parse-sym"   "parse_sym_fn")
(def-tgt "comb-val"         "val_fn")

;;; ── API ──

(def-tgt "api" "")

(def-tgt "main-fn"
"%% ── API ──

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
                    io:format(standard_error, \"Error: cannot open '~s'~n\", [File]),
                    halt(1)
            end;
        [] ->
            case is_stdin_tty() of
                true ->
                    io:format(standard_error, \"Usage: peg_yaml [file]~n\", []),
                    io:format(standard_error, \"  Reads YAML from file or stdin.~n\", []),
                    io:format(standard_error, \"  If no file given and stdin is a terminal, shows this help.~n\", []),
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
            io:format(\"OK: ~B chars~n\", [Pos]);
        true ->
            {_, Pos, _, _} = r_rest(R),
            io:format(standard_error, \"FAIL @~B~n\", [Pos]),
            halt(1)
    end.

is_stdin_tty() ->
    case os:type() of
        {unix, _} ->
            case os:cmd(\"test -t 0 && echo yes || echo no\") of
                \"yes\\n\" -> true;
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
    end.")

(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)
