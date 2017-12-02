-module(day25).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day25"),
    solve(parse(Input)).

parse(Bin) ->
    string:tokens(binary_to_list(Bin), "\n").

solve(Lines) ->
    Ops = [string:tokens(Line, " ") || Line <- Lines],
    solve(Ops, 175).

solve(Ops, A) ->
    io:format("~p, ", [A]),
    case solve(Ops, #{"a" => A}, 1, []) of
        error ->
            solve(Ops, A+1);
        _Regs ->
            A
    end.

solve(Ops, Regs, PC, _Outs) when PC > length(Ops) ->
    Regs;
solve(_Ops, Regs, _PC, Outs) when length(Outs) > 1000 ->
    Regs;
solve(Ops, Regs0, PC0, Outs) ->
    Op = lists:nth(PC0, Ops),
    {Regs, PC, Out} = eval(Op, Regs0, PC0),
    case Out of
        nil ->
            solve(Ops, Regs, PC, Outs);
        Out when Out =:= hd(Outs) ->
            error;
        Out ->
            solve(Ops, Regs, PC, [Out|Outs])
    end.

eval(["cpy", X, Y], Regs, PC) ->
    {maps:put(Y, val(X, Regs), Regs), PC+1, nil};
eval(["inc", X], Regs, PC) ->
    {maps:put(X, val(X, Regs)+1, Regs), PC+1, nil};
eval(["dec", X], Regs, PC) ->
    {maps:put(X, val(X, Regs)-1, Regs), PC+1, nil};
eval(["jnz", X, Y], Regs, PC) ->
    case val(X, Regs) of
        0 -> {Regs, PC+1, nil};
        _ -> {Regs, PC+list_to_integer(Y), nil}
    end;
eval(["out", X], Regs, PC) ->
    Out = val(X, Regs),
    {Regs, PC+1, Out}.

val([X], Regs) when X >= $a, X =< $d ->
    maps:get([X], Regs, 0);
val(N, _Regs) ->
    list_to_integer(N).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day25_test_() ->
    [ ?_assertEqual(175, solve_part1())
    ].
