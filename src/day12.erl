-module(day12).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day12"),
    solve(parse(Input), #{}).

solve_part2() ->
    {ok, Input} = file:read_file("input/day12"),
    solve(parse(Input), #{"c" => 2}).

parse(Bin) ->
    string:tokens(binary_to_list(Bin), "\n").

solve(Lines, Regs) ->
    Ops = [string:tokens(Line, " ") || Line <- Lines],
    solve(Ops, Regs, 1).

solve(Ops, Regs, PC) when PC > length(Ops) ->
    Regs;
solve(Ops, Regs0, PC0) ->
    Op = lists:nth(PC0, Ops),
    {Regs, PC} = eval(Op, Regs0, PC0),
    solve(Ops, Regs, PC).

eval(["cpy", X, Y], Regs, PC) ->
    {maps:put(Y, val(X, Regs), Regs), PC+1};
eval(["inc", X], Regs, PC) ->
    {maps:put(X, val(X, Regs)+1, Regs), PC+1};
eval(["dec", X], Regs, PC) ->
    {maps:put(X, val(X, Regs)-1, Regs), PC+1};
eval(["jnz", X, Y], Regs, PC) ->
    case val(X, Regs) of
        0 -> {Regs, PC+1};
        _ -> {Regs, PC+list_to_integer(Y)}
    end.

val([X], Regs) when X >= $a, X =< $d ->
    maps:get([X], Regs, 0);
val(N, _Regs) ->
    list_to_integer(N).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day12_test_() ->
    Example = [ "cpy 41 a"
              , "inc a"
              , "inc a"
              , "dec a"
              , "jnz a 2"
              , "dec a"
              ],
    [ ?_assertMatch(#{"a" := 42}, solve(Example, #{}))
    , ?_assertMatch(#{"a" := 318117}, solve_part1())
    , {timeout, 60, ?_assertMatch(#{"a" := 9227771}, solve_part2())}
    ].
