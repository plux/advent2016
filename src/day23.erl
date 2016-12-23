-module(day23).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day23"),
    solve(parse(Input), #{"a" => 7}).

solve_part2() ->
    {ok, Input} = file:read_file("input/day23"),
    solve(parse(Input), #{"a" => 12}).

parse(Bin) ->
    string:tokens(binary_to_list(Bin), "\n").

solve(Lines, Regs) ->
    Ops = array:from_list([string:tokens(Line, " ") || Line <- Lines]),
    solve(Ops, Regs, 0).

solve(Ops0, #{"b" := B, "d" := D} = Regs0, 4) ->
    %% For speed
    solve(Ops0, Regs0#{"a" => B*D, "c" => 0, "d" => 0}, 10);
solve(Ops0, Regs0, PC0) ->
    case array:size(Ops0) > PC0 of
        true ->
            Op = array:get(PC0, Ops0),
            {Ops, Regs, PC} = eval(Op, Ops0, Regs0, PC0),
            solve(Ops, Regs, PC);
        false ->
            Regs0
    end.

eval(["cpy", X, Y], Ops, Regs, PC) ->
    case is_reg(Y) of
        true -> {Ops, maps:put(Y, val(X, Regs), Regs), PC+1};
        false -> {Ops, Regs, PC+1}
    end;
eval(["inc", X], Ops, Regs, PC) ->
    {Ops, maps:put(X, val(X, Regs)+1, Regs), PC+1};
eval(["dec", X], Ops, Regs, PC) ->
    {Ops, maps:put(X, val(X, Regs)-1, Regs), PC+1};
eval(["jnz", X, Y], Ops, Regs, PC) ->
    case val(X, Regs) of
        0 -> {Ops, Regs, PC+1};
        _ -> {Ops, Regs, PC+val(Y, Regs)}
    end;
eval(["tgl", X], Ops0, Regs, PC) ->
    Pos = PC + val(X, Regs),
    case array:get(Pos, Ops0) of
        undefined ->
            {Ops0, Regs, PC+1};
        Op ->
            Ops = array:set(Pos, toggle(Op), Ops0),
            {Ops, Regs, PC+1}
    end.

val([X], Regs) when X >= $a, X =< $d ->
    maps:get([X], Regs, 0);
val(N, _Regs) ->
    list_to_integer(N).

is_reg(X) ->
    lists:member(X, ["a", "b", "c", "d"]).

toggle(["jnz", X, Y]) -> ["cpy", X, Y];
toggle([_, X, Y])     -> ["jnz", X, Y];
toggle(["inc", X])    -> ["dec", X];
toggle([_, X])        -> ["inc", X].

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day23_test_() ->
    Example = [ "cpy 2 a"
              , "tgl a"
              , "tgl a"
              , "tgl a"
              , "cpy 1 a"
              , "dec a"
              , "dec a"
              ],
    [ ?_assertMatch(#{"a" := 3}, solve(Example, #{}))
    , ?_assertMatch(#{"a" := 12000}, solve_part1())
    ].
