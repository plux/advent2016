-module(day18).

-compile([export_all]).

solve_part1() ->
    solve(parse(input()), 40).

solve_part2() ->
    solve(parse(input()), 400000).

input() ->
    "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^."
    "^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^".

parse(Input) ->
    Width = length(Input),
    L = [{{X, 1}, lists:nth(X, Input) =:= $^} || X <- lists:seq(1, Width)],
    {Width, maps:from_list(L)}.

solve({Width, Map}, Height) ->
    Coords = [{X, Y} || Y <- lists:seq(2, Height), X <- lists:seq(1, Width)],
    SolvedMap = lists:foldl(fun check_trap/2, Map, Coords),
    length([{X, Y} || {{X, Y}, false} <- maps:to_list(SolvedMap)]).

check_trap({X, Y}, Map) ->
    Left   = maps:get({X-1, Y-1}, Map, false),
    Center = maps:get({X, Y-1}, Map, false),
    Right  = maps:get({X+1, Y-1}, Map, false),
    maps:put({X, Y}, is_trap(Left, Center, Right), Map).

is_trap(true,  true,   false) -> true;
is_trap(false, true,   true)  -> true;
is_trap(true,  false,  false) -> true;
is_trap(false, false,  true)  -> true;
is_trap(_,     _,      _)     -> false.

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day18_test_() ->
    [ ?_assertEqual(6, solve(parse("..^^."), 3))
    , ?_assertEqual(38, solve(parse(".^^.^.^^^^"), 10))
    , ?_assertEqual(1974, solve_part1())
    %% Too slow
    %% ?_assertEqual(19991126, solve_part2())
    ].
