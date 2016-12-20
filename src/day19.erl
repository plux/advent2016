-module(day19).

-compile([export_all]).

solve_part1() ->
    solve(3014387).

solve_part2() ->
    solve2(3014387).

solve(NumElves) ->
    solve(lists:seq(1, NumElves), []).

solve2(NumElves) ->
    solve2(lists:seq(1, NumElves), []).

solve([], [Elf])       -> Elf;
solve([N,_|Rest], Acc) -> solve(Rest, [N|Acc]);
solve(E, Acc)          -> solve(E ++ lists:reverse(Acc), []).


solve2([], [Elf])     -> Elf;
solve2([Elf], Acc)    -> solve2([Elf|lists:reverse(Acc)], []);
solve2([], Acc)       -> solve2(lists:reverse(Acc), []);
solve2([N|Rest], Acc) ->
    %% This works, but it super slow
    Elves = Rest ++ lists:reverse(Acc),
    Len = length(Elves),
    Remove = Len div 2 + (Len rem 2),
    Nth    = lists:nth(Remove, Elves),
    io:format("~p removes ~p [~p] (len: ~p)\n", [N, Nth, Remove, Len]),
    solve2(lists:delete(Nth, Rest), [N|lists:delete(Nth, Acc)]).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day19_test_() ->
    [ ?_assertEqual(3, solve(5))
    , ?_assertEqual(1834471, solve_part1())
    , ?_assertEqual(2, solve2(5))
    ].
