-module(day19).

-compile([export_all]).

solve_part1() ->
    solve(3014387).

solve_part2() ->
    solve2(3014387).

solve(NumElves) ->
    solve(lists:seq(1, NumElves), []).

solve2(NumElves) ->
    X = find_pow(1, NumElves),
    case 2*X < NumElves of
        false -> NumElves-X;
        true  -> X + 2*(NumElves-(2*X))
    end.

find_pow(Pow, NumElves) ->
    case math:pow(3, Pow) >= NumElves of
        true  -> trunc(math:pow(3, Pow-1));
        false -> find_pow(Pow+1, NumElves)
    end.

solve([], [Elf])       -> Elf;
solve([N,_|Rest], Acc) -> solve(Rest, [N|Acc]);
solve(E, Acc)          -> solve(E ++ lists:reverse(Acc), []).

solve2_sim(NumElves) ->
    solve2_sim(lists:seq(1, NumElves), []).

solve2_sim([], [Elf])     -> Elf;
solve2_sim([Elf], [])     -> Elf;
solve2_sim([Elf], Acc)    -> solve2_sim([Elf|lists:reverse(Acc)], []);
solve2_sim([], Acc)       -> solve2_sim(lists:reverse(Acc), []);
solve2_sim([N|Rest], Acc) ->
    %% This works, but it super slow, used it to figure out the pattern
    Elves  = Rest ++ lists:reverse(Acc),
    Len    = length(Elves),
    Remove = Len div 2 + (Len rem 2),
    Nth    = lists:nth(Remove, Elves),
    io:format("~p removes ~p [~p] (len: ~p)\n", [N, Nth, Remove, Len]),
    solve2_sim(lists:delete(Nth, Rest), [N|lists:delete(Nth, Acc)]).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day19_test_() ->
    [ ?_assertEqual(3, solve(5))
    , ?_assertEqual(1834471, solve_part1())
    , ?_assertEqual(1420064, solve_part2())
    , ?_assertEqual(9, solve2(90))
    , ?_assertEqual(241, solve2(242))
    ] ++ [?_assertEqual( solve2_sim(N)
                       , solve2(N)) || N <- lists:seq(2, 100)].
