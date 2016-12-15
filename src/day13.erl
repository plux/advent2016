-module(day13).

-compile([export_all]).

solve_part1() ->
    solve(1362, {31, 39}).

solve_part2() ->
    solve2(1362).

solve(N, Goal) ->
    solve(N, [{1, 1, 0}], gb_sets:from_list([{1, 1}]), Goal).

solve(N, [Pos|Rest], Visited, Goal) ->
    Moves = moves(Pos, N, Visited),
    case [Steps || {X, Y, Steps} <- Moves, {X, Y} =:= Goal] of
        [] ->
            NewVisited = gb_sets:from_list([{X, Y} || {X, Y, _} <- Moves]),
            solve(N, Rest ++ Moves, gb_sets:union(NewVisited, Visited), Goal);
        [Solved] ->
            Solved
    end.

solve2(N) ->
    solve2(N, [{1, 1, 0}], gb_sets:from_list([{1, 1}])).

solve2(_N, [{_, _, 50}|_], Visited) ->
    gb_sets:size(Visited);
solve2(N, [Pos|Rest], Visited) ->
    Moves = moves(Pos, N, Visited),
    NewVisited = gb_sets:from_list([{X, Y} || {X, Y, _} <- Moves]),
    solve2(N, Rest ++ Moves, gb_sets:union(NewVisited, Visited)).

moves({X, Y, Steps}, N, Visited) ->
    [{X+DX, Y+DY, Steps+1} || {DX, DY} <- directions(),
                              is_valid({X+DX, Y+DY}, N, Visited)].

directions() ->
    [{-1,0}, {1,0}, {0,-1}, {0,1}].

is_valid({X, Y}, N, Visited) ->
    (not gb_sets:is_member({X, Y}, Visited))
        andalso (not is_wall({X, Y}, N))
        andalso X >= 0
        andalso Y >= 0.

is_wall({X, Y}, N0) ->
    N = X*X + 3*X + 2*X*Y + Y + Y*Y + N0,
    SumBits = lists:sum([C - $0 || C <- integer_to_list(N, 2)]),
    (SumBits rem 2) =:= 1.

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day13_test_() ->
    [ ?_assertEqual(false, is_wall({0,0},10))
    , ?_assertEqual(true, is_wall({1,0},10))
    , ?_assertEqual(false, is_wall({0,1},10))
    , ?_assertEqual(11, solve(10, {7,4}))
    , ?_assertEqual(82, solve_part1())
    , ?_assertEqual(151, solve2(10))
    , ?_assertEqual(138, solve_part2())
    ].
