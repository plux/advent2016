-module(day9).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day9"),
    solve(preprocess(Input), 1).

solve_part2() ->
    {ok, Input} = file:read_file("input/day9"),
    solve(preprocess(Input), 2).

preprocess(Str) ->
    [C || C <- binary_to_list(Str), C =/= $\n].

solve([], _Part) ->
    0;
solve([$(|_] = Input, Part) ->
    {ok, [Length, Times], Rest0} = io_lib:fread("(~dx~d)", Input),
    {Str, Rest} = lists:split(Length, Rest0),
    case Part of
        1 -> (length(Str)      * Times) + solve(Rest, Part);
        2 -> (solve(Str, Part) * Times) + solve(Rest, Part)
    end;
solve([_|T], Part) ->
    1 + solve(T, Part).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day9_test_() ->
    [ ?_assertEqual(97714, solve_part1())
    , ?_assertEqual(10762972461, solve_part2())
    ].
