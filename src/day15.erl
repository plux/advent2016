-module(day15).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day15"),
    solve(parse(Input), 0).

solve_part2() ->
    {ok, Input} = file:read_file("input/day15_part2"),
    solve(parse(Input), 0).

parse(Bin) ->
    [parse_line(Line) || Line <- string:tokens(binary_to_list(Bin), "\n")].

parse_line(Line) ->
    Re = "Disc #(\\d+) has (\\d+) positions; "
        "at time=0, it is at position (\\d+).",
    {match, [_, Disc, NumPos, Pos]} = re:run(Line, Re, [{capture, all, list}]),
    {list_to_integer(Disc), list_to_integer(NumPos), list_to_integer(Pos)}.

solve(Discs, T) ->
    case lists:sum([position_disc(Disc, T) || Disc <- Discs]) of
        0 -> T;
        _ -> solve(Discs, T+1)
    end.

position_disc({Disc, NumPos, Pos}, T) ->
    (Disc + Pos + T) rem NumPos.

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day15_test_() ->
    Example = [ {1, 5, 4}
              , {2, 2, 1}
              ],
    [ ?_assertEqual(5, solve(Example, 0))
    , ?_assertEqual(121834, solve_part1())
    , ?_assertEqual(3208099, solve_part2())
    ].
