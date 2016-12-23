-module(day22).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day22"),
    length(solve(parse(Input))).

parse(Input) ->
    [_, _|Lines] = string:tokens(binary_to_list(Input), "\n"),
    lists:map(fun parse_line/1, Lines).

parse_line(Line) ->
    io:format("lines: ~p\n", [Line]),
    Re = "/dev/grid/node-x(\\d+)-y(\\d+)\s+(\\d+)T\s+(\\d+)T\s+(\\d+)T\s+(\\d+)%",
    {match, [X, Y, _Size, Used, Avail, _Use]} =
        re:run(Line, Re, [{capture, all_but_first, list}]),
    {{X, Y}, list_to_integer(Used), list_to_integer(Avail)}.

solve(Nodes) ->
    lists:usort([lists:sort([A, B]) || A <- Nodes, B <- Nodes, valid_pair(A, B)]).

valid_pair(Node, Node)                    -> false;
valid_pair({_, 0, _}, {_, _, _})          -> false;
valid_pair({_, UsedA, _}, {_, _, AvailB}) -> UsedA =< AvailB.

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").
day21_test_() ->
    [ ?_assertEqual(864, solve_part1())
    ].
