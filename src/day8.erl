-module(day8).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day8"),
    solve(parse(Input)).

solve_part2() ->
    {ok, Input} = file:read_file("input/day8"),
    print(eval(parse(Input))).

parse(Bin) ->
    [parse_line(Line) || Line <- string:tokens(binary_to_list(Bin), "\n")].

parse_line("rect " ++ Rest) ->
    {ok, [W, H], []} = io_lib:fread("~dx~d", Rest),
    {rect, W, H};
parse_line("rotate row " ++ Rest) ->
    {ok, [Row, N], []} = io_lib:fread("y=~d by ~d", Rest),
    {rot_row, Row, N};
parse_line("rotate column " ++ Rest) ->
    {ok, [Col, N], []} = io_lib:fread("x=~d by ~d", Rest),
    {rot_col, Col, N}.

solve(Ops) ->
    length(eval(Ops)).

eval(Ops) ->
    lists:foldl(fun(Op, Screen) -> eval_op(Op, Screen) end, [], Ops).

eval_op({rect, W, H}, Screen) ->
    On = [{X-1, Y-1} || X <- lists:seq(1, W), Y <- lists:seq(1, H)],
    lists:usort(On ++ Screen);
eval_op({rot_row, Row, N}, Screen) ->
    lists:map(fun({X, Y}) when Y =:= Row -> {(X+N) rem 50, Y};
                 ({X, Y})                -> {X, Y}
              end, Screen);
eval_op({rot_col, Col, N}, Screen) ->
    lists:map(fun({X, Y}) when X =:= Col -> {X, (Y+N) rem 6};
                 ({X, Y})                -> {X, Y}
              end, Screen).

print(Screen) ->
    lists:map(fun(Y) ->
                      lists:map(fun(X) ->
                                        case lists:member({X, Y}, Screen) of
                                            true  -> io:format("#");
                                            false -> io:format(" ")
                                        end
                                end, lists:seq(0, 49)),
                      io:format("\n")
              end, lists:seq(0, 5)).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day8_test_() ->
    Example = [ {rect, 3, 2}
              , {rot_col, 1, 1}
              , {rot_row, 0, 5}
              , {rot_col, 1, 1}
              ],
    [ ?_assertEqual({rect, 1, 2}, parse_line("rect 1x2"))
    , ?_assertEqual({rot_row, 0, 5}, parse_line("rotate row y=0 by 5"))
    , ?_assertEqual({rot_col, 0, 1}, parse_line("rotate column x=0 by 1"))
    , ?_assertEqual(6, solve(Example))
    , ?_assertEqual(128, solve_part1())
    ].
