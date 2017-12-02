-module(day24).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day24"),
    solve(parse(Input), [$0]).

solve_part2() ->
    {ok, Input} = file:read_file("input/day24"),
    solve(parse(Input), []).

parse(Input) ->
    Lines = string:tokens(binary_to_list(Input), "\n"),
    {_, Map, Goals} =
        lists:foldl(fun(Line, {Y, Map0, Goals0}) ->
                            {_, Map1, Goals1} =
                                lists:foldl(
                                  fun(C, {X, Acc, Gs}) when C >= $0, C =< $9 ->
                                          {X+1, maps:put({X, Y}, C, Acc), [C|Gs]};
                                     (C, {X, Acc, Gs}) ->
                                          {X+1, maps:put({X, Y}, C, Acc), Gs}
                                  end, {0, Map0, Goals0}, Line),
                            {Y+1, Map1, Goals1}
                end, {0, #{}, []}, Lines),
    {Map, lists:usort(Goals)}.

solve({Map, Goals}, VGoals) ->
    StartPos = find_start_pos(Map),
    Visited = gb_sets:from_list([{StartPos, [$0]}]),
    solve([{StartPos, VGoals, 0}], Map, Goals, Visited).

find_start_pos(Map) ->
    hd([Pos || {Pos, $0} <- maps:to_list(Map)]).

solve([{_Pos, Goals, Steps}|_], _Map, Goals, _Visited) ->
    Steps;
solve([Curr|T], Map, Goals, Visited) ->
    Moves = valid_moves(Curr, Goals, Map, Visited),
    solve(T ++ Moves, Map, Goals, update_visited(Moves, Visited)).

update_visited(Moves, Visited) ->
    S = gb_sets:from_list([{Pos, Goals} || {Pos, Goals, _Steps} <- Moves]),
    gb_sets:union(S, Visited).

valid_moves({{X, Y}, VGoals, Steps}, [_|TGoals], Map, Visited) ->
    ValidMoves =
        lists:flatmap(fun({DX, DY}) ->
                              Pos = {X+DX, Y+DY},
                              case maps:get(Pos, Map) of
                                  $# -> [];
                                  $. -> [{Pos, VGoals}];
                                  $0 when VGoals =/= TGoals ->
                                      [{Pos, VGoals}];
                                  C  -> [{Pos, lists:usort([C|VGoals])}]
                              end
                      end, directions()),
    [{Pos, Gs, Steps+1} || {Pos, Gs} <- ValidMoves,
                           not gb_sets:is_member({Pos, VGoals}, Visited)].

directions() ->
     [{0,1}, {1,0}, {0,-1}, {-1,0}].

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day24_test_() ->
    Example = <<"###########\n"
                "#0.1.....2#\n"
                "#.#######.#\n"
                "#4.......3#\n"
                "###########">>,
    [ ?_assertEqual(14, solve(parse(Example), [$0]))
    , ?_assertEqual(20, solve(parse(Example), []))
    , {timeout, 60, ?_assertEqual(464, solve_part1())}
%    , {timeout, 60, ?_assertEqual(652, solve_part2())}
    ].
