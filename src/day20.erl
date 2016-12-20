-module(day20).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day20"),
    solve(parse(Input)).

solve_part2() ->
    {ok, Input} = file:read_file("input/day20"),
    solve2(parse(Input)).

parse(Input) ->
    lists:sort(lists:map(fun(Line) ->
                                 [Min, Max] = string:tokens(Line, "-"),
                                 {list_to_integer(Min), list_to_integer(Max)}
                         end, string:tokens(binary_to_list(Input), "\n"))).

solve([{_, N}|Blacklist]) ->
    case is_blacklisted(N+1, Blacklist) of
        {true, NewBlacklist} -> solve(NewBlacklist);
        false                -> N+1
    end.

solve2([{_, N}|Blacklist]) ->
    case is_blacklisted(N+1, Blacklist) of
        {true, NewBlacklist} -> solve2(NewBlacklist);
        false                ->
            case [{Start, End} || {Start, End} <- Blacklist, N =< End] of
                []                      -> 0;
                [{Start, _}|_] = Pruned -> (Start-N-1) + solve2(Pruned)
            end
    end.

is_blacklisted(_, []) ->
    false;
is_blacklisted(N, [{Start, End}|_] = Blacklist) when Start =< N, N =< End ->
    {true, Blacklist};
is_blacklisted(N, [_|Rest]) ->
    is_blacklisted(N, Rest).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day20_test_() ->
    Example = [{0,2}, {4,7}, {5,8}],
    [ ?_assertEqual(3, solve(Example))
    , ?_assertEqual(32259706, solve_part1())
    , ?_assertEqual(113, solve_part2())
    ].
