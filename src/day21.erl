-module(day21).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day21"),
    solve(parse(Input), "abcdefgh").

solve_part2() ->
    {ok, Input} = file:read_file("input/day21"),
    solve2(perms("abcdefgh"), parse(Input), "fbgdceah").

solve2([H|T], Ops, Scrambled) ->
    case solve(Ops, H) of
        Scrambled -> H;
        _         -> solve2(T, Ops, Scrambled)
    end.

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

parse(Input) ->
    Lines = string:tokens(binary_to_list(Input), "\n"),
    [parse_line(string:tokens(Line, " ")) || Line <- Lines].

parse_line(["swap", "position", X, "with", "position", Y]) ->
    {swap_position, list_to_integer(X), list_to_integer(Y)};
parse_line(["swap", "letter", [X], "with", "letter", [Y]]) ->
    {swap_letter, X, Y};
parse_line(["rotate", "left", X, _]) ->
    {rotate_left, list_to_integer(X)};
parse_line(["rotate", "right", X, _]) ->
    {rotate_right, list_to_integer(X)};
parse_line(["rotate", "based", "on", "position", "of", "letter", [X]]) ->
    {rotate_position, X};
parse_line(["reverse", "positions", X, "through", Y]) ->
    {reverse, list_to_integer(X), list_to_integer(Y)};
parse_line(["move", "position", X, "to", "position", Y]) ->
    {move, list_to_integer(X), list_to_integer(Y)}.

solve(Ops, Str) ->
    lists:foldl(fun eval/2, Str, Ops).

eval({swap_position, X, Y}, Str) ->
    Arr0 = array:from_list(Str),
    Arr1 = array:set(X, array:get(Y, Arr0), Arr0),
    Arr2 = array:set(Y, array:get(X, Arr0), Arr1),
    array:to_list(Arr2);
eval({swap_letter, X, Y}, Str) ->
    lists:map(fun(C) when C =:= X -> Y;
                 (C) when C =:= Y -> X;
                 (C)              -> C
              end, Str);
eval({rotate_left, 0}, Str) ->
    Str;
eval({rotate_left, X}, [H|T]) ->
    eval({rotate_left, X-1}, T ++ [H]);
eval({rotate_right, 0}, Str) ->
    Str;
eval({rotate_right, X}, Str) ->
    eval({rotate_right, X-1}, [lists:last(Str)|lists:droplast(Str)]);
eval({rotate_position, X}, Str) ->
    case string:chr(Str, X) of
        N when N > 4 -> eval({rotate_right, N+1}, Str);
        N            -> eval({rotate_right, N}, Str)
    end;
eval({reverse, X, Y}, Str) ->
    {Left, Right0} = lists:split(X, Str),
    {Middle, Right} = lists:split(Y-X+1, Right0),
    Left ++ lists:reverse(Middle) ++ Right;
eval({move, X, Y}, Str0) ->
    CX = lists:nth(X+1, Str0),
    Str1 = [C || C <- Str0, C =/= CX],
    {Left, Right} = lists:split(Y, Str1),
    Left ++ [CX] ++ Right.

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").
day21_test_() ->
    [ ?_assertEqual("ebcda", eval({swap_position, 4, 0}, "abcde"))
    , ?_assertEqual("edcba", eval({swap_letter, $d, $b}, "ebcda"))
    , ?_assertEqual("abcde", eval({reverse, 0, 4}, "edcba"))
    , ?_assertEqual("bcdea", eval({rotate_left, 1}, "abcde"))
    , ?_assertEqual("eabcd", eval({rotate_right, 1}, "abcde"))
    , ?_assertEqual("bdeac", eval({move, 1, 4}, "bcdea"))
    , ?_assertEqual("abdec", eval({move, 3, 0}, "bdeac"))
    , ?_assertEqual("ecabd", eval({rotate_position, $b}, "abdec"))
    , ?_assertEqual("decab", eval({rotate_position, $d}, "ecabd"))
    , ?_assertEqual("gfdhebac", solve_part1())
    , ?_assertEqual("dhaegfbc", solve_part2())
    ].
