-module(day16).

-compile([export_all]).

solve_part1() ->
    solve("10001110011110000", 272).

solve_part2() ->
    solve("10001110011110000", 35651584).

solve(Str, Size) ->
    check_sum(fill_disk(Str, Size)).

fill_disk(Str, Size) when length(Str) >= Size ->
    lists:sublist(Str, Size);
fill_disk(Str0, Size) ->
    fill_disk(Str0 ++ "0" ++ invert(Str0, []), Size).

invert([], Acc)     -> Acc;
invert([$0|T], Acc) -> invert(T, [$1|Acc]);
invert([$1|T], Acc) -> invert(T, [$0|Acc]).

check_sum(Str) ->
    CheckSum = pair_check_sum(Str),
    case length(CheckSum) rem 2 of
        1 -> CheckSum;
        0 -> check_sum(CheckSum)
    end.

pair_check_sum([])      -> [];
pair_check_sum([A,A|T]) -> [$1|pair_check_sum(T)];
pair_check_sum([_,_|T]) -> [$0|pair_check_sum(T)].

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day16_test_() ->
    [ ?_assertEqual("10000011110010000111", fill_disk("10000", 20))
    , ?_assertEqual("01100", solve("10000", 20))
    , ?_assertEqual("10010101010011101", solve_part1())
    , ?_assertEqual("01100111101101111", solve_part2())
    ].
