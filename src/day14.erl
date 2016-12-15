-module(day14).

-compile([export_all]).

solve_part1() ->
    solve(input(), 0, 1, []).

solve_part2() ->
    solve(input(), 0, 2017, []).

input() ->
    "zpqevtbw".

solve(_, _N, _Rounds, Keys) when length(Keys) =:= 64 ->
    hd(Keys);
solve(Salt, N, Rounds, Keys) ->
    Md5 = md5_stretch(Salt, N, Rounds),
    case triple(Md5) of
        nil ->
            solve(Salt, N + 1, Rounds, Keys);
        C ->
            case find_five(Salt, N + 1, C, 1000, Rounds) of
                true  -> solve(Salt, N + 1, Rounds, [{N, Md5} | Keys]);
                false -> solve(Salt, N + 1, Rounds, Keys)
            end
    end.

find_five(_Salt, _N, _C, 0, _Rounds) -> false;
find_five(Salt, N, C, I, Rounds) ->
    Md5 = md5_stretch(Salt, N, Rounds),
    case five(Md5, C) of
      true  -> true;
      false -> find_five(Salt, N + 1, C, I - 1, Rounds)
    end.

triple([])        -> nil;
triple([A,A,A|_]) -> A;
triple([_|Rest])  -> triple(Rest).

five([], _)            -> false;
five([A,A,A,A,A|_], A) -> true;
five([_|Rest], A)      -> five(Rest, A).

md5_stretch(Salt, N, I) ->
    Str = Salt ++ integer_to_list(N),
    K = list_to_binary("stretch_" ++ integer_to_list(I) ++ "_" ++ Str),
    case get(K) of
        undefined ->
            Res = md5_stretch(Str, I),
            put(K, list_to_binary(Res)),
            Res;
        Md5 -> binary_to_list(Md5)
    end.

md5_stretch(Str, 0) -> Str;
md5_stretch(Str, I) -> md5_stretch(md5(Str), I - 1).

md5(Str) ->
    to_hex(crypto:hash(md5, Str)).

to_hex(Bin) ->
    [HexChar || <<C:4>> <= Bin, [HexChar] <- io_lib:format("~.16b", [C])].

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day14_test_() ->
    [ ?_assertMatch({22728, _}, solve("abc", 0, 1, []))
    , ?_assertMatch({16106, _}, solve_part1())
    %% Too slow
    %% , ?_assertMatch({22423, _}, solve_part2())
    ].
