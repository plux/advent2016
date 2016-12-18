-module(day17).

-compile([export_all]).

solve_part1() ->
    solve([[]], "hhhxzeay").

solve_part2() ->
    solve2([[]], "hhhxzeay", []).

solve([Seq|Seqs], Pass) ->
    Moves = moves(Seq, Pass),
    case [Move || Move <- Moves, pos(Move) =:= {4, 4}] of
        []       -> solve(Seqs ++ Moves, Pass);
        [Solved] -> Solved
    end.

solve2([], _, Solution) ->
    length(Solution);
solve2([Seq|Seqs], Pass, Solution) ->
    Moves = moves(Seq, Pass),
    case [Move || Move <- Moves, pos(Move) =:= {4, 4}] of
        []       -> solve2(Seqs ++ Moves, Pass, Solution);
        [Solved] -> solve2(Seqs ++ Moves -- [Solved], Pass, Solved)
    end.

moves(Seq, Pass) ->
    Pos = pos(Seq),
    Md5 = md5(Pass ++ Seq),
    [Seq ++ [Dir] || {N, Dir} <- lists:zip(lists:seq(1, 4), "UDLR"),
                     is_valid(move(Dir, Pos)), is_open(N, Md5)].

pos(Seq) ->
    lists:foldl(fun move/2, {1, 1}, Seq).

is_valid({X,Y}) ->
    X > 0 andalso X < 5 andalso Y > 0 andalso Y < 5.

is_open(N, Md5) ->
    C = lists:nth(N, Md5),
    $b =< C andalso C =< $f.

move($U, {X, Y}) -> {X, Y-1};
move($D, {X, Y}) -> {X, Y+1};
move($L, {X, Y}) -> {X-1, Y};
move($R, {X, Y}) -> {X+1, Y}.

md5(Str) ->
    to_hex(crypto:hash(md5, Str)).

to_hex(Bin) ->
    [HexChar || <<C:4>> <= Bin, [HexChar] <- io_lib:format("~.16b", [C])].

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day17_test_() ->
    [ ?_assertEqual("DDRRRD", solve([[]], "ihgpwlah"))
    , ?_assertEqual("DDRUDLRRRD", solve_part1())
    , ?_assertEqual(398, solve_part2())
    ].
