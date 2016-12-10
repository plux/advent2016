-module(day10).

-compile([export_all]).

solve_part1() ->
    {ok, Input} = file:read_file("input/day10"),
    solve(split_lines(Input), 17, 61).

solve(Input, Low, High) ->
    State = eval(parse(Input), #{}),
    hd([Bot || {Bot, Val} <- maps:to_list(State), Val =:= [Low, High]]).

solve_part2() ->
    {ok, Input} = file:read_file("input/day10"),
    State = eval(parse(split_lines(Input)), #{}),
    #{"output0" := [Out0], "output1" := [Out1], "output2" := [Out2]} = State,
    Out0 * Out1 * Out2.

split_lines(Bin) ->
    string:tokens(binary_to_list(Bin), "\n").

parse(Lines) ->
    [parse_line(string:tokens(Line, " ")) ||  Line <- Lines].

parse_line(["value", Value, "goes", "to", "bot", N]) ->
    {value, list_to_integer(Value), "bot" ++ N};
parse_line(["bot", From, "gives", "low", "to", TypeLow, NumberLow,
            "and", "high", "to", TypeHigh, NumberHigh]) ->
    {give, "bot" ++ From, TypeLow ++ NumberLow, TypeHigh ++ NumberHigh}.

eval([], State) ->
    State;
eval([{value, Value, Bot}|Ops], State) ->
    eval(Ops, send_value(Bot, Value, State));
eval([{give, Giver, ToLow, ToHigh} = Op|Ops], State0) ->
    case maps:get(Giver, State0, []) of
        [Low, High] ->
            State1 = send_value(ToLow, Low, State0),
            State2 = send_value(ToHigh, High, State1),
            eval(Ops, State2);
        _Else ->
            eval(Ops ++ [Op], State0)
    end.

send_value(Bot, Value, State) ->
    Values = maps:get(Bot, State, []),
    maps:put(Bot, lists:sort([Value|Values]), State).

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day10_test_() ->
    Example = [ "value 5 goes to bot 2"
              , "bot 2 gives low to bot 1 and high to bot 0"
              , "value 3 goes to bot 1"
              , "bot 1 gives low to output 1 and high to bot 0"
              , "bot 0 gives low to output 2 and high to output 0"
              , "value 2 goes to bot 2"
              ],
    [ ?_assertMatch(#{ "output0" := [5]
                     , "output1" := [2]
                     , "output2" := [3]
                     , "bot2"    := [2, 5]
                     }, eval(parse(Example), #{}))
    , ?_assertEqual(12803, solve_part2())
    , ?_assertEqual("bot2", solve(Example, 2, 5))
    , ?_assertEqual("bot113", solve_part1())
    ].
