-module(day11).

-compile([export_all]).

solve_part1() ->
    #{steps := Steps} = solve(input_part1()),
    length(Steps).

solve_part2() ->
    #{steps := Steps} = solve(input_part2()),
    length(Steps).

%% The first floor contains a promethium generator and a
%% promethium-compatible microchip.
%% The second floor contains a cobalt generator, a curium generator, a
%% ruthenium generator, and a plutonium generator.
%% The third floor contains a cobalt-compatible microchip, a
%% curium-compatible microchip, a ruthenium-compatible microchip, and
%% a plutonium-compatible microchip.
%% The fourth floor contains nothing relevant.
input_part1() ->
    #{ 1 => [ {gen, promethium}, {chip, promethium}]
     , 2 => [{gen, cobalt}, {gen, curium}, {gen, ruthenium}, {gen, plutonium}]
     , 3 => [ {chip, cobalt}, {chip, curium}
            , {chip, ruthenium}, {chip, plutonium}]
     , 4 => []
     , elevator => 1
     , steps => []
     }.

%% Upon entering the isolated containment area, however, you notice
%% some extra parts on the first floor that weren't listed on the
%% record outside:

%% - An elerium generator.
%% - An elerium-compatible microchip.
%% - A dilithium generator.
%% - A dilithium-compatible microchip.
input_part2() ->
    #{ 1 => [ {gen, promethium}, {chip, promethium}
            , {gen, elerium}, {chip, elerium}
            , {gen, dilithium}, {chip, dilithium}]
     , 2 => [{gen, cobalt}, {gen, curium}, {gen, ruthenium}, {gen, plutonium}]
     , 3 => [ {chip, cobalt}, {chip, curium}
            , {chip, ruthenium}, {chip, plutonium}]
     , 4 => []
     , elevator => 1
     , steps => []
     }.

solve(Input) ->
    solve([Input], gb_sets:from_list([comparable(Input)])).

solve([State|Rest], History) ->
    States0 = make_moves(State),
    {StatesSet, States} = prune_states(States0, History),
    case lists:filter(fun is_done/1, States) of
        [] ->
            solve(Rest ++ States, gb_sets:union(StatesSet, History));
        [Solved|_] ->
            Solved
    end.

prune_states(States, History) ->
    CompStates = [comparable(S) || S <- States],
    ValidStates = valid_states(CompStates),
    StatesSet = gb_sets:from_list(ValidStates),
    Diff = gb_sets:difference(StatesSet, History),
    PrunedStates = [S || S <- States, gb_sets:is_member(comparable(S), Diff)],
    {Diff, PrunedStates}.

comparable(#{1 := F1, 2 := F2, 3 := F3, 4 := F4, elevator := E}) ->
    {E, lists:map(fun comparable/1, [F1, F2, F3, F4])};
comparable(Items0) when is_list(Items0) ->
    Items = lists:sort(Items0),
    Chips = [Chip || {chip, Chip} <- Items],
    Gens = [Gen || {gen, Gen} <- Items],
    Pairs = [Chip || Chip <- Chips, lists:member(Chip, Gens)],
    {length(Pairs), Chips -- Pairs, Gens -- Pairs}.

valid_states(States) ->
    lists:filter(fun is_valid/1, States).

is_valid({_, Floors}) ->
    lists:all(fun is_valid_floor/1, Floors).

is_valid_floor({_Pairs, [],     _Gens}) -> true;
is_valid_floor({0,      _Chips, []})    -> true;
is_valid_floor({_Pairs, _Chips, _Gens}) -> false.

make_moves(State) ->
    up_moves(State) ++ down_moves(State).

up_moves(#{elevator := 4}) ->
    [];
up_moves(#{elevator := E} = State) ->
    [make_move(E, E+1, Items, State) || Items <- take_items(State)].

down_moves(#{elevator := 1}) ->
    [];
down_moves(#{elevator := E} = State) ->
    [make_move(E, E-1, Items, State) || Items <- take_items(State)].

take_items(#{elevator := E} = State) ->
    #{E := Items} = State,
    take_two_items(Items) ++ take_one_item(Items).

take_one_item(Items) ->
    [[Item] || Item <- Items].

take_two_items(Items) ->
    lists:usort([lists:sort([Item1, Item2]) || Item1 <- Items,
                                               Item2 <- (Items -- [Item1])]).

make_move(From, To, Items, State) ->
    #{From := Curr, To := Next, steps := Steps} = State,
    State#{ From := Curr -- Items
          , To := Next ++ Items
          , elevator := To
          , steps := [{From, To, Items}|Steps]
          }.

is_done(#{1 := [], 2 := [], 3 := []}) ->
    true;
is_done(_) ->
    false.

%%%_* Tests ============================================================
-include_lib("eunit/include/eunit.hrl").

day11_test_() ->
    %% The first floor contains a hydrogen-compatible microchip
    %% and a lithium-compatible microchip.
    %% The second floor contains a hydrogen generator.
    %% The third floor contains a lithium generator"
    %% "The fourth floor contains nothing relevant.
    Example = #{ 1 => [{chip, hydrogen}, {chip, lithium}]
               , 2 => [{gen, hydrogen}]
               , 3 => [{gen, lithium}]
               , 4 => []
               , elevator => 1
               , steps => []
               },
     [ ?_assertEqual(11, length(maps:get(steps, solve(Example))))
     , {timeout, 60, ?_assertEqual(33, solve_part1())}
     %% Too slow
     %% , {timeout, 60, ?_assertEqual(57, solve_part2())}
     ].
