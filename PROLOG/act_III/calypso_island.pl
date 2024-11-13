:- multifile describe/1.
:- dynamic holding/2, raft_step_completed/1, you_are_at/1.

required_material(wood, 2).
required_material(logs, 2).
required_material(rope, 2).
required_material(cloth, 1).

describe(calypso_island) :- !,
    write("You stand on the shores of an island. It it breathtaking but empty, nobody is to be seen.\n"),
    write("\nThe only sound is wind blowing through the wind. You are exhausted from all travels and you collapse on the ground.\n"),
    write("When you wake up, first thing you see is a woman, she must be a Goddess. When you ask her name she turns out to be Calypso\n"),
    write("To leave this place, to return to the life you truly seek, you must craft a raft that can withstand Poseidon’s\n"),
    write("storms. Gather the resources of the island: wood, logs, rope and cloth, only then will you be able to return to home").

gather(Material) :- required_material(Material, RequiredAmount), holding(Material, RequiredAmount), !,
    format("You have enough ~w for the raft.\n", [Material]).
gather(Material) :- you_are_at(calypso_island), required_material(Material, _), !,
    required_material(Material, RequiredAmount),
    (holding(Material, CurrentAmount) -> true ; CurrentAmount = 0),
    NewAmount is CurrentAmount + 1,
    NewAmount =< RequiredAmount,
    retractall(holding(Material, CurrentAmount)),
    assert(holding(Material, NewAmount)),
    format("You have gathered ~w ~w(s).\n", [NewAmount, Material]),
    (holding(Material, RequiredAmount) ->
        format("You have enough ~w for the raft.\n", [Material])
    ; true),
    (has_all_materials ->
        write("You have gathered all necessary materials for the raft! You can start building it.\n")
    ; true).
gather(_) :-
    write("There's no need to gather anything like that now.").

has_all_materials :-
    forall(required_material(Material, Amount), holding(Material, Amount)).

build(_) :- \+ has_all_materials, !,
    write("You don't have enough materials to build the raft. Keep gathering.\n").
build(cloth) :- raft_step_completed(base), raft_step_completed(frame), raft_step_completed(binding), \+ raft_step_completed(mast), !,
    assert(raft_step_completed(mast)),
    write("You set up the mast. The raft is complete! You can now attempt to escape Calypso's Island.\n").
build(rope) :- raft_step_completed(base), raft_step_completed(frame), \+ raft_step_completed(binding), !,
    assert(raft_step_completed(binding)),
    write("You tie everything together with rope to stabilize the structure.\n").
build(logs) :- raft_step_completed(base), \+ raft_step_completed(frame), !,
    assert(raft_step_completed(frame)),
    write("You arrange the logs into a stable frame on top of the base.\n").
build(wood) :- \+ raft_step_completed(base), !,
    assert(raft_step_completed(base)),
    write("You lay the wooden base as the foundation of your raft.\n").
build(Material) :- required_material(Material, _), \+ raft_step_completed(mast), !,
    write("You have to build something else first.\n").
build(_) :-
    write("Nothing to build with that.").

escape :- !,
    raft_step_completed(mast),
    write("With your raft complete, you set out to sea, leaving Calypso's Island behind.\n"),
    retractall(you_are_at(_)), assert(you_are_at(ithaca)),
    look.
escape :-
    write("You cannot leave without completing the raft first.\n").
