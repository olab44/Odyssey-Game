:- dynamic holding/2, raft_step_completed/1.

you_are_at(calypso_island).

% Materials for the raft
required_material(wood, 5).     
required_material(logs, 5).      
required_material(rope, 8).      
required_material(mast, 1).      

describe(calypso_island) :-
    write("You stand on the shores of Ogygia, Calypso’s Island, a paradise forged by the gods but filled with the weight\n"),
    write("of years lost. The island itself is breathtaking—golden sands give way to groves of ancient trees, and wild\n"),
    write("flowers bloom in vivid colors, as if to distract you from the ache in your heart.\n\n"),
    write("Calypso, the nymph who claimed you, offers her love and a life of ease here. Her voice, soft as the waves, tempts\n"),
    write("you to forget your past, your journey, your home. And yet, in the quiet moments, your mind drifts to Penelope.\n"),
    write("The image of her—faithful, waiting, steadfast—cuts through the enchantments woven around you. You remember her\n"),
    write("eyes, her touch, and the promise of Ithaca. Calypso’s paradise is a prison when it keeps you from her.\n\n"),
    write("To leave this place, to return to the life you truly seek, you must craft a raft that can withstand Poseidon’s\n"),
    write("storms. Gather the resources of the island: wood from mighty oaks, logs from the forest’s heart, vines thick\n"),
    write("enough to bind your raft together, and a tall tree strong enough to serve as your mast.\n\n"),
    write("These will be your tools, your defiance against a love that seeks to keep you. Only when you’ve gathered every\n"),
    write("element—wood, logs, rope, and mast—will you have the means to defy Calypso’s hold and set sail toward home.\n").



gather(Material) :-
    you_are_at(calypso_island),
    required_material(Material, RequiredAmount),
    (holding(Material, CurrentAmount) -> true ; CurrentAmount = 0),
    NewAmount is CurrentAmount + 1,
    NewAmount =< RequiredAmount,
    retractall(holding(Material, CurrentAmount)),
    assert(holding(Material, NewAmount)),
    format("You have gathered ~w ~w(s).\n", [NewAmount, Material]).
gather(Material) :-
    required_material(Material, RequiredAmount),
    holding(Material, RequiredAmount),
    format("You have enough ~w for the raft.\n", [Material]).

has_all_materials :-
    forall(required_material(Material, Amount), holding(Material, Amount)).

% Raft construction sequence
build_raft :-
    raft_step_completed(base), raft_step_completed(frame), raft_step_completed(binding),
    \+ raft_step_completed(mast),
    assert(raft_step_completed(mast)),
    write("You set up the mast. The raft is complete!\n").
build_raft :-
    raft_step_completed(base), raft_step_completed(frame), \+ raft_step_completed(binding),
    assert(raft_step_completed(binding)),
    write("You tie everything together with rope to stabilize the structure.\n").
build_raft :-
    raft_step_completed(base), \+ raft_step_completed(frame),
    assert(raft_step_completed(frame)),
    write("You arrange the logs into a stable frame on top of the base.\n").
build_raft :-
    \+ raft_step_completed(base),
    assert(raft_step_completed(base)),
    write("You lay the wooden base as the foundation of your raft.\n").
build_raft :-
    has_all_materials,
    raft_step_completed(mast),
    write("Your raft is ready! You can now attempt to escape Calypso's Island.\n").
build_raft :-
    \+ has_all_materials,
    write("You don't have enough materials to build the raft. Keep gathering.\n").

% Attempt to escape
attempt_escape :-
    raft_step_completed(mast),
    write("With your raft complete, you set out to sea, leaving Calypso's Island behind.\n"),
    retract(you_are_at(calypso_island)),
    assert(you_are_at(open_sea)),
    look.
attempt_escape :-
    write("You cannot leave without completing the raft first.\n").
