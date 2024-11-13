:- multifile describe/1, sail/1, plot/1.
:- dynamic potion_recipe/1, crew/1, holding/1, crew_death/1.

describe(sun_god_sea) :- !,
    write("A violent storm catches your ship, forcing you to seek refuge on a nearby island.\n"),
    write("This is the sacred island of the Sun God, Helios, where his holy cattle roam.\n").

describe(sun_god_island) :- !,
    write("You disembark onto the island, hoping the storm will soon pass.\n"),
    write("Unfortunately, the storm shows no sign of stopping, and you will need to stay here longer.\n"),
    write("Your food supplies are exhausted, and the crew is looking to you for guidance.\n"),
    write("You had better "), ansi_format([fg(yellow)], "talk", []), write(" to them.\n").

talk(crew) :-
    you_are_at(sun_god_island), !,
    write("Your crew gathers around, their faces pale with hunger.\n"),
    write("One of them speaks up: 'Captain, we cannot last without food. These cattle are our only chance.'\n"),
    write("You remember Tiresias's warning not to eat the sacred cattle of Helios.\n"),
    write("Make your choice: type "), ansi_format([fg(yellow)], "do_not_eat_cattle", []), write(" to let your crew starve, or "),
    ansi_format([fg(yellow)], "eat_cattle", []), write(" to eat the cattle and risk the wrath of the gods.\n").

do_not_eat_cattle :-
    write("You stand firm in your decision: 'We will not eat the cattle of the Sun God.'\n"),
    write("The crew protests but ultimately obeys. Over the next days, hunger claims the lives of 50 members.\n"),
    crew(CurrentCrew),
    crew_death(50).

eat_cattle :-
    write("Reluctantly, you allow the crew to slaughter the sacred cattle of Helios.\n"),
    write("The feast restores their strength, but you sense a dark omen in the air.\n"),
    (   potion_recipe(true) ->
        write("As you consume the cattle, you find a special vial containing the blood of Helios’s sacred beast.\n"),
        write("This is the final ingredient needed to complete the elixir of strength.\n"),
        write("Type "), ansi_format([fg(yellow)], "complete_elixir", []), write(" to obtain it.\n"),
        assert(holding(helios_blood))
    ;   true
    ),
    write("The storm miraculously ceases. You may now sail onwards.\n"),
    write("Use command "), ansi_format([fg(yellow)], "plot(continue_journey)", []), write(" to move further and sail.\n").


complete_elixir :-
    holding(empty_bottle),
    holding(helios_blood),
    write("Using the empty bottle and the blood of Helios’s cattle, you complete the elixir of strength.\n"),
    retractall(holding(empty_bottle)),
    retractall(holding(helios_blood)),
    assert(holding(strength_elixir)),
    write("You now possess the Elixir of Strength, a potent potion that may save your life.\n").

plot(continue_journey) :-
    you_are_at(sun_god_island),
    (   holding(strength_elixir) ->
        write("As you leave the island, a thunderous voice echoes: 'For your desecration, you shall be punished!'\n"),
        write("The sea rages as a powerful storm descends upon your ship.\n"),
        write("But with the Elixir of Strength, you manage to survive the ordeal and press on to the next challenge.\n\n"),
        retract(you_are_at(_)), assert(you_are_at(calypso_island)), assert(disembarked), look
    ;   write("As you sail eastward, the wrath of the gods descends upon you.\n"),
        write("A powerful storm engulfs your ship, smashing it to pieces.\n"),
        write("If only you had something, a magic spell or a potion, to give you strength...\n"),
        ansi_format([fg(red)], "GAME OVER: You have perished at sea due to the wrath of the gods.\n", []),
        finish
    ).