:- multifile describe/1, sail/1.
:- dynamic scylla_survival_rate/1, crew/1, holding/1, crew_death/1.

describe(scylla_charybdis_sea) :- !,
    write("The sea grows treacherous as you approach the domain of Scylla and Charybdis.\n"),
    write("To your left, you see Scylla's ominous cliffs, while Charybdis churns the water violently to your right.\n"),
    write("You must choose which path to take to continue.\n"),
    write("Type "), ansi_format([fg(yellow)], "sail_scylla", []), write(" to navigate past Scylla, or "),
    ansi_format([fg(yellow)], "sail_charybdis", []), write(" to risk the waters near Charybdis.\n").

sail_scylla :-
    scylla_survival_rate(SurvivalRate),
    random(0.0, 1.0, Chance),
    (Chance > SurvivalRate ->
        write("Scylla strikes with terrifying speed, catching you off guard...\n"),
        ansi_format([fg(red)], "GAME OVER: You have been taken by Scylla.\n", []),
        finish
    ;   
        write("You sail past Scylla successfully, but she manages to claim some of your crew.\n"),
		crew(CurrentCrew),
		Loss is max(6, round(0.06 * CurrentCrew)),
		crew_death(Loss),
        format("Scylla devours ~w of your crew members as you pass.\n", [Loss]),
        proceed_to_sun_god_island
    ).

sail_charybdis :-
    (holding(charybdis_lure) ->
        write("Using the mysterious Charybdis Lure, you safely pass by the churning whirlpool without incident.\n"),
        proceed_to_sun_god_island
    ;
        write("The whirlpool pulls your ship into its deadly currents, engulfing you and your entire crew...\n"),
        ansi_format([fg(red)], "GAME OVER: Your ship and crew are lost to Charybdis.\n", []),
        finish
    ).

proceed_to_sun_god_island :-
    write("After surviving the perilous pass, a fierce storm catches you off guard.\n"),
    write("The raging waves drive your ship eastward, and you find yourselves on the shores of the Island of the Sun God.\n"),
    sail(east).