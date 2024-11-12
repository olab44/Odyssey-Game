:- multifile describe/1, plot/1, disembark/0, embark/0, talk/1, holding/1.

:- dynamic original_crew_count/1.
:- dynamic access_to_underworld/1.
:- dynamic crew_survived_sirens/1.
:- dynamic visited_underworld/1.
:- dynamic scylla_survival_rate/1.

describe(circe_sea) :- !,
    write("The waters here feel thick with enchantment, and Circe's island lies ominously ahead.\n").

describe(circe_island) :-
    visited_underworld(true), !,
    write("You step back onto Circe's island, feeling the weight of your journey to the Underworld lingering upon you.\n"),
    write("The familiar sights of her enchanted island are a strange comfort after the shadows of Hades.\n"),
    write("Circe greets you with a knowing look, sensing the trials you’ve endured and the wisdom you have gained.\n"),
    write("It’s clear she has more guidance to offer, should you seek her counsel.\n"),
    write("You may now "), ansi_format([fg(yellow)], "talk to Circe", []), write(" to ask for further instructions on your journey.\n").

describe(circe_island) :- !,
    write("After disembarking on Circe's island, you step into a lush, enchanted forest. The air is thick with mystery, and your instincts warn you of hidden dangers.\n"),
    write("To continue, you can "), ansi_format([fg(yellow)], "talk to Hermes", []), write(" or "), ansi_format([fg(yellow)], "plot(confront_circe)", []), write(" to confront the enchantress Circe.\n").

talk(hermes) :- you_are_at(circe_island), !,
    write("Hermes, appearing like a ghostly figure, approaches with wisdom and a gift.\n"),
    write("Hermes offers you a magical herb, saying it will protect you from Circe's spells. You may need it if you choose to confront her.\n"),
    assert(holding(magic_herb)).

plot(confront_circe) :-
    holding(magic_herb), !,
    crew(CurrentCrew),
    retractall(original_crew_count(_)),
    assert(original_crew_count(CurrentCrew)),
    write("Holding the magical herb, you feel its protective aura as you step into Circe's palace.\n"),
    write("You sense her spells failing against you.\n"),
    plot(confront_result).

plot(confront_circe) :-
    ansi_format([fg(red)], "GAME OVER: Without the protection of the magical herb, Circe’s spell overwhelms you, turning you into a pig, and you lose the game.\n", []),
    finish.

plot(confront_result) :-
    write("You now have a choice: will you "), ansi_format([fg(yellow)], "spare", []), write(" her life or "), ansi_format([fg(yellow)], "kill", []), write(" her?\n"),
    read(Choice),
    (Choice == spare ->
        (   original_crew_count(OriginalCrew) ->
            retract(crew(_)),
            assert(crew(OriginalCrew))
        ;   write("Warning: Original crew count not recorded. Restoring default crew.\n"),
            retract(crew(_)),
            assert(crew(600))
        ),
        write("You decide to spare Circe, who restores your crew to human form. They are relieved, and gratitude fills the air.\n"),
        plot(year_passed);
    Choice == kill ->
        ansi_format([fg(red)], "GAME OVER: You kill Circe, and in doing so, you lose the chance to undo the curse on your crew. You are left stranded, and your journey ends in failure.\n", []),
        finish;
    write("Invalid choice. You hesitate, and Circe takes advantage of your indecision. She casts a spell, and you and your crew are lost forever.\n"),
    finish).


plot(year_passed) :-
    write("Time passes; a full year slips by as Circe becomes your ally and lover. The comforts of the island nearly make you forget your quest.\n"),
    write("One day, your crew approaches, urging you to remember Ithaca.\n"),
    write("You can now "), ansi_format([fg(yellow)], "talk to crew", []), write(" to discuss the journey ahead.\n").

talk(crew) :- you_are_at(circe_island), crew(CurrentCrew), CurrentCrew > 0, !,
    write("Your crew gathers, their expressions serious. 'Captain,' they say, 'it’s time we resume our journey to Ithaca.'\n"),
    write("They remind you of the goal that has driven you across the seas, and their loyalty fills you with resolve.\n"),
    write("You can now "), ansi_format([fg(yellow)], "talk to circe", []), write(" to seek further guidance from her.\n").

talk(circe) :-
    you_are_at(circe_island),
    visited_underworld(true),
    crew(CurrentCrew), !,   
    write("Circe sees the weight of your journey and speaks again with wisdom:\n"),
    write("'Your next trial, Odysseus, is the Sirens' Sea, where their hypnotic singing lures sailors to destruction.\n"),
    write("You must have your crew plug their ears with beeswax to resist the sound. But you—remain unsealed and tied to the mast to hear their song.'\n\n"),
    write("'After passing the Sirens, you will face Scylla and Charybdis. I advise you to sail closer to Scylla; though she will claim 6% of your crew, Charybdis devours entire ships.'\n\n"),
    write("Decide now how many of your crew will assist me in gathering ingredients for a potion that could protect you against Scylla.\n"),
    write("This potion will be crucial if you choose to pass near Scylla, as it may shield you from becoming one of the 6% she devours.\n"),
    write("Each crew member represents a 1% chance of survival. The rest will collect beeswax to seal their ears.\n"),
    write("There are 100 hives, requiring 5 crew members per hive.\n"),
    write("Your crew now consists of "), crew(X), write(X), write(" brave warriors.\n"),
    write("Please enter the number of crew for the potion:\n"),
    read(PotionCrew),
    RemainingCrew is CurrentCrew - min(PotionCrew, 100),
    HivesCovered is RemainingCrew // 5,
    ProtectedCrew is round(CurrentCrew * min(HivesCovered, 100)/100),
    retractall(crew_lost_to_sirens(_)),
    retractall(crew_survived_sirens(_)),
    assert(crew_survived_sirens(ProtectedCrew)),
    write("Kirke confirms your plan:\n"),
    format("~w crew members will work on the potion, granting you a ~w% chance of survival.\n", [PotionCrew, PotionCrew]),
    format("~w crew members are protected.\n", [ProtectedCrew]),
    retractall(scylla_survival_rate(_)),
    assert(scylla_survival_rate(min(PotionCrew, 100)/100)),
    write("Those unprotected by wax will perish under the Sirens' spell.\n"),
    write("When ready, you may depart towards the Sirens' Sea.\n"),
    retractall(visited_underworld(_)),
    assert(visited_underworld(false)).

talk(circe) :- you_are_at(circe_island), crew(_), !,
    write("Circe nods in understanding, sensing your readiness to continue your journey.\n"),
    write("'If you wish to return to Ithaca,' she says, 'you must first venture to the Underworld, then directly come back to me. But heed my warning—avoid the north, for giants dwell there, and their strength is unmatched.'\n"),
    write("Circe hands you an empty bottle, saying it may prove useful later in your journey.\n"),
    assert(holding(empty_bottle)),
    assert(access_to_underworld(true)),
    write("With her guidance, you feel prepared to set sail once more.\n").

