:- multifile describe/1, disembark/0, embark/0, talk/1, holding/1, crew_death/1, show_map/0.
:- dynamic longer_stay/1, access_to_underworld/1, visited_underworld/1, on_bank1/1, on_bank2/1.

describe(underworld_sea) :- !,
    write("You have reached the mysterious and eerie waters of the Underworld.\n").

describe(underworld) :- !,
	write("You sense an opportunity to "), ansi_format([fg(yellow)], "talk", []), write(" to Charon, the ferryman who will guide you across the River Styx.\n").

talk(charon) :-
    you_are_at(underworld), !,
    write("Charon, the ferryman, explains the rules of the River Styx.\n"),
    write("He says he will help you cross, but only if you solve the puzzle.\n"),
    write("He can only take one thing at a time: the crew, the wine, or Cerber, the three-headed dog.\n"),
    write("However, if the crew is left with the wine or Cerber, disaster will strike.\n"),
    write("You must cross the river in a specific order to avoid tragedy.\n"),
    write("To start, Charon will take the crew across.\n"),
    ansi_format([fg(yellow)], "start_ferry_puzzle\n", []),
    write("You can either take the wine or Cerber next.\n"),
    write("The following commands are available to you:\n"),
    write("1. 'ferry(X)' - to have Charon take X across the river.\n"),
    write("2. 'return(X)' - to have Charon return with X.\n"),
    write("X can be 'crew', 'wine', 'cerber', or 'none'.\n"),
    write("Please make sure to follow the correct order to avoid losing crew members.\n").


start_ferry_puzzle :-
    retractall(on_bank1(_)),
    retractall(on_bank2(_)),
    assert(on_bank1(crew)),
    assert(on_bank1(wine)),
    assert(on_bank1(cerber)),
    assert(on_bank1(charon)),
    write("The puzzle has started! Charon will help you cross the River Styx.\n"),
    write("You can issue "), ansi_format([fg(yellow)], "ferry", []),  write(" and "),
	ansi_format([fg(yellow)], "return", []), write(" commands to move the crew, wine, cerber or none.\n"),
    write("Make sure to follow the correct order to avoid disaster!\n").

ferry(Item) :-
    you_are_at(underworld),
    on_bank1(charon),
    (Item \= none -> 
        (on_bank1(Item) ->
            move_across(Item),
            retract(on_bank1(charon)), assert(on_bank2(charon)),
            write(Item), write(" has been ferried across.\n");
        write("Cannot ferry "), write(Item), write(". Check if it's on the correct side of the river.\n")
        );
    retract(on_bank1(charon)), 
    assert(on_bank2(charon)),
    write("Charon crosses without taking anything.\n")
    ),
    (check_ferry_state -> true; true).

return(Item) :-
    you_are_at(underworld),
    on_bank2(charon),
    (Item \= none ->
        (on_bank2(Item) ->
            move_across(Item),
            retract(on_bank2(charon)), assert(on_bank1(charon)),
            write(Item), write(" has been returned.\n");
        write("Cannot return "), write(Item), write(". Check if it's on the correct side of the river.\n")
        );
    retract(on_bank2(charon)),
    assert(on_bank1(charon)),
    write("Charon crosses back without taking anything.\n")
    ),
    (check_ferry_state -> true; true).

move_across(Item) :-
    (Item \= none ->
        (on_bank1(Item) -> retract(on_bank1(Item)), assert(on_bank2(Item)); 
         retract(on_bank2(Item)), assert(on_bank1(Item)));
     true).

check_ferry_state :-
    (on_bank1(wine), on_bank1(crew), \+ on_bank1(charon)) ->
        write("Some from your crew drowned in the River Styx after drinking the wine.\n"), game_over;
    (on_bank1(crew), on_bank1(cerber), \+ on_bank1(charon)) ->
        write("Cerberus attacked the crew.\n"), game_over;
    (on_bank2(wine), on_bank2(crew), \+ on_bank2(charon)) ->
        write("Some from crew drowned in the River Styx after drinking the wine.\n"), game_over;
    (on_bank2(crew), on_bank2(cerber), \+ on_bank2(charon)) ->
        write("Cerberus attacked the crew.\n"), game_over;

    (on_bank2(crew), on_bank2(wine), on_bank2(cerber), on_bank2(charon)) ->
        write("Congratulations! You have successfully ferried everything across the River Styx.\n"),
        write("Charon opens the gates to Hades, and you may now speak with Tiresias.\n"),
        assert(access_to_underworld(true)),
        true.

game_over :-
	crew(CurrentCrew),
    Loss is max(20, round(0.2 * CurrentCrew)),
    format("You crew reduced by ~d members.\n", [Loss]),
    crew_death(Loss),
    write("Nevertheless, Charon opens the gates to Hades, and you may now speak with Tiresias.\n"),
	assert(access_to_underworld(true)),
	true.


talk(tiresias) :-
	you_are_at(underworld), !,
    write("Tiresias, the prophet, speaks to you with grave solemnity:\n"),
    write("I see your future, Odysseus. You may reach Ithaca, but the path will be fraught with hardships.\n"),
    write("The island of Helios and its sacred cattle will be your doom if you dare approach them.\n"),
    write("The fates have already sealed the tragic death of your mother, which you have yet to learn.\n"),
	write("The ferryman urges you to hurry, though he offers you the chance to "), 
	ansi_format([fg(yellow)], "talk", []), 
	write(" to one of three shades: your mother, Achilles, or Agamemnon.\nBut choose wisely, for you can only speak with one of them.\n"),
	retractall(visited_underworld(_)),
    assert(visited_underworld(true)).


talk(mother) :-
    you_are_at(underworld), !,
    write("A spectral figure appears before you—your mother, Anticleia.\n"),
    write("She tells you with a sorrowful voice, 'My dear son, I died of grief and longing for you.'\n"),
    write("The days of our separation broke my heart. I wish I could have seen you return to Ithaca.\n"),
    write("With a soft sigh, she hands you a charm—a Charybdis Lure, a precious item that may one day save you from the whirlpool.\n"),
    assert(holding(charybdis_lure)),
    (   longer_stay(false) -> 
        write("Now you must return to your ship. Further conversations are no longer possible.\n"),
        retractall(access_to_underworld(_)), assert(access_to_underworld(false)), 
        retractall(you_are_at(_)), assert(you_are_at(underworld_sea)), retract(disembarked)
    ;   true
    ),
    retractall(longer_stay(_)), assert(longer_stay(false)).

talk(agamemnon) :-
    you_are_at(underworld), !,
    write("Agamemnon, the great king of Mycenae, speaks to you from the shadows:\n"),
    write("'I was betrayed by my wife, Clytemnestra, and murdered upon my return to Mycenae. But you, Odysseus, will fare better, I hope.'\n"),
    write("He offers you a map of the seas ahead—knowledge of the islands that await you in Act II.\n"),
    write("Type "), ansi_format([fg(yellow)], "show_map", []), write(" to look at it.\n"),
    (   longer_stay(false) -> 
        write("Now you must return to your ship. Further conversations are no longer possible.\n"),
        retractall(access_to_underworld(_)), assert(access_to_underworld(false)), 
        retractall(you_are_at(_)), assert(you_are_at(underworld_sea)), retract(disembarked)
    ;   true
    ),
    retractall(longer_stay(_)), assert(longer_stay(false)).

talk(achilles) :-
    you_are_at(underworld), !,
    write("Achilles, the great warrior, speaks to you with fiery intensity:\n"),
    write("'Odysseus, your journey is long, and I know the struggles you face. The gods play cruel games with you.\n"),
    write("But you must know, I feel no peace here in the Underworld. The thought of my death haunts me still.'\n"),
    write("Achilles decides to speak to Charon to extend your stay. You now have time to speak to both your mother and Agamemnon for any wisdom they can provide.\n"),
    retractall(longer_stay(_)), assert(longer_stay(true)).

