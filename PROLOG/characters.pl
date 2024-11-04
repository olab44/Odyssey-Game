/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- dynamic crew/1.

crew(600).

character_at(crew, open_sea).
character_at(polyphemus_cave, polyphemus).
character_at(aeolus_island, aeolus).
character_at(lotus_island, lotus-eaters).

talk(Character) :- you_are_at(Location), character_at(Location, Character), !,
            describe_talk(Character).
talk(_) :-
            write("It's not a time nor place for a talk with someone who's busy - or someone who's not even there.").

describe_talk(crew) :- holding(wind-bag), !,
            write("TALKING TO THE CREW - KEEP YOUR FRIENDS CLOSE THEMED").
describe_talk(crew) :- visited(polyphemus), !,
            write("TALKING TO THE CREW - LUCK RUNS OUT THEMED").
describe_talk(crew) :-
            write("TALKING TO THE CREW - WATCH WHERE THE BIRDS FLY, HUNGER").
describe_talk(lotus-eaters) :-
            write("LOTUS EATERS HIGH TALK (WINE TIP)").
describe_talk(polyphemus) :-
            write("TALKING TO POLYPHEMUS - NAME DROP").
describe_talk(aeolus) :-
            write("AEOLUS TALK"),
            assert(object_at(aeolus_island, wind-bag)).
