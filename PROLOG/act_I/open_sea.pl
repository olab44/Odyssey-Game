:- multifile you_are_at/1, describe/1, talk/1, land/2, object_at/2.

describe(open_sea) :- land(open_sea, aeolus_island), !,
            write("Where once was nothing but water, now lies an island, floating on the waves. The blowing winds feel"),
            write("\ndifferent, too, and you can't stop the thought - the home of the wind god. It means hope, a lone chance"),
            write("\nof getting help against the storm, if only you"), ansi_format([fg(magenta)], " disembark ", []), write("and beg.\n").
describe(open_sea) :-
            write("Open sea stretches in all directions, no land in sight. Were you to believe your charts,"),
            write("\nIthaca lies north. North, where you can see a mass of dark clouds covering the sky.\n").


talk(crew) :- you_are_at(open_sea), holding(wind-bag), !,
            write("TALKING TO THE CREW - KEEP YOUR FRIENDS CLOSE THEMED").
talk(crew) :- you_are_at(open_sea), land(open_sea, aeolus_island), !,
            write("TALKING TO THE CREW - LUCK RUNS OUT THEMED").
talk(crew) :- you_are_at(open_sea), !,
            write("TALKING TO THE CREW - WATCH WHERE THE BIRDS FLY, HUNGER").

talk(aeolus) :- you_are_at(aeolus_island), !,
            write("AEOLUS TALK"),
            assert(object_at(aeolus_island, wind-bag)).


describe(aeolus_island) :- write("The home of the wind god.").