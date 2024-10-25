/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- dynamic you_are_at/1, land/2, disembarked.

you_are_at(open_sea).
disembarked :- fail;

% MAP AND MOVEMENT CHART

sea_path(open_sea, north, ithaca_sea).
sea_path(open_sea, west, lotus_sea).
sea_path(ithaca_sea, south, open_sea).
sea_path(lotus_sea, north, polyphemus_sea).
sea_path(lotus_sea, east, open_sea).
sea_path(polyphemus_sea, east, ithaca_sea).
sea_path(polyphemus_sea, south, lotus_sea).

land(lotus_sea, lotus_island).
land(polyphemus_sea, polyphemus_cave).

% MOVEMENT

sail(_) :- disembarked, write("\nYou should embark on a ship first.\n"), !.
sail(Direction) :- you_are_at(Here), sail(Here, Direction).

sail(ithaca_sea, Direction) :- Direction \= south,
                        plot(ithaca_sea_storm), !.
sail(Here, Direction) :- sea_path(Here, Direction, There),
                        retract(you_are_at(Here)), assert(you_are_at(There)), look, !.
sail(_, _) :-
            write("\nYou set sail, but you either find nothing of note in that direction, or the way's impassable."),
            write("\nYou end up turning back.\n"),
            look.

disembark :- disembarked,
        write("\nYou're already on land.\n").
disembark :- you_are_at(Sea), land(Sea, Land),
        retract(you_are_at(Sea)), assert(you_are_at(Land)), assert(disembarked),
        look, !.
disembark :-
        write("\nThere's no solid land to disembark on.\n").

embark :- you_are_at(Land), land(Sea, Land),
        retract(you_are_at(Land)), assert(you_are_at(Sea)), retract(disembarked),
        look, !.
embark :-
        write("\nYou're already on a ship.\n").

% DESCRIPTIONS

look :- you_are_at(Here),
        describe(Here).

describe(open_sea) :- land(open_sea, aeolus_island),
                write("\nWhere once was nothing but water, now lies an island, floating on the waves. The blowing winds feel"),
                write("\ndifferent, too, and you can't stop the thought - the home of the wind god. It means hope, a lone chance"),
                write("\nof help, if only you"), ansi_format([fg(magenta)], " disembark ", []), write("and beg.\n"),
                !.
describe(open_sea) :-
                write("\nOpen sea stretches in all directions, no land in sight. Were you to believe your charts,"),
                write("\nIthaca lies north.\n").


describe(ithaca_sea) :- holding(wind_bag),
                write("\nThe sky is clear. You see Ithaca - your destination, your kingdom, your home - on the horizon.\n"),
                !.
describe(ithaca_sea) :-
                write("\nYour way is blocked by giant waves and giant storms. You should turn back south, towards calmer waters.\n").

describe(lotus_sea) :- write("The land of Lotus Eaters.").
describe(lotus_island) :- write("Lotus land - wine can be found here.").
describe(polyphemus_sea) :- write("Polyphemus' cave nearby.").
describe(polyphemus_cave) :- write("Polyphemus' cave - easier with wine - certain god's curiosity piqued").
describe(circe_island) :- write("Circe's island - lots of men dead").
