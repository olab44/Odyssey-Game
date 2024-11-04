/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- dynamic you_are_at/1, land/2, disembarked.

you_are_at(open_sea).

% MAP CHART

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

sail(_) :- disembarked, !,
            write("You should embark on a ship first.\n").
sail(Direction) :- you_are_at(ithaca_sea), Direction \= south, !,
            plot(ithaca_sea_storm).
sail(Direction) :- you_are_at(Here), sea_path(Here, Direction, There), !,
            retract(you_are_at(Here)), assert(you_are_at(There)), look.
sail(_) :-
            write("You set sail, but you either find nothing of note in that direction, or the way's impassable."),
            write("\nYou end up turning back.\n"),
            look.

disembark :- disembarked, !,
            write("You're already on land.\n").
disembark :- you_are_at(polyphemus_sea), visited(polyphemus), !,
            write("Going back to the Cyclops' cave after all that has happened is a suicide. You crew knows it"),
            write("\nand refuses to risk it. You should know better, too.\n").
disembark :- you_are_at(polyphemus_sea),
            plot(meet_polyphemus), nl, fail.
disembark :- you_are_at(Sea), land(Sea, Land), !,
            retract(you_are_at(Sea)), assert(you_are_at(Land)), assert(disembarked),
            look.
disembark :-
            write("There's no solid land to disembark on.\n").

embark :- you_are_at(polyphemus_cave),
            plot(leave_polyphemus), nl, fail.
embark :- you_are_at(Land), land(Sea, Land), !,
            retract(you_are_at(Land)), assert(you_are_at(Sea)), retract(disembarked),
            look.
embark :-
            write("You're already on a ship.\n").

% DESCRIPTIONS

look :- you_are_at(Here),
            nl, describe(Here).

describe(open_sea) :- land(open_sea, aeolus_island), !,
            write("Where once was nothing but water, now lies an island, floating on the waves. The blowing winds feel"),
            write("\ndifferent, too, and you can't stop the thought - the home of the wind god. It means hope, a lone chance"),
            write("\nof getting help against the storm, if only you"), ansi_format([fg(magenta)], " disembark ", []), write("and beg.\n").
describe(open_sea) :-
            write("Open sea stretches in all directions, no land in sight. Were you to believe your charts,"),
            write("\nIthaca lies north. North, where you can see a mass of dark clouds covering the sky.\n").


describe(ithaca_sea) :- holding(wind-bag), !,
            write("The sky is clear, the water smooth - no storm, no tidal wave. You see Ithaca - your destination, your"),
            write("\nkingdom, your home - on the horizon. You can already feel the ghost of your wife's embrace.").
describe(ithaca_sea) :-
            write("Your way is blocked by giant waves and giant storms. You should probably turn back south, towards calmer"),
            write("\nwaters - but Ithaca's never been closer in the last ten years and the way home is through.").

describe(lotus_sea) :- write("The land of Lotus Eaters.").
describe(lotus_island) :- write("Lotus land - wine can be found here.").
describe(polyphemus_sea) :- write("Polyphemus' cave nearby.").
describe(polyphemus_cave) :- write("Polyphemus' cave - easier with wine - certain god's curiosity piqued").
describe(aeolus_island) :- write("The home of the wind god.").

describe(circe_island) :- write("Circe's island - lots of men dead").
