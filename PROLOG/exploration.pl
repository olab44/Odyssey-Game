/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- dynamic you_are_at/1, land/2, disembarked/0.
:- dynamic holding/1, object_at/2.
:- dynamic crew/1.

:- multifile you_are_at/1, land/2, sail/1, disembark/0, embark/0, disembarked/0, look/0, talk/1, take/1, holding/1, crew/1.

you_are_at(open_sea).
object_at(lotus_island, wine).
crew(600).

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
sail(Direction) :- you_are_at(Here), sea_path(Here, Direction, There), !,
        retract(you_are_at(Here)), assert(you_are_at(There)), look.
sail(_) :-
        write("You set sail, but you either find nothing of note in that direction, or the way's impassable."),
        write("\nYou end up turning back.\n"),
        look.

disembark :- disembarked, !,
        write("You're already on land.\n").
disembark :- you_are_at(Sea), land(Sea, Land), !,
        retract(you_are_at(Sea)), assert(you_are_at(Land)), assert(disembarked),
        look.
disembark :-
        write("There's no solid land to disembark on.\n").

embark :- you_are_at(Land), land(Sea, Land), !,
        retract(you_are_at(Land)), assert(you_are_at(Sea)), retract(disembarked),
        look.
embark :-
        write("You're already on a ship.\n").

% ACTIONS

look :- you_are_at(Here),
        describe(Here).

talk(_) :-
        write("It's not a time nor place for a talk with someone who's busy - or someone who's not even there.").

take(Object) :- holding(Object), !,
        write("You already have that.\n").
take(Object) :- you_are_at(Location), object_at(Location, Object), !,
        format("You pick up the ~w.", [Object]),
        assert(holding(Object)).
take(_) :-
        write("You can't find anything like that here.\n").

% HELPER LOGIC

describe(_) :-
        write("Nothing. A whole lot of nothing. Something probably went wrong.").

crew_death(X) :- crew(Old),
        New is Old - X,
        retract(crew(Old)), assert(crew(New)).