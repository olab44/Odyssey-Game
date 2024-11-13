/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- dynamic you_are_at/1, land/2, disembarked/0.
:- dynamic holding/1, object_at/2.
:- dynamic crew/1.
:- dynamic original_crew_count/1.
:- dynamic access_to_underworld/1 , longer_stay/1.
:- dynamic crew_survived_sirens/1.
:- dynamic potion_recipe/1.
:- dynamic scylla_survival_rate/1.

:- multifile you_are_at/1, land/2, sail/1, disembark/0, embark/0, disembarked/0, look/0, talk/1, take/1, holding/1, crew/1.
:- multifile crew_count/0, crew_death/1, check_game_loss/0.
:- multifile show_map/0.

you_are_at(open_sea).
object_at(lotus_island, wine).
crew(600).
access_to_underworld(false).
longer_stay(false).
visited_underworld(false).
crew_survived_sirens(0).
potion_recipe(false).
scylla_survival_rate(0).


% MAP CHART

% ACT I
sea_path(open_sea, north, ithaca_sea).
sea_path(open_sea, west, lotus_sea).
sea_path(ithaca_sea, south, open_sea).
sea_path(lotus_sea, north, polyphemus_sea).
sea_path(lotus_sea, east, open_sea).
sea_path(polyphemus_sea, east, ithaca_sea).
sea_path(polyphemus_sea, south, lotus_sea).

% ACT II
sea_path(circe_sea, north, giants_sea).
sea_path(circe_sea, east, sirens_sea).
sea_path(underworld_sea, north, giants_sea).
sea_path(underworld_sea, east, circe_sea).
sea_path(sirens_sea, north, giants_sea).
sea_path(sirens_sea, south, scylla_charybdis_sea).
sea_path(giants_sea, south, circe_sea).
sea_path(scylla_charybdis_sea, east, sun_god_sea).
sea_path(sun_god_sea, south, calypso_island).

sea_path(circe_sea, west, underworld_sea) :-
    access_to_underworld(true).

land(lotus_sea, lotus_island).
land(polyphemus_sea, polyphemus_cave).

land(circe_sea, circe_island).
land(sun_god_sea, sun_god_island).
land(underworld_sea, underworld) :-
    access_to_underworld(true).

% MOVEMENT

sail(act_II) :- !,
        retractall(you_are_at(_)), assert(you_are_at(circe_sea)).
sail(act_III) :- !,
        retractall(you_are_at(_)), assert(you_are_at(calypso_island)),
        assert(disembarked).
sail(_) :- disembarked, !,
        write("You should embark on a ship first.\n").
sail(Direction) :- you_are_at(Here), sea_path(Here, Direction, There), !,
        retract(you_are_at(Here)), assert(you_are_at(There)), look.
sail(_) :-
        write("You set sail, but you either find nothing of note in that direction, or the way's impassable."),
        write("\nYou end up turning back.\n"),
        nl, look.

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
        write("You're already on a ship or there's no ship at all.\n").

unlock_underworld_access :-
    retractall(access_to_underworld(_)),
    assert(access_to_underworld(true)).

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

show_map :-
        write("Possible moves:\n"),
        write("circe_sea, north, giants_sea\n"),
        write("circe_sea, west, underworld_sea\n"),
        write("circe_sea, east, sirens_sea\n"),
        write("underworld_sea, north, giants_sea\n"),
        write("underworld_sea, east, circe_sea\n"),
        write("sirens_sea, north, giants_sea\n"),
        write("sirens_sea, south, scylla_charybdis_sea\n"),
        write("giants_sea, south, circe_sea\n"),
        write("scylla_charybdis_sea, east, sun_god_sea\n"),
        write("sun_god_sea, south, calypso_sea\n\n"),
        write("Allowed disembarks:\n"),
        write("circe_sea, circe_islan\n"),
        write("underworld_sea, underworld\n"),
        write("sun_god_sea, sun_god_island\n").

% HELPER LOGIC

describe(_) :-
        write("Nothing. A whole lot of nothing. Something probably went wrong.").

check_game_loss :- crew(0), !,
        ansi_format([fg(red)], "All crew members have perished. You have lost the game.\n", []),
        finish,
        halt.
check_game_loss.

% CREW MANAGEMENT

crew_death(X) :- crew(Old),
        New is max(Old - X, 0),
        retract(crew(Old)), assert(crew(New)),
        check_game_loss.

crew_count :- crew(-1), !,
        ansi_format([fg(red)], "Your crew has been turned into pigs. You must break the spell to restore them.\n", []).
crew_count :-
        crew(Count), format("You have ~w crew members remaining.\n", [Count]).

store_original_crew_count :-
    crew(CurrentCrew),
    \+ original_crew_count(_),
    assert(original_crew_count(CurrentCrew)).

