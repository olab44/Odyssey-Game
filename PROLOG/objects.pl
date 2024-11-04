/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- dynamic holding/1, object_at/2.

object_at(lotus_island, wine).

take(Object) :- holding(Object),
            write("\nYou already have that.\n"),
            !.

take(Object) :- you_are_at(Location), object_at(Location, Object), !,
            assert(holding(Object)).

take(_) :-
            write("\nYou can't find anything like that here.\n").
