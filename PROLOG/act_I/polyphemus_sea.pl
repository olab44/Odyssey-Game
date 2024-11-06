:- multifile you_are_at/1, disembark/0, describe/1, talk/1, plot/1, embark/0, crew/1, crew_death/1, holding/1, land/1.

disembark :- you_are_at(polyphemus_sea), land(open_sea, aeolus_island), !,
            write("Going back to the Cyclops' cave after all that has happened is a suicide. You crew knows it"),
            write("\nand refuses to risk it. You should know better, too.\n").
disembark :- you_are_at(polyphemus_sea),
            plot(meet_polyphemus), nl, fail.

describe(polyphemus_sea) :- write("Polyphemus' cave nearby.").
describe(polyphemus_cave) :- write("Polyphemus' cave - easier with wine - certain god's curiosity piqued").

plot(meet_polyphemus) :- holding(wine), !,
            write("BETTER CYCLPOS MEETING VERSION"),
            crew_death(6).
plot(meet_polyphemus) :-
            write("WORSE CYCLOPS MEETING VERSION"),
            crew_death(44).

embark :- you_are_at(polyphemus_cave),
            plot(leave_polyphemus), nl, fail.

plot(leave_polyphemus) :- !,
            write("\nYou manage to embark on a ship and leave the Cyclops' cave behind. As you do, you can't shake"),
            write("\naway the feeling of being watched, but the eyes are not only those of your men nor the foes you've"),
            write("\nescaped from - the gods have taken an intrest in your actions. Something in the air has changed."),
            assert(visited(polyphemus)), assert(land(open_sea, aeolus_island)).

talk(polyphemus) :- you_are_at(polyphemus_cave), !,
            write("TALKING TO POLYPHEMUS - NAME DROP").
