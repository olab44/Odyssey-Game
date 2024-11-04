/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- dynamic visited/1.

plot_start :-
            write("\nThe city of Troy has been seized, the war won. Ten years away from home are finally coming to"),
            write("\nan end. You're on your way to Ithaca now, hundreds of sea miles both behind and ahead of you."),
            write("\nThe problem's not the distance, you know very well, but the dangers that lie in between"),
            write("\n- you'll have to act smart to get back to your homeland.\n"),
            write("\nYou think of your wife. You think of your little boy, who's probably not so little anymore.\n"),
            write("\nFailure is not an option.\n"),
            write("\nWith trusted"), ansi_format([fg(blue)], " crew ", []), write("by your side, the six hundred men under your command, there just might be"),
            write("\nhope for you after all.\n"),
            write("\nYou should"), ansi_format([fg(magenta)], " look ", []), write("around,"), ansi_format([fg(magenta)], " talk ", []),
            write("to them while it's not busy or"), ansi_format([fg(magenta)], " sail ", []), write("to cross the sea and find your way home.\n").

plot(ithaca_sea_storm) :- holding(wind-bag), !,
            write("\nYou guard the wind bag to the best of your abilities, but the need to sleep proves stronger."),
            write("\nYour men open the bag."),
            retract(you_are_at(ithaca_sea)), assert(you_are_at(circe_island)).
plot(ithaca_sea_storm) :- crew(X), X > 150, !,
            write("\nIt turns out you can't beat the force of nature that easily. You lose ships - the screams of over"),
            write("\na hundred men are drowned out by the storm as they disappear below the waves. The only safe direction"),
            write("\nnow is towards the calmer open sea - but away from your destination."),
            Y is X - 150,
            retract(crew(X)), assert(crew(Y)).
plot(ithaca_sea_storm) :-
            write("\nThe storm is your final fight, the only thing still blocking your way home - and so you're ready"),
            write("\nto risk it all, look for the way through even when there seems to be none.\n"),
            write("\nYou try your best...\n"),
            write("\nIt's just not enough.\n"),
            write("\nYour ship is the last one from the fleet to fall victim to the crashing waves.\n"),
            finish.

plot(meet_polyphemus) :- holding(wine), !,
            write("BETTER CYCLPOS MEETING VERSION"),
            crew(X), Y is X - 6,
            retract(crew(X)), assert(crew(Y)).
plot(meet_polyphemus) :-
            write("WORSE CYCLOPS MEETING VERSION"),
            crew(X), Y is X - 44,
            retract(crew(X)), assert(crew(Y)).
plot(leave_polyphemus) :- !,
            write("\nYou manage to embark on a ship and leave the Cyclops' cave behind. As you do, you can't shake"),
            write("\naway the feeling of being watched, but the eyes are not only those of your men nor the foes you've"),
            write("\nescaped from - the gods have taken an intrest in your actions. Something in the air has changed."),
            assert(visited(polyphemus)), assert(land(open_sea, aeolus_island)).
