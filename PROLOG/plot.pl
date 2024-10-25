/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

plot_start :- nl,
            write("The city of Troy has been seized, the war won. Ten years away from home are finally coming to"),
            write("\nan end. You're on your way to Ithaca now, hundreds of sea miles both behind and ahead of you."),
            write("\nThe problem's not the distance, you know very well, but the dangers that lie in between"),
            write("\n- you'll have to act smart to get back to your homeland.\n"),
            write("\nYou think of your wife. You think of your little boy, who's probably not so little anymore.\n"),
            write("\nFailure is not an option.\n"),
            write("\nWith trusted friends by your side - "), ansi_format([fg(blue)], "Eurylochus", []), write(" and "), ansi_format([fg(blue)], "Polites", []),
            write(" - there just might be hope for you \nand all the six hundred men under your command.\n"),
            write("\nYou should"), ansi_format([fg(magenta)], " talk ", []), write("to them,"), ansi_format([fg(magenta)], " look ", []),
            write("around or"), ansi_format([fg(magenta)], " sail ", []), write("to cross the sea and find your way home."),
            nl.


plot(ithaca_sea_storm) :- holding(wind_bag),
                    write("You guard the wind bag to the best of your abilities, but the need to sleep proves stronger. Your men open the bag."),
                    retract(you_are_at(ithaca_sea)), assert(you_are_at(circe_island)), !.
plot(ithaca_sea_storm) :- crew(X), X > 50,
                    write("You can't beat the force of nature. You lose a ship - you lose 50 men. The only safe direction now is south, towards the calmer open sea."),
                    Y is X - 50,
                    retract(crew(X)), assert(crew(Y)), !.
% plot(ithaca_sea_storm) :- death(storm).
