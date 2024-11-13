/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- [act_I/open_sea], [act_I/ithaca_sea], [act_I/lotus_sea], [act_I/polyphemus_sea].
:- [act_II/circe_sea], [act_II/giants_sea], [act_II/scylla_charybdis], [act_II/sirens_sea], [act_II/sun_god_sea], [act_II/underworld_sea].
:- [act_III/calypso_island], [act_III/ithaca].
:- [exploration].

welcome :- nl,
        ansi_format([fg(green)], "---------------------- THE ODYSSEY : THE GAME ----------------------", []), nl,
        ansi_format([fg(yellow)], "                                                    _  _                ", []), nl,
        ansi_format([fg(yellow)], "                                                   ' \\/ '              ", []), nl,
        ansi_format([fg(yellow)], "   _  _                         <|                                      ", []), nl,
        ansi_format([fg(yellow)], "    \\/                  _'_______'___________'___                      ", []), nl,
        ansi_format([fg(yellow)], "                      /'                      //                        ", []), nl,
        ansi_format([fg(yellow)], "                    /'                       '/           _  _          ", []), nl,
        ansi_format([fg(yellow)], "                   |                       '/ \\            \\/         ", []), nl,
        ansi_format([fg(yellow)], "                   ||                      ||  \\                       ", []), nl,
        ansi_format([fg(yellow)], "                  /`|                      `\\   \\                     ", []), nl,
        ansi_format([fg(yellow)], "        _==_     /  \\\\                      `\\   \\                  ", []), nl,
        ansi_format([fg(yellow)], "       //  //   /    `\\                       \\\\  \\                 ", []), nl,
        ansi_format([fg(yellow)], "       \\\\      /       \\\\                      \\\\  \\             ", []), nl,
        ansi_format([fg(yellow)], "        \\ \\   /         `\\\\______________________\\  \\        ^%%  ", []), nl,
        ansi_format([fg(yellow)], "         `\\ \\/           /       | |             \\   \\/   ^%= ^%%=  ", []), nl,
        ansi_format([fg(yellow)], "           \\\\ \\_______ _/_____ __|_|__ ___________\\_/||   ^^     %^ ", []), nl,
        ansi_format([fg(yellow)], "            `\\\\|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_/|/         %=%    ", []), nl,
        ansi_format([fg(yellow)], " =_  %%!_   __\\    _______________                 /   __.%% !^%% _^   ", []), nl,
        ansi_format([fg(yellow)], "^^%%^=%^=^^^%%^^\\'/_)/_)_/_)__)/_)/)/)_)_'_'_'__//)/)/)/)%%=%^^^%^^    ", []), nl,
        ansi_format([fg(yellow)], "   =^=!%%=^^^!^^^!!^^^%%=%%!!!!^^^%%^^^!!%%%=^^^!!!%%!!%%%^^^^%^%       ", []), nl,
        ansi_format([fg(green)], "--------------------------------------------------------------------", []), nl.

instructions :-
        ansi_format([fg(green)], "--------------------------------------------------------------------", []), nl,
        write("AVAILABLE COMMANDS:\n"),
        write("In all acts, you can use:\n"),
        write("crew_count. : to check how many crew members are still alive\n"),
        write("- look. : To observe your surroundings.\n"),
        write("- talk(person): To interact with characters.\n"),
        write("- embark. : To board your ship\n"),
        write("- disembark. : To leave your ship and explore on land.\n\n"),
        write("- sail(direction). : to sail to another location\n"),
        write("In Act I, you can additionally use:\n"),
        write("- take(object). : To collect items you find.\n\n"),
        write("In Act III, you can additionally use:\n"),
        write("- gather(material). : To gather essential materials for crafting.\n"),
        write("- build(material). : To construct a raft or other items needed to progress.\n\n"),
        write("Good luck on your journey, brave Odysseus!\n"),
        ansi_format([fg(yellow)], "Use the 'start.' command to play the game or 'halt.' to close it.", []), nl,
        ansi_format([fg(green)], "--------------------------------------------------------------------", []), nl.

:- welcome, instructions.

start :-
        write("\nThe city of Troy has been seized, the war won. Ten years away from home are finally coming to"),
        write("\nan end. You're on your way to Ithaca now, hundreds of sea miles both behind and ahead of you."),
        write("\nThe problem's not the distance, you know very well, but the dangers that lie in between"),
        write("\n- you'll have to act smart to get back to your homeland.\n"),
        write("\nYou think of your wife. You think of your little boy, who's probably not so little anymore.\n"),
        write("\nFailure is not an option.\n"),
        write("\nWith trusted crew by your side, the six hundred men under your command, there just might be"),
        write("\nhope for you after all.\n"),
        write("\nYou should look around, talk to them while it's not busy or sail to cross the sea and reach home.\n"),
        write("You can also check the current size of your crew by typing crew_count.\n").

finish :- nl,
        ansi_format([fg(green)], "------------------------------ THE END -----------------------------", []), nl,
        ansi_format([fg(yellow)], "Thank you for playing! Close the game with the 'halt.' command.", []), nl,
        ansi_format([fg(green)], "--------------------------------------------------------------------", []), nl.
