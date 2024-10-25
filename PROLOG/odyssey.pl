/* PARP 24Z                                           */
/* THE ODYSSEY : THE GAME                             */
/* Aleksandra Buczma, Zofia Jasina, Marcin Bagnowski  */

:- [map], [objects], [plot], [characters].

welcome :- nl,
        ansi_format([fg(green)], '---------------------- THE ODYSSEY : THE GAME ----------------------', []), nl,
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
        ansi_format([fg(green)], '--------------------------------------------------------------------', []), nl.

instructions :-
        ansi_format([fg(green)], '--------------------------------------------------------------------', []), nl,
        write("Add instructions later"), nl,
        ansi_format([fg(yellow)], 'Use "start." command to play the game.', []), nl,
        ansi_format([fg(green)], '--------------------------------------------------------------------', []), nl.

:- welcome, instructions.

start :- plot_start.

finish :- nl,
        ansi_format([fg(green)], '------------------------------ THE END -----------------------------', []), nl,
        ansi_format([fg(yellow)], 'Thank you for playing! Close the game with the "halt." command.', []), nl,
        ansi_format([fg(green)], '--------------------------------------------------------------------', []), nl.
