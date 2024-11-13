multifile you_are_at/1.

describe(ithaca) :-
    write("At last, you set foot on Ithaca, your homeland.\n"),
    write("After all the trials, you've made it home.\n").

victory :-
    you_are_at(ithaca),
    congratulations_message,
    end_message,
    finish.

congratulations_message :-
    ansi_format([fg(yellow)], 'Congratulations! You have completed The Odyssey and returned home.', []), nl.

end_message :-
    write("\nAfter years lost at sea, battles fought, and gods defied, you've finally reached Ithaca.\n"),
    write("Your heart swells as you take in the familiar sight of your homeland. The journey has changed you,\n"),
    write("left its scars, but it has also strengthened your resolve.\n"),
    write("You’ve proven that courage and loyalty can overcome even the wrath of gods.\n"),
    write("Welcome home, Odysseus. Your story will echo through time as a testament to enduring hope.\n\n").
