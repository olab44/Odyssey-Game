:- multifile describe/1.
:- dynamic you_are_at/1.

describe(ithaca) :- !,
    write("At last, you set foot on Ithaca, your homeland.\n"),
    write("\nAfter all the trials, you've made it home, where every person on a street talks about\n"),
    write("the queen's challenge.\n"),
    write("\nBut that's a story for another day.\n"),
    victory.

victory :-
    end_message,
    finish.

end_message :-
    write("\nAfter years lost at sea, battles fought, and gods defied, you've finally reached Ithaca.\n"),
    write("You've proven that courage and loyalty can overcome even the wrath of gods.\n"),
    write("Welcome home, Odysseus.\n").
