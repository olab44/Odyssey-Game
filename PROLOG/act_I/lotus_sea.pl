:- multifile describe/1, talk/1.

describe(lotus_sea) :- !,
        write("There's solid land nearby - you notice the glowing light of a fire. It seems inviting.").

describe(lotus_island) :- !,
        write("The island is calm and serene, the whole atmospheare making you sleepy. You see a lake"),
        write("\nthat's surrounded by plain houses. The people milling around pay you no mind at all,"),
        write("\ntheir thoughts far away.\n"),
        write("\nThere's plenty of food on their tables - strange, glowing fruits, which you recognize as"),
        write("\nmind-numbing lotus. There's also an abundance of wine - surely the lotus-eaters wouldn't"),
        write("\nbe mad, were you to take a jug for yourself.").

talk(lotus-eaters) :- you_are_at(lotus_island), !,
        write("It takes a while to find someone present enough to talk to you. Most of the lotus-eaters"),
        write("\nare too far gone to even notice you. The talk itself doesn't amount to much, though."),
        write("\nThe old woman speaks of great dangeres up north, but she assures you everyting is far"),
        write("\neasier with wine.").
