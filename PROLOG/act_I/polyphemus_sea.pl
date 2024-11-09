:- multifile disembark/0, describe/1, plot/1, embark/0.

describe(polyphemus_sea) :- !,
        write("You're not far from an island, rugged shoreline making way to green fields and rocky mountains"),
        write("\nfull of caves. One cave in particular looks easy to reach.").

disembark :- you_are_at(polyphemus_sea), land(open_sea, aeolus_island), !,
        write("Going back to the Cyclops' cave after all that has happened is a suicide. You crew knows it"),
        write("\nand refuses to risk it. You should know better, too.\n").
disembark :- you_are_at(polyphemus_sea), !,
        write("At the entrance of a cave, you find a herd of sheep - food much tastier than anything you"),
        write("\nhave left from your supplies. Some of your men stay behind, while the rest of you ventures"),
        write("\nonward to search the tunnels.\n"),
        retract(you_are_at(polyphemus_sea)), assert(you_are_at(polyphemus_cave)), assert(disembarked),
        nl, plot(meet_polyphemus).

plot(meet_polyphemus) :- !,
        write("You walk a long while, deep into the cave, when deep voice echoes through the darkness.\n"),
        write("\n'Who are you? What are you doing, breaking into the house of Polyphemus?'\n"),
        write("\nSingle, massive eye opens behind you, glowing in the light of your torch. Polyphemus"),
        write("\ndoes not look happy to see you as he waits for an answer. 'What's your name, stranger?'\n"), flush_output,
        read(Name),
        format("\n'Are you the one who killed my sheep, ~w? My favourite sheep. You will pay for what you did", [Name]),
        write("\nwith your own blood.'\n"),
        write("\nThere's not much time to wonder what's happening, before the Cyclop roars and readies for an attack.\n"),
        (holding(wine) ->
                write("At the last second, you grab the flask of wine taken from the island of lotus-eaters and aim"),
                write("\nfor the Cyclops still opened mouth.\n"),
                write("\nThe wine from lotus flowers makes his movements slow and heavy, and he manages to land only a few blows.\n"),
                retract(holding(wine)),
                crew_death(6);
        write("\nThe fight is long and grueling, with much death on your side. Pools of fresh blood form on the cave's floor.\n"),
        crew_death(44)),
        write("\nIn a heat of a battle, you strike for the Cyclops' eye, blinding him. He's not much of a threat after that.\n"),
        format("\nPolyphemus screams and screams, loud enough to be heard outside of his cave. 'Help! ~w hurts me!'\n", [Name]),
        (Name == nobody ->
                write("\nBut no one comes to his aid.");
        write("\nThen there's a sound of heavy steps coming from the direction of the only exit.\n"),
        write("\nThere are more of them. Much, much more.\n"),
        write("\nYou're not leaving this cave alive.\n"),
        finish).

describe(polyphemus_cave) :- !,
        write("The cave is dark and musty, even with a lit torch you barely see more than a few steps ahead."),
        write("\nYou navigate mostly by the sound of the sea waves to find your way back.").

talk(polyphemus) :- you_are_at(polyphemus_cave), !,
        write("Wrong decision.\n"),
        write("\nPolyphemus doesn't hear any of the words you've spoken, but he does hear your voice and"),
        write("\nblindly strikes in your direction, enraged. You barely avoid being crushed to death."),
        crew(X),
        (X > 3 ->
                write("\n\nSome of your men are not so fast."),
                crew_death(3);
        true).
talk(cyclops) :- !,
        talk(polyphemus).

embark :- you_are_at(polyphemus_cave),
        plot(leave_polyphemus), nl, fail.

plot(leave_polyphemus) :- !,
        write("\nYou manage to embark on a ship and leave the Cyclops' cave behind. As you do, you can't shake"),
        write("\naway the feeling of being watched, but the eyes are not only those of your men nor the foes you've"),
        write("\nescaped from - the gods have taken an intrest in your actions. Something in the air has changed."),
        assert(land(open_sea, aeolus_island)).
