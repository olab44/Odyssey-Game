:- multifile describe/1, talk/1.

describe(open_sea) :- holding(wind-bag), !,
        write("Calm open sea and the island of a god, nothing more for you here. You instinctively look"),
        write("\ntowards north, where the sky's a clear expanse of blue.\n").
describe(open_sea) :- land(open_sea, aeolus_island), !,
        write("Where once was nothing but water, now lies an island, floating on the waves. The blowing winds feel"),
        write("\ndifferent, too, and you can't stop the thought - the home of the wind god. It means hope, a lone"),
        write("\nchance of getting help against the storm, if only you disembark and beg.\n").
describe(open_sea) :- !,
        write("Open sea stretches in all directions, no land in sight. Were you to believe your charts,"),
        write("\nIthaca lies north. North, where you can see a mass of dark clouds covering the sky.\n").

talk(crew) :- you_are_at(open_sea), holding(wind-bag), !,
        write("The crew's overly eager about about the wind-bag, you can't help but notice. They whisper of"),
        write("\ntreasure and trail off as soon as you come close - clearly not in a mood to talk.").
talk(crew) :- you_are_at(open_sea), land(open_sea, aeolus_island), !,
        write("Your second-in-command doesn't at all like the idea of asking a god for help. 'They're easy to"),
        write("\nanger, captain, and there's only so much time before your luck with them runs out for good.").
talk(crew) :- you_are_at(open_sea), !,
        write("Your men tell you that there's nothing to look for east and south from here, but west - west"),
        write("\nis where the birds fly, which probably means solid land.").

describe(aeolus_island) :- !,
        write("The home of the wind god, Aeolus, is just as unique as its owner might suggest. Loud and playful"),
        write("\nwith various puffs of clouds twisting and turning in the air, as if alive.\n"),
        write("\nAeolus himself flies around as well, never in one place for long. You feel his gaze following you.").

talk(aeolus) :- you_are_at(aeolus_island), !,
        write("You kneel down and describe your situation, knowing that the god can hear."),
        write("\nYour ask for assistance is first met with disheartening nothing, but then - laughter.\n"),
        write("\nAeolus comes to a stop right before you. 'I suppose we could play a game, Odysseus of Ithaca."),
        write("\nI'll show you a bag with the winds of the storm all trapped. If it gets opened, whatever"),
        write("\nthe reason... well, good luck.'\n"),
        write("\n'Do be careful who you trust, captain, you never really know.'\n"),
        write("\nAnd with that, he's gone on a breeze again, leaving a tied wind-bag at your feet."),
        assert(object_at(aeolus_island, wind-bag)).
