:- multifile you_are_at/1, describe/1, talk/1.

describe(lotus_sea) :- write("The land of Lotus Eaters.").
describe(lotus_island) :- write("Lotus land - wine can be found here.").

talk(lotus-eaters) :- you_are_at(lotus_island), !,
            write("LOTUS EATERS HIGH TALK (WINE TIP)").