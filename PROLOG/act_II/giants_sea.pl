:- multifile describe/1.

describe(giants_sea) :- !,
    write("You have entered the territory of dangerous giants! They begin hurling massive stones at your ships.\n"),
    crew(CurrentCrew),
    Loss is max(20, round(0.2 * CurrentCrew)),
    crew_death(Loss),
    format("The giants' attack reduces your crew by ~d members.\n", [Loss]),
    write("You have no choice but to retreat south.\n"),
    sail(south).