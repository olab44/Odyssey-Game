:- multifile describe/1, sail/1.

:- dynamic crew/1, crew_death/1, crew_survived_sirens/1.
:- dynamic potion_recipe/1.

describe(sirens_sea) :- !,
    write("The waters are calm but ominous as you approach the domain of the Sirens.\n"),
    write("In the distance, their figures are barely visible, their songs ready to ensnare anyone who listens.\n"),
    write("You must now decide whether to block your own ears or not.\n"),
    write("Type "), ansi_format([fg(yellow)], "plug_ears", []), write(" if you want to block your ears, or "),
    ansi_format([fg(yellow)], "leave_ears_open", []), write(" if you want to hear the Sirens' song.\n"),
    read(Choice),
    (Choice == plug_ears ->
        write("You decide to block your ears with wax.\n"),
        write("As you sail past, you see the Sirens singing, but their voices cannot reach you.\n")
    ; Choice == leave_ears_open ->
        write("You choose to leave your ears open. Your crew ties you tightly to the mast, as per Circe's advice.\n"),
        write("The Sirens' voices fill the air, haunting and beautiful.\n"),
        write("You listen, enthralled, and in their song, you learn of a mystical potion recipe that grants strength and protects life.\n"),
        retractall(potion_recipe(_)), assert(potion_recipe(true))
    ),
    
    crew(CurrentCrew),
    crew_survived_sirens(Survived),
    
    Lost is CurrentCrew - min(Survived, CurrentCrew),
    format("Not all of your crew were protected, and ~w of them succumbed to the Sirens' song.\n", [Lost]),
    crew_death(Lost),
    
    write("With the Sirens behind you, your ship sails on towards the looming cliffs of Scylla and Charybdis.\n"),
    write("You brace yourself for another challenge as the journey continues.\n\n"),
    
    sail(south).
