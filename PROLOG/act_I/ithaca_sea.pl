:- multifile you_are_at/1, describe/1, plot/1, holding/1, crew/1, crew_death/1.

describe(ithaca_sea) :- holding(wind-bag), !,
            write("The sky is clear, the water smooth - no storm, no tidal wave. You see Ithaca - your destination, your"),
            write("\nkingdom, your home - on the horizon. You can already feel the ghost of your wife's embrace.").
describe(ithaca_sea) :-
            write("Your way is blocked by giant waves and giant storms. You should probably turn back south, towards calmer"),
            write("\nwaters - but Ithaca's never been closer in the last ten years and the way home is through.").

sail(Direction) :- you_are_at(ithaca_sea), Direction \= south, !,
            plot(ithaca_sea_storm).

plot(ithaca_sea_storm) :- holding(wind-bag), !,
            write("\nYou guard the wind bag to the best of your abilities, but the need to sleep proves stronger."),
            write("\nYour men open the bag."),
            retract(you_are_at(ithaca_sea)), assert(you_are_at(circe_island)).
plot(ithaca_sea_storm) :- crew(X), X > 150, !,
            write("\nIt turns out you can't beat the force of nature that easily. You lose ships - the screams of over"),
            write("\na hundred men are drowned out by the storm as they disappear below the waves. The only safe direction"),
            write("\nnow is towards the calmer open sea - but away from your destination."),
            crew_death(150).
plot(ithaca_sea_storm) :-
            write("\nThe storm is your final fight, the only thing still blocking your way home - and so you're ready"),
            write("\nto risk it all, look for the way through even when there seems to be none.\n"),
            write("\nYou try your best...\n"),
            write("\nIt's just not enough.\n"),
            write("\nYour ship is the last one from the fleet to fall victim to the crashing waves.\n"),
            finish.