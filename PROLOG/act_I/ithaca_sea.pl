:- multifile describe/1, plot/1.

describe(ithaca_sea) :- holding(wind-bag), !,
        write("The sky is clear, the water smooth - no storm, no tidal wave. You see Ithaca - your destination, your"),
        write("\nkingdom, your home - on the horizon. You can already feel the ghost of your wife's embrace.").
describe(ithaca_sea) :- !,
        write("Your way is blocked by giant waves and giant storms. You should probably turn back south, towards"),
        write("\ncalmer waters - but Ithaca's never been closer in the last ten years and the way home is through.").

sail(Direction) :- you_are_at(ithaca_sea), Direction \= south, !,
        plot(ithaca_sea_storm).

plot(ithaca_sea_storm) :- holding(wind-bag), !,
        write("\nWith the storm contained, it shouldn't take much longer to reach the coast. Mere weeks.\n"),
        write("\nYou wish you could relax now, but the wind gods' words won't let you. 'Be careful who you"),
        write("\ntrust, captain' pushes you to keep your eyes open at all times.\n"),
        write("\nYou guard the wind-bag to the best of your abilities, but the need to sleep proves stronger eventually.\n"),
        write("\nThe dream is a lovely one - the family reunited, Penelope and Telemachus in your arms.\n"),
        write("\nThe reality is not. The bag lies open. The raging storm tears at your ships with vengeance, vicious"),
        write("\ncurrents leading you to distant shores far, far away from Ithaca."),
        retract(holding(wind-bag)),
        retract(you_are_at(ithaca_sea)), assert(you_are_at(circe_sea)).
plot(ithaca_sea_storm) :- crew(X),
        (X > 150 ->
                write("\nIt turns out you can't beat the force of nature that easily. You lose ships - the screams of over"),
                write("\na hundred men are drowned out by the storm as they disappear below the waves - just to end up where"),
                write("\nyou started. The only safe direction now is towards the calmer open sea - but away from your destination."),
                crew_death(150);
        write("\nThe storm is your final fight, the only thing still blocking your way home - and so you're ready"),
        write("\nto risk it all, look for the way through even when there seems to be none.\n"),
        write("\nYou try your best...\n"),
        write("\nIt's just not enough.\n"),
        write("\nYour ship is the last one from the fleet to fall victim to the crashing waves.\n"),
        finish).
