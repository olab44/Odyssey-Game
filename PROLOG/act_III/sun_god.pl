:- dynamic crew/1.

you_are_at(sun_god_island).

describe(sun_god_island) :-
    write("You have reached the sacred island of the Sun God, Helios. The land is strikingly serene, untouched by time\n"),
    write("or mortal hands. As you and your crew disembark, an unsettling stillness hangs in the air, as if the island\n"),
    write("itself holds its breath. The familiar cries of seagulls and rush of the waves are absent here.\n\n"),
    write("Your men are weary and starving, their eyes sunken from endless days at sea. In the distance, a herd of cattle\n"),
    write("appears, their coats gleaming like polished bronze under the sun’s gaze. They graze peacefully, undisturbed,\n"),
    write("too perfect to be mere beasts of the earth. You remember the warnings—these are the sacred cattle of Helios,\n"),
    write("guarded by the gods themselves. Any harm done to them would be an offense of the gravest kind.\n\n"),
    write("But hunger is a merciless companion. Despite your warnings, despite the dire consequences, your men are consumed\n"),
    write("by desperation. Their eyes gleam with the madness of survival, and one by one, they break their oaths, seizing\n"),
    write("the cattle. Soon, the rich aroma of roasted meat fills the air, drowning out all thoughts of caution or honor.\n\n"),
    trigger_sun_god_wrath.

trigger_sun_god_wrath :-
    write("As the last flames of the sacrilegious feast fade, a darkness falls over the island, sudden and absolute.\n"),
    write("The sun vanishes, blotted out by roiling clouds that swirl like ink in water. A chill sweeps through the\n"),
    write("air, and the earth itself trembles. Your men look around, their bravado replaced by a creeping terror.\n\n"),
    write("A voice booms from above, filling the heavens with fury and sorrow—it is the voice of Helios, the Sun God.\n"),
    write("He condemns the act, his voice echoing like thunder across the barren shores. Your men fall to their knees,\n"),
    write("but it is too late. Their fate is sealed.\n\n"),
    crew(Count),
    final_curse(Count).

final_curse(Count) :-
    Count > 1,
    retract(crew(Count)),
    write("One by one, your men meet their doom. The earth cracks beneath them, swallowing some into the darkness below,\n"),
    write("while others are struck down by bolts of divine fire. You stand helpless, witnessing the swift justice of the gods.\n\n"),
    write("When the storm finally passes, a heavy silence remains. All your men have perished, leaving you alone amidst\n"),
    write("the wreckage of what was once your crew. The weight of their lost lives presses upon you, but you know you must\n"),
    write("continue, for their sacrifice cannot be in vain.\n"),
    assert(crew(1)).
final_curse(1) :-
    write("The gods’ fury has left you as the lone survivor, a cursed witness to the folly of defying divine law.\n"),
    write("The empty island seems to echo with the voices of your fallen crew, yet there is no comfort in their memory.\n"),
    write("You are alone, the last to carry forth the hope of returning to Ithaca, though at a cost beyond measure.\n\n").
