module WorldMap where
import Data.List (find)
import Types

evaluateCondition :: Condition -> State -> Bool
evaluateCondition AlwaysTrue _ = True
evaluateCondition (Holding item) state = item `elem` holding state
evaluateCondition (FlagCondition cond) state = cond state

describeLocation :: State -> String
describeLocation state =
    let location = you_are_at state
    in case find (\(cond, _) -> evaluateCondition cond state) (descriptions location) of
        Just (_, description) -> description
        Nothing -> "There's nothing special around here."

-- MAP LAYOUT
sea_paths :: [((Location, String), Location)]
sea_paths =
  -- act I
  [ ((open_sea, "north"), ithaca_sea)
  , ((open_sea, "west"), lotus_sea)
  , ((ithaca_sea, "south"), open_sea)
  , ((lotus_sea, "north"), polyphemus_sea)
  , ((lotus_sea, "east"), open_sea)
  , ((polyphemus_sea, "east"), ithaca_sea)
  , ((polyphemus_sea, "south"), lotus_sea)
  -- -- act II
  , ((circe_sea, "north"), giants_sea)
  , ((circe_sea, "east"), sirens_sea)
  , ((circe_sea, "west"), underworld_sea)
  , ((underworld_sea, "north"), giants_sea)
  , ((underworld_sea, "east"), circe_sea)
  , ((sirens_sea, "north"), giants_sea)
  , ((sirens_sea, "south"), scylla_charybdis_sea)
  , ((giants_sea, "south"), circe_sea)
  , ((scylla_charybdis_sea, "east"), sun_god_sea)
  , ((sun_god_sea, "south"), calypso_island)
  ]

lands :: [(Location, Location)]
lands =
  [ (lotus_sea, lotus_island),
    (polyphemus_sea, polyphemus_cave),
    (open_sea, aeolus_island),
    (circe_sea, circe_island),
    (underworld_sea, underworld),
    (sun_god_sea, sun_god_island)
  ]

-- LOCATIONS DATA
open_sea :: Location
open_sea = Location {
    name = "open_sea",
    descriptions =
        [
        (Holding "wind-bag",
        "Calm open sea and the island of a god, nothing more for you here. You instinctively look\n" ++
        "towards north, where the sky's a clear expanse of blue."),

        (FlagCondition aeolusAccess,
        "Where once was nothing but water, now lies an island, floating on the waves. The blowing winds feel\n" ++
        "different, too, and you can't stop the thought - the home of the wind god. It means hope, a lone\n" ++
        "chance of getting help against the storm, if only you disembark and beg."),

        (AlwaysTrue,
        "Open sea stretches in all directions, no land in sight. Were you to believe your charts,\n" ++
        "Ithaca lies north. North, where you can see a mass of dark clouds covering the sky.")
        ]
}
aeolus_island :: Location
aeolus_island = Location {
    name = "aeolus_island",
    descriptions =
        [ (AlwaysTrue,
        "The home of the wind god, Aeolus, is just as unique as its owner might suggest. Loud and playful\n" ++
        "with various puffs of clouds twisting and turning in the air, as if alive.\n" ++
        "Aeolus himself flies around as well, never in one place for long. You feel his gaze following you." ) ]
}

lotus_sea :: Location
lotus_sea = Location {
    name = "lotus_sea",
    descriptions =
        [ (AlwaysTrue, "There's solid land nearby - you notice the glowing light of a fire. It seems inviting." ) ]
}
lotus_island :: Location
lotus_island = Location {
    name = "lotus_island",
    descriptions =
        [ (AlwaysTrue,
        "The island is calm and serene, the whole atmosphere making you sleepy. You see a lake\n" ++
        "that's surrounded by plain houses. The people milling around pay you no mind at all,\n" ++
        "their thoughts far away.\n" ++
        "There's plenty of food on their tables - strange, glowing fruits, which you recognize as\n" ++
        "mind-numbing lotus. There's also an abundance of wine - surely the lotus-eaters wouldn't\n" ++
        "be mad, were you to take a jug for yourself.")
        ]
}

polyphemus_sea :: Location
polyphemus_sea = Location {
    name = "polyphemus_sea",
    descriptions =
        [ (AlwaysTrue,
        "You're not far from an island, rugged shoreline making way to green fields and rocky mountains\n" ++
        "full of caves. One cave in particular looks easy to reach." ) ]
}
polyphemus_cave :: Location
polyphemus_cave = Location {
    name = "polyphemus_cave",
    descriptions =
        [ (AlwaysTrue,
        "The cave is dark and musty, even with a lit torch you barely see more than a few steps ahead.\n" ++
        "You navigate mostly by the sound of the sea waves to find your way back." ) ]
}

ithaca_sea :: Location
ithaca_sea = Location {
    name = "ithaca_sea",
    descriptions =
        [
        (Holding "wind-bag",
        "The sky is clear, the water smooth - no storm, no tidal wave. You see Ithaca - your destination, your\n" ++
        "kingdom, your home - on the horizon. You can already feel the ghost of your wife's embrace."),

        (AlwaysTrue, "Your way is blocked by giant waves and giant storms. You should probably turn back south, towards\n" ++
        "calmer waters - but Ithaca's never been closer in the last ten years and the way home is through.")
        ]
}

circe_sea :: Location
circe_sea = Location {
    name = "circe_sea",
    descriptions =
      [ (AlwaysTrue, "The waters here feel thick with enchantment, and Circe's island lies ominously ahead.") ]
}
circe_island :: Location
circe_island = Location {
    name = "circe_island",
    descriptions =
        [
        (FlagCondition visitedUnderworld,
        "You step back onto Circe's island, feeling the weight of your journey to the Underworld lingering upon you.\n" ++
        "The familiar sights of her enchanted island are a strange comfort after the shadows of Hades.\n" ++
        "Circe greets you with a knowing look, sensing the trials you’ve endured and the wisdom you have gained.\n" ++
        "It’s clear she has more guidance to offer, should you seek her counsel.\n" ++
        "You may now talk to Circe to ask for further instructions on your journey."),

        (AlwaysTrue,
        "After disembarking on Circe's island, you step into a lush, enchanted forest.\n" ++
        "The air is thick with mystery, and your instincts warn you of hidden dangers.\n" ++
        "To continue, you can talk to Hermes or confront Circe to face her enchantments.")
      ]
}

underworld_sea :: Location
underworld_sea = Location {
    name = "underworld_sea",
    descriptions =
        [ (AlwaysTrue, "You have reached the mysterious and eerie waters of the Underworld.") ]
}
underworld :: Location
underworld = Location {
    name = "underworld",
    descriptions =
        [ (AlwaysTrue, "You sense an opportunity to talk to Charon, the ferryman who will guide you across the River Styx.")]
}

giants_sea :: Location
giants_sea = Location {
    name = "giants_sea",
    descriptions =
        [ (AlwaysTrue, "You have entered the territory of dangerous giants! They begin hurling massive stones at your ships.") ]
}

sirens_sea :: Location
sirens_sea = Location {
    name = "sirens_sea",
    descriptions =
        [ (AlwaysTrue,
        "The waters are calm but ominous as you approach the domain of the Sirens.\n" ++
        "In the distance, their figures are barely visible, their songs ready to ensnare anyone who listens.\n" ++
        "You must now decide whether to block your own ears or not.\n" ++
        "Type plug_ears if you want to block your ears or leave_ears_open if you want to hear the Sirens' song.")
        ]
}

scylla_charybdis_sea :: Location
scylla_charybdis_sea = Location {
    name = "scylla_charybdis_sea",
    descriptions =
        [ (AlwaysTrue,
        "The sea grows treacherous as you approach the domain of Scylla and Charybdis.\n" ++
        "To your left, you see Scylla's ominous cliffs, while Charybdis churns the water violently to your right.\n" ++
        "You must choose which path to take to continue.\n" ++
        "Type sail_scylla to navigate past Scylla or sail_charybdis to risk waters near Charybdis.")
        ]
}

sun_god_sea :: Location
sun_god_sea = Location {
    name = "sun_god_sea",
    descriptions =
        [ (AlwaysTrue,
        "A violent storm catches your ship, forcing you to seek refuge on a nearby island.\n" ++
        "This is the sacred island of the Sun God, Helios, where his holy cattle roam.")
        ]
}

sun_god_island :: Location
sun_god_island = Location {
    name = "sun_god_island",
    descriptions =
        [ (AlwaysTrue,
        "You disembark onto the island, hoping the storm will soon pass.\n" ++
        "Unfortunately, the storm shows no sign of stopping, and you will need to stay here longer.\n" ++
        "Your food supplies are exhausted, and the crew is looking to you for guidance.\n" ++
        "You had better talk to them for advice.")
        ]
}

calypso_island :: Location
calypso_island = Location {
    name = "calypso_island",
    descriptions =
        [ (AlwaysTrue,
        "You stand on the shores of an island. It is breathtaking but empty, nobody is to be seen.\n" ++
        "The only sound is wind blowing through the trees. You are exhausted from all your travels and collapse on the ground.\n" ++
        "Your crew is no longer alive, your ship is gone. You are left all alone.\n" ++
        "When you wake up, the first thing you see is a woman, a goddess, who introduces herself as Calypso.\n" ++
        "She is beautiful, but you want to return home. You can try asking her how to leave.")
        ]
}

ithaca :: Location
ithaca = Location {
    name = "ithaca",
    descriptions =
        [ (AlwaysTrue,
        "At last, you set foot on Ithaca, your homeland.\n" ++
        "After all the trials, you've made it home, where every person on the street talks about\n" ++
        "the queen's challenge.\n" ++
        "But that's a story for another day.\n" ++
        "After years lost at sea, battles fought, and gods defied, you've finally reached Ithaca.\n" ++
        "You've proven that courage and loyalty can overcome even the wrath of gods.\n" ++
        "Welcome home, Odysseus.")
        ]
}