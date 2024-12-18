module ActII.SunGod where
import Types
import WorldMap

eatCattle :: State -> IO State
eatCattle state = do
    putStrLn "Reluctantly, you allow the crew to slaughter the sacred cattle of Helios."
    putStrLn "The feast restores their strength, but you sense a dark omen in the air."
    if potion_recipe state
        then do
            putStrLn "As you consume the cattle, you find a special vial containing the blood of Helios’s sacred beast."
            putStrLn "This is the final ingredient needed to complete the elixir of strength."
            putStrLn "Type \"complete_elixir\" to obtain it."
            return state { helios_blood = True}
        else return state


doNotEatCattle :: State -> IO State
doNotEatCattle state = do
    let currentCrew = crew state
    let loss = 50
    let newCrewCount = max 0 (currentCrew - loss)
    putStrLn "You stand firm in your decision: 'We will not eat the cattle of the Sun God.'"
    putStrLn "The crew protests but ultimately obeys. Over the next days, hunger claims the lives of 50 members."
    return state {crew = newCrewCount}

completeElixir :: State -> IO State
completeElixir state = do
    if potion_recipe state && helios_blood state then do
        putStrLn "Using the empty bottle and the blood of Helios’s cattle, you complete the Elixir of Strength. You are now ready to continue journey"
        return state { strength_elixir = True}
    else if not (potion_recipe state) then do
        putStrLn "You lack the potion recipe."
        return state
    else do
        putStrLn "You lack the blood of Helios cattle."
        return state


continueJourney :: State -> IO State
continueJourney state
    | you_are_at state == sun_god_island =
        if strength_elixir state
            then do
                putStrLn "As you leave the island, a thunderous voice echoes: 'For your desecration, you shall be punished!'"
                putStrLn "The sea rages as a powerful storm descends upon your ship."
                putStrLn "Thanks to the Elixir of Strength, you alone withstand the gods' wrath and survive."
                putStrLn "Your crew perishes, and you lose consciousness..."
                return state { you_are_at = calypso_island, disembarked = True, crew = 1 }
            else do
                putStrLn "As you sail eastward, the wrath of the gods descends upon you."
                putStrLn "A powerful storm engulfs your ship, smashing it to pieces."
                putStrLn "If only you had something, a magic spell or a potion, to give you strength..."
                putStrLn "GAME OVER: You have perished at sea due to the wrath of the gods."
                return state { game_over = True }
    | otherwise = do
        putStrLn "You cannot continue your journey from here."
        return state

