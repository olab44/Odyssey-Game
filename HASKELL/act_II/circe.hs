module ActII.Circe where
import State

confrontCirce :: State -> IO State
confrontCirce state
    | magic_herb_available state = do
        putStrLn "Holding the magical herb, you feel its protective aura as you step into Circe's palace."
        putStrLn "You sense her spells failing against you."
        confrontResult state
    | otherwise = do
        putStrLn "GAME OVER: Without the protection of the magical herb, Circe’s spell overwhelms you, turning you into a pig, and you lose the game."
        return state { game_over = True }

confrontResult :: State -> IO State
confrontResult state = do
    putStrLn "You now have a choice: will you 'spare' her life or 'kill' her?"
    choice <- getLine
    case choice of
        "spare" -> do
            let restoredCrew = crew state
            putStrLn "You decide to spare Circe, who restores your crew to human form. They are relieved, and gratitude fills the air."
            yearPassed (state { crew = restoredCrew })
        "kill" -> do
            putStrLn "GAME OVER: You kill Circe, and in doing so, you lose the chance to undo the curse on your crew. You are left stranded, and your journey ends in failure."
            return state { game_over = True }
        _ -> do
            putStrLn "Invalid choice. You hesitate, and Circe takes advantage of your indecision. She casts a spell, and you and your crew are lost forever."
            return state { game_over = True }

yearPassed :: State -> IO State
yearPassed state = do
    putStrLn "Time passes; a full year slips by as Circe becomes your ally and lover. The comforts of the island nearly make you forget your quest."
    putStrLn "One day, your crew approaches, urging you to remember Ithaca."
    putStrLn "You can now TALK TO CREW to discuss the journey ahead."
    return state