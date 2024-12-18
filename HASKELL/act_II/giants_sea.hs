module ActII.GiantsSea where
import Types
import WorldMap

giantsSea :: State -> IO State
giantsSea state = do
    let currentCrew = crew state
    let loss = max 20 (min currentCrew (round (0.2 * fromIntegral currentCrew)))
    let newCrewCount = currentCrew - loss
    let updatedState = state { crew = newCrewCount }

    putStrLn $ "Current crew before loss: " ++ show currentCrew
    putStrLn $ "Calculated loss: " ++ show loss
    putStrLn $ "New crew count after loss: " ++ show newCrewCount
    putStrLn $ "The giants' attack reduces your crew by " ++ show loss ++ " members."
    putStrLn "You have no choice but to retreat south."
    let newplace = circe_sea
    let finalState = updatedState { you_are_at = newplace}
    return finalState