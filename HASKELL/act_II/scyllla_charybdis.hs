module ActII.ScyllaCharybdis where
import Types
import System.Random (randomRIO)
import WorldMap

sail_scylla :: State -> IO State
sail_scylla state = do
    let survivalRate = scyllaSurvivalRate state
    case survivalRate of
        Just rate -> do
            putStrLn "You approach Scylla..."
            randomChance <- randomRIO (0.0, 1.0)
            if randomChance > rate
                then do
                    putStrLn "Scylla strikes with terrifying speed, catching you off guard..."
                    putStrLn "GAME OVER: You have been taken by Scylla."
                    return state { game_over = True }
                else do
                    putStrLn "You sail past Scylla successfully, but she manages to claim some of your crew."
                    let currentCrew = crew state
                    let loss = max 6 (round (0.06 * fromIntegral currentCrew))
                    let newCrewCount = currentCrew - loss
                    putStrLn $ "Scylla devours " ++ show loss ++ " of your crew members."
                    proceed_to_sun_god_island state { crew = newCrewCount }

sail_charybdis :: State -> IO State
sail_charybdis state = do
    if charybdis_lure state
        then do
            putStrLn "You use the mysterious Charybdis Lure, guiding your ship safely past the deadly whirlpool."
            putStrLn "The waters calm, and you find yourselves out of danger."
            proceed_to_sun_god_island state
            return state
        else do
            putStrLn "The whirlpool's powerful currents pull your ship into its deadly grasp."
            putStrLn "GAME OVER: Your ship and crew are lost to Charybdis."
            return state { game_over = True }

proceed_to_sun_god_island :: State -> IO State
proceed_to_sun_god_island state = do
    putStrLn "After surviving the perilous pass, a fierce storm catches you off guard."
    putStrLn "The raging waves drive your ship eastward, and you find yourselves on the shores of the Island of the Sun God."
    return state { you_are_at = sun_god_sea }