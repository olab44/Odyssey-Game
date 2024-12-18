module ActII.Sirens where
import Types
import Data.Maybe (fromMaybe)


processSirensChoice :: String -> State -> IO State
processSirensChoice choice state
  | choice == "plug_ears" = do
      putStrLn "You decide to block your ears with wax."
      putStrLn "As you sail past, you see the Sirens singing, but their voices cannot reach you."
      return state
  | choice == "leave_ears_open" = do
      putStrLn "You choose to leave your ears open. Your crew ties you tightly to the mast, as per Circe's advice."
      putStrLn "The Sirens' voices fill the air, haunting and beautiful."
      putStrLn "You listen, enthralled, and in their song, you learn of a mystical potion recipe that grants strength and protects life."
      putStrLn "You have learned the potion recipe!"
      let currentCrew = crew state
      let survived = fromMaybe 0 (crewSurvivedSirens state)
      let lostCrew = calculateCrewLoss currentCrew survived
      putStrLn $ "Not all of your crew were protected, and " ++ show lostCrew ++ " of them succumbed to the Sirens' song."
      putStrLn "With the Sirens behind you, you should sail south on towards the looming cliffs of Scylla and Charybdis."
      putStrLn "You brace yourself for another challenge as the journey continues."
      let newstate = state {potion_recipe = True, crew = currentCrew - lostCrew}
      return newstate
  | otherwise = do
      putStrLn "Invalid choice. Please type 'plug_ears' or 'leave_ears_open'."
      return state

calculateCrewLoss :: Int -> Int -> Int
calculateCrewLoss currentCrew survived = currentCrew - min survived currentCrew
