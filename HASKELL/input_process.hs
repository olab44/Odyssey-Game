module InputProcess where
import Exploration
import ActII.Circe
import ActII.Underworld
import ActII.GiantsSea
import ActII.Sirens
import ActII.SunGod
import ActII.ScyllaCharybdis
import Types
import WorldMap


process_input :: String -> State -> IO State
process_input input state
  | "sail" `elem` words input = sail (last (words input)) state
  | "embark" == input = embark state
  | "disembark" == input = disembark state
  | "look" == input = describe state
  | "crew_count" == input = crew_count state
  | "take" `elem` words input = take_item (last (words input)) state
  | "talk" `elem` words input = talk (last (words input)) state
  | "gather" `elem` words input = gather (last (words input)) state
  | "build" `elem` words input = build (last (words input)) state
  | "escape" == input = escape state
  | "finish" == input = return (finish state)
  | "confront" `elem` words input && "circe" `elem` words input = confrontCirce state
  | "sail_scylla" == input = sail_scylla state
  | "sail_charybdis" == input = sail_charybdis state
  | "start_ferry_puzzle" == input = do
      let puzzleState = start_ferry_puzzle
      putStrLn "The ferry puzzle has begun! Type 'ferry <item>' or 'return <item>' to proceed."
      return state { ferryPuzzleState = Just puzzleState }
  | "start_ferry_puzzle" == input = do
      let puzzleState = start_ferry_puzzle
      putStrLn "The ferry puzzle has begun! Type 'ferry <item>' or 'return <item>' to proceed."
      return state { ferryPuzzleState = Just puzzleState }
  | "ferry" `elem` words input = do
      case ferryPuzzleState state of
        Nothing -> do
          putStrLn "You are not currently in the ferry puzzle."
          return state
        Just puzzleState -> do
          let item = parseFerryItem (last (words input))
          case item of
            Nothing -> do
              putStrLn "Invalid item to ferry."
              return state
            Just validItem -> do
              let (newPuzzleState, message) = ferry puzzleState validItem
              putStrLn message
              case check_ferry_state newPuzzleState of
                Nothing -> return state { ferryPuzzleState = Just newPuzzleState }
                Just endMessage -> do
                  putStrLn endMessage
                  return state { ferryPuzzleState = Nothing }
  | "return" `elem` words input = do
      case ferryPuzzleState state of
        Nothing -> do
          putStrLn "You are not currently in the ferry puzzle."
          return state
        Just puzzleState -> do
          let item = parseFerryItem (last (words input))
          case item of
            Nothing -> do
              putStrLn "Invalid item to return."
              return state
            Just validItem -> do
              let (newPuzzleState, message) = returnItem puzzleState validItem
              putStrLn message
              case check_ferry_state newPuzzleState of
                Nothing -> return state { ferryPuzzleState = Just newPuzzleState }
                Just endMessage -> do
                  putStrLn endMessage
                  return state { ferryPuzzleState = Nothing }
  | "show_map" == input = showMap state
  | "eat_cattle" == input = eatCattle state
  | "do_not_eat_cattle" == input = doNotEatCattle state
  | "complete_elixir" == input = completeElixir state
  | "continue_journey" == input = continueJourney state
  | "debug_act_II" == input = do return state { you_are_at = circe_sea }
  | "debug_act_III" == input = do return state { you_are_at = calypso_island }
  | otherwise = do
      putStrLn "Invalid command"
      return state
