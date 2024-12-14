module ActII.Underworld where
import State


parseFerryItem :: String -> Maybe FerryItem
parseFerryItem "crew" = Just Crew
parseFerryItem "wine" = Just Wine
parseFerryItem "cerber" = Just Cerber
parseFerryItem "charon" = Just Charon
parseFerryItem "none" = Just None
parseFerryItem _ = Nothing


start_ferry_puzzle :: FerryPuzzleState
start_ferry_puzzle = FerryPuzzleState
  {itemsOnStartSide = [Crew, Wine, Cerber, Charon]
  , itemsOnEndSide = []
  }

printState :: FerryPuzzleState -> String
printState puzzleState =
  "Start side: " ++ show (itemsOnStartSide puzzleState) ++ "\n" ++
  "End side: " ++ show (itemsOnEndSide puzzleState)


ferry :: FerryPuzzleState -> FerryItem -> (FerryPuzzleState, String)
ferry puzzleState item =
  if item == None
  then let newState = FerryPuzzleState
             { itemsOnStartSide = filter (/= Charon) (itemsOnStartSide puzzleState)
             , itemsOnEndSide = Charon : itemsOnEndSide puzzleState
             }
           in (newState, "You ferried nothing\n" ++ printState newState)
  else if item `elem` itemsOnStartSide puzzleState
  then let newState = FerryPuzzleState
             { itemsOnStartSide = filter (\x -> x /= item && x /= Charon) (itemsOnStartSide puzzleState)
             , itemsOnEndSide = item : Charon : itemsOnEndSide puzzleState
             }
           in (newState, "You ferried " ++ show item ++ " and Charon.\n" ++ printState newState)
  else (puzzleState, "Item is not on the starting side.\n" ++ printState puzzleState)


returnItem :: FerryPuzzleState -> FerryItem -> (FerryPuzzleState, String)
returnItem puzzleState item =
  if item == None
  then let newState = FerryPuzzleState
             { itemsOnEndSide = filter (/= Charon) (itemsOnEndSide puzzleState)
             , itemsOnStartSide = Charon : itemsOnStartSide puzzleState
             }
           in (newState, "You returned with nothing\n" ++ printState newState)
  else if item `elem` itemsOnEndSide puzzleState
  then let newState = FerryPuzzleState
              { itemsOnEndSide = filter (\x -> x /= item && x /= Charon) (itemsOnEndSide puzzleState)
              , itemsOnStartSide = item : (if Charon `elem` itemsOnStartSide puzzleState then [] else [Charon]) ++ itemsOnStartSide puzzleState
              }
            in (newState, "You returned " ++ show item ++ " and Charon to the start side.\n" ++ printState newState)
  else (puzzleState, "Item is not on the end side.\n" ++ printState puzzleState)


check_ferry_state :: FerryPuzzleState -> Maybe String
check_ferry_state puzzleState
  | (Crew `elem` itemsOnStartSide puzzleState) && (Wine `elem` itemsOnStartSide puzzleState) && not (Charon `elem` itemsOnStartSide puzzleState) =
      Just "Some from your crew drowned in the River Styx after drinking the wine."
  | (Crew `elem` itemsOnStartSide puzzleState) && (Cerber `elem` itemsOnStartSide puzzleState) && not (Charon `elem` itemsOnStartSide puzzleState) =
      Just "Cerberus attacked the crew."
  | (Crew `elem` itemsOnEndSide puzzleState) && (Wine `elem` itemsOnEndSide puzzleState) && not (Charon `elem` itemsOnEndSide puzzleState) =
      Just "Some from crew drowned in the River Styx after drinking the wine."
  | (Crew `elem` itemsOnEndSide puzzleState) && (Cerber `elem` itemsOnEndSide puzzleState) && not (Charon `elem` itemsOnEndSide puzzleState) =
      Just "Cerberus attacked the crew."
  | all (`elem` itemsOnEndSide puzzleState) [Crew, Wine, Cerber, Charon] =
      Just "Congratulations! You have successfully ferried everything across the River Styx. You can now talk to Tiresias"
  | otherwise = Nothing