module Exploration (play) where
import Data.Map (Map)
import Data.String
import qualified Data.Map as Map
import Control.Monad (when)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (delete)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout, putStrLn)
import System.Random (randomRIO)
import Data.List (find)
import Prelude
import Prelude (putStrLn, putStr)

red, green, yellow, reset :: String
red = "\ESC[31m"
green = "\ESC[32m"
yellow = "\ESC[33m"
reset = "\ESC[0m"

data RaftStep = Base | Frame | Binding | Mast deriving (Eq, Ord, Show)
type Material = String
type Inventory = Map Material Int
data FerryItem = Crew | Wine | Cerber | Charon | None deriving (Eq, Show)
data FerryPuzzleState = FerryPuzzleState
  { itemsOnStartSide :: [FerryItem]
  , itemsOnEndSide :: [FerryItem]
  } deriving (Show)


requiredMaterials :: [(Material, Int)]
requiredMaterials =
  [ ("wood", 2),
    ("logs", 2),
    ("rope", 2),
    ("cloth", 1)
  ]

data State = State
  { you_are_at :: String
  , crew :: Int
  , disembarked :: Bool
  , holding :: [String]
  , game_over :: Bool
  , aeolus_island :: Bool
  , wind_bag_available :: Bool
  , magic_herb_available :: Bool
  , accessToUnderworld :: Bool
  , visitedUnderworld :: Bool
  , scyllaSurvivalRate :: Maybe Double
  , crewSurvivedSirens :: Maybe Int
  , charybdis_lure :: Bool
  , longerStay :: Bool
  , mapAvailable :: Bool
  , inventory :: Inventory
  , potion_recipe :: Bool
  , helios_blood :: Bool
  , strength_elixir :: Bool
  , raftStepsCompleted :: [RaftStep]
  , ferryPuzzleState :: Maybe FerryPuzzleState
  } deriving Show


init_state :: State
init_state = State
  { you_are_at = "open_sea"
  , crew = 600
  , disembarked = False
  , holding = []
  , game_over = False
  , aeolus_island = False
  , wind_bag_available = False
  , magic_herb_available = False
  , accessToUnderworld = False
  , visitedUnderworld = False
  , scyllaSurvivalRate = Nothing
  , crewSurvivedSirens = Nothing
  , charybdis_lure = False
  , potion_recipe = False
  , mapAvailable = False
  , helios_blood = False
  , longerStay = False
  , inventory = Map.empty
  , strength_elixir = False
  , raftStepsCompleted = []
  , ferryPuzzleState = Nothing
  }



sea_paths :: [((String, String), String)]
sea_paths =
  -- act I
  [ (("open_sea", "north"), "ithaca_sea")
  , (("open_sea", "west"), "lotus_sea")
  , (("ithaca_sea", "south"), "open_sea")
  , (("lotus_sea", "north"), "polyphemus_sea")
  , (("lotus_sea", "east"), "open_sea")
  , (("polyphemus_sea", "east"), "ithaca_sea")
  , (("polyphemus_sea", "south"), "lotus_sea")
  -- -- act II
  , (("circe_sea", "north"), "giants_sea")
  , (("circe_sea", "east"), "sirens_sea")
  , (("circe_sea", "west"), "underworld_sea")
  , (("underworld_sea", "north"), "giants_sea")
  , (("underworld_sea", "east"), "circe_sea")
  , (("sirens_sea", "north"), "giants_sea")
  , (("sirens_sea", "south"), "scylla_charybdis_sea")
  , (("giants_sea", "south"), "circe_sea")
  , (("scylla_charybdis_sea", "east"), "sun_god_sea")
  , (("sun_god_sea", "south"), "calypso_island")
  ]


lands :: [(String, String)]
lands =
  [ ("lotus_sea", "lotus_island"),
    ("polyphemus_sea", "polyphemus_cave"),
    ("open_sea", "aeolus_island"),
    ("circe_sea", "circe_island"),
    ("underworld_sea", "underworld"),
    ("sun_god_sea", "sun_god_island")
  ]


sail_debug :: String -> State -> IO State
sail_debug location state = do
  putStrLn $ "Moved to " ++ location ++ "."
  return state {you_are_at = location}


sail :: String -> State -> IO State
sail direction state
  | disembarked state = do
    putStrLn "You need to embark first."
    return state
  | you_are_at state == "ithaca_sea" && direction /= "south" = ithacaSeaStorm state
  | otherwise = case lookup_sea_path (you_are_at state) direction of
      Just destination ->
        if direction == "west" && you_are_at state == "circe_sea" && not (accessToUnderworld state) then do
          putStrLn "The path to the Underworld is blocked. You need access to the Underworld to sail west from Circe's Sea."
          return state
        else do
          putStrLn "Sailing..."
          return state {you_are_at = destination}
      Nothing -> do
        putStrLn "You set sail, but you either find nothing of note in that direction, or the way's impassable. You end up turning back."
        return state

showMap :: State -> IO State
showMap state
    | mapAvailable state = do
        putStrLn "Possible moves:"
        putStrLn "circe_sea, north, giants_sea"
        putStrLn "circe_sea, west, underworld_sea"
        putStrLn "circe_sea, east, sirens_sea"
        putStrLn "underworld_sea, north, giants_sea"
        putStrLn "underworld_sea, east, circe_sea"
        putStrLn "sirens_sea, north, giants_sea"
        putStrLn "sirens_sea, south, scylla_charybdis_sea"
        putStrLn "giants_sea, south, circe_sea"
        putStrLn "scylla_charybdis_sea, east, sun_god_sea"
        putStrLn "sun_god_sea, south, calypso_sea"
        putStrLn ""
        putStrLn "Allowed disembarks:"
        putStrLn "circe_sea, circe_island"
        putStrLn "underworld_sea, underworld"
        putStrLn "sun_god_sea, sun_god_island"
        return state
    | otherwise = do
        putStrLn "The map is not available at this time."
        return state


ithacaSeaStorm :: State -> IO State
ithacaSeaStorm state
  | "wind-bag" `elem` holding state = do
    putStrLn "\nWith the storm contained, it shouldn't take much longer to reach the coast. Mere weeks.\n"
    putStrLn "You wish you could relax now, but the wind gods' words won't let you. 'Be careful who you"
    putStrLn "trust, captain' pushes you to keep your eyes open at all times.\n"
    putStrLn "You guard the wind-bag to the best of your abilities, but the need to sleep proves stronger eventually.\n"
    putStrLn "The dream is a lovely one - the family reunited, Penelope and Telemachus in your arms.\n"
    putStrLn "The reality is not. The bag lies open. The raging storm tears at your ships with vengeance, vicious"
    putStrLn "currents leading you to distant shores far, far away from Ithaca."
    let newHolding = filter (/= "wind-bag") (holding state)
    return state { holding = newHolding, you_are_at = "circe_sea" }
  | crew state > 150 = do
    putStrLn "\nIt turns out you can't beat the force of nature that easily. You lose ships - the screams of over"
    putStrLn "a hundred men are drowned out by the storm as they disappear below the waves.\n"
    let updatedState = state { crew = crew state - 150 }
    newPlace <- randomPlace
    putStrLn "\nYou can't control your course as the currents are too strong. You should look around to see where you've ended up."
    return updatedState { you_are_at = newPlace }
  | otherwise = do
    putStrLn "\nThe storm is your final fight, the only thing still blocking your way home - and so you're ready"
    putStrLn "to risk it all, look for the way through even when there seems to be none.\n"
    putStrLn "\nYou try your best...\n"
    putStrLn "\nIt's just not enough.\n"
    putStrLn "\nYour ship is the last one from the fleet to fall victim to the crashing waves.\n"
    return $ finish state

-- Pomocnicza funkcja do losowania nowego miejsca
randomPlace :: IO String
randomPlace = do
  let places = ["lotus_sea", "ithaca_sea", "polyphemus_sea", "open_sea"]
  idx <- randomRIO (0, length places - 1)
  return (places !! idx)


lookup_sea_path :: String -> String -> Maybe String
lookup_sea_path current direction = lookup (current, direction) sea_paths


embark :: State -> IO State
embark state
  | you_are_at state == "polyphemus_cave" = do
      putStrLn "\nYou manage to embark on a ship and leave the Cyclops' cave behind."
      putStrLn "As you do, you can't shake the feeling of being watched, but the eyes are not only those of your men nor the foe you've escaped from."
      putStrLn "The gods have taken an interest in your actions. Something in the air has changed."
      let newState = state { you_are_at = "open_sea", disembarked = False, aeolus_island = True }
      return newState
  | disembarked state = do
      putStrLn "Embarking!"
      case lookup_sea (you_are_at state) of
        Just sea -> do
          let newState = state { you_are_at = sea, disembarked = False }
          return newState
        Nothing -> do
          putStrLn "There's no available sea to embark on."
          return state
  | otherwise = do
      putStrLn "You're already on a ship or there's no ship at all."
      return state


disembark :: State -> IO State
disembark state
  | disembarked state = do
    putStrLn "You're already on land."
    return state
  | you_are_at state == "polyphemus_sea" =
      if aeolus_island state then do
        putStrLn "Going back to the Cyclops' cave after all that has happened is a suicide. Your crew knows it"
        putStrLn "and refuses to risk it. You should know better, too."
        return state
      else do
        putStrLn "At the entrance of a cave, you find a herd of sheep - food much tastier than anything you"
        putStrLn "have left from your supplies. Some of your men stay behind, while the rest of you ventures"
        putStrLn "onward to search the tunnels."
        let newState = state { you_are_at = "polyphemus_cave", disembarked = True }
        meet_polyphemus newState
  | otherwise =
    case lookup_land (you_are_at state) of
      Just land -> do
          putStrLn "Disembarking!"
          return state { you_are_at = land, disembarked = True }
      Nothing -> do
          putStrLn "There's no solid land to disembark on."
          return state


meet_polyphemus :: State -> IO State
meet_polyphemus state = do
    putStrLn "You walk a long while, far into the cave, when a deep voice echoes through the darkness."
    putStrLn "'Who are you? What are you doing, breaking into the house of Polyphemus?'"
    putStrLn "Single, massive eye opens behind you, glowing in the light of your torch. Polyphemus"
    putStrLn "does not look happy as he waits for an answer. 'What's your name, stranger?'"
    putStr "|: "
    hFlush stdout
    name <- getLine
    putStrLn $ "'Are you the one who killed my sheep, " ++ name ++ "? My favourite sheep. You will pay for what you did"
    putStrLn "with your own blood.'"
    if "wine" `elem` holding state then do
        putStrLn "At the last second, you grab the flask of wine taken from the island of lotus-eaters and aim"
        putStrLn "for the cyclops' still opened mouth. The wine from lotus flowers makes his thoughts and movements"
        putStrLn "sluggish. He manages to land only a few blows."
        crew_death 6 state
    else do
        putStrLn "The fight is long and grueling, with much death on your side. Pools of fresh blood form on the cave's floor."
        crew_death 44 state
    if name == "nobody" then do
        putStrLn "But no one comes to his aid."
        putStrLn "It's your opportunity to leave the cave, embark on a ship and get the hell away."
        return state
    else do
        putStrLn "Then there's a sound of heavy steps coming from the direction of the only exit."
        putStrLn "There are more of them. Much, much more."
        putStrLn "You're not leaving this cave alive."
        return (finish state)


lookup_land :: String -> Maybe String
lookup_land sea = lookup sea lands


lookup_sea :: String -> Maybe String
lookup_sea land = fmap fst (find ((== land) . snd) lands)


look :: State -> IO State
look state = do
  let location = you_are_at state
  describe state


describe :: State -> IO State
describe state
  | you_are_at state == "lotus_sea" = do
    putStrLn "There's solid land nearby - you notice the glowing light of a fire. It seems inviting."
    return state
  | you_are_at state == "lotus_island" = do
    putStrLn "The island is calm and serene, the whole atmosphere making you sleepy. You see a lake"
    putStrLn "that's surrounded by plain houses. The people milling around pay you no mind at all,"
    putStrLn "their thoughts far away."
    putStrLn ""
    putStrLn "There's plenty of food on their tables - strange, glowing fruits, which you recognize as"
    putStrLn "mind-numbing lotus. There's also an abundance of wine - surely the lotus-eaters wouldn't"
    putStrLn "be mad, were you to take a jug for yourself."
    return state
  | you_are_at state == "ithaca_sea" && "wind-bag" `elem` holding state = do
    putStrLn "The sky is clear, the water smooth - no storm, no tidal wave. You see Ithaca - your destination, your"
    putStrLn "kingdom, your home - on the horizon. You can already feel the ghost of your wife's embrace."
    return state
  | you_are_at state == "ithaca_sea" = do
    putStrLn "Your way is blocked by giant waves and giant storms. You should probably turn back south, towards"
    putStrLn "calmer waters - but Ithaca's never been closer in the last ten years and the way home is through."
    return state
  | you_are_at state == "polyphemus_sea" = do
      putStrLn "You're not far from an island, rugged shoreline making way to green fields and rocky mountains"
      putStrLn "full of caves. One cave in particular looks easy to reach."
      return state
  | you_are_at state == "polyphemus_cave" = do
      putStrLn "The cave is dark and musty, even with a lit torch you barely see more than a few steps ahead."
      putStrLn "You navigate mostly by the sound of the sea waves to find your way back."
      return state
  | you_are_at state == "open_sea" && "wind-bag" `elem` holding state = do
      putStrLn "Calm open sea and the island of a god, nothing more for you here. You instinctively look"
      putStrLn "towards north, where the sky's a clear expanse of blue."
      return state
  | you_are_at state == "open_sea" && aeolus_island state = do
      putStrLn "Where once was nothing but water, now lies an island, floating on the waves. The blowing winds feel"
      putStrLn "different, too, and you can't stop the thought - the home of the wind god. It means hope, a lone"
      putStrLn "chance of getting help against the storm, if only you disembark and beg."
      return state
  | you_are_at state == "open_sea" = do
      putStrLn "Open sea stretches in all directions, no land in sight. Were you to believe your charts,"
      putStrLn "Ithaca lies north. North, where you can see a mass of dark clouds covering the sky."
      return state
  | you_are_at state == "aeolus_island" = do
    putStrLn "The home of the wind god, Aeolus, is just as unique as its owner might suggest. Loud and playful"
    putStrLn "with various puffs of clouds twisting and turning in the air, as if alive."
    putStrLn "Aeolus himself flies around as well, never in one place for long. You feel his gaze following you."
    return state
  | you_are_at state == "circe_sea" = do
    putStrLn "The waters here feel thick with enchantment, and Circe's island lies ominously ahead."
    return state
  | you_are_at state == "circe_island" =
      if visitedUnderworld state
      then do
        putStrLn "You step back onto Circe's island, feeling the weight of your journey to the Underworld lingering upon you."
        putStrLn "The familiar sights of her enchanted island are a strange comfort after the shadows of Hades."
        putStrLn "Circe greets you with a knowing look, sensing the trials you’ve endured and the wisdom you have gained."
        putStrLn "It’s clear she has more guidance to offer, should you seek her counsel."
        putStrLn "You may now talk to Circe to ask for further instructions on your journey."
        return state
      else do
        putStrLn "After disembarking on Circe's island, you step into a lush, enchanted forest."
        putStrLn "The air is thick with mystery, and your instincts warn you of hidden dangers."
        putStrLn "To continue, you can talk to Hermes or confront Circe to face her enchantments."
        return state
  | you_are_at state == "underworld_sea" = do 
    putStrLn "You have reached the mysterious and eerie waters of the Underworld."
    return state
  | you_are_at state == "underworld" = do
    putStrLn "You sense an opportunity to talk to Charon, the ferryman who will guide you across the River Styx."
    return state
  | you_are_at state == "giants_sea" = do
    putStrLn "You have entered the territory of dangerous giants! They begin hurling massive stones at your ships."
    updatedState <- giantsSea state 
    return updatedState
  | you_are_at state == "sirens_sea" = do
    putStrLn "The waters are calm but ominous as you approach the domain of the Sirens."
    putStrLn "In the distance, their figures are barely visible, their songs ready to ensnare anyone who listens."
    putStrLn "You must now decide whether to block your own ears or not."
    putStrLn "Type plug_ears if you want to block your ears or leave_ears_open if you want to hear the Sirens' song"
    choice <- getLine
    newState <- processSirensChoice choice state
    return newState
  | you_are_at state == "scylla_charybdis_sea" = do
    putStrLn "The sea grows treacherous as you approach the domain of Scylla and Charybdis."
    putStrLn "To your left, you see Scylla's ominous cliffs, while Charybdis churns the water violently to your right."
    putStrLn "You must choose which path to take to continue."
    putStrLn "Type sail_scylla to navigate past Scylla or sail_charybdis to risk waters near Charybdis."
    return state
  | you_are_at state == "sun_god_sea" = do
    putStrLn "A violent storm catches your ship, forcing you to seek refuge on a nearby island."
    putStrLn "This is the sacred island of the Sun God, Helios, where his holy cattle roam"
    return state
  | you_are_at state == "sun_god_island" = do
      putStrLn "You disembark onto the island, hoping the storm will soon pass."
      putStrLn "Unfortunately, the storm shows no sign of stopping, and you will need to stay here longer."
      putStrLn "Your food supplies are exhausted, and the crew is looking to you for guidance."
      putStrLn "You had better talk to them for advice."
      return state
  | you_are_at state == "calypso_island" = do
    putStrLn "You stand on the shores of an island. It is breathtaking but empty, nobody is to be seen."
    putStrLn "The only sound is wind blowing through the trees. You are exhausted from all your travels and collapse on the ground."
    putStrLn "Your crew is no loger alive, your ship is gone. You are left all alone."
    putStrLn "When you wake up, the first thing you see is a woman, a goddess, who introduces herself as Calypso."
    putStrLn "She is beautiful, but you want to return home. You can try asking her how to leave."
    return state
  | you_are_at state == "ithaca" = do
    putStrLn "At last, you set foot on Ithaca, your homeland."
    putStrLn "After all the trials, you've made it home, where every person on the street talks about"
    putStrLn "the queen's challenge."
    putStrLn "\nBut that's a story for another day."
    putStrLn "After years lost at sea, battles fought, and gods defied, you've finally reached Ithaca."
    putStrLn "You've proven that courage and loyalty can overcome even the wrath of gods.\n"
    putStrLn "Welcome home, Odysseus."
    return (finish state)
  | otherwise = do
    putStrLn "There's nothing special around here."
    return state


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
    let newplace = "circe_sea"
    let finalState = updatedState { you_are_at = newplace}
    return finalState


crew_count :: State -> IO State
crew_count state = do
  putStrLn $ "You have " ++ show (crew state) ++ " crew members remaining."
  return state


crew_death :: Int -> State -> IO State
crew_death n state = do
  let newState = state { crew = crew state - n }
  if crew newState <= 0 then do
      putStrLn (red ++ "You have lost all your crew members. Game Over!" ++ reset)
      return (finish newState)
  else do
      crew_count newState


take_item :: String -> State -> IO State
take_item object state
  | object `elem` holding state = do
    putStrLn $ "You already have the " ++ object ++ "."
    return state
  | you_are_at state == "aeolus_island" && wind_bag_available state = do
    putStrLn $ "You pick up the " ++ object ++ "."
    return state  { holding = object : holding state }
  | you_are_at state == "lotus_island" && object == "wine" = do
    putStrLn $ "You pick up the " ++ object ++ "."
    return state  { holding = object : holding state }
  | otherwise = do
    putStrLn "You can't find anything like that here."
    return state


talk :: String -> State -> IO State
talk person state
  | you_are_at state == "lotus_island" && person == "lotus_eaters" = do
      putStrLn "It takes a while to find someone present enough to talk to you. Most of the lotus-eaters"
      putStrLn "are too far gone to even notice you. The talk itself doesn't amount to much, though."
      putStrLn "The old woman speaks of great dangers up north, but she assures you everything is far"
      putStrLn "easier with wine."
      return state
  | you_are_at state == "polyphemus_cave" && (person == "polyphemus" || person == "cyclops") = do
      putStrLn "Wrong decision."
      putStrLn "\nPolyphemus doesn't hear any of the words you've spoken, but he does hear your voice and"
      putStrLn "blindly strikes in your direction, enraged. You barely avoid being crushed to death."
      if (crew state) > 84 then do
          putStrLn "\nSome of your men are not so fast."
          crew_death 4 state
      else
          return state
  | you_are_at state == "open_sea" && "wind-bag" `elem` holding state && person == "crew" = do
      putStrLn "The crew's overly eager about the wind-bag, you can't help but notice. They whisper of"
      putStrLn "treasure and trail off as soon as you come close - clearly not in a mood to talk."
      return state
  | you_are_at state == "open_sea" && aeolus_island state && person == "crew" = do
      putStrLn "Your second-in-command doesn't at all like the idea of asking a god for help. 'They're easy to"
      putStrLn "anger, captain, and there's only so much time before your luck with them runs out for good.'"
      return state
  | you_are_at state == "open_sea" && person == "crew" = do
      putStrLn "Your men tell you that there's nothing to look for east and south from here, but west - west"
      putStrLn "is where the birds fly, which probably means solid land."
      return state
  | you_are_at state == "aeolus_island" && not (wind_bag_available state) = do
      putStrLn "You kneel down and describe your situation, knowing that the god can hear."
      putStrLn "Your ask for assistance is first met with disheartening nothing, but then - laughter."
      putStrLn "\nAeolus comes to a stop right before you. 'I suppose we could play a game, Odysseus of Ithaca.'"
      putStrLn "I'll show you a bag with the winds of the storm all trapped. If it gets opened, whatever"
      putStrLn "the reason... well, good luck.'"
      putStrLn "'Do be careful who you trust, captain, you never really know.'"
      putStrLn "\nAnd with that, he's gone on a breeze again, leaving a tied wind-bag at your feet."
      let newState = state { wind_bag_available = True }
      return newState
  | you_are_at state == "circe_island" && person == "hermes" = do
    putStrLn "Hermes, appearing like a ghostly figure, approaches with wisdom and a gift."
    putStrLn "Hermes offers you a magical herb, saying it will protect you from Circe's spells. You may need it if you choose to confront her."
    putStrLn "You now have a magical herb in your possession."
    let newState = state {magic_herb_available = True}
    return newState
  | you_are_at state == "circe_island" && person == "circe" && visitedUnderworld state == False = do
        putStrLn "Circe nods in understanding, sensing your readiness to continue your journey."
        putStrLn "'If you wish to return to Ithaca,' she says, 'you must first venture west, to the Underworld, then directly come back to me.'"
        putStrLn "But heed my warning — avoid the north, for giants dwell there, and their strength is unmatched."
        putStrLn "Circe hands you an empty bottle, saying it may prove useful later in your journey."
        let newState = state
                { holding = "empty_bottle" : holding state
                , accessToUnderworld = True
                }
        putStrLn "With her guidance, you feel prepared to set sail once more."
        return newState
  | you_are_at state == "circe_island" && person == "circe" && visitedUnderworld state == True = do
        let currentCrew = crew state
        putStrLn "Circe sees the weight of your journey and speaks again with wisdom:"
        putStrLn "'Your next trial, Odysseus, is the Sirens' Sea, where their hypnotic singing lures sailors to destruction."
        putStrLn "You must have your crew plug their ears with beeswax to resist the sound. But you—remain unsealed and tied to the mast to hear their song.'\n"
        putStrLn "'After passing the Sirens, you will face Scylla and Charybdis. I advise you to sail closer to Scylla; though she will claim 6% of your crew, Charybdis devours entire ships.'\n"
        putStrLn "Decide now how many of your crew will assist me in gathering ingredients for a potion that could protect you against Scylla."
        putStrLn "This potion will be crucial if you choose to pass near Scylla, as it may shield you from becoming one of the 6% she devours."
        putStrLn "Each crew member represents a 1% chance of survival. The rest will collect beeswax to seal their ears."
        putStrLn $ "Your crew now consists of " ++ show currentCrew ++ " brave warriors."
        putStrLn "Please enter the number of crew for the potion:"
        potionCrew <- readLn :: IO Int
        let remainingCrew = currentCrew - min potionCrew 100
        let hivesCovered = remainingCrew `div` 5
        let protectedCrew = round $ fromIntegral currentCrew * fromIntegral (min hivesCovered 100) / 100
        let newState = state {
            crewSurvivedSirens = Just protectedCrew,
            scyllaSurvivalRate = Just (fromIntegral (min potionCrew 100) / 100.0),
            visitedUnderworld = False
        }
        putStrLn "Circe confirms your plan:"
        putStrLn $ show potionCrew ++ " crew members will work on the potion, granting you a " ++ show potionCrew ++ "% chance of survival."
        putStrLn $ show protectedCrew ++ " crew members are protected."
        putStrLn "Those unprotected by wax will perish under the Sirens' spell."
        putStrLn "When ready, you may depart towards the Sirens' Sea."
        return newState
  | you_are_at state == "circe_island" && crew state > 0 = do
        putStrLn "Your crew gathers, their expressions serious. 'Captain,' they say, 'it’s time we resume our journey to Ithaca.'"
        putStrLn "They remind you of the goal that has driven you across the seas, and their loyalty fills you with resolve."
        putStrLn "You can now TALK TO CIRCE to seek further guidance from her."
        return state
  | you_are_at state == "underworld" && person == "charon" = do
        putStrLn "Charon, the ferryman, explains the rules of the River Styx."
        putStrLn "He says he will help you cross, but only if you solve the puzzle."
        putStrLn "He can only take one thing at a time: the crew, the wine, or Cerberus, the three-headed dog."
        putStrLn "However, if the crew is left with the wine or Cerberus, disaster will strike."
        putStrLn "You must cross the river in a specific order to avoid tragedy."
        putStrLn "To start, Charon will take the crew across."
        putStrLn "start_ferry_puzzle"
        putStrLn "You can either take the wine or Cerberus next."
        putStrLn "The following commands are available to you:"
        putStrLn "1. 'ferry X' - to have Charon take X across the river."
        putStrLn "2. 'return X' - to have Charon return with X."
        putStrLn "X can be 'crew', 'wine', 'cerber', or 'none'."
        putStrLn "Please make sure to follow the correct order to avoid losing crew members."
        return state
  | you_are_at state == "underworld" && person == "tiresias" = do
      putStrLn "Tiresias, the prophet, speaks to you with grave solemnity:"
      putStrLn "I see your future, Odysseus. You may reach Ithaca, but the path will be fraught with hardships."
      putStrLn "The island of Helios and its sacred cattle will be your doom if you dare approach them."
      putStrLn "The fates have already sealed the tragic death of your mother, which you have yet to learn."
      putStrLn "The ferryman urges you to hurry, though he offers you the chance to talk to one of three shades: your mother, Achilles, or Agamemnon."
      putStrLn "But choose wisely, for you can only speak with one of them."
      return state { visitedUnderworld = True }
  | you_are_at state == "underworld" && longerStay state = do
        putStrLn "A spectral figure appears before you—your mother, Anticleia."
        putStrLn "'My dear son, I died of grief and longing for you.'"
        putStrLn "The days of our separation broke my heart. I wish I could have seen you return to Ithaca."
        putStrLn "With a soft sigh, she hands you a charm—a Charybdis Lure, a precious item that may one day save you from the whirlpool."
        if not (longerStay state) then do
            putStrLn "Now you must return to your ship. Further conversations are no longer possible."
            return state { charybdis_lure = True, accessToUnderworld = False, disembarked = False, you_are_at = "underworld_sea" }
        else
            return state { charybdis_lure = True, longerStay = False }
  | you_are_at state == "underworld" && person == "achilles" = do
      putStrLn "Achilles, the great warrior, speaks to you with fiery intensity:"
      putStrLn "'Odysseus, your journey is long, and I know the struggles you face. The gods play cruel games with you."
      putStrLn "But you must know, I feel no peace here in the Underworld. The thought of my death haunts me still.'"
      putStrLn "Achilles decides to speak to Charon to extend your stay. You now have time to speak to both your mother and Agamemnon for any wisdom they can provide."
      return state { longerStay = True }
  | you_are_at state == "underworld" && person == "agamemnon" = do
        putStrLn "Agamemnon, the great king of Mycenae, speaks to you from the shadows:"
        putStrLn "'I was betrayed by my wife, Clytemnestra, and murdered upon my return to Mycenae. But you, Odysseus, will fare better, I hope.'"
        putStrLn "He offers you a map of the seas ahead—knowledge of the islands that await you in Act II."
        putStrLn "Type 'show_map' to look at it."
        if not (longerStay state) then do
            putStrLn "Now you must return to your ship. Further conversations are no longer possible."
            return state { mapAvailable = True, accessToUnderworld = False, disembarked = False, you_are_at = "underworld_sea" }
        else
            return state { mapAvailable = True, longerStay = False }
  | you_are_at state == "sun_god_island" && person == "crew" = do
      putStrLn "Your crew gathers around, their faces pale with hunger."
      putStrLn "One of them speaks up: 'Captain, we cannot last without food. These cattle are our only chance.'"
      putStrLn "You remember Tiresias's warning not to eat the sacred cattle of Helios."
      putStrLn "Make your choice:"
      putStrLn "Type \"eat_cattle\" to let the crew feast and risk the wrath of the gods."
      putStrLn "Type \"do_not_eat_cattle\" to let your crew starve."
      return state
  | you_are_at state == "calypso_island" && person == "calypso" && hasAllMaterials (inventory state) = do
      putStrLn "Why don't you want to stay with me? I would give you everything you need."
      putStrLn "But I see that nothing can deter you from leaving me. If you truly wish to leave, the correct order to build the raft is cloth first, then rope, then logs, then wood."
      return state
  | you_are_at state == "calypso_island" && person == "calypso" = do
      putStrLn "Calypso gazes at you and says: 'Why hurry to leave? This island is paradise, and I will care for you here forever.'"
      putStrLn "To return to the life you truly seek, you must craft a raft that can withstand Poseidon's storms."
      putStrLn "Gather the resources of the island: wood, logs, rope, and cloth. Only then will you be able to return home."
      return state
  | otherwise = do
      putStrLn "It's not a time nor place for a talk with someone who's busy - or someone who's not even there."
      return state

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


proceed_to_sun_god_island :: State -> IO State
proceed_to_sun_god_island state = do
    putStrLn "After surviving the perilous pass, a fierce storm catches you off guard."
    putStrLn "The raging waves drive your ship eastward, and you find yourselves on the shores of the Island of the Sun God."
    return state { you_are_at = "sun_god_island" }

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

completeElixir :: State -> IO State
completeElixir state = do
    if potion_recipe state && helios_blood state then do
        putStrLn "Using the empty bottle and the blood of Helios’s cattle, you complete the Elixir of Strength."
        return state { strength_elixir = True}
    else if not (potion_recipe state) then do
        putStrLn "You lack the potion recipe."
        return state
    else do
        putStrLn "You lack the blood of Helios cattle."
        return state


continueJourney :: State -> IO State
continueJourney state
    | you_are_at state == "sun_god_island" =
        if strength_elixir state
            then do
                putStrLn "As you leave the island, a thunderous voice echoes: 'For your desecration, you shall be punished!'"
                putStrLn "The sea rages as a powerful storm descends upon your ship."
                putStrLn "Thanks to the Elixir of Strength, you alone withstand the gods' wrath and survive."
                putStrLn "Your crew perishes, and you lose consciousness..."
                return state { you_are_at = "calypso_island", disembarked = True, crew = 1 }
            else do
                putStrLn "As you sail eastward, the wrath of the gods descends upon you."
                putStrLn "A powerful storm engulfs your ship, smashing it to pieces."
                putStrLn "If only you had something, a magic spell or a potion, to give you strength..."
                putStrLn "GAME OVER: You have perished at sea due to the wrath of the gods."
                return state { game_over = True }
    | otherwise = do
        putStrLn "You cannot continue your journey from here."
        return state



doNotEatCattle :: State -> IO State
doNotEatCattle state = do
    let currentCrew = crew state
    let loss = 50
    let newCrewCount = max 0 (currentCrew - loss)
    putStrLn "You stand firm in your decision: 'We will not eat the cattle of the Sun God.'"
    putStrLn "The crew protests but ultimately obeys. Over the next days, hunger claims the lives of 50 members."
    return state {crew = newCrewCount}




hasAllMaterials :: Inventory -> Bool
hasAllMaterials inv = all (\(mat, reqAmt) -> Map.findWithDefault 0 mat inv >= reqAmt) requiredMaterials

gather :: Material -> State -> IO State
gather mat state
  | you_are_at state /= "calypso_island" || not (elem mat (map fst requiredMaterials)) = do
    putStrLn "There's no need to gather anything like that now."
    return state
  | hasAllMaterials (inventory state) = do
    putStrLn "You have gathered all necessary materials for the raft! You can start building it."
    return state
  | otherwise = do
      let newInventory = addMaterial mat (inventory state)
      putStrLn $ "You have gathered " ++ show (Map.findWithDefault 0 mat newInventory) ++ " " ++ mat ++ "(s) already."
      return state {inventory = newInventory}

addMaterial :: Material -> Inventory -> Inventory
addMaterial mat inv =
  let currentAmt = Map.findWithDefault 0 mat inv
      requiredAmt = fromMaybe 0 (lookup mat requiredMaterials)
   in if currentAmt < requiredAmt
        then Map.insert mat (currentAmt + 1) inv
        else inv

build :: Material -> State -> IO State
build mat state =
  if hasAllMaterials (inventory state)
    then
      let steps = raftStepsCompleted state
      in case mat of
        "logs" ->
          if Base `elem` steps
            then do
              putStrLn "You have to build something else."
              return state
            else do
              putStrLn "You lay the base from logs as the foundation of your raft."
              return state {raftStepsCompleted = Base : steps}
        "wood" ->
          if Base `elem` steps && Binding `elem` steps && not (Frame `elem` steps)
            then do
              putStrLn "You arrange the wood into a stable frame on top of the base."
              return state {raftStepsCompleted = Frame : steps}
            else do
              putStrLn "You have to build something else."
              return state
        "rope" ->
          if Base `elem` steps && not (Binding `elem` steps)
            then do
              putStrLn "You tie everything together with rope to stabilize the structure."
              return state {raftStepsCompleted = Binding : steps}
            else do
              putStrLn "You have to build something else."
              return state
        "cloth" ->
          if Base `elem` steps && Frame `elem` steps && Binding `elem` steps && not (Mast `elem` steps)
            then do
              putStrLn "You set up the mast. The raft is complete! You can now attempt to escape Calypso's Island."
              return state {raftStepsCompleted = Mast : steps}
            else do
              putStrLn "You have to build something else."
              return state
        _ -> do
          putStrLn "Nothing to build with that."
          return state
    else do
      putStrLn "Gather the materials first."
      return state

escape :: State -> IO State
escape state
  | Mast `elem` raftStepsCompleted state = do
    putStrLn "With your raft complete, you set out to sea, leaving Calypso's Island behind."
    let newState = state { you_are_at = "ithaca", disembarked = False }
    describe newState
  | otherwise = do
    putStrLn "You cannot leave without completing the raft first."
    return state

finish :: State -> State
finish state = state { game_over = True }


game_loop state = do
  if game_over state then do
    putStrLn ""
    putStrLn (green ++ "------------------------------ THE END -----------------------------" ++ reset)
    putStrLn (yellow ++ "                      Thank you for playing!" ++ reset)
    putStrLn (green ++ "--------------------------------------------------------------------" ++ reset)
    return ()
  else do
    putStr "|: "
    hFlush stdout
    input <- getLine
    let newStateIO = process_input input state
    newState <- newStateIO
    game_loop newState



process_input :: String -> State -> IO State
process_input input state
  | "sail" `elem` words input = sail (last (words input)) state
  | "embark" == input = embark state
  | "disembark" == input = disembark state
  | "look" == input = look state
  | "crew_count" == input = crew_count state
  | "take" `elem` words input = take_item (last (words input)) state
  | "talk" `elem` words input = talk (last (words input)) state
  | "gather" `elem` words input = gather (last (words input)) state
  | "build" `elem` words input = build (last (words input)) state
  | "escape" == input = escape state
  | "finish" == input = return (finish state)
  | "sail_debug" `elem` words input = sail_debug (last (words input)) state
  | "confront" `elem` words input && "circe" `elem` words input = confrontCirce state
  | "sail_scylla" == input = sail_scylla state
  | "sail_charybdis" == input = sail_charybdis state
  | "act_II" == input = do return state { you_are_at = "circe_sea" }
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
  | otherwise = do
      putStrLn "Invalid command"
      return state





play :: IO ()
play = game_loop init_state
