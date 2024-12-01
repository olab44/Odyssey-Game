module Exploration (play) where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Data.List (find)

red, green, yellow, reset :: String
red = "\ESC[31m"
green = "\ESC[32m"
yellow = "\ESC[33m"
reset = "\ESC[0m"

data RaftStep = Base | Frame | Binding | Mast deriving (Eq, Ord, Show)
type Material = String
type Inventory = Map Material Int

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
  , inventory :: Inventory
  , raftStepsCompleted :: [RaftStep]
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
  , inventory = Map.empty
  , raftStepsCompleted = []
  }


sea_paths :: [((String, String), String)]
sea_paths =
  [ (("open_sea", "north"), "ithaca_sea")
  , (("open_sea", "west"), "lotus_sea")
  , (("ithaca_sea", "south"), "open_sea")
  , (("lotus_sea", "north"), "polyphemus_sea")
  , (("lotus_sea", "east"), "open_sea")
  , (("polyphemus_sea", "east"), "ithaca_sea")
  , (("polyphemus_sea", "south"), "lotus_sea")
  ]


lands :: [(String, String)]
lands =
  [ ("lotus_sea", "lotus_island")
  , ("polyphemus_sea", "polyphemus_cave")
  , ("open_sea", "aeolus_island")
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
      Just destination -> do
        putStrLn "Sailing..."
        return state { you_are_at = destination }
      Nothing -> do
        putStrLn "You set sail, but you either find nothing of note in that direction, or the way's impassable. You end up turning back."
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


game_loop :: State -> IO ()
game_loop state
  | game_over state = do
    putStrLn ""
    putStrLn (green ++ "------------------------------ THE END -----------------------------" ++ reset)
    putStrLn (yellow ++ "                      Thank you for playing!" ++ reset)
    putStrLn (green ++ "--------------------------------------------------------------------" ++ reset)
    return ()
  | otherwise = do
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
  | otherwise = do
    putStrLn "Invalid command"
    return state


play :: IO ()
play = game_loop init_state
