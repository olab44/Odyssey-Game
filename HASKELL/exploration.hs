module Exploration where
import ActII.GiantsSea
import ActII.Sirens
import Types
import Utils
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
import WorldMap

sail :: String -> State -> IO State
sail direction state
  | disembarked state = do
    putStrLn "You need to embark first."
    return state
  | you_are_at state == ithaca_sea && direction /= "south" = ithacaSeaStorm state
  | otherwise = case lookup_sea_path (you_are_at state) direction of
      Just destination ->
        if direction == "west" && you_are_at state == circe_sea && not (accessToUnderworld state) then do
          putStrLn "The path to the Underworld is blocked. You need access to the Underworld to sail west from Circe's Sea."
          return state
        else do
          newState <- describe state {you_are_at = destination}
          return newState
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
    return state { holding = newHolding, you_are_at = circe_sea }
  | crew state > 150 = do
    putStrLn "\nIt turns out you can't beat the force of nature that easily. You lose ships - the screams of over"
    putStrLn "a hundred men are drowned out by the storm as they disappear below the waves."
    let updatedState = state { crew = crew state - 150 }
    newPlace <- randomPlace
    putStrLn "You can't control your course as the currents are too strong. You should look around to see where you've ended up."
    return updatedState { you_are_at = newPlace }
  | otherwise = do
    putStrLn "\nThe storm is your final fight, the only thing still blocking your way home - and so you're ready"
    putStrLn "to risk it all, look for the way through even when there seems to be none."
    putStrLn "\nYou try your best...\n"
    putStrLn "It's just not enough."
    putStrLn "Your ship is the last one from the fleet to fall victim to the crashing waves."
    return $ finish state

-- Pomocnicza funkcja do losowania nowego miejsca
randomPlace :: IO Location
randomPlace = do
  let places = [lotus_sea, ithaca_sea, polyphemus_sea, open_sea]
  idx <- randomRIO (0, length places - 1)
  return (places !! idx)

lookup_sea_path :: Location -> String -> Maybe Location
lookup_sea_path current direction = lookup (current, direction) sea_paths

embark :: State -> IO State
embark state
  | you_are_at state == polyphemus_cave = do
      putStrLn "\nYou manage to embark on a ship and leave the Cyclops' cave behind."
      putStrLn "As you do, you can't shake the feeling of being watched, but the eyes are not only those of your men nor the foe"
      putStrLn "you've escaped from. The gods have taken an interest in your actions. Something in the air has changed."
      putStrLn "You end up back at the same open sea, but maybe you should look around more."
      let newState = state { you_are_at = open_sea, disembarked = False, aeolusAccess = True }
      return newState
  | disembarked state = do
      case lookup_sea (you_are_at state) of
        Just sea -> do
          newState <- describe state { you_are_at = sea, disembarked = False }
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
  | you_are_at state == polyphemus_sea =
      if aeolusAccess state then do
        putStrLn "Going back to the Cyclops' cave after all that has happened is a suicide. Your crew knows it"
        putStrLn "and refuses to risk it. You should know better, too."
        return state
      else do
        putStrLn "At the entrance of a cave, you find a herd of sheep - food much tastier than anything you"
        putStrLn "have left from your supplies. Some of your men stay behind, while the rest of you ventures"
        putStrLn "onward to search the tunnels."
        let newState = state { you_are_at = polyphemus_cave, disembarked = True }
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
    stateAfterFight <- if "wine" `elem` holding state 
        then do
            putStrLn "At the last second, you grab the flask of wine taken from the island of lotus-eaters and aim"
            putStrLn "for the cyclops' still opened mouth. The wine from lotus flowers makes his thoughts and movements"
            putStrLn "sluggish. He manages to land only a few blows."
            crew_death 6 state
        else do
            putStrLn "The fight is long and grueling, with much death on your side. Pools of fresh blood form on the cave's floor."
            crew_death 44 state
    if name == "nobody" 
        then do
            putStrLn "But no one comes to his aid."
            putStrLn "It's your opportunity to leave the cave, embark on a ship and get the hell away."
            return stateAfterFight
        else do
            putStrLn "Then there's a sound of heavy steps coming from the direction of the only exit."
            putStrLn "There are more of them. Much, much more."
            putStrLn "You're not leaving this cave alive."
            return (finish stateAfterFight)


lookup_land :: Location -> Maybe Location
lookup_land sea = lookup sea lands


lookup_sea :: Location -> Maybe Location
lookup_sea land = fmap fst (find ((== land) . snd) lands)


describe :: State -> IO State
describe state = do
    putStrLn $ describeLocation state

    -- Handle special locations with auto actions
    if you_are_at state == giants_sea then do
        updatedState <- giantsSea state
        return updatedState
    else if you_are_at state == sirens_sea then do
        putStrLn "You must now decide whether to block your own ears or not."
        putStrLn "Type 'plug_ears' if you want to block your ears or 'leave_ears_open' if you want to hear the Sirens' song"
        choice <- getLine
        newState <- processSirensChoice choice state
        return newState
    else if you_are_at state == ithaca_sea then do
        return (finish state)
    else
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
  | you_are_at state == aeolus_island && wind_bag_available state = do
    putStrLn $ "You pick up the " ++ object ++ "."
    return state  { holding = object : holding state }
  | you_are_at state == lotus_island && object == "wine" = do
    putStrLn $ "You pick up the " ++ object ++ "."
    return state  { holding = object : holding state }
  | otherwise = do
    putStrLn "You can't find anything like that here."
    return state


talk :: String -> State -> IO State
talk person state
  | you_are_at state == lotus_island && person == "lotus_eaters" = do
      putStrLn "It takes a while to find someone present enough to talk to you. Most of the lotus-eaters"
      putStrLn "are too far gone to even notice you. The talk itself doesn't amount to much, though."
      putStrLn "The old woman speaks of great dangers up north, but she assures you everything is far"
      putStrLn "easier with wine."
      return state
  | you_are_at state == polyphemus_cave && (person == "polyphemus" || person == "cyclops") = do
      putStrLn "Wrong decision."
      putStrLn "\nPolyphemus doesn't hear any of the words you've spoken, but he does hear your voice and"
      putStrLn "blindly strikes in your direction, enraged. You barely avoid being crushed to death."
      if (crew state) > 84 then do
          putStrLn "\nSome of your men are not so fast."
          crew_death 4 state
      else
          return state
  | you_are_at state == open_sea && "wind-bag" `elem` holding state && person == "crew" = do
      putStrLn "The crew's overly eager about the wind-bag, you can't help but notice. They whisper of"
      putStrLn "treasure and trail off as soon as you come close - clearly not in a mood to talk."
      return state
  | you_are_at state == open_sea && aeolusAccess state && person == "crew" = do
      putStrLn "Your second-in-command doesn't at all like the idea of asking a god for help. 'They're easy to"
      putStrLn "anger, captain, and there's only so much time before your luck with them runs out for good.'"
      return state
  | you_are_at state == open_sea && person == "crew" = do
      putStrLn "Your men tell you that there's nothing to look for east and south from here, but west - west"
      putStrLn "is where the birds fly, which probably means solid land."
      return state
  | you_are_at state == aeolus_island && not (wind_bag_available state) = do
      putStrLn "You kneel down and describe your situation, knowing that the god can hear."
      putStrLn "Your ask for assistance is first met with disheartening nothing, but then - laughter."
      putStrLn "\nAeolus comes to a stop right before you. 'I suppose we could play a game, Odysseus of Ithaca.'"
      putStrLn "I'll show you a bag with the winds of the storm all trapped. If it gets opened, whatever"
      putStrLn "the reason... well, good luck.'"
      putStrLn "'Do be careful who you trust, captain, you never really know.'"
      putStrLn "\nAnd with that, he's gone on a breeze again, leaving a tied wind-bag at your feet."
      let newState = state { wind_bag_available = True }
      return newState
  | you_are_at state == circe_island && person == "hermes" = do
    putStrLn "Hermes, appearing like a ghostly figure, approaches with wisdom and a gift."
    putStrLn "Hermes offers you a magical herb, saying it will protect you from Circe's spells. You may need it if you choose to confront her."
    putStrLn "You now have a magical herb in your possession."
    let newState = state {magic_herb_available = True}
    return newState
  | you_are_at state == circe_island && person == "circe" && visitedUnderworld state == False = do
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
  | you_are_at state == circe_island && person == "circe" && visitedUnderworld state == True = do
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
  | you_are_at state == circe_island && crew state > 0 = do
        putStrLn "Your crew gathers, their expressions serious. 'Captain,' they say, 'it’s time we resume our journey to Ithaca.'"
        putStrLn "They remind you of the goal that has driven you across the seas, and their loyalty fills you with resolve."
        putStrLn "You can now TALK TO CIRCE to seek further guidance from her."
        return state
  | you_are_at state == underworld && person == "charon" = do
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
  | you_are_at state == underworld && person == "tiresias" = do
      putStrLn "Tiresias, the prophet, speaks to you with grave solemnity:"
      putStrLn "I see your future, Odysseus. You may reach Ithaca, but the path will be fraught with hardships."
      putStrLn "The island of Helios and its sacred cattle will be your doom if you dare approach them."
      putStrLn "The fates have already sealed the tragic death of your mother, which you have yet to learn."
      putStrLn "The ferryman urges you to hurry, though he offers you the chance to talk to one of three shades: your mother, Achilles, or Agamemnon."
      putStrLn "But choose wisely, for you can only speak with one of them."
      return state { visitedUnderworld = True }
  | you_are_at state == underworld && longerStay state = do
        putStrLn "A spectral figure appears before you—your mother, Anticleia."
        putStrLn "'My dear son, I died of grief and longing for you.'"
        putStrLn "The days of our separation broke my heart. I wish I could have seen you return to Ithaca."
        putStrLn "With a soft sigh, she hands you a charm—a Charybdis Lure, a precious item that may one day save you from the whirlpool."
        if not (longerStay state) then do
            putStrLn "Now you must return to your ship. Further conversations are no longer possible."
            return state { charybdis_lure = True, accessToUnderworld = False, disembarked = False, you_are_at = underworld_sea }
        else
            return state { charybdis_lure = True, longerStay = False }
  | you_are_at state == underworld && person == "achilles" = do
      putStrLn "Achilles, the great warrior, speaks to you with fiery intensity:"
      putStrLn "'Odysseus, your journey is long, and I know the struggles you face. The gods play cruel games with you."
      putStrLn "But you must know, I feel no peace here in the Underworld. The thought of my death haunts me still.'"
      putStrLn "Achilles decides to speak to Charon to extend your stay. You now have time to speak to both your mother and Agamemnon for any wisdom they can provide."
      return state { longerStay = True }
  | you_are_at state == underworld && person == "agamemnon" = do
        putStrLn "Agamemnon, the great king of Mycenae, speaks to you from the shadows:"
        putStrLn "'I was betrayed by my wife, Clytemnestra, and murdered upon my return to Mycenae. But you, Odysseus, will fare better, I hope.'"
        putStrLn "He offers you a map of the seas ahead—knowledge of the islands that await you in Act II."
        putStrLn "Type 'show_map' to look at it."
        if not (longerStay state) then do
            putStrLn "Now you must return to your ship. Further conversations are no longer possible."
            return state { mapAvailable = True, accessToUnderworld = False, disembarked = False, you_are_at = underworld_sea }
        else
            return state { mapAvailable = True, longerStay = False }
  | you_are_at state == sun_god_island && person == "crew" = do
      putStrLn "Your crew gathers around, their faces pale with hunger."
      putStrLn "One of them speaks up: 'Captain, we cannot last without food. These cattle are our only chance.'"
      putStrLn "You remember Tiresias's warning not to eat the sacred cattle of Helios."
      putStrLn "Make your choice:"
      putStrLn "Type \"eat_cattle\" to let the crew feast and risk the wrath of the gods."
      putStrLn "Type \"do_not_eat_cattle\" to let your crew starve."
      return state
  | you_are_at state == calypso_island && person == "calypso" && hasAllMaterials (inventory state) = do
      putStrLn "Why don't you want to stay with me? I would give you everything you need."
      putStrLn "But I see that nothing can deter you from leaving me. If you truly wish to leave, the correct order to build the raft is cloth first, then rope, then logs, then wood."
      return state
  | you_are_at state == calypso_island && person == "calypso" = do
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
  | you_are_at state /= calypso_island || not (elem mat (map fst requiredMaterials)) = do
    putStrLn "There's no need to gather anything like that now."
    return state
  | hasAllMaterials (inventory state) = do
    putStrLn "You have gathered all necessary materials for the raft! You can start building it."
    return state
  | otherwise = do
      let (response, newInventory) = addMaterial mat (inventory state)
      putStrLn response
      return state {inventory = newInventory}

addMaterial :: Material -> Inventory -> (String, Inventory)
addMaterial mat inv =
  let currentAmt = Map.findWithDefault 0 mat inv
      requiredAmt = fromMaybe 0 (lookup mat requiredMaterials)
      newInv = Map.insert mat (currentAmt + 1) inv
    in if currentAmt < requiredAmt
      then ("You have gathered " ++ show (Map.findWithDefault 0 mat newInv) ++ " of " ++ mat ++ " so far.", newInv)
      else ("You have enough of " ++ mat ++ ".", inv)

build :: Material -> State -> IO State
build mat state
  | not (hasAllMaterials (inventory state)) = do
      putStrLn "Gather the materials first."
      return state
  | otherwise = case mat of
      "logs" -> buildStep Base "You lay the base from logs as the foundation of your raft." Base state
      "wood" -> buildStep Frame "You arrange the wood into a stable frame on top of the base." Frame state
      "rope" -> buildStep Binding "You tie everything together with rope to stabilize the structure." Binding state
      "cloth" -> buildStep Mast "You set up the mast. The raft is complete! You can now attempt to escape Calypso's Island." Mast state
      _ -> do
          putStrLn "Nothing to build with that."
          return state
  where
    buildStep step message requiredStep state
      | requiredConditions state step = do
          putStrLn message
          return state { raftStepsCompleted = step : raftStepsCompleted state }
      | otherwise = do
          putStrLn "You have to build something else."
          return state
    requiredConditions state step = case step of
      Base -> not (Base `elem` raftStepsCompleted state)
      Frame -> Base `elem` raftStepsCompleted state && not (Frame `elem` raftStepsCompleted state)
      Binding -> Base `elem` raftStepsCompleted state && not (Binding `elem` raftStepsCompleted state)
      Mast -> Base `elem` raftStepsCompleted state && Frame `elem` raftStepsCompleted state && Binding `elem` raftStepsCompleted state && not (Mast `elem` raftStepsCompleted state)

escape :: State -> IO State
escape state
  | Mast `elem` raftStepsCompleted state = do
    putStrLn "With your raft complete, you set out to sea, leaving Calypso's Island behind."
    let newState = state { you_are_at = ithaca, disembarked = False }
    describe newState
  | otherwise = do
    putStrLn "You cannot leave without completing the raft first."
    return state

finish :: State -> State
finish state = state { game_over = True }