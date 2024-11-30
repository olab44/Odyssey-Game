module Exploration (play) where

import Control.Monad (when)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Exit (exitSuccess)

red, green, yellow, reset :: String
red = "\ESC[31m"
green = "\ESC[32m"
yellow = "\ESC[33m"
reset = "\ESC[0m"

data State = State
  { youAreAt :: String
  , crewCount :: Int
  , disembarked :: Bool
  , holding :: [String]
  , game_over :: Bool
  } deriving Show


initState :: State
initState = State
  { youAreAt = "open_sea"
  , crewCount = 600
  , disembarked = False
  , holding = []
  , game_over = False
  }


seaPaths :: [((String, String), String)]
seaPaths =
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
  ]


sail :: String -> State -> IO State
sail direction state
  | disembarked state = do
    putStrLn "You need to embark first."
    return state
  | otherwise =
    case lookupSeaPath (youAreAt state) direction of
      Just destination -> do
          putStrLn "Sailing..."
          return state { youAreAt = destination }
      Nothing -> do
          putStrLn "You set sail, but you either find nothing of note in that direction, or the way's impassable. You end up turning back."
          return state


lookupSeaPath :: String -> String -> Maybe String
lookupSeaPath current direction = lookup (current, direction) seaPaths


embark :: State -> IO State
embark state
  | disembarked state = do
    putStrLn "Embarking!"
    return state { disembarked = False }
  | otherwise = do
    putStrLn "You're already on a ship or there's no ship at all."
    return state


disembark :: State -> IO State
disembark state
  | disembarked state = do
    putStrLn "You're already on land."
    return state
  | otherwise =
    case lookupLand (youAreAt state) of
      Just land -> do
          putStrLn "Disembarking!"
          return state { youAreAt = land, disembarked = True }
      Nothing -> do
          putStrLn "There's no solid land to disembark on."
          return state


lookupLand :: String -> Maybe String
lookupLand sea = lookup sea lands


look :: State -> String
look state = "You are at: " ++ youAreAt state


crewCountFn :: State -> String
crewCountFn state
  | crewCount state <= 0 = "All crew members have perished. You have lost the game.\n"
  | otherwise = "You have " ++ show (crewCount state) ++ " crew members remaining."


takeItem :: String -> State -> IO State
takeItem object state
  | object `elem` holding state = do
    putStrLn $ "You already have the " ++ object ++ "."
    return state
  | otherwise = do
    putStrLn $ "You take the " ++ object
    return state { holding = object : holding state }


talk :: String -> State -> IO State
talk person state = do
  putStrLn $ "Talking to " ++ person
  return state


play :: IO ()
play = gameLoop initState

gameLoop :: State -> IO ()
gameLoop state
  | game_over state = do
    putStrLn ""
    putStrLn (green ++ "------------------------------ THE END -----------------------------" ++ reset)
    putStrLn (yellow ++ "                      Thank you for playing!" ++ reset)
    putStrLn (green ++ "--------------------------------------------------------------------" ++ reset)
    return ()
  | otherwise = do
    putStrLn "Enter your command:"
    input <- getLine
    let newStateIO = processInput input state
    newState <- newStateIO
    gameLoop newState


processInput :: String -> State -> IO State
processInput input state
  | "sail" `elem` words input = sail (last (words input)) state
  | "embark" == input = embark state
  | "disembark" == input = disembark state
  | "look" == input = do
    putStrLn (look state)
    return state
  | "crew_count" == input = do
    putStrLn (crewCountFn state)
    return state
  | "take" `elem` words input = takeItem (last (words input)) state
  | "talk" `elem` words input = talk (last (words input)) state
  | "finish" == input = return (finish state)
  | otherwise = do
    putStrLn "Invalid command"
    return state



finish :: State -> State
finish state = state { game_over = True }

