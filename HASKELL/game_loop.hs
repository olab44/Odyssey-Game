module GameLoop where
import System.IO (hFlush, stdout)
import Utils
import Types
import InputProcess
import InitSetup

play :: IO ()
play = game_loop init_state

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
    newState <- process_input input state
    game_loop newState
