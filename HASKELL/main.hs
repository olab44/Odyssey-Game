module Main (main) where
import Utils
import GameLoop (play)
import WorldMap

-- Welcome Screen
welcome :: IO ()
welcome = do
    putStrLn ""
    putStrLn (green ++ "---------------------- THE ODYSSEY : THE GAME ----------------------" ++ reset)
    putStrLn ""
    putStrLn (yellow ++ "                                                    _  _                ")
    putStrLn "                                                   ' \\/ '              "
    putStrLn "   _  _                         <|                                      "
    putStrLn "    \\/                  _'_______'___________'___                      "
    putStrLn "                      /'                      //                        "
    putStrLn "                    /'                       '/           _  _          "
    putStrLn "                   |                       '/ \\            \\/         "
    putStrLn "                   ||                      ||  \\                       "
    putStrLn "                  /`|                      `\\   \\                     "
    putStrLn "        _==_     /  \\\\                      `\\   \\                  "
    putStrLn "       //  //   /    `\\                       \\\\  \\                 "
    putStrLn "       \\\\      /       \\\\                      \\\\  \\             "
    putStrLn "        \\ \\   /         `\\\\______________________\\  \\        ^%%  "
    putStrLn "         `\\ \\/           /       | |             \\   \\/   ^%= ^%%=  "
    putStrLn "           \\\\ \\_______ _/_____ __|_|__ ___________\\_/||   ^^     %^ "
    putStrLn "            `\\\\|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_/|/         %=%    "
    putStrLn " =_  %%!_   __\\    _______________                 /   __.%% !^%% _^   "
    putStrLn "^^%%^=%^=^^^%%^^\\'/_)/_)_/_)__)/_)/)/)_)_'_'_'__//)/)/)/)%%=%^^^%^^    "
    putStrLn "   =^=!%%=^^^!^^^!!^^^%%=%%!!!!^^^%%^^^!!%%%=^^^!!!%%!!%%%^^^^%^%       "
    putStrLn (green ++ "--------------------------------------------------------------------" ++ reset)
    putStrLn ""

-- Instructions
instructions :: IO ()
instructions = do
    putStrLn (green ++ "--------------------------------------------------------------------" ++ reset)
    putStrLn "AVAILABLE COMMANDS:"
    putStrLn "In all acts, you can use:"
    putStrLn "- crew_count : to check how many crew members are still alive"
    putStrLn "- look : To observe your surroundings."
    putStrLn "- talk person : To interact with characters."
    putStrLn "- embark : To board your ship"
    putStrLn "- disembark : To leave your ship and explore on land."
    putStrLn "- sail direction : to sail to another location"
    putStrLn ""
    putStrLn "In Act I, you can additionally use:"
    putStrLn "- take object : To collect items you find."
    putStrLn ""
    putStrLn "In Act III, you can additionally use:"
    putStrLn "- gather material : To gather essential materials for crafting."
    putStrLn "- build material : To construct a raft or other items needed to progress."
    putStrLn ""
    putStrLn "Good luck on your journey, brave Odysseus!"
    putStrLn (yellow ++ "Type 'start' to play the game or type 'finish' anytime to close it." ++ reset)
    putStrLn (green ++ "--------------------------------------------------------------------" ++ reset)
    putStrLn ""
    prompt_for_start

-- Starting the Game
prompt_for_start :: IO ()
prompt_for_start = do
    input <- getLine
    if input == "start"
        then start
        else do
            putStrLn "Input 'start' to play the game."
            prompt_for_start

start :: IO ()
start = do
    putStrLn "\nThe city of Troy has been seized, the war won. Ten years away from home are finally coming to"
    putStrLn "an end. You're on your way to Ithaca now, hundreds of sea miles both behind and ahead of you."
    putStrLn "The problem's not the distance, you know very well, but the dangers that lie in between"
    putStrLn "- you'll have to act smart to get back to your homeland.\n"
    putStrLn "You think of your wife. You think of your little boy, who's probably not so little anymore.\n"
    putStrLn "Failure is not an option.\n"
    putStrLn "With trusted crew by your side, the six hundred men under your command, there just might be"
    putStrLn "hope for you after all.\n"
    putStrLn "You should look around, talk to them while it's not busy or sail to cross the sea and reach home.\n"
    play

-- Main Program
main :: IO ()
main = do
    welcome
    instructions