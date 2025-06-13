module GameLoop (startGame) where

import System.IO (hFlush, stdout)

-- Starts the interactive game loop
startGame :: IO ()
startGame = do
    putStrLn "----------------------"
    putStrLn "Welcome to the Adventure Game!"
    putStrLn "Type 'look' to observe your surroundings."
    putStrLn "Type 'inventory' to check your items."
    putStrLn "Type 'help' at any time to see this message again."
    putStrLn "----------------------"
    loop

-- REPL loop: prompt → command → response → repeat
loop :: IO ()
loop = do
    putStr "\n> "
    hFlush stdout
    input <- getLine
    putStrLn $ "You entered: " ++ input
    loop
