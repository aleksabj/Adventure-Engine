module GameLoop (startGame) where

import System.IO (hFlush, stdout)
import Engine
import GameWorld

-- Starts the interactive game loop
startGame :: GameState -> IO ()
startGame state = do
    putStrLn "\n"
    putStrLn "----------------------"
    putStrLn "Welcome to the Adventure Game!"
    putStrLn "Type 'look' to observe your surroundings."
    putStrLn "Type 'inventory' to check your items."
    putStrLn "Type 'help' at any time to see this message again."
    putStrLn "----------------------"
    putStrLn "\n"
    putStrLn "----------------------"
    putStrLn $ describeCurrentLocation state
    putStrLn "----------------------"
    loop state

-- REPL loop: prompt → command → response → repeat
loop :: GameState -> IO ()
loop state = do
    putStr "\n> "
    hFlush stdout
    input <- getLine
    let (msg, newState) = processCommand input state
    putStrLn msg
    case checkGoal newState of
        Just winMsg -> putStrLn winMsg >> return ()
        Nothing     -> loop newState

