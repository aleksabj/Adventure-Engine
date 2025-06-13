module GameLoop (startGame) where

-- Placeholder game loop
startGame :: IO ()
startGame = do
    putStrLn "Starting game..."
    gameLoop

gameLoop :: IO ()
gameLoop = do
    putStr "> "
    input <- getLine
    putStrLn $ "You said: " ++ input
    gameLoop
