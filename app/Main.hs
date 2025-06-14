module Main where

import Parser
import GameWorld
import GameLoop
import Engine

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "----------------------"
  putStrLn "Welcome to the Adventure Engine!"
  putStrLn "Please choose a game to play:"
  putStrLn "  1) Intro Escape"
  putStrLn "  2) Library Secrets"
  putStrLn "  3) Castle Treasure"
  putStr "Enter choice (1/2/3): "
  choice <- getLine
  let filePath = case choice of
        "1" -> "examples/intro-escape.txt"
        "2" -> "examples/library-secrets.txt"
        "3" -> "examples/castle-treasure.txt"
  putStrLn $ "Loading: " ++ filePath
  contents <- readFile filePath

  case parseGameFile contents of
    Left err -> print err
    Right gw -> do
      let startLoc = cfgStart (config gw)
          initialState = GameState startLoc [] gw
      startGame initialState