module Main where

import Parser
import GameWorld
import GameLoop
import Engine

main :: IO ()
main = do
  -- contents <- readFile "examples/library-secrets.txt"
  -- contents <- readFile "examples/castle-treasure.txt"
  contents <- readFile "examples/intro-escape.txt"
  case parseGameFile contents of
    Left err -> print err
    Right gw -> do
      let startLoc = cfgStart (config gw)
          initialState = GameState startLoc [] gw
      startGame initialState