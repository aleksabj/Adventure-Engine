module Main where

import Parser
import GameWorld
import GameLoop

main :: IO ()
main = do
  -- contents <- readFile "examples/library-secrets.txt"
  -- contents <- readFile "examples/castle-treasure.txt"
  contents <- readFile "examples/intro-escape.txt"
  case parseGameFile contents of
    Left err -> print err
    Right world -> print world

