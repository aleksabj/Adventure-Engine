module Engine where

import GameWorld

-- Represents the current game state
data GameState = GameState
  { currentLoc :: Name
  , inventory  :: [Name]
  , world      :: GameWorld
  } deriving (Show)

-- Stub: process user input and update game state
processCommand :: String -> GameState -> (String, GameState)
processCommand input state =
  ("You said: " ++ input, state)
