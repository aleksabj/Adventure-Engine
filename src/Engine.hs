module Engine where

import GameWorld
import Data.Maybe (fromMaybe)
import Data.List (find)

data GameState = GameState
  { currentLoc :: Name
  , inventory  :: [Name]
  , world      :: GameWorld
  } deriving (Show)

-- Process a user command
processCommand :: String -> GameState -> (String, GameState)
processCommand input state =
    case words input of
        ["look"]      -> (describeCurrentLocation state, state)
        ["inventory"] -> (showInventory state, state)
        ["go", dir]   -> movePlayer dir state
        _             -> ("I don't understand that command.", state)

-- Describe the current location, items, and exits
describeCurrentLocation :: GameState -> String
describeCurrentLocation gs =
    let loc = getLocationByName (currentLoc gs) (world gs)
        itemStr = case locItems loc of
                    [] -> "There is nothing here."
                    xs -> "You see: " ++ unwords xs
        available = [connDir c | c <- connections (world gs), connFrom c == locName loc]
        dirStr = case available of
                    [] -> "There is nowhere to go from here."
                    xs -> "Exits: " ++ unwords xs
    in locDescription loc ++ "\n" ++ itemStr ++ "\n" ++ dirStr

-- show the player's inventory
showInventory :: GameState -> String
showInventory gs =
    case inventory gs of
        [] -> "You are carrying nothing."
        xs -> "You have: " ++ unwords xs

-- Move the player if a connection exists
movePlayer :: String -> GameState -> (String, GameState)
movePlayer dir gs =
    let from = currentLoc gs
        conns = connections (world gs)
        mConn = find (\c -> connFrom c == from && connDir c == dir) conns
    in case mConn of
        Just conn -> 
            ("You move " ++ dir ++ " to " ++ connTo conn ++ ".", gs { currentLoc = connTo conn })
        Nothing ->
            ("You can't go " ++ dir ++ " from here.", gs)

-- Utility: get a location by name or crash
getLocationByName :: Name -> GameWorld -> Location
getLocationByName name gw =
    fromMaybe (error $ "Unknown location: " ++ name)
              (lookup name [(locName l, l) | l <- locations gw])
