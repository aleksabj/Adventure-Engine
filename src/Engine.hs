module Engine where

import GameWorld
import Data.Maybe (fromMaybe)
import Data.List (find, delete)

data GameState = GameState
  { currentLoc :: Name
  , inventory  :: [Name]
  , world      :: GameWorld
  } deriving (Show)

-- Process a user command
processCommand :: String -> GameState -> (String, GameState)
processCommand input state =
    let ws = cleanWords (words input)
    in case ws of
        ["look"]        -> (describeCurrentLocation state, state)
        ["inventory"]   -> (showInventory state, state)
        ["go", dir]     -> movePlayer dir state
        ["take", item]  -> takeItem item state
        ["open", item]  -> openItem item state
        ["read", item]  -> readItem item state
        ["move", item]  -> moveItem item state
        ["help"]        -> (helpMessage, state)
        _               -> ("I don't understand that command.", state)

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
        available = [connDir c | c <- conns, connFrom c == from]
        mConn = find (\c -> connFrom c == from && connDir c == dir) conns
    in case mConn of
        Just conn -> case connRequired conn of
            Nothing ->
                ("You move " ++ dir ++ " to " ++ connTo conn ++ ".", gs { currentLoc = connTo conn })
            Just item
                | item `elem` inventory gs ->
                    ("You unlock the path with the " ++ item ++ " and go " ++ dir ++ " to " ++ connTo conn ++ ".", gs { currentLoc = connTo conn })
                | otherwise ->
                    ("You need a " ++ item ++ " to go " ++ dir ++ ".", gs)
        Nothing -> ("You can't go " ++ dir ++ " from here. Try: " ++ unwords available, gs)

-- Take an item if it's present and portable
takeItem :: Name -> GameState -> (String, GameState)
takeItem item gs =
    let loc = getLocationByName (currentLoc gs) (world gs)
        allItems = items (world gs)
        isHere = item `elem` locItems loc
        mItem = findItem item allItems
    in case (isHere, mItem) of
        (True, Just it) | Portable `elem` itemBehaviors it ->
            let newLoc = loc { locItems = delete item (locItems loc) }
                newWorld = gsWorldUpdate gs newLoc
            in ("You take the " ++ item ++ ".", gs { inventory = item : inventory gs, world = newWorld })
        (True, _) -> ("You can't take that.", gs)
        _ -> ("There is no " ++ item ++ " here.", gs)

-- Open an item if it can be opened
openItem :: Name -> GameState -> (String, GameState)
openItem name gs =
    let mItem = findItem name (items (world gs))
    in case mItem of
        Just it ->
            case [ (isOpen, contents) | CanOpen isOpen contents <- itemBehaviors it ] of
                [(False, contents)] ->
                    let updatedItem = it { itemBehaviors = replaceBehavior (CanOpen False contents) (CanOpen True contents) (itemBehaviors it) }
                        newItems     = updateItem updatedItem (items (world gs))
                        updatedLocs  = addItemsToLocation contents (currentLoc gs) (locations (world gs))
                        msg          = if null contents
                                       then "You open the " ++ name ++ "."
                                       else "You open the " ++ name ++ " and find: " ++ unwords contents ++ "."
                    in (msg, gs { world = (world gs) { items = newItems, locations = updatedLocs } })
                [(True, _)] ->
                    ("It's already open.", gs)
                _ ->
                    ("You can't open that.", gs)
        Nothing -> ("No such item.", gs)

readItem :: Name -> GameState -> (String, GameState)
readItem name gs =
    case findItem name (items (world gs)) of
        Just it | Readable `elem` itemBehaviors it ->
            ("You read the " ++ name ++ ". It's full of old, faded text.", gs)
        Just _ ->
            ("You can't read that.", gs)
        Nothing ->
            ("No such item.", gs)

moveItem :: Name -> GameState -> (String, GameState)
moveItem name gs =
    let mItem = findItem name (items (world gs))
    in case mItem of
        Just it -> case [r | Movable r <- itemBehaviors it] of
            (Just revealed : _) ->
                let newLocs = revealItemInLocation revealed (currentLoc gs) (locations (world gs))
                in ("You move the " ++ name ++ " and reveal: " ++ revealed ++ ".", gs { world = (world gs) { locations = newLocs } })
            _ -> ("You can't move that.", gs)
        Nothing -> ("No such item.", gs)


-- Utility functions
checkGoal :: GameState -> Maybe String
checkGoal gs =
    let GameConfig _ goalLoc goalItems = config (world gs)
    in if currentLoc gs == goalLoc && all (`elem` inventory gs) goalItems
        then Just "*** Congratulations! You completed the quest! ***"
        else Nothing

findItem :: Name -> [Item] -> Maybe Item
findItem name = lookup name . map (\i -> (itemName i, i))

updateItem :: Item -> [Item] -> [Item]
updateItem newItem = map (\i -> if itemName i == itemName newItem then newItem else i)

replaceBehavior :: ItemBehavior -> ItemBehavior -> [ItemBehavior] -> [ItemBehavior]
replaceBehavior old new = map (\b -> if b == old then new else b)

addItemsToLocation :: [Name] -> Name -> [Location] -> [Location]
addItemsToLocation names targetName =
    map (\loc -> if locName loc == targetName
                then loc { locItems = locItems loc ++ names }
                else loc)

gsWorldUpdate :: GameState -> Location -> GameWorld
gsWorldUpdate gs newLoc =
    let oldWorld = world gs
        updatedLocs = map (\loc -> if locName loc == locName newLoc then newLoc else loc) (locations oldWorld)
    in oldWorld { locations = updatedLocs }

getLocationByName :: Name -> GameWorld -> Location
getLocationByName name gw =
    fromMaybe (error $ "Unknown location: " ++ name)
              (lookup name [(locName l, l) | l <- locations gw])

revealItemInLocation :: Name -> Name -> [Location] -> [Location]
revealItemInLocation itemName targetName =
    map (\loc -> if locName loc == targetName
                then loc { locItems = itemName : locItems loc }
                else loc)

-- Help message for the player
helpMessage :: String
helpMessage = unlines
    [ "Available commands:"
    , "- look"
    , "- inventory"
    , "- go [direction]"
    , "- take [item]"
    , "- open [item]"
    , "- read [item]"
    , "- move [item]"
    , "- help"
    ]

cleanWords :: [String] -> [String]
cleanWords = filter (`notElem` ["the", "a", "an", "to", "on", "at", "in"])
