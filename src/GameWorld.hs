module GameWorld where

-- Unique identifiers for locations and items
type Name = String

-- | A location in the game world
data Location = Location
  { locName        :: Name
  , locDescription :: String
  , locItems       :: [Name]
  } deriving (Show, Eq)

-- a connection between two locations (e.g., "hall east -> kitchen")
data Connection = Connection
  { connFrom  :: Name
  , connDir   :: String
  , connTo    :: Name
  , connRequired :: Maybe Name
  } deriving (Show, Eq)

-- Types of item behaviors
data ItemBehavior
  = CanOpen { isOpen :: Bool, contains :: [Name] }
  | Readable
  | Movable { reveals :: Maybe Name }
  | Hidden
  | LeadsTo Name
  | Container
  | Portable
  deriving (Show, Eq)

-- An item definition
data Item = Item
  { itemName     :: Name
  , itemBehaviors :: [ItemBehavior]
  } deriving (Show, Eq)

-- Starting state of the game
data GameConfig = GameConfig
  { cfgStart     :: Name
  , cfgGoalLoc   :: Name
  , cfgGoalItems :: [Name]
  } deriving (Show, Eq)

-- The full parsed world definition
data GameWorld = GameWorld
  { locations   :: [Location]
  , connections :: [Connection]
  , items       :: [Item]
  , config      :: GameConfig
  } deriving (Show, Eq)
