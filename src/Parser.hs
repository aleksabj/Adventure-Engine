module Parser (parseGameFile) where

import Text.Parsec
import Text.Parsec.String (Parser)
import GameWorld
import Control.Monad (void)

data Block = 
  PLoc Location
  | PItem Item
  | PConn Connection
  | PStart Name
  | PGoal GoalBlock
  deriving (Show)

-- Helper: parse an identifier (alphanumeric + '-' or '_')
identifier :: Parser String
identifier = many1 (alphaNum <|> oneOf "-_")

-- Consume end of line or file
eol :: Parser ()
eol = void newline <|> eof

-- Skip at least one space or tab
indent :: Parser ()
indent = skipMany1 (oneOf " \t")

-- Skip blank or whitespace-only lines
blankLine :: Parser ()
blankLine = try (skipMany (oneOf " \t") >> newline >> return ())

-- Parse an entire game world from a file
parseGameFile :: String -> Either ParseError GameWorld
parseGameFile = parse gameWorldParser ""

-- Parse the whole file, allowing blank lines between top-level sections
gameWorldParser :: Parser GameWorld
gameWorldParser = do
  spaces
  blocks <- many (topLevelBlock <* many blankLine)
  let locs = [l | PLoc l   <- blocks]
      items = [i | PItem i  <- blocks]
      conns = [c | PConn c  <- blocks]
      starts = [s | PStart s <- blocks]
      goals = [g | PGoal g  <- blocks]
      allConns = conns ++ generateReverseConnections conns
  case (starts, goals) of
    ([s], [g]) -> return $ GameWorld locs allConns items (GameConfig s (goalLocation g) (goalItems g))
    _ -> fail "Missing or duplicate start/goal section"

-- Reverse connections based on standard directions
generateReverseConnections :: [Connection] -> [Connection]
generateReverseConnections conns =
  [ Connection (connTo c) revDir (connFrom c) (connRequired c)
  | c <- conns
  , Just revDir <- [oppositeDirection (connDir c)]
  , not (any (\c' -> connFrom c' == connTo c && connDir c' == revDir && connTo c' == connFrom c) conns)
  ]

oppositeDirection :: String -> Maybe String
oppositeDirection dir = lookup dir
  [ ("north", "south")
  , ("south", "north")
  , ("east",  "west")
  , ("west",  "east")
  , ("up",    "down")
  , ("down",  "up")
  , ("inside","outside")
  , ("outside","inside")
  , ("forward", "back")
  , ("back", "forward")
  ]

-- Unified block parser for all top-level DSL elements
topLevelBlock :: Parser Block
topLevelBlock =
      (PLoc <$> locationParser)
  <|> (PItem <$> itemParser)
  <|> (PConn <$> connectionParser)
  <|> (PStart <$> startParser)
  <|> (PGoal <$> goalParser)

-- Parse a location block
locationParser :: Parser Location
locationParser = do
  _ <- string "location"
  name <- spaces *> identifier
  eol
  description <- indentedField "description"
  items <- many (indentedField "item")
  return $ Location name description items

-- Parse an item block with behaviors
itemParser :: Parser Item
itemParser = do
  _ <- string "item"
  name <- spaces *> identifier
  eol
  behaviors <- many itemBehavior
  return $ Item name behaviors

-- Parse a single item behavior line
itemBehavior :: Parser ItemBehavior
itemBehavior = try parseCanOpenBehavior
           <|> try (do msg <- indentedField "readable"; return (Readable msg))
           <|> try (indentedField "movable reveals" >>= \s -> return (Movable (Just s)))
           <|> try (indented "hidden" >> return Hidden)
           <|> try (indentedField "leads_to" >>= \s -> return (LeadsTo s))
           <|> try (indented "container" >> return Container)
           <|> try (indented "portable" >> return Portable)

parseCanOpenBehavior :: Parser ItemBehavior
parseCanOpenBehavior = do
  _ <- indented "can_open"
  -- try to grab following optional lines
  flags <- many (try parseOpenProperty)
  let openState = case lookup "state" flags of
                    Just "open"   -> True
                    _             -> False
      containsItems = maybe [] (\s -> [s]) (lookup "contains" flags)
  return $ CanOpen openState containsItems

parseOpenProperty :: Parser (String, String)
parseOpenProperty =
      try (do k <- "state" <$ indent *> string "state"
              v <- spaces *> manyTill anyChar eol
              return (k, v))
  <|> try (do k <- "contains" <$ indent *> string "contains"
              v <- spaces *> manyTill anyChar eol
              return (k, v))


-- parse a directional connection
connectionParser :: Parser Connection
connectionParser = do
  _ <- string "connection"
  from <- spaces *> identifier
  dir <- spaces *> identifier
  _ <- spaces *> string "->"
  to <- spaces *> identifier
  required <- optionMaybe parseCondition
  eol
  return $ Connection from dir to required

parseCondition :: Parser Name
parseCondition = try $ do
  spaces
  _ <- string "if"
  spaces
  _ <- string "has("
  item <- identifier
  _ <- char ')'
  return item

-- Parse the start location
startParser :: Parser Name
startParser = do
  _ <- string "start"
  name <- spaces *> identifier
  eol
  return name

-- Parse goal block with location + required items
data GoalBlock = GoalBlock { goalLocation :: Name, goalItems :: [Name] } deriving Show

goalParser :: Parser GoalBlock
goalParser = do
  _ <- string "goal"
  eol
  loc <- indentedField "location"
  items <- many (indentedField "has")
  return $ GoalBlock loc items

-- Parse a keyword field like 'description <value>'
indentedField :: String -> Parser String
indentedField field = try $ do
  indent
  _ <- string field
  spaces
  val <- manyTill anyChar eol
  return val

-- Parse a keyword line like 'can_open'
indented :: String -> Parser ()
indented str = try $ do
  indent
  _ <- string str
  eol
