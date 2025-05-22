module Parser (parseGameFile) where

import Text.Parsec
import Text.Parsec.String (Parser)
import GameWorld
import Control.Monad (void)

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
  let locs  = [l | Left l <- blocks]
      items = [i | Right (Left i) <- blocks]
      conns = [c | Right (Right (Left c)) <- blocks]
      starts = [s | Right (Right (Right (Left s))) <- blocks]
      goals = [g | Right (Right (Right (Right g))) <- blocks]
  case (starts, goals) of
    ([s], [g]) -> return $ GameWorld locs conns items (GameConfig s (goalLocation g) (goalItems g))
    _ -> fail "Missing or duplicate start/goal section"

-- Unified block parser for all top-level DSL elements
topLevelBlock :: Parser (Either Location (Either Item (Either Connection (Either Name GoalBlock))))
topLevelBlock =
      (Left <$> locationParser)
  <|> (Right . Left <$> itemParser)
  <|> (Right . Right . Left <$> connectionParser)
  <|> (Right . Right . Right . Left <$> startParser)
  <|> (Right . Right . Right . Right <$> goalParser)

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
           <|> try (indented "readable" >> return Readable)
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
  eol
  return $ Connection from dir to

-- Parse the start location
startParser :: Parser Name
startParser = do
  _ <- string "start"
  name <- spaces *> identifier
  eol
  return name

-- Parse goal block with location + required items
data GoalBlock = GoalBlock { goalLocation :: Name, goalItems :: [Name] }

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
