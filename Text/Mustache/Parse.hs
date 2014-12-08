{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.Mustache.Parse (
    runParse
  , readTemplate
) where
import Text.Mustache.Types
import Text.Parsec
import Data.Functor.Identity
import Control.Applicative hiding (many, (<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import System.Directory 

-- Custom delimiters may not contain whitespace or the equals sign.
type DelimiterState = (String, String)  -- left and right delimiters

defDelimiters = ("{{", "}}")

type Parser a = ParsecT String DelimiterState Identity a

readTemplate :: String -> IO [Chunk]
readTemplate = loadPartials . runParse 

-- | preload partials

loadPartials :: [Chunk] -> IO [Chunk]
loadPartials xs = mapM loadPartial xs >>= return . concat

loadPartial :: Chunk -> IO [Chunk]
loadPartial (Partial path) = do
  file <- do e <- doesFileExist path
             if e 
             then return path
             else do
               let path' = path ++ ".mustache"
               e' <- doesFileExist path'
               if e'
               then return path'
               else error $ "Partial file missing: " ++ path
  s <- readFile file
  return $ runParse s 
loadPartial (Section k cs sep) = do
      cs' <- mapM loadPartial cs
      return [Section k (concat cs') sep]
loadPartial (InvertedSection k cs) = do
      cs' <- mapM loadPartial cs
      return [InvertedSection k (concat cs')]
loadPartial x = return [x]


runParse :: String -> [Chunk]
runParse input = 
    case (runParserT (many chunk) defDelimiters "" input) of
        Identity (Left x) -> error $ "parser failed: " ++ show x
        Identity (Right xs') -> xs'

delimiters :: Monad m => ParsecT s DelimiterState m DelimiterState
delimiters = getState 

leftDelimiter :: Parser String
leftDelimiter = do 
    (x,_) <- delimiters 
    string x <* spaces

rightDelimiter = do
    (_,x) <- delimiters 
    string x 

inDelimiters p = (between leftDelimiter (spaces >> rightDelimiter) p) <?> "inDelimiters"

varname :: Parser String
varname = (many1 (alphaNum <|> oneOf ".[]0-9_")) <?> "varname"

chunk :: Parser Chunk
chunk = choice [
      try unescapedVar
    , try var
    , try section
    , try invertedSection
    , try setDelimiter
    , try partial
    , plain
    ]

var :: Parser Chunk 
var = (Var <$> inDelimiters keyPath) <?> "var"

unescapedVar = 
  (UnescapedVar 
    <$> (try tripleBraceForm <|> ampersandForm)) <?> "unescapedVar"
  where tripleBraceForm = between (string "{{{" <* spaces)
                                  (spaces *> string "}}}")
                                  keyPath
        ampersandForm = inDelimiters ((char '&' >> spaces) *> keyPath)

-- {{#section}} is a section; optional separator may be designated as {{#section|,}}
section :: Parser Chunk
section = do
    (key, sep) <- inDelimiters ((char '#' >> spaces) *> ((,) <$> keyPath <*> sep))
    xs :: [Chunk] <- manyTill chunk (closeTag key)
    (return  $ Section key xs sep) <?> ("section " ++ show key)

sep :: Parser (Maybe Text)
sep  = do
  spaces 
  Just <$> do
    spaces
    notFollowedBy rightDelimiter
    x <- anyChar
    xs <- manyTill anyChar ((eof >> (string "")) <|> bumpClose)
    return . T.pack $ x:xs
  <|> pure Nothing

invertedSection :: Parser Chunk
invertedSection = do
    key <- inDelimiters ((char '^' >> spaces) *> keyPath)
    xs :: [Chunk] <- manyTill chunk (closeTag key)
    (return  $ InvertedSection key xs) <?> ("section " ++ show key)

setDelimiter :: Parser Chunk
setDelimiter = do
  (left, right) <- inDelimiters $ do
      char '='
      left <- many1 (noneOf "= ")
      spaces
      right <- many1 (noneOf "= ")
      char '='
      return (left, right)
  setState (left, right)
  return $ SetDelimiter left right

closeTag :: KeyPath -> Parser String
closeTag k = try (inDelimiters (char '/' *> string k')) 
  where k' = keyPathToString k

partial = do
  Partial <$> (inDelimiters ((char '>' >> spaces) *> filename))

filename :: Parser String
filename = (many1 (alphaNum <|> oneOf "/.[]0-9_")) <?> "filename"

plain = do
    -- thanks to http://stackoverflow.com/a/20735868/232417
    notFollowedBy leftDelimiter
    x <- anyChar
    xs <- manyTill anyChar ((eof >> (string "")) <|> bumpOpen)
    return . Plain . T.pack $ x:xs

bumpOpen = (lookAhead $ try leftDelimiter) <?> "bumpOpen"
bumpClose = (lookAhead $ try rightDelimiter) <?> "bumpClose"


------------------------------------------------------------------------

keyPath :: Parser KeyPath
keyPath = do
  raw <- varname
  let res = parse (sepBy1 pKeyOrIndex (many1 $ oneOf ".[")) "" raw
  spaces
  return 
    $ case res of 
        Left err -> error $ "Can't parse keypath: " ++ raw
        Right res' -> res'

keyPathToString :: KeyPath -> String
keyPathToString xs = go xs
  where go ((Key x):[]) = T.unpack x 
        go ((Key x):xs) = T.unpack x
                  <> "."
                  <> keyPathToString xs
        go ((Index x):xs) = 
              "[" <> (show x) 
              <> (']':keyPathToString xs)
        go [] = []

pKeyOrIndex = pIndex <|> pKey

pKey = Key . T.pack 
    <$> (
        ((:[]) <$> char '.')  -- immediate context key
        <|> (many1 (alphaNum <|> noneOf ".[")) 
        )

pIndex = Index . read <$> (many1 digit) <* char ']'

