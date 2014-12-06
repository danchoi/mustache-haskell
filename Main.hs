{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Text.Parsec
import Data.Functor.Identity
import Control.Applicative hiding (many, (<|>))
import qualified Text.Show.Pretty as Pr

data Chunk = Var String  
         | UnescapedVar String
         | Section String [Chunk]
         | InvertedSection String [Chunk]
         | Comment String
         | SetDelimiter String String -- a stateful operation
         | Plain String
         deriving (Show, Read, Eq)

-- Custom delimiters may not contain whitespace or the equals sign.

type DelimiterState = (String, String)  -- left and right delimiters

defDelimiters = ("{{", "}}")

main = do
    s <- getContents
    let xs = runParse s
    putStrLn $ Pr.ppShow xs

type Parser a = ParsecT String DelimiterState Identity a

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
    spaces >> string x

inDelimiters p = (between leftDelimiter rightDelimiter p) <?> "inDelimiters"

varname :: Parser String
varname = (many1 (alphaNum <|> oneOf ".[]0-9_")) <?> "varname"

chunk :: Parser Chunk
chunk = choice [
      try unescapedVar
    , try var
    , try section
    , try invertedSection
    , try setDelimiter
    , plain
    ]

var :: Parser Chunk 
var = (Var <$> inDelimiters varname) <?> "var"

unescapedVar = 
  (UnescapedVar 
      <$> (between
            (string "{{{" <* spaces)
            (spaces *> string "}}}")
            varname))
  <?> "unescapedVar"

section :: Parser Chunk
section = do
    key <- inDelimiters (char '#' *> varname)
    xs :: [Chunk] <- manyTill chunk (closeTag key)
    (return  $ Section key xs) <?> ("section " ++ key)

invertedSection :: Parser Chunk
invertedSection = do
    key <- inDelimiters (char '^' *> varname)
    xs :: [Chunk] <- manyTill chunk (closeTag key)
    (return  $ InvertedSection key xs) <?> ("section " ++ key)

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

closeTag :: String -> Parser String
closeTag k = try (inDelimiters (char '/' *> string k)) 



plain = do
    -- thanks to http://stackoverflow.com/a/20735868/232417
    notFollowedBy leftDelimiter
    x <- anyChar
    xs <- manyTill anyChar ((eof >> (string "")) <|> bumpOpen)
    return $ Plain xs

  where bumpOpen = (lookAhead $ try leftDelimiter) <?> "bumpOpen"




-- | Parse Mustache template into Chunks


