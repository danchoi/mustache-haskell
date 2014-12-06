{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Parsec
import Data.Functor.Identity
import Control.Applicative hiding (many)


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
    mapM_ print xs

type Parser a = ParsecT String DelimiterState Identity a

runParse :: String -> [Chunk]
runParse input = 
    case (runParserT (many chunk) defDelimiters "" input) of
        Identity (Left x) -> error $ "parser failed: " ++ show x
        Identity (Right xs') -> xs'

delimiters :: Monad m => ParsecT s DelimiterState m DelimiterState
delimiters = getState 

inDelimiters p = do
    (a,z) <- delimiters
    between (string a) (string z) p

varname = many1 $ oneOf "a-zA-Z_.[]0-9"

chunk :: Parser Chunk
chunk = choice [var]

var :: Parser Chunk 
var = Var <$> inDelimiters varname




-- | Parse Mustache template into Chunks


