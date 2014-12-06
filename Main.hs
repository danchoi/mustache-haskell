{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Parsec
import Data.Functor.Identity

data Chunk = Var String  
         | UnescapedVar String
         | Section String [Chunk]
         | InvertedSection String [Chunk]
         | Comment String
         | SetDelimiter String String -- a stateful operation
         | Plain String
         deriving (Show, Read, Eq)

-- Custom delimiters may not contain whitespace or the equals sign.

data DelimiterState = DelimiterState (String, String) deriving Show

defDelimiters = DelimiterState ("{{", "}}")

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

chunk :: Parser Chunk
chunk = choice [var]

var :: Parser Chunk 
var = undefined




-- | Parse Mustache template into Chunks


