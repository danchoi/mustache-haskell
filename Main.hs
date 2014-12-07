{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Text.Mustache.Parse
import qualified Text.Show.Pretty as Pr

main = do
    s <- getContents
    let xs = runParse s
    putStrLn $ Pr.ppShow xs

