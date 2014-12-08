{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Text.Mustache
import qualified Text.Show.Pretty as Pr
import System.Environment
import Data.Aeson
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B

main = do
    [file] <- getArgs
    s <- readFile file
    chunks <- readTemplate s
    input <- BL.getContents
    let value = fromJust $ decode input
    let res = runTemplate chunks value
    TL.putStrLn . B.toLazyText $ res



