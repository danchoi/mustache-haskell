{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}
module Main where
import Text.Mustache.Parse
import qualified Text.Show.Pretty as Pr
import System.Environment
import Data.Aeson
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B
import Data.String.QQ 

main = do
    input <- BL.getContents
    let value = fromJust $ decode input
    let res = runTemplate chunks value
    TL.putStrLn . B.toLazyText $ res

chunks = 
  [ Plain "hello "
  , Var [ Key "name" ]
  , Plain "\n\n"
  , Section [ Key "address" ] [ Var [ Key "street" ] ]
  , Plain " "
  , Section [ Key "hobbies" ] [ Var [ Key "name" ] ]
  , Plain "\n\n"
  , Section [ Key "pets" ] [ Var [ Key "." ] , Plain ", " ]
  , Plain "\n\n"
  , Var [ Key "name" ]
  , Plain " "
  , InvertedSection [ Key "nokey" ] [ Plain " this should print " ]
  , Plain "\n\n"
  , Plain "\nhello "
  , Var [ Key "name" ]
  , Plain "\n"
  , Plain "\n\nEND\n\n"
  ]



