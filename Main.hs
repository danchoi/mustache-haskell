module Main where
import Text.Mustache
import qualified Text.Show.Pretty as Pr
import System.Environment
import Data.Aeson
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Maybe
import qualified Text.Show.Pretty as Pr
import Options.Applicative

data Options = Options Bool FilePath deriving (Show)

parseOpts :: Parser Options
parseOpts = Options 
    <$> (flag False True (short 'c' <> help "Just output parse tree of template file"))
    <*> (argument str (metavar "FILE"))

opts = info 
          (helper <*> parseOpts)
          (fullDesc 
            <> progDesc "A Haskell implementation of Mustache templates.\nOn STDIN provide the JSON to insert into the template."
            <> header "mustashehs v0.1.4.1")

main = do
    opts' <- execParser opts 
    case opts' of
      Options True file -> do
        s <- readFile file
        xs <- readTemplate s
        putStrLn $ Pr.ppShow xs
      Options False file -> do
        s <- readFile file
        chunks <- readTemplate s
        input <- BL.getContents
        let value = fromJust $ decode input
        let res = runTemplate chunks value
        TL.putStrLn . B.toLazyText $ res



