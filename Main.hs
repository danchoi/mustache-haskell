module Main where
import Text.Mustache
import qualified Text.Show.Pretty as Pr
import Data.Aeson
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Maybe
import qualified Text.Show.Pretty as Pr
import Options.Applicative
import System.Directory

data Options = Options Bool (Maybe FilePath) FilePath deriving (Show)

parseOpts :: Parser Options
parseOpts = Options 
    <$> (flag False True (short 'c' <> help "Just output parse tree of template file"))
    <*> (Just <$> (strOption (short 'd' <> metavar "TEMPLATE_DIRECTORY" <> help "Template directory")) <|> pure Nothing)
    <*> (argument str (metavar "TEMPLATE_FILE"))

opts = info 
          (helper <*> parseOpts)
          (fullDesc 
            <> progDesc "A Haskell implementation of Mustache templates.\nOn STDIN provide the JSON to insert into the template."
            <> header "mus v0.1.0.2")

main = do
    opts' <- execParser opts 
    case opts' of
      Options _ (Just dir) _ -> setCurrentDirectory dir
      _ -> return ()
    case opts' of
      Options True _ file -> do
        s <- readFile file
        xs <- readTemplate s
        putStrLn $ Pr.ppShow xs
      Options False _ file -> do
        s <- readFile file
        chunks <- readTemplate s
        input <- BL.getContents
        let value = fromJust $ decode input
        let res = runTemplate chunks value
        TL.putStrLn . B.toLazyText $ res



