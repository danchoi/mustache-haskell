{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.Mustache (
  runTemplate
, module Text.Mustache.Parse
) where
import Text.Mustache.Types
import Text.Mustache.Parse
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import Data.List (intersperse)
import Data.Monoid
import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific 

------------------------------------------------------------------------ 
-- | Evaluation functions

runTemplate :: [Chunk] -> Value -> B.Builder
runTemplate xs v = mconcat
    $ map (chunkToBuilder v) xs

chunkToBuilder :: Value -> Chunk -> B.Builder
chunkToBuilder v (Var k) = evalToBuilder True k v
chunkToBuilder v (UnescapedVar k) = evalToBuilder False k v  
chunkToBuilder v (Comment _) = mempty
chunkToBuilder v (SetDelimiter _ _) = mempty
chunkToBuilder v (Plain x) = B.fromText x
chunkToBuilder v (Section ks chunks sep) = 
    case evalKeyPath ks v of 
      Array v' -> 
          let evalItem :: Value -> B.Builder
              evalItem loopValue = mconcat $ map (chunkToBuilder $ mergeValues v loopValue) chunks
          in mconcat $ intersperse (maybe mempty B.fromText sep) $ map evalItem $ V.toList v'
      x@(Object _) -> mconcat $ map (chunkToBuilder $ mergeValues v x) chunks 
      _ -> mempty
chunkToBuilder v (InvertedSection ks chunks) = 
    case evalKeyPath ks v of
      Null -> chunkToBuilder v (Section (init ks) chunks Nothing)
      Bool False -> chunkToBuilder v (Section (init ks) chunks Nothing)
      _ -> mempty
chunkToBuilder v (Partial s) = B.fromText $ "{{ERROR: include partial " <> T.pack s <> "}}"

mergeValues :: Value -> Value -> Value
mergeValues (Object outer) (Object inner) = Object $ HM.union inner outer
mergeValues _ inner = inner

evalToBuilder :: Bool -> KeyPath -> Value -> B.Builder
evalToBuilder escape k v = valToBuilder escape $ evalKeyPath k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: KeyPath -> Value -> Value
evalKeyPath [] x@(String _) = x
evalKeyPath [] x@Null = x
evalKeyPath [] x@(Number _) = x
evalKeyPath [] x@(Bool _) = x
evalKeyPath [] x@(Object _) = x
evalKeyPath (Key ".":[]) x = x
evalKeyPath [] x@(Array _) = x 
evalKeyPath (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath ks x
        Nothing -> Null
evalKeyPath (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath ks e'
        Nothing -> Null
evalKeyPath ((Index _):_) _ = Null
evalKeyPath _ _ = Null

valToBuilder :: Bool -> Value -> B.Builder
valToBuilder True (String x) = B.fromText . htmlEscape $ x
valToBuilder False (String x) = B.fromText x
valToBuilder _ Null = B.fromText "null"
valToBuilder _ (Bool True) = B.fromText "true"
valToBuilder _ (Bool False) = B.fromText "false"
valToBuilder _ (Number x) = 
    case floatingOrInteger x of
        Left float -> B.realFloat float
        Right int -> B.decimal int
valToBuilder _ (Object x) = B.fromText . T.pack . show $ x



-- | Escape HTML symbols
-- adapted from Hastache.hs -- thank you
htmlEscape :: T.Text -> T.Text
htmlEscape = T.concatMap proc
  where
    proc '&'  = "&amp;"
    proc '\\' = "&#92;"
    proc '"'  = "&quot;"
    proc '\'' = "&#39;"
    proc '<'  = "&lt;"
    proc '>'  = "&gt;"
    proc h    = T.singleton h

