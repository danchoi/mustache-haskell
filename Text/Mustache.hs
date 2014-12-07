{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.Mustache
where
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

data Chunk = Var KeyPath  
         | UnescapedVar KeyPath
         | Section KeyPath [Chunk]
         | InvertedSection KeyPath [Chunk]
         | Comment KeyPath
         | SetDelimiter String String -- a stateful operation
         | Plain String
         deriving (Show, Read, Eq)

type KeyPath = [Key]
data Key = Key Text | Index Int deriving (Eq, Show, Read)

------------------------------------------------------------------------ 
-- | Evaluation functions

evalToLineBuilder :: Text -> String -> [KeyPath] -> Value -> B.Builder 
evalToLineBuilder arrayDelim delim ks v = 
    mconcat $ intersperse (B.fromText . T.pack $ delim) $  map (flip (evalToBuilder arrayDelim) v) ks

type ArrayDelimiter = Text

evalToList :: Text -> [KeyPath] -> Value -> [Text]
evalToList arrayDelim ks v = map (flip (evalToText arrayDelim) v) ks

evalToBuilder :: ArrayDelimiter -> KeyPath -> Value -> B.Builder
evalToBuilder d k v = valToBuilder $ evalKeyPath d k v

evalToText :: ArrayDelimiter -> KeyPath -> Value -> Text
evalToText d k v = valToText $ evalKeyPath d k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: ArrayDelimiter -> KeyPath -> Value -> Value
evalKeyPath d [] x@(String _) = x
evalKeyPath d [] x@Null = x
evalKeyPath d [] x@(Number _) = x
evalKeyPath d [] x@(Bool _) = x
evalKeyPath d [] x@(Object _) = x
evalKeyPath d [] x@(Array v) = 
          let vs = V.toList v
              xs = intersperse d $ map (evalToText d []) vs
          in String . mconcat $ xs
evalKeyPath d (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath d ks x
        Nothing -> Null
evalKeyPath d (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath d ks e'
        Nothing -> Null
-- traverse array elements with additional keys
evalKeyPath d ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat . intersperse d $ map (evalToText d ks) vs
evalKeyPath _ ((Index _):_) _ = Null
evalKeyPath _ _ _ = Null

valToBuilder :: Value -> B.Builder
valToBuilder (String x) = B.fromText x
valToBuilder Null = B.fromText "null"
valToBuilder (Bool True) = B.fromText "t"
valToBuilder (Bool False) = B.fromText "f"
valToBuilder (Number x) = 
    case floatingOrInteger x of
        Left float -> B.realFloat float
        Right int -> B.decimal int
valToBuilder (Object _) = B.fromText "[Object]"

valToText :: Value -> Text
valToText (String x) = x
valToText Null = "null"
valToText (Bool True) = "t"
valToText (Bool False) = "f"
valToText (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText (Object _) = "[Object]"

