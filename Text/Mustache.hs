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
         | Plain Text
         deriving (Show, Read, Eq)

type KeyPath = [Key]
data Key = Key Text | Index Int deriving (Eq, Show, Read)

------------------------------------------------------------------------ 
-- | Evaluation functions

chunkToBuilder :: Value -> Chunk -> B.Builder
chunkToBuilder v (Var k) = evalToBuilder k v
chunkToBuilder v (UnescapedVar k) = evalToBuilder k v  -- TODO?
chunkToBuilder v (Comment _) = mempty
chunkToBuilder v (SetDelimiter _ _) = mempty
chunkToBuilder v (Plain x) = B.fromText x
chunkToBuilder v (Section ks xs) = mempty
chunkToBuilder v (InvertedSection ks xs) = mempty


evalToBuilder :: KeyPath -> Value -> B.Builder
evalToBuilder k v = valToBuilder $ evalKeyPath k v

evalToText :: KeyPath -> Value -> Text
evalToText k v = valToText $ evalKeyPath k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: KeyPath -> Value -> Value
evalKeyPath [] x@(String _) = x
evalKeyPath [] x@Null = x
evalKeyPath [] x@(Number _) = x
evalKeyPath [] x@(Bool _) = x
evalKeyPath [] x@(Object _) = x
evalKeyPath [] x@(Array v) = 
          let vs = V.toList v
              xs = map (evalToText []) vs
          in String . mconcat $ xs
evalKeyPath (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath ks x
        Nothing -> Null
evalKeyPath (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath ks e'
        Nothing -> Null
-- traverse array elements with additional keys
evalKeyPath ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat $ map (evalToText ks) vs
evalKeyPath ((Index _):_) _ = Null
evalKeyPath _ _ = Null

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

