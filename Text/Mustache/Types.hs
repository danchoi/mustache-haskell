module Text.Mustache.Types
where
import Data.Text (Text)

data Chunk = Var KeyPath  
         | UnescapedVar KeyPath
         | Section KeyPath [Chunk] (Maybe Text)  -- separator text
         | InvertedSection KeyPath [Chunk]
         | Comment KeyPath
         | SetDelimiter String String -- a stateful operation
         | Plain Text
         | Partial FilePath
         deriving (Show, Read, Eq)

type KeyPath = [Key]
data Key = Key Text | Index Int deriving (Eq, Show, Read)


