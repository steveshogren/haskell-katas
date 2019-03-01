module Anagrams where

import Data.List.Split(splitOn, wordsBy)
import qualified System.IO as IO
import qualified Data.Map as M

data Trie = Node (M.Map Char Trie)
           | EmptyNode
  deriving (Show,Eq)

dictionary :: IO [String]
dictionary = do
  text <- readFile "dict.txt"
  return $ drop 7 $ words text

populateMap :: String -> Trie -> Trie
populateMap (letter:word) (Node root) =
  Node $ M.insert letter (populateMap word (Node root)) root
populateMap "" (Node root) = EmptyNode

dict = populateMap "yes" (Node M.empty)
