module Anagrams where

import Data.List.Split(splitOn, wordsBy)
import qualified System.IO as IO
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Trie = Node (M.Map Char Trie)
           | EmptyNode
  deriving (Show,Eq)

dictionary :: IO [String]
dictionary = do
  text <- readFile "dict.txt"
  return $ drop 7 $ words text

populateMap :: String -> Trie -> Trie
populateMap (letter:word) EmptyNode =
  Node $ M.insert letter (populateMap word (Node M.empty)) M.empty
populateMap (letter:word) (Node map) =
  if M.member letter map then
    let dic = fromMaybe EmptyNode $ M.lookup letter map
        newMap = populateMap word dic
        newOuter = M.insert letter newMap map
    in Node newOuter
  else
    Node $ M.insert letter (populateMap word (Node M.empty)) map
  -- in Node $ M.insert letter (populateMap word (Node dic)) dic
populateMap "" map = map

dict :: IO Trie
dict = do
  words <- dictionary
  return $ foldr (\word node -> populateMap word node) (EmptyNode) words --["abc", "ab3"]

