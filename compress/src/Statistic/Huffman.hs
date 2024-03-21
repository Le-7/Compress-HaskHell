{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman(tree) where

import Statistic.EncodingTree
import Data.List (sort, group, sortOn)

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree xs
    | null xs   = Nothing  -- Empty input list
    | otherwise = Just $ buildHuffmanTree $ buildLeafNodes xs

-- | Build leaf nodes from input list
buildLeafNodes :: Ord a => [a] -> [EncodingTree a]
buildLeafNodes xs = map (\(symbol, freq) -> EncodingLeaf freq symbol) $ frequencyCount xs
    where
        frequencyCount = map (\x -> (head x, length x)) . group . sort

buildHuffmanTree :: Ord a => [EncodingTree a] -> EncodingTree a
buildHuffmanTree [node] = node
buildHuffmanTree nodes = buildHuffmanTree $ insertInternalNode $ sortByFrequency nodes

-- | Trie les nœuds par fréquence croissante
sortByFrequency :: Ord a => [EncodingTree a] -> [EncodingTree a]
sortByFrequency = sortOn count

-- | Insère un nœud interne dans la liste des nœuds
insertInternalNode :: Ord a => [EncodingTree a] -> [EncodingTree a]
insertInternalNode (n1:n2:rest) = sortByFrequency $ EncodingNode (count n1 + count n2) n1 n2 : rest
insertInternalNode nodes = nodes 
