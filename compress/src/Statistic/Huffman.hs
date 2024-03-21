module Statistic.Huffman(tree) where

import Statistic.EncodingTree
import Data.List (group, sort)
import Data.List (sortBy)
import Data.Function (on)

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree xs
    | null xs   = Nothing  -- Empty input list
    | otherwise = Just $ buildHuffmanTree $ buildLeafNodes xs

-- | Build leaf nodes from input list
buildLeafNodes :: Ord a => [a] -> [EncodingTree a]
buildLeafNodes = map (\(symbol, freq) -> EncodingLeaf freq symbol) . frequencyCount
  where
    frequencyCount = map (\x -> (head x, length x)) . group . sort

-- | Build Huffman tree from leaf nodes
buildHuffmanTree :: Ord a => [EncodingTree a] -> EncodingTree a
buildHuffmanTree [node] = node
buildHuffmanTree nodes = buildHuffmanTree $ insertInternalNode $ sortByFrequency nodes


-- | Sort nodes by frequency
sortByFrequency :: Ord a => [EncodingTree a] -> [EncodingTree a]
sortByFrequency = sortBy (compare `on` count)

-- | Insert internal node into the list of nodes
insertInternalNode :: Ord a => [EncodingTree a] -> [EncodingTree a]
insertInternalNode (n1:n2:rest) = sortByFrequency $ EncodingNode (count n1 + count n2) n1 n2 : rest
insertInternalNode nodes = nodes
