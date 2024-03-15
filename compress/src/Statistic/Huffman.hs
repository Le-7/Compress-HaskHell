-- Statistic/Huffman.hs
module Statistic.Huffman (tree, EncodingTree(..)) where

import Statistic.EncodingTree
import Data.List (sort, nub)

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree symbols
  | length (nub symbols) < 2 = Nothing  -- Handle cases with too few distinct symbols
  | otherwise = Just $ buildHuffmanTree $ frequencyList symbols

-- Helper function to build Huffman tree from frequency list
buildHuffmanTree :: Ord a => [(a, Int)] -> EncodingTree a
buildHuffmanTree freqList = buildTree (map (\(x, f) -> EncodingLeaf f x) freqList)

-- Helper function to build the Huffman tree recursively
buildTree :: Ord a => [EncodingTree a] -> EncodingTree a
buildTree [singleTree] = singleTree
buildTree trees = buildTree $ mergeNodes trees

-- Helper function to merge nodes in the Huffman tree
mergeNodes :: Ord a => [EncodingTree a] -> [EncodingTree a]
mergeNodes (tree1:tree2:rest) =
  let mergedNode = mergeTrees tree1 tree2
  in insertByFrequency mergedNode rest
mergeNodes nodes = nodes

-- Helper function to merge two nodes in the Huffman tree
mergeTrees :: EncodingTree a -> EncodingTree a -> EncodingTree a
mergeTrees tree1 tree2 =
  EncodingNode (count tree1 + count tree2) tree1 tree2

-- Helper function to insert a node into a sorted list of nodes by frequency
insertByFrequency :: Ord a => EncodingTree a -> [EncodingTree a] -> [EncodingTree a]
insertByFrequency node [] = [node]
insertByFrequency node (x:xs)
  | count node <= count x = node : x : xs
  | otherwise = x : insertByFrequency node xs

-- Helper function to calculate the frequency list of symbols
frequencyList :: Ord a => [a] -> [(a, Int)]
frequencyList symbols = countOccurrences (sort symbols)

-- Helper function to count occurrences of symbols in a sorted list
countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences [] = []
countOccurrences (x:xs) = (x, length (x:xs)) : countOccurrences xs
