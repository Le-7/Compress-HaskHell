{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
-- Importing necessary modules
module Statistic.ShannonFano (tree) where

import Statistic.EncodingTree
import Data.List (group, sort, sortOn)

-- Calculate symbol frequencies
frequency :: Ord a => [a] -> [(a, Int)]
frequency = map (\x -> (head x, length x)) . group . sort

-- Sort symbols based on frequencies
sortSymbols :: Ord a => [a] -> [a]
sortSymbols = map fst . sortOn snd . frequency

-- Recursive function to generate Shannon-Fano tree
shannonFanoTree :: Ord a => [a] -> Maybe (EncodingTree a)
shannonFanoTree []  = Just (EncodingNode 0 (EncodingLeaf 0 undefined) (EncodingLeaf 0 undefined))
shannonFanoTree [x] = Just (EncodingLeaf 1 x)
shannonFanoTree symbols =
  let mid = length symbols `div` 2
      leftSymbols = take mid symbols
      rightSymbols = drop mid symbols
      leftTree = shannonFanoTree leftSymbols
      rightTree = shannonFanoTree rightSymbols
  in case (leftTree, rightTree) of
    (Just lt, Just rt) -> Just (EncodingNode (count lt + count rt) lt rt)
    _                  -> Nothing

-- Updated tree function using Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree = shannonFanoTree . sortSymbols
