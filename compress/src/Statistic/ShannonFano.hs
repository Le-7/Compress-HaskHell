module Statistic.ShannonFano (tree) where

import Statistic.EncodingTree
import Statistic.Source (orderedCounts)

-- | Split the distribution into two parts
splitDistribution :: [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitDistribution dist = splitAt (findSplitPoint dist 0 (sum (map snd dist))) dist

-- | Find the split point in the distribution
findSplitPoint :: [(a, Int)] -> Int -> Int -> Int
findSplitPoint [] _ _ = 0
findSplitPoint ((_, count) : rest) acc total
  | acc * 2 >= total || null rest = 0
  | otherwise = 1 + findSplitPoint rest (acc + count) total

-- | Build the encoding tree
buildTree :: [(a, Int)] -> EncodingTree a
buildTree [(symbol, count)] = EncodingLeaf count symbol
buildTree distribution =
  case splitDistribution distribution of
    (left, right) -> EncodingNode (sum (map snd distribution)) (buildTree left) (buildTree right)

-- | Build the encoding tree using Shannon-Fano algorithm
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree symbols = Just (buildTree (orderedCounts symbols))
