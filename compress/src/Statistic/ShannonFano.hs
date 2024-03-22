{-# LANGUAGE TupleSections #-}

module Statistic.ShannonFano (tree) where

import Statistic.EncodingTree
import Statistic.Source (orderedCounts)

-- Recursive function to generate Shannon-Fano tree
shannonFanoTree :: Ord a => [(a, Int)] -> Maybe (EncodingTree a)
shannonFanoTree [] = Just (EncodingLeaf 0 undefined)
shannonFanoTree [(x, _)] = Just (EncodingLeaf 1 x)
shannonFanoTree xs =
  let total = sum (map snd xs)
      half = total `div` 2
      (left, right) = splitAtHalf half xs
  in case (shannonFanoTree left, shannonFanoTree right) of
       (Just lt, Just rt) -> Just (EncodingNode total lt rt)
       _                  -> Nothing

splitAtHalf :: Int -> [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitAtHalf _ [] = ([], [])
splitAtHalf n ((x, cnt):rest)
  | n' < 0 = ([], (x, cnt):rest)
  | otherwise = ((x, cnt):left, right)
  where
    n' = n - cnt
    (left, right) = splitAtHalf n' rest

-- Updated tree function using Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree symbols = shannonFanoTree (orderedCounts symbols)
