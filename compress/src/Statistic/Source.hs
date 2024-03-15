module Statistic.Source (occurrences, entropy, orderedCounts) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences symbols = Map.fromListWith (+) $ zip symbols (repeat 1)

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy symbols =
  let counts = Map.elems $ occurrences symbols
      total = sum counts
      probabilities = map (\count -> fromIntegral count / fromIntegral total) counts
  in negate $ sum $ map (\p -> p * logBase 2 p) probabilities

-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts symbols = reverse $ sort $ Map.toList $ occurrences symbols
