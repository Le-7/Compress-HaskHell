-- |
-- Module      : RLE
-- Description : An implementation of the run-length encoding method
-- Maintainer  : ???
--
module RLE (compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress input = reverse $ compress' input [] 0 []

-- Helper function for RLE compression
compress' :: Eq a => [a] -> [a] -> Int -> [(a, Int)] -> [(a, Int)]
compress' [] currentRun _ result = ((head currentRun, length currentRun) : result)
compress' (x:xs) currentRun count result
  | null currentRun || x == head currentRun = compress' xs (x:currentRun) (count + 1) result
  | otherwise = compress' xs [x] 1 ((head currentRun, count) : result)

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress pairs = fmap concat $ sequence $ map expandPair pairs

-- Helper function to expand an RLE pair into a list
expandPair :: (a, Int) -> Maybe [a]
expandPair (sym, count)
  | count < 1 = Nothing
  | otherwise = Just $ replicate count sym
