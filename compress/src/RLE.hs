-- |
-- Module      : RLE
-- Description : An implementation of the run-length encoding method
-- Maintainer  : ???
--
module RLE (compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = let (first, rest) = span (==x) xs
                  in (x, length first + 1) : compress rest

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []
uncompress ((sym, count) : xs)
  | count < 1 = Nothing
  | otherwise = case uncompress xs of
                  Nothing -> Nothing
                  Just rest -> Just (replicate count sym ++ rest)
