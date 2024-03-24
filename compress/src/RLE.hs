-- |
-- Module      : RLE
-- Description : An implementation of the run-length encoding method
-- Maintainer  : AÏT CHADI Anissa, COSTA Mathéo, TRAN Alexandre
--
module RLE (compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []  -- Si la liste est vide on ne retourne rien
compress (x:xs) = (x, length (takeWhile (==x) xs) + 1) : compress (dropWhile (==x) xs)

-- | RLE uncompress method
-- Si la chaine ne peut pas être compressée, on retourne `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just [] -- Si liste d'entrée vide, on renvoie ca pour indiquer que c'est décompressé en liste vide
uncompress ((sym, count) : xs)
  | count < 1 = Nothing -- Si count est inférieur à 1, c'est qu'il y a une erreur dans la compression
  | otherwise = case uncompress xs of 
                  Nothing -> Nothing
                  Just rest -> Just (replicate count sym ++ rest)
