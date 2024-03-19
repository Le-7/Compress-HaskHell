module LZ.LZ78 (compress, uncompress) where

import LZ.Dictionaries (empty)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (elemIndex, isPrefixOf,find, genericIndex)

-- | Compress a string using LZ78 algorithm
compress :: String -> [(Int, Char)]
compress input = compress' input empty
  where
    compress' :: String -> [String] -> [(Int, Char)]
    compress' [] _ = []
    compress' (c:cs) dict =
      case elemIndex prefix dict of
        Just index -> (index, nextChar) : compress' rest (dict ++ [prefix ++ [nextChar]])
        Nothing    -> (0, c) : compress' cs (dict ++ [[c]])
      where
        (prefix, nextChar, rest) = findLongestPrefixAndNextChar (c:cs) dict

    findLongestPrefixAndNextChar :: String -> [String] -> (String, Char, String)
    findLongestPrefixAndNextChar str dict = findLongestPrefixAndNextChar' str "" dict
      where
        findLongestPrefixAndNextChar' :: String -> String -> [String] -> (String, Char, String)
        findLongestPrefixAndNextChar' [] _ _ = error "Input string cannot be empty"
        findLongestPrefixAndNextChar' (x:xs) acc [] = (acc, x, xs)
        findLongestPrefixAndNextChar' (x:xs) acc (d:ds)
          | d `isPrefixOf` (acc ++ [x]) = findLongestPrefixAndNextChar' xs (acc ++ [x]) ds
          | otherwise                   = (acc, x, xs)


-- Decompress a list of (index, character) pairs into a string
uncompress :: [(Int, Char)] -> Maybe String
uncompress tokens = fmap concat . mapM expandToken $ tokens
  where
    expandToken :: (Int, Char) -> Maybe String
    expandToken (0, c) = Just [c]
    expandToken (idx, c) = do
      prev <- sequenceA $ replicate idx (getPrev tokens)
      return (prev ++ [c])

    getPrev :: [(Int, Char)] -> Maybe Char
    getPrev [] = Nothing
    getPrev ((i, c):rest)
      | i == length rest + 1 = Just c
      | otherwise = getPrev rest
