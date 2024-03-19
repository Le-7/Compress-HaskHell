module LZ.LZ78 (compress, uncompress) where

import LZ.Dictionaries (empty, zeroAsChar)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe)

type Dictionary = [String]

-- | Compress a string using LZ78
compress :: String -> [(Int, Char)]
compress = compress' empty

compress' :: Dictionary -> String -> [(Int, Char)]
compress' _ [] = []
compress' dict str = compress2helper dict str []

compress2helper :: Dictionary -> String -> String -> [(Int, Char)]
compress2helper _ [] _ = []
compress2helper dict (x:xs) prefix =
  case elemIndex (prefix ++ [x]) dict of
    Just idx -> compress2helper dict xs (prefix ++ [x])
    Nothing  -> (fromMaybe 0 (elemIndex prefix dict), x) : compress2helper (dict ++ [prefix ++ [x]]) xs []



-- Find the longest prefix of the dictionary that matches the beginning of the string
findLongestPrefix :: Dictionary -> String -> (String, String)
findLongestPrefix dict str = go dict "" str
  where
    go _ prefix [] = (prefix, "")
    go [] prefix rest = (prefix, rest)
    go (entry:entries) prefix rest =
      if entry `isPrefixOf` rest
        then go entries entry (drop (length entry) rest)
        else go entries prefix rest

-- | Uncompress a list of LZ78-encoded tuples
uncompress :: [(Int, Char)] -> Maybe String
uncompress = fmap concat . uncompress' empty

-- Helper function for decompression
uncompress' :: Dictionary -> [(Int, Char)] -> Maybe [String]
uncompress' _ [] = Just []
uncompress' dict ((index, char):rest) = do
  prefix <- dict `atIndex` index
  let newEntry = prefix ++ [char]
  let newDict = dict ++ [newEntry]
  suffix <- uncompress' newDict rest
  return (newEntry : suffix)

-- Get the element at a specific index in a list
atIndex :: [a] -> Int -> Maybe a
atIndex [] _ = Nothing
atIndex (x:xs) i
  | i == 0    = Just x
  | otherwise = xs `atIndex` (i - 1)
