{-# LANGUAGE TupleSections #-}

module LZ.LZ78 (compress, uncompress) where

import LZ.Dictionaries (empty, ascii, zeroAsChar)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe)

type Dictionary = [String]

-- | Compress a string using LZ78
compress :: String -> [(Int, Char)]
compress = compress' empty

-- Helper function for compression
compress' :: Dictionary -> String -> [(Int, Char)]
compress' _ [] = []
compress' dict str =
  case findLongestPrefix dict str of
    (prefix, rest) ->
      if null rest
        then [(fromMaybe 0 (elemIndex prefix dict), zeroAsChar)]
        else (fromMaybe 0 (elemIndex prefix dict), head rest) : compress' (dict ++ [prefix ++ [head rest]]) rest



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
  suffix <- uncompress' (dict ++ [prefix ++ [char]]) rest
  return (prefix : suffix)

-- Get the element at a specific index in a list
atIndex :: [a] -> Int -> Maybe a
atIndex [] _ = Nothing
atIndex (x:xs) i
  | i == 0    = Just x
  | otherwise = xs `atIndex` (i - 1)
