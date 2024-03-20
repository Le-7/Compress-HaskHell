module LZ.LZ78 (compress, uncompress) where

import LZ.Dictionaries (empty)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Dictionary = [String]

-- | Compress a string using LZ78.
compress :: String -> [(Int, Char)]
compress = compress' empty

compress' :: Dictionary -> String -> [(Int, Char)]
compress' _ [] = []
compress' dict str = compress2helper dict str []

compress2helper :: Dictionary -> String -> String -> [(Int, Char)]
compress2helper _ [] _ = []

compress2helper dict (x:[]) prefix =
  case elemIndex (prefix ++ [x]) dict of
    Just idx -> [(fromMaybe 0 (elemIndex prefix dict), x)]
    Nothing  -> [(fromMaybe 0 (elemIndex prefix dict), x)]

compress2helper dict (x:xs) prefix =
  case elemIndex (prefix ++ [x]) dict of
    Just idx -> compress2helper dict xs (prefix ++ [x])
    Nothing  -> (fromMaybe 0 (elemIndex prefix dict), x) : compress2helper (dict ++ [prefix ++ [x]]) xs []

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