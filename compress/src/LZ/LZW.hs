module LZ.LZW (compress, uncompress) where

-- Import necessary modules
import Data.List (find)

-- Define the initial dictionary for LZW
initialDictionary :: [(String, Int)]
initialDictionary = zip (map (:[]) ['\0'..'\255']) [0..255]

-- Function to compress a string using LZW
compress :: String -> [Int]
compress = compress' initialDictionary ""

-- Helper function for compression
compress' :: [(String, Int)] -> String -> String -> [Int]
compress' _ _ [] = []
compress' dict currentString (x:xs) = case find (\(s, _) -> s == currentString ++ [x]) dict of
    Just entry -> compress' dict (currentString ++ [x]) xs
    Nothing -> case find (\(s, _) -> s == currentString) dict of
        Just (_, code) -> code : compress' ((currentString ++ [x], length dict) : dict) [x] xs
        Nothing -> error "Dictionary entry not found during compression"

-- Function to uncompress a list of integers using LZW
uncompress :: [Int] -> Maybe String
uncompress = uncompress' initialDictionary ""

-- Helper function for uncompression
uncompress' :: [(String, Int)] -> String -> [Int] -> Maybe String
uncompress' _ currentString [] = Just currentString
uncompress' dict currentString (code:codes) = case find (\(_, c) -> c == code) dict of
    Just (entry, _) -> uncompress' ((currentString ++ [head entry], length dict) : dict) (currentString ++ entry) codes
    Nothing -> Nothing  -- Invalid code, dictionary entry not found during uncompression
