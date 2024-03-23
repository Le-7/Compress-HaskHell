module LZ.LZW (compress, uncompress) where

import Data.List (find)
import Data.Char(ord)

-- Define the initial dictionary for LZW
initialDictionary :: [(String, Int)]
initialDictionary = zip (map (:[]) ['\0'..'\255']) [0..255]

-- Function to compress a string using LZW
compress :: String -> [Int]
compress = compress' initialDictionary ""

-- Helper function for compression
compress' :: [(String, Int)] -> String -> String -> [Int]
compress' _ _ [] = []
-- End of input string, output the code for the currentString and stop
compress' dict currentString [x] = case find (\(s, _) -> s == currentString ++ [x]) dict of
    Just entry -> [snd entry]
    Nothing -> case find (\(s, _) -> s == currentString) dict of
        Just (_, code) -> [code] ++ [ord x]
        Nothing -> error ("Dictionary entry not found during compression: " ++ show currentString ++ " ++ [" ++ show x ++ "]")
-- Process the string
compress' dict currentString (x:xs) = case find (\(s, _) -> s == currentString ++ [x]) dict of
    Just entry -> compress' dict (currentString ++ [x]) xs
    Nothing -> case find (\(s, _) -> s == currentString) dict of
        Just (_, code) -> code : compress' ((currentString ++ [x], length dict) : dict) [x] xs
        Nothing -> error ("Dictionary entry not found during compression: " ++ show currentString ++ " ++ [" ++ show x ++ "]")
        
-- Function to uncompress a list of integers using LZW
uncompress :: [Int] -> Maybe String
uncompress compressed = Just (uncompress' compressed initialDictionary)

-- Helper function for decompression
uncompress' :: [Int] -> [(String, Int)] -> String
uncompress' [] _ = ""
uncompress' [x] dict = case find (\(_, code) -> code == x) dict of
    Just (entry, _) -> entry
    Nothing -> error ("Dictionary entry not found during decompression: " ++ show x)
uncompress' (x:y:xs) dict = case find (\(_, code) -> code == x) dict of
    Just (entry, _) ->
        case find (\(_, code) -> code == y) dict of
            Just (nextEntry, _) ->
                entry ++ uncompress' (y:xs) (updateDictionary (entry ++ [head nextEntry]) dict)
            Nothing ->
                entry ++ uncompress' (y:xs) (((entry ++ [head entry]), length dict) : dict)
    Nothing -> error ("Dictionary entry not found during decompression: " ++ show x)


-- Update dictionary with new entry
updateDictionary :: String -> [(String, Int)] -> [(String, Int)]
updateDictionary newEntry dict = (newEntry, length dict) : dict

