module Main (main) where

import System.IO
import qualified RLE
import qualified LZ.LZ78 as LZ78
import qualified LZ.LZW as LZW
import qualified Statistic.Huffman as Huffman
import qualified Statistic.ShannonFano as ShannonFano
import qualified Statistic.EncodingTree as EncodingTree

-- Menu options
data MenuOption = RLE | LZ78 | LZW | Huffman | ShannonFano | Exit deriving (Read, Show, Eq)

-- Main menu
mainMenu :: IO ()
mainMenu = do
    putStrLn "Choose a compression method:"
    putStrLn "1. RLE"
    putStrLn "2. LZ78"
    putStrLn "3. LZW"
    putStrLn "4. Huffman"
    putStrLn "5. Shannon-Fano"
    putStrLn "6. Exit"
    putStr "Enter your choice: "
    hFlush stdout
    choice <- getLine
    handleChoice choice

-- Handle user choice
handleChoice :: String -> IO ()
handleChoice "1" = handleCompression RLE.compress RLE.uncompress "RLE"
handleChoice "2" = handleCompression LZ78.compress LZ78.uncompress "LZ78"
handleChoice "3" = handleCompression LZW.compress LZW.uncompress "LZW"
handleChoice "4" = handleHuffman
handleChoice "5" = handleShannonFano
handleChoice "6" = putStrLn "Exiting program."
handleChoice _   = putStrLn "Invalid choice. Please enter a valid option." >> mainMenu

-- General compression function
handleCompression :: (Show a, Eq a) => (String -> a) -> (a -> Maybe String) -> String -> IO ()
handleCompression compressFunc uncompressFunc method = do
    putStrLn $ "Enter the input string for " ++ method ++ ":"
    input <- getLine
    let compressed = compressFunc input
    case uncompressFunc compressed of
        Just uncompressed -> do
            putStrLn $ "Compressed: " ++ show compressed
            putStrLn $ "Uncompressed: " ++ show uncompressed
            mainMenu
        Nothing -> putStrLn $ "Error in " ++ method ++ " compression."


-- Handle Huffman compression
handleHuffman :: IO ()
handleHuffman = do
    putStrLn "Enter the input string for Huffman:"
    input <- getLine
    let (encodingTree, compressedBits) = EncodingTree.compress  Huffman.tree input
    putStrLn $ "Input String: " ++ input
    putStrLn $ "Encoding Tree: " ++ show encodingTree
    putStrLn $ "Compressed Bits: " ++ show compressedBits
    case EncodingTree.uncompress (encodingTree, compressedBits) of
        Just decompressedString -> putStrLn $ "Decompressed String: " ++ decompressedString
        Nothing -> putStrLn "Decompression failed."
    mainMenu

-- Handle Shannon-Fano compression
handleShannonFano :: IO ()
handleShannonFano = do
    putStrLn "Enter the input string for Shannon-Fano:"
    input <- getLine
    let (encodingTree, compressedBits) = EncodingTree.compress ShannonFano.tree input
    putStrLn $ "Input String: " ++ input
    putStrLn $ "Encoding Tree: " ++ show encodingTree
    putStrLn $ "Compressed Bits: " ++ show compressedBits
    case EncodingTree.uncompress (encodingTree, compressedBits) of
        Just decompressedString -> putStrLn $ "Decompressed String: " ++ decompressedString
        Nothing -> putStrLn "Decompression failed."
    mainMenu


main :: IO ()
main = mainMenu
