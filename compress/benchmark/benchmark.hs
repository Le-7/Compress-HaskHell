import System.TimeIt (timeItT)
import Control.Monad (forM_)
import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified RLE
import qualified LZ.LZ78 as LZ78
import qualified LZ.LZW as LZW
import qualified Statistic.Huffman as Huffman
import qualified Statistic.ShannonFano as ShannonFano
import qualified Statistic.EncodingTree as EncodingTree

-- Define a common input string for benchmarking
inputString :: String
inputString = "But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born and I will give you a complete account of the system, and expound the actual teachings of the great explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure rationally encounter consequences that are extremely painful. Nor again is there anyone who loves or pursues or desires to obtain pain of itself, because it is pain, but because occasionally circumstances occur in which toil and pain can procure him some great pleasure. To take a trivial example, which of us ever undertakes laborious physical exercise, except to obtain some advantage from it? But who has any right to find fault with a man who chooses to enjoy a pleasure that has no annoying consequences, or one who avoids a pain that produces no resultant pleasure?"

-- Benchmark each compression and decompression method
benchmark :: IO ()
benchmark = do
    putStrLn "Benchmarking compression and decompression methods..."
    putStrLn ""

    -- Benchmark RLE
    putStrLn "Benchmarking RLE..."
    timeRLE <- timeCompressionAndDecompression (RLE.compress :: String -> [(Char, Int)]) RLE.uncompress "RLE"
    putStrLn $ "RLE Compression/Decompression Time: " ++ show timeRLE

    -- Benchmark LZ78
    putStrLn "Benchmarking LZ78..."
    timeLZ78 <- timeCompressionAndDecompression (LZ78.compress :: String -> [(Int, Char)]) LZ78.uncompress "LZ78"
    putStrLn $ "LZ78 Compression/Decompression Time: " ++ show timeLZ78

    -- Benchmark LZW
    putStrLn "Benchmarking LZW..."
    timeLZW <- timeCompressionAndDecompression (LZW.compress :: String -> [Int]) LZW.uncompress "LZW"
    putStrLn $ "LZW Compression/Decompression Time: " ++ show timeLZW

    -- Benchmark Huffman
    putStrLn "Benchmarking Huffman..."
    timeHuffman <- huffmanCompressionAndDecompression (Huffman.tree :: String -> Maybe (EncodingTree.EncodingTree Char)) "Huffman"
    putStrLn $ "Huffman Compression/Decompression Time: " ++ show timeHuffman

    -- Benchmark Shannon-Fano
    putStrLn "Benchmarking Shannon-Fano..."
    timeShannonFano <- shannonCompressionAndDecompression (ShannonFano.tree :: String -> Maybe (EncodingTree.EncodingTree Char)) "Shannon-Fano"
    putStrLn $ "Shannon-Fano Compression/Decompression Time: " ++ show timeShannonFano

    putStrLn ""
    putStrLn "Benchmarking completed."

-- Benchmark compression and decompression for a given compression method
timeCompressionAndDecompression :: (Show a) => (String -> a) -> (a -> Maybe String) -> String -> IO Double
timeCompressionAndDecompression compressFunc uncompressFunc method = do
    startTime <- getCurrentTime
    let compressed = compressFunc inputString
    case uncompressFunc compressed of
        Just _ -> return ()
        Nothing -> putStrLn $ "Error in " ++ method ++ " decompression."
    endTime <- getCurrentTime
    return $ realToFrac $ diffUTCTime endTime startTime

-- Benchmark compression and decompression for a given compression method using an encoding tree

-- Benchmark Huffman compression and decompression
huffmanCompressionAndDecompression :: (String -> Maybe (EncodingTree.EncodingTree Char)) -> String -> IO Double
huffmanCompressionAndDecompression treeConstructor method = do
    startTime <- getCurrentTime
    let maybeTree = treeConstructor inputString
    case maybeTree of
        Just huffmanTree -> do
            let (encodingTree, compressedBits) = EncodingTree.compress Huffman.tree inputString
            case EncodingTree.uncompress (encodingTree, compressedBits) of
                Just _ -> return ()
                Nothing -> putStrLn $ "Error in " ++ method ++ " decompression."
        Nothing -> putStrLn $ "Error in " ++ method ++ " tree construction."
    endTime <- getCurrentTime
    return $ realToFrac $ diffUTCTime endTime startTime

-- Benchmark Shannon-Fano compression and decompression
shannonCompressionAndDecompression :: (String -> Maybe (EncodingTree.EncodingTree Char)) -> String -> IO Double
shannonCompressionAndDecompression treeConstructor method = do
    startTime <- getCurrentTime
    let maybeTree = treeConstructor inputString
    case maybeTree of
        Just shannonTree -> do
            let (encodingTree, compressedBits) = EncodingTree.compress ShannonFano.tree inputString
            case EncodingTree.uncompress (encodingTree, compressedBits) of
                Just _ -> return ()
                Nothing -> putStrLn $ "Error in " ++ method ++ " decompression."
        Nothing -> putStrLn $ "Error in " ++ method ++ " tree construction."
    endTime <- getCurrentTime
    return $ realToFrac $ diffUTCTime endTime startTime


main :: IO ()
main = do
    benchmark
    return()