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

inputString :: String
inputString = "But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born and I will give you a complete account of the system, and expound the actual teachings of the great explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure rationally encounter consequences that are extremely painful. Nor again is there anyone who loves or pursues or desires to obtain pain of itself, because it is pain, but because occasionally circumstances occur in which toil and pain can procure him some great pleasure. To take a trivial example, which of us ever undertakes laborious physical exercise, except to obtain some advantage from it? But who has any right to find fault with a man who chooses to enjoy a pleasure that has no annoying consequences, or one who avoids a pain that produces no resultant pleasure?"


benchmark :: IO ()
benchmark = do
    putStrLn "Benchmarking compression and decompression methods..."
    putStrLn ""

    -- Benchmark RLE
    putStrLn "Benchmarking RLE..."
    timeRLE <- timeCompressionAndDecompression (RLE.compress :: String -> [(Char, Int)]) RLE.uncompress "RLE"
    putStrLn $ "RLE Compression/Decompression Time: " ++ show timeRLE ++ "s, Compression Ratio: " ++ show ((fromIntegral (length (RLE.compress inputString)) / fromIntegral (length inputString)) * 100) ++ "%"
    putStrLn $ "Original Size: " ++ show (length inputString) ++ " bytes, Compressed Size: " ++ show (length (RLE.compress inputString)) ++ " bytes"

    -- Benchmark LZ78
    putStrLn "Benchmarking LZ78..."
    timeLZ78 <- timeCompressionAndDecompression (LZ78.compress :: String -> [(Int, Char)]) LZ78.uncompress "LZ78"
    putStrLn $ "LZ78 Compression/Decompression Time: " ++ show timeLZ78 ++ "s, Compression Ratio: " ++ show ((fromIntegral (length (LZ78.compress inputString)) / fromIntegral (length inputString)) * 100) ++ "%"
    putStrLn $ "Original Size: " ++ show (length inputString) ++ " bytes, Compressed Size: " ++ show (length (LZ78.compress inputString)) ++ " bytes"

    -- Benchmark LZW
    putStrLn "Benchmarking LZW..."
    timeLZW <- timeCompressionAndDecompression (LZW.compress :: String -> [Int]) LZW.uncompress "LZW"
    putStrLn $ "LZW Compression/Decompression Time: " ++ show timeLZW ++ "s, Compression Ratio: " ++ show ((fromIntegral (length (LZW.compress inputString)) / fromIntegral (length inputString)) * 100) ++ "%"
    putStrLn $ "Original Size: " ++ show (length inputString) ++ " bytes, Compressed Size: " ++ show (length (LZW.compress inputString)) ++ " bytes"

    -- Benchmark Huffman
    putStrLn "Benchmarking Huffman..."
    timeHuffman <- huffmanCompressionAndDecompression (Huffman.tree :: String -> Maybe (EncodingTree.EncodingTree Char)) "Huffman"
    putStrLn $ "Huffman Compression/Decompression Time: " ++ show timeHuffman ++ "s, Compression Ratio: " ++ show ((fromIntegral (length (EncodingTree.compress Huffman.tree inputString)) / fromIntegral (length inputString)) * 100) ++ "%"
    putStrLn $ "Original Size: " ++ show (length inputString) ++ " bytes, Compressed Size: " ++ show (length (EncodingTree.compress Huffman.tree inputString)) ++ " bytes"

    -- Benchmark Shannon-Fano
    putStrLn "Benchmarking Shannon-Fano..."
    timeShannonFano <- shannonCompressionAndDecompression (ShannonFano.tree :: String -> Maybe (EncodingTree.EncodingTree Char)) "Shannon-Fano"
    putStrLn $ "Shannon-Fano Compression/Decompression Time: " ++ show timeShannonFano ++ "s, Compression Ratio: " ++ show ((fromIntegral (length (EncodingTree.compress ShannonFano.tree inputString)) / fromIntegral (length inputString)) * 100) ++ "%"
    putStrLn $ "Original Size: " ++ show (length inputString) ++ " bytes, Compressed Size: " ++ show (length (EncodingTree.compress ShannonFano.tree inputString)) ++ " bytes"

    putStrLn ""
    putStrLn "Benchmarking completed."

timeCompressionAndDecompression :: (Show a) => (String -> a) -> (a -> Maybe String) -> String -> IO Double
timeCompressionAndDecompression compressFunc uncompressFunc method = do
    startTime <- getCurrentTime
    case uncompressFunc (compressFunc inputString) of
        Just _ -> return ()
        Nothing -> putStrLn $ "Error in " ++ method ++ " decompression."
    endTime <- getCurrentTime
    return $ realToFrac $ diffUTCTime endTime startTime

huffmanCompressionAndDecompression :: (String -> Maybe (EncodingTree.EncodingTree Char)) -> String -> IO Double
huffmanCompressionAndDecompression treeConstructor method = do
    startTime <- getCurrentTime
    case (treeConstructor inputString) of
        Just huffmanTree -> do
            let compressedHuffman = EncodingTree.compress Huffman.tree inputString
            let compressionRatioHuffman = (fromIntegral (length inputString) / fromIntegral (length compressedHuffman)) * 100
            case EncodingTree.uncompress compressedHuffman of
                Just _ -> return ()
                Nothing -> putStrLn $ "Error in " ++ method ++ " decompression."
        Nothing -> putStrLn $ "Error in " ++ method ++ " tree construction."
    endTime <- getCurrentTime
    return $ realToFrac $ diffUTCTime endTime startTime

shannonCompressionAndDecompression :: (String -> Maybe (EncodingTree.EncodingTree Char)) -> String -> IO Double
shannonCompressionAndDecompression treeConstructor method = do
    startTime <- getCurrentTime
    case (treeConstructor inputString) of
        Just shannonTree -> do
            let compressedShannonFano = EncodingTree.compress ShannonFano.tree inputString
            let compressionRatioShannonFano = (fromIntegral (length inputString) / fromIntegral (length compressedShannonFano)) * 100
            case EncodingTree.uncompress compressedShannonFano of
                Just _ -> return ()
                Nothing -> putStrLn $ "Error in " ++ method ++ " decompression."
        Nothing -> putStrLn $ "Error in " ++ method ++ " tree construction."
    endTime <- getCurrentTime
    return $ realToFrac $ diffUTCTime endTime startTime

main :: IO ()
main = do
    benchmark
    return ()
