import Test.HUnit
import RLE
import LZ.LZ78
import LZ.LZW
import Statistic.Huffman
import Statistic.ShannonFano

-- Test cases for RLE compression and decompression
rleTests :: Test
rleTests = TestList
    [ "RLE Compression" ~: RLE.compress "aaaabbcbbb" ~?= [('a', 4), ('b', 2), ('c', 1), ('b', 3)]
    , "RLE Uncompression" ~: RLE.uncompress [('a', 4), ('b', 2), ('c', 1), ('b', 3)] ~?= Just "aaaabbcbbb"
    ]

-- Test cases for LZ78 compression and decompression
lz78Tests :: Test
lz78Tests = TestList
    [ "LZ78 Compression" ~: LZ.LZ78.compress "belle echelle !" ~?= [(0, 'b'), (0, 'e'), (0, 'l'), (3, 'e'), (0, ' '), (2, 'c'), (0, 'h'), (2, 'l'), (4, ' '), (0, '!')]
    , "LZ78 Uncompression" ~: LZ.LZ78.uncompress [(0, 'b'), (0, 'e'), (0, 'l'), (3, 'e'), (0, ' '), (2, 'c'), (0, 'h'), (2, 'l'), (4, ' '), (0, '!')] ~?= Just "belle echelle !"
    ]

-- Test cases for LZW compression and decompression
lzwTests :: Test
lzwTests = TestList
    [ "LZW Compression" ~: LZ.LZW.compress "belle echelle" ~?= [98, 101, 108, 108, 101, 32, 101, 99, 104, 257, 259]
    , "LZW Uncompression" ~: LZ.LZW.uncompress [98, 101, 108, 108, 101, 32, 101, 99, 104, 257, 259] ~?= Just "belle echelle"
    ]

-- Test cases for Huffman tree generation
huffmanTests :: Test
huffmanTests = TestList
    [ "Huffman Tree Generation" ~: Statistic.Huffman.tree "aaaabbcbbb" ~?= Just (EncodingNode 10 (EncodingLeaf 4 'a') (EncodingNode 6 (EncodingLeaf 2 'b') (EncodingNode 3 (EncodingLeaf 1 'c') (EncodingLeaf 3 'b'))))
    ]

-- Test cases for Shannon-Fano tree generation
shannonFanoTests :: Test
shannonFanoTests = TestList
    [ "Shannon-Fano Tree Generation" ~: Statistic.ShannonFano.tree "aaaabbcbbb" ~?= Just (EncodingNode 10 (EncodingLeaf 4 'a') (EncodingNode 6 (EncodingLeaf 2 'b') (EncodingNode 3 (EncodingLeaf 1 'c') (EncodingLeaf 3 'b'))))
    ]

main :: IO ()
main = do
    runTestTT $ TestList [rleTests, lz78Tests, lzwTests, huffmanTests, shannonFanoTests]
    return ()
