{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ x) symbol = x == symbol
has (EncodingNode _ left right) symbol = has left symbol || has right symbol

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode (EncodingLeaf _ x) symbol
  | x == symbol = Just []
  | otherwise   = Nothing
encode (EncodingNode _ left right) symbol =
  case (encode left symbol, encode right symbol) of
    (Just bitsLeft, _) -> Just (Zero : bitsLeft)
    (_, Just bitsRight) -> Just (One : bitsRight)
    _                   -> Nothing

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ x) bits = Just (x, bits)
decodeOnce (EncodingNode _ l r) (Zero : bits) = decodeOnce l bits
decodeOnce (EncodingNode _ l r) (One : bits)  = decodeOnce r bits
decodeOnce _ _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decode' tree bits []
  where
    decode' _ [] acc = Just (reverse acc)
    decode' t b acc =
      case decodeOnce t b of
        Just (symbol, remainingBits) -> decode' tree remainingBits (symbol : acc)
        Nothing                      -> Nothing

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength t = fromIntegral (count t) / fromIntegral (countSymbols t)
  where
  countSymbols :: EncodingTree a -> Int
  countSymbols (EncodingLeaf cnt _)   = cnt
  countSymbols (EncodingNode _ l r)   = countSymbols l + countSymbols r

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress _ [] = (Nothing, [])
compress generateTree input =
  case generateTree input of
    Just tree -> (Just tree, concatMap (encodeSymbol tree) input)
    Nothing   -> (Nothing, [])
  where
    encodeSymbol t symbol =
      case encode t symbol of
        Just bits -> bits
        Nothing   -> error "Symbol not present in encoding tree"

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Nothing, _) = Nothing
uncompress (Just tree, bits) = uncompress' tree bits []
  where
    uncompress' _ [] acc = Just (reverse acc)
    uncompress' t b acc =
      case decodeOnce t b of
        Just (symbol, remainingBits) -> uncompress' tree remainingBits (symbol : acc)
        Nothing                      -> Nothing
