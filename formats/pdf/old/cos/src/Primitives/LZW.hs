{-# Language OverloadedStrings, DataKinds #-}
module Primitives.LZW (lzwDecode) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M 
import Data.List(uncons)
import Data.Word 
import Data.Bits 
import Data.Maybe

import System.IO.Unsafe(unsafePerformIO)
import Control.Exception(evaluate,try)

import Daedalus.RTS.Input
import RTS.ParseError

import PdfMonad.Transformer


lzwDecode :: PdfParser m => 
    Integer -> Integer -> Integer -> Integer -> Integer -> Input -> m Input 
lzwDecode predi colors bpc cols early inp =
  -- pError' FromUser [] "LZW not supported yet"
  do lzwres <- lzwDecompress (inputBytes inp)
     bs <- unPredict predi colors bpc cols lzwres
     pure (newInput name bs) 
  where name = C.pack ("LzwDecode" ++ show (inputOffset inp))

-- XXX: Code copied from Deflate.hs - should de-duplicate 
unPredict :: PdfParser m => Integer -> Integer -> Integer -> Integer ->
              B.ByteString -> m B.ByteString
unPredict predi colors bpc columns bs
  | predi == 1 = pure bs
  | predi == 12 && colors == 1 && bpc == 8 = pngUp columns bs

  | otherwise = pError' FromUser [] "Unsupported predictor algorithm."

pngUp :: PdfParser m => Integer -> B.ByteString -> m B.ByteString
pngUp columns bs =
  case uncons rows of
    Just (start,rows') -> pure (B.concat (scanl adder start rows'))
    Nothing            -> pError' FromUser [] "malformed PNG UP stream"
                              --- XXX: better error?

  where
  rows = map B.tail (rowify (fromIntegral columns + 1) bs)

  adder l r = B.pack (B.zipWith (+) l r)


-- ^ Divide a bytestream into rows of length n.
-- Assumes the total length is divisible by n.
rowify :: Int -> B.ByteString -> [B.ByteString]
rowify n bs
  | B.length bs < n = []
  | otherwise =
    let (front, tl) = B.splitAt n bs
    in front : rowify n tl

-- Reference LZW input / output (PDF standard, S7.4.4.2) 

testInput :: B.ByteString 
testInput = B.pack [ 45, 45, 45, 45, 45, 65, 45, 45, 45, 66 ] 

testOutput :: B.ByteString
testOutput = B.pack [ 0x80, 0x0B, 0x60, 0x50, 0x22, 0x0C, 0x0C, 0x85, 0x01 ]


--------------------------------------------------------------------------------
-- The following code is modified from haskell-lzw, by Erik Gunnarsson ---------
-- https://github.com/egunnarsson/haskell-lzw/blob/master/lzw.hs ---------------
--------------------------------------------------------------------------------

data Bit = Zero | One

instance Show Bit where
  show Zero = "0"
  show One = "1"

instance Eq Bit where
  Zero == Zero = True
  One == One = True
  _ == _ = False

type Chunk = B.ByteString
type Key = Int
type BitStream = [Bit]
type EncodeTable = M.Map Chunk Key
type DecodeTable = M.Map Key Chunk

--------------------------------------------------------------------------------
-- Util ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- lastToMaybe :: [a] -> Maybe a
-- lastToMaybe [] = Nothing
-- lastToMaybe x  = Just (last x)

splitAt' :: Int -> [a] -> Maybe ([a],[a])
splitAt' 0 zs = Just ([],zs)
splitAt' n [] = Nothing
splitAt' n (z:zs)
  | n > 0     = fmap (\(zs',zs'') -> (z:zs',zs'')) (splitAt' (n-1) zs)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Conversions -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- shift . or
(#) :: (Bits a, Num a) => a -> Bit -> a
(#) w One  = shiftL w 1 .|. 1
(#) w Zero = shiftL w 1

packBits :: BitStream -> [Word8]
packBits (b8:b7:b6:b5:b4:b3:b2:b1:xs) = (0 # b8 # b7 # b6 # b5 # b4 # b3 # b2 # b1) : packBits xs
packBits (b8:b7:b6:b5:b4:b3:b2:[]) = [shiftL (0 # b8 # b7 # b6 # b5 # b4 # b3 # b2) 1]
packBits (b8:b7:b6:b5:b4:b3:[]) = [shiftL (0 # b8 # b7 # b6 # b5 # b4 # b3) 2]
packBits (b8:b7:b6:b5:b4:[]) = [shiftL (0 # b8 # b7 # b6 # b5 # b4) 3]
packBits (b8:b7:b6:b5:[]) = [shiftL (0 # b8 # b7 # b6 # b5) 4]
packBits (b8:b7:b6:[]) = [shiftL (0 # b8 # b7 # b6) 5]
packBits (b8:b7:[]) = [shiftL (0 # b8 # b7) 6]
packBits (b8:[]) = [shiftL (0 # b8) 7]
packBits [] = []

getBit :: (Bits a) => a -> Int -> Bit
getBit w n = if testBit w n then One else Zero

getBits :: (Bits a) => a -> Int -> [Bit]
getBits w n
  | n < 0 = []
  | otherwise = getBit w n : getBits w (n - 1)

toBits :: (Bits a, FiniteBits a) => a -> [Bit]
toBits w = getBits w (finiteBitSize w - 1)

unpackBits :: B.ByteString -> BitStream
unpackBits bs = concatMap toBits (B.unpack bs)

-- Returns the minimun number of bits needed to represent a given number
bitCount :: Int -> Int
bitCount n = finiteBitSize n - countLeadingZeros n

-- Converts a number of bits to a key
toKey :: [Bit] -> Key
toKey key = f key 0
  where f (k:ks)  n = f ks (n # k)
        f [] n = n

-- Converts a bitstream to a list of keys, assumes first key has been extracted
unpackKeys :: BitStream -> [[Key]] -- for decoding
unpackKeys bs = unpackKeys' bs 258 [] 

unpackKeys' :: BitStream -> Int -> [Key] -> [[Key]] 
unpackKeys' xs a acc = 
  case splitAt' (bitCount a) xs of
    Nothing -> []
    Just (k, rest) -> 
      -- Handle PDF magic control codes 
      case toKey k of 
        256 -> acc : unpackKeys' rest (a + 1) [] -- Clear table 
        257 -> [acc] -- EOD 
        key -> unpackKeys' rest (a + 1) (acc ++ [key]) 

--------------------------------------------------------------------------------
-- Decompress Code--------------------------------------------------------------
--------------------------------------------------------------------------------

initialDecodeTable :: DecodeTable
initialDecodeTable = 
  M.fromDistinctAscList $ 
    [ (fromIntegral n, B.singleton n) | n <- [(0::Word8)..] ] 
    ++ [(256, B.singleton 0x00), (257, B.singleton 0x00)] -- Pad PDF dummy values

updateDecodeTable :: Chunk -> DecodeTable -> DecodeTable
updateDecodeTable c t = M.insert (M.size t) c t

getKey :: Key -> Chunk -> DecodeTable -> Chunk
getKey key previous table = 
  fromMaybe (B.snoc previous (B.head previous)) (M.lookup key table)

decompressKeys :: PdfParser m => [Key] -> m [Chunk]
decompressKeys [] = pure [] 
decompressKeys (k:keys) = 
    do first <- case M.lookup k initialDecodeTable of 
                Just key -> pure key 
                Nothing  -> pError' FromUser []
                  "Ill-formed LZW stream: first key is not in initial dictionary."
       pure (first : decompressKeys' initialDecodeTable keys first) 

decompressKeys' :: DecodeTable -> [Key] -> Chunk -> [Chunk]
decompressKeys' table [] _ = []
decompressKeys' table (key:ks) previous = 
  let output = getKey key previous table
      newTable = updateDecodeTable (B.snoc previous (B.head output)) table
  in output : decompressKeys' newTable ks output

lzwDecompress :: PdfParser m => Chunk -> m Chunk   
lzwDecompress input = 
  do keys <- mapM decompressKeys $ (unpackKeys . unpackBits) input 
     pure (B.concat $ concat keys)
