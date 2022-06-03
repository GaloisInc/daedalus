{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module Primitives.Deflate (flateDecode) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.List(uncons)

import System.IO.Unsafe(unsafePerformIO)
import Control.Exception(evaluate,try)
import Codec.Compression.Zlib(decompress)
import Codec.Compression.Zlib.Internal(DecompressError(..))

import RTS.Input

import PdfMonad.Transformer

import Debug.Trace

flateDecode :: PdfParser m =>
               Integer -> Integer -> Integer -> Integer -> Input -> m Input
flateDecode predi colors bpc cols inp =
  case strictDecompress (inputBytes inp) of
    Right a -> do bs <- unPredict predi colors bpc cols a
                  pure (newInput name bs)
                  -- XXX: better indicattion of where these bytes came from.
    Left err -> trace "WARNING: deflate is failing" $ pError' FromUser [] (show err)
  where name = C.pack ("FlateDecode" ++ show (inputOffset inp))

strictDecompress :: B.ByteString -> Either DecompressError B.ByteString
strictDecompress =
  unsafePerformIO . try . evaluate . LBS.toStrict . decompress . LBS.fromStrict

-- XXX: prediction strategies other than PNG Up
-- XXX: A bunch of this can happen in Daedalus.
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

