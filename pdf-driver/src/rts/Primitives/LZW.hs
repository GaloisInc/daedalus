{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module Primitives.LZW (lzwDecode) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.List(uncons)

import System.IO.Unsafe(unsafePerformIO)
import Control.Exception(evaluate,try)
import Codec.Compression.Zlib(decompress)
import Codec.Compression.Zlib.Internal(DecompressError(..))

import PdfMonad.Transformer


lzwDecode :: PdfParser m => 
             Integer -> Integer -> Integer -> Integer -> Integer -> Input -> m Input 
lzwDecode predi colors bpc cols early inp =
    pError FromUser "LZW.lzwDecode" "Not implemented yet" 
