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


{--
Structure: 
* Recursive function: grab the first code, process it, and then decode the continuation
* Codes can be just bit sequences 
* Algorithm
  - Update the table from the previous code
  - Take n bits fron the input sequence 
  - look up bit sequence in the table (modulo sequence length change)
  - add the result to the decoded sequence

Once code size change, all the emitted codes are longer. 

Type of dictionary: 
Dict Int [Word8]
...This is because we want to translate each code into an integer based 
on the current code width, and look up the result. 

Decode process notes: 
* At each point, update the table with the current and next word 
* if the next word is a code, only take the first character from 
  dictionary lookup 
* When you create the *last* code that fits into the word size, 
  then, increase the word size (becuase the decode is one step
  behind the encode)
* There's a weird corner case when the same sequence repeats in 
  the input - this can mean that a code appears that isn't in the 
  current dictionary. This can only happen for inputs with shape 
  cScSc. See the wikipedia article for examples. 
--}