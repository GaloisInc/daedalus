{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module Primitives.ASCII85 (ascii85Decode) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C 
import Data.Word 
import Data.Char (ord) 

import Daedalus.RTS.Input
import RTS.ParseError

import PdfMonad.Transformer

-- List of whitespace characters
isWS = [' ']

ascii85Decode :: PdfParser m => Input -> m Input 
ascii85Decode inp = 
  case a85Decode (inputBytes inp) of 
    Just bs -> pure (newInput name bs)
    Nothing -> pError' FromUser [] "ASCII85.ascii85Decode"
  where
  name = C.pack ("ASCII85" ++ show (inputOffset inp))

a85Decode :: B.ByteString -> Maybe B.ByteString 
a85Decode bs =
    Just $ 
      case toChunksN 5 subZ of 
        (xs, Just stub) -> 
          B.pack $ concat ((map a85compute xs) ++ [a85computeStub stub])
        (xs, Nothing)   -> 
          B.pack $ concat (map a85compute xs) 
  where 
    prefix = C.unpack $ fst $ C.breakSubstring (C.pack "~>") bs
    noWS = filter (\x -> notElem x isWS) prefix 
    subZ = replace "z" "!!!!!" noWS 

a85compute :: [Char] -> [Word8] 
a85compute cs =
    map fromIntegral $ reverse $ take 4 $ toDigits tot 256
  where 
    vals = map ((\x -> x - 33) . ord) cs 
    tot = foldl (\x y -> (x * 85) + y) 0 vals

a85computeStub :: [Char] -> [Word8] 
a85computeStub cs =
    take ((length cs) - 1) (a85compute padded) 
  where 
    padded = cs ++ (take (5 - length cs) $ repeat 'u') 

toDigits :: Int -> Int -> [Int]
toDigits val base = 
  (val `mod` base) : toDigits (val `div` base) base

toChunksN :: Int -> [a] -> ([[a]], Maybe [a])
toChunksN size [] = ([], Nothing) 
toChunksN size xs = 
    if (length $ last cs) == size
    then (cs, Nothing) 
    else (init cs, Just (last cs))
  where 
    cs = chunksOf xs 

    chunksOf [] = [] 
    chunksOf xs = 
      let (h,r) = splitAt size xs in 
      [h] ++ (chunksOf r) 

replace :: Eq a => [a] -> [a] -> [a] -> [a] 
replace pat rep [] = [] 
replace pat rep xs = 
    case h == pat of 
      True -> rep ++ (replace pat rep t) 
      False -> (head xs) : replace pat rep (tail xs)
  where 
    (h,t) = splitAt (length pat) xs 
