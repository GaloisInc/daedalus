{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module Primitives.ASCIIHex (asciiHexDecode) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C 
import Data.Word 
import Data.Char (ord) 

import PdfMonad.Transformer


asciiHexDecode :: PdfParser m => Input -> m Input 
asciiHexDecode inp =
  case hexDecode (inputBytes inp) of 
    Just bs -> pure Input { inputBytes = bs, inputOffset = 0 }
    Nothing -> pError FromUser "ASCIIHex.asciiHexDecode" "Unknown error" 

-- XXX: thread errors through here properly 
hexDecode :: B.ByteString -> Maybe B.ByteString 
hexDecode bs =
    case sequence $ map pairDecode pairs of 
      Just xs -> Just $ B.pack xs 
      Nothing -> Nothing 
  where 
    filtered = C.filter (\x -> notElem x wsList) $ fst $ C.break (=='>') bs

    pairs = case group $ C.unpack $ filtered of 
                (ps, Nothing)   -> ps  
                (ps, Just stub) -> ps ++ [(stub, '0')]

    -- XXX: add the full list of PDF whitespace characters
    wsList = [' ']

pairDecode :: (Char, Char) -> Maybe Word8 
pairDecode (h,l) = 
    case (cVal h, cVal l) of 
      (Just hr, Just br) -> Just $ fromIntegral (16 * hr + br) 
      _                  -> Nothing 
  where
    cVal c
      | '0' <= c && c <= '9' = Just (ord c - ord '0')  
      | 'A' <= c && c <= 'F' = Just (ord c - ord '0')
      | 'a' <= c && c <= 'f' = Just (ord c - ord '0')
      | otherwise = Nothing 

group :: [a] -> ([(a, a)], Maybe a)
group []         = ([], Nothing) 
group [a]        = ([], Just a) 
group (a:(b:cs)) = ( (a,b):ps, res ) 
  where (ps, res) = group cs 
