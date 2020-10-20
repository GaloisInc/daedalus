{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module Primitives.Decrypt (decrypt) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.List(uncons)
import Data.ByteArray as BA 

import Crypto.Cipher.AES as Y 
import Crypto.Cipher.Types as Y 
import Crypto.Hash as Y 
import Crypto.Error as Y

import Data.Serialize as S 

import RTS.Input

import PdfMonad.Transformer

decrypt :: PdfParser m => Input -> m Input 
decrypt inp = do 
  ctx <- getEncContext 
  case ctx of 
    Nothing -> pure inp 
    Just p  -> 
      let aeskey = makeObjKey p 
      -- XXX: Baked in AES128 
      in case Y.cipherInit @Y.AES128 aeskey of 
        Y.CryptoFailed error -> 
          pError FromUser "Decrypt.decrypt" (show error) 
        Y.CryptoPassed ciph -> 
          case Y.makeIV head of 
            Nothing -> 
              pError FromUser "Decrypt.decrypt" "Could not construct AES IV" 
            Just iv -> 
              let res = Y.cbcDecrypt ciph iv dat 
              in pure (newInput name res) 
  where 
    (head, dat) = B.splitAt 2 $ inputBytes inp
    name = C.pack ("Decrypt" ++ show (inputOffset inp))


makeObjKey :: EncContext -> B.ByteString 
makeObjKey ctx = 
    B.take ((keylen ctx) + 5) (BA.convert digest)
  where 
    ob = B.take 3 $ B.reverse $ S.encode (refObj (obj ctx))
    gb = B.take 2 $ B.reverse $ S.encode (refGen (obj ctx))
    salt = B.pack [0x73, 0x41, 0x6C, 0x54] -- magic string 
    digest = hashFinalize $ hashUpdates (Y.hashInit @Y.MD5) [(key ctx), ob, gb, salt]
     
     

