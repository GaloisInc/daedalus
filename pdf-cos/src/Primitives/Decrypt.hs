{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module Primitives.Decrypt (decrypt,makeFileKey) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteArray as BA 
import Data.Word 

import Crypto.Cipher.AES as Y 
import Crypto.Cipher.Types as Y 
import Crypto.Hash as Y 
import Crypto.Error as Y

import Data.Serialize as S 

import RTS.Input

import PdfMonad.Transformer

decrypt :: PdfParser m => Input -> m Input 
decrypt inp = do 
  ctxMaybe <- getEncContext 
  case ctxMaybe of 
    Nothing -> pure inp 
    Just ctx ->  
      if B.length dat `mod` 16 /= 0 then 
        pError FromUser "Decrypt.decrypt" "Encrypted data size is not a multiple of 16"
      else do 
        let aeskey = makeObjKey ctx  
        dec <- case keylen ctx of 
                128 -> applyCipher (Y.cipherInit @Y.AES128 aeskey) hd dat 
                192 -> applyCipher (Y.cipherInit @Y.AES192 aeskey) hd dat 
                256 -> applyCipher (Y.cipherInit @Y.AES256 aeskey) hd dat 
                _   -> pError FromUser "Decrypt.decrypt" "Unsupported AES key length" 
        res <- stripPadding dec 
        pure $ newInput name res
  where 
    (hd, dat) = B.splitAt 16 $ inputBytes inp
    name = C.pack ("Decrypt" ++ show (inputOffset inp))


applyCipher :: (PdfParser m, BlockCipher a) => 
                   CryptoFailable a  
                -> B.ByteString 
                -> B.ByteString 
                -> m B.ByteString
applyCipher ciph hd dat = 
  case ciph of 
    Y.CryptoFailed err -> 
      pError FromUser "Decrypt.decrypt" (show err) 
    Y.CryptoPassed ci -> 
      case Y.makeIV hd of 
        Nothing -> 
          pError FromUser "Decrypt.decrypt" "Could not construct AES initial vector" 
        Just iv -> pure $ Y.cbcDecrypt ci iv dat  


makeObjKey :: EncContext 
           -> B.ByteString 
makeObjKey ctx = 
    B.take (keylen ctx + 5) (BA.convert digest)
  where 
    ob = B.take 3 $ B.reverse $ S.encode (robj ctx)
    gb = B.take 2 $ B.reverse $ S.encode (rgen ctx)
    salt = B.pack [0x73, 0x41, 0x6C, 0x54] -- magic string 
    digest = hashFinalize $ hashUpdates (Y.hashInit @Y.MD5) [key ctx, ob, gb, salt]


makeFileKey :: Int 
            -> B.ByteString 
            -> B.ByteString 
            -> Int
            -> B.ByteString 
            -> B.ByteString
makeFileKey len pwd opwd perm fileid = 
    iterate doHash firsthash !! 50
  where 
    pwdPadding :: B.ByteString  
    pwdPadding = B.pack -- magic string 
        [ 0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41
        , 0x64, 0x00, 0x4E, 0x56, 0xFF, 0xFA, 0x01, 0x08
        , 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68, 0x3E, 0x80
        , 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A ]

    padded = B.take 32 $ B.append pwd pwdPadding 
    pbytes = B.reverse $ S.encode (fromIntegral perm :: Word32) 

    doHash :: B.ByteString -> B.ByteString 
    doHash bs = B.take len $ BA.convert $ 
                  hashFinalize $ 
                    hashUpdate (Y.hashInit @Y.MD5) bs 

    firsthash = doHash $ B.concat [padded, opwd, pbytes, fileid]


stripPadding :: PdfParser m => B.ByteString -> m B.ByteString 
stripPadding input =  
    if B.length input < 32 then 
        pError FromUser "Decrypt.decrypt" "Bad encrypted data length"
    else   
    if padsize > 16 || not padWF then 
      pError FromUser "Decrypt.decrypt" "Bad padding in decrypted data. This is usually caused by an incorrect document password" 
    else pure res 
  where 
    padsize = fromIntegral (B.last input) 
    (res, pad) = B.splitAt (B.length input - padsize) input 

    padWF = and [ i == B.last input | i <- B.unpack pad ] 