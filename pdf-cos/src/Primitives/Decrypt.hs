{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module Primitives.Decrypt (decrypt,makeFileKey) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteArray as BA 
import Data.Word 

import Crypto.Cipher.AES as Y 
import Crypto.Cipher.RC4 as Y 
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
    Just ctx -> do 
      dec <- case (ciph ctx) of 
              V4AES -> applyCipherAES ctx inp 
              V4RC4 -> applyCipherRC4 ctx inp 
              V2RC4 -> applyCipherRC4 ctx inp 
      pure $ newInput name dec 
  where 
    name = C.pack ("Decrypt" ++ show (inputOffset inp))

applyCipherAES :: PdfParser m => 
                  EncContext 
               -> Input 
               -> m B.ByteString
applyCipherAES ctx inp = 
  if B.length (inputBytes inp) `mod` 16 /= 0 -- XXX: maybe try to recover by padding out with zeroes? 
  then pError FromUser "Decrypt.decrypt" $ 
        ("Length of encrypted data must be a multiple of block size (16). Actual length: "
          ++ (show $ B.length (inputBytes inp))) 
  else case ciph of 
    Y.CryptoFailed err -> 
      pError FromUser "Decrypt.decrypt" (show err) 
    Y.CryptoPassed ci -> 
      case Y.makeIV hd of 
        Nothing -> 
          pError FromUser "Decrypt.decrypt" "Could not construct AES initial vector" 
        Just iv -> stripPadding $ Y.cbcDecrypt ci iv dat 
  where 
    objKey = makeObjKey128 ctx True 
    ciph = Y.cipherInit @Y.AES128 objKey
    (hd, dat) = B.splitAt 16 $ inputBytes inp

applyCipherRC4 :: PdfParser m => 
                  EncContext 
               -> Input 
               -> m B.ByteString
applyCipherRC4 ctx inp = 
    let i = Y.initialize $ makeObjKey128 ctx False 
    in pure $ snd $ Y.combine i $ inputBytes inp 

makeObjKey128 :: EncContext
           -> Bool 
           -> B.ByteString 
makeObjKey128 ctx isAES = 
    B.take (128 + 5) (BA.convert digest)
  where 
    ob = B.take 3 $ B.reverse $ S.encode (robj ctx)
    gb = B.take 2 $ B.reverse $ S.encode (rgen ctx)
    salt = 
      case isAES of 
        True  -> B.pack [0x73, 0x41, 0x6C, 0x54] -- magic string 
        False -> B.empty 
    digest = hashFinalize $ hashUpdates (Y.hashInit @Y.MD5) [key ctx, ob, gb, salt]

-- XXX: maybe should live somewhere else? 
makeFileKey ::
               B.ByteString 
            -> B.ByteString 
            -> Int
            -> B.ByteString 
            -> B.ByteString
makeFileKey pwd opwd perm fileid = 
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
    doHash bs = BA.convert $ 
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