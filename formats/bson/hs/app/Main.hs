module Main where

import Data.Word
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Bson
import Data.Bson.Binary

main :: IO ()
main =
  do bytes <- LBS.readFile "../data.dat"
     let doc = runGet getDocument bytes
     print (docSize doc)

docSize :: Document -> Word64
docSize = sum . map (valueSize . value)

valueSize :: Value -> Word64
valueSize v =
  case v of
    String txt -> fromIntegral (BS.length (Text.encodeUtf8 txt))
    Doc d -> docSize d
    _ -> 1
