module Main where

import Data.Attoparsec.ByteString
import Control.Monad
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString as BS
import Control.Applicative

main :: IO ()
main =
  do bytes <- BS.readFile "../data.dat"
     case parseOnly parseSum bytes of
       Right a -> print a
       Left err -> print err

data Loop = Loop { done :: !Bool, bits :: !Int, value :: !Word64 }

parseLoop :: Parser Loop
parseLoop =
  do (_, newLoop) <- runScanner start step
     guard (done newLoop)
     pure newLoop
  where
  start = Loop { done = False, bits = 0, value = 0 }
  step loop b
    | done loop = Nothing
    | otherwise =
      Just Loop { done   = b < 128
                , bits   = bits loop + 7
                , value  = (fromIntegral (b .&. 0x7F) `shiftL` bits loop)
                        .|. value loop
                }

parseLoop' :: Parser Loop
parseLoop' =
  do l <- loop start
     guard (done l)
     pure l
  where
  loop l =
    do b <- anyWord8
       case step l b of
         Just l1 -> loop l1
         Nothing -> pure l
    <|> pure l

  start = Loop { done = False, bits = 0, value = 0 }
  step loop b
    | done loop = Nothing
    | otherwise =
      Just Loop { done   = b < 128
                , bits   = bits loop + 7
                , value  = (fromIntegral (b .&. 0x7F) `shiftL` bits loop)
                        .|. value loop
                }



parseU64 :: Parser Word64
parseU64 = value <$> parseLoop'

parseS64 :: Parser Int64
parseS64 =
  do w <- parseLoop
     let sign = 1 `shiftL` (bits w - 1)
     pure (fromIntegral ((value w `xor` sign) - sign))

parseSum :: Parser Word64
parseSum =
  do x <- sum <$> many' parseU64
     guard =<< atEnd
     pure x


