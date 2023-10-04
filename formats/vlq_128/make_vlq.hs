#!/usr/bin/env cabal
{-# Language BlockArguments #-}
{- cabal:
build-depends: base, tf-random
-}

import Data.Word
import Data.Bits
import Data.List
import System.Random.TF
import System.Random.TF.Instances
import System.IO

main :: IO ()
main =
  do let rep = 1000000
     g <- newTFGen
     _ <- withBinaryFile "data.dat" WriteMode \h -> nums h rep g
     pure ()

nums :: Handle -> Int -> TFGen -> IO TFGen
nums h n g
  | n > 0 = nums h (n-1) =<< num h g
  | otherwise = pure g

num :: Handle -> TFGen -> IO TFGen
num h g =
  do let (n,g1) = randomR (1,8) g
     chunks h n g1

chunks :: Handle -> Int -> TFGen -> IO TFGen
chunks h n g
  | n == 0    = pure g
  | n == 1    = chunk h True g
  | otherwise = chunks h (n-1) =<< chunk h False g

chunk :: Handle -> Bool -> TFGen -> IO TFGen
chunk h is_last g =
  do let rng = if is_last then (0,127) else (128,255)
     let (a,g1) = randomR rng g
     byte h a
     pure g1

byte :: Handle -> Word8 -> IO ()
byte h b = hPutChar h (toEnum (fromEnum b))



