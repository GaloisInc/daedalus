#!/usr/bin/env cabal
{-# Language BlockArguments #-}
{- cabal:
build-depends: base, text, bytestring, tf-random, monadLib
-}

import Debug.Trace

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString.Builder
import System.Random.TF
import System.Random.TF.Instances
import MonadLib

main :: IO ()
main =
  do g <- newTFGen
     let size = 100 * 1024 * 1024
     let ((_,bytes),_) = runGen g size (gSizedDocument size)
     LBS.writeFile "data.dat" bytes


type Gen = StateT RW Id

data RW = RW
  { gen        :: !TFGen
  , size       :: !Int
  , targetSize :: !Int
  , out        :: !Builder
  }

runGen :: TFGen -> Int -> Gen () -> ((Int, LBS.ByteString), TFGen)
runGen g t m = ((size rw, toLazyByteString (out rw)), gen rw)
  where
  rw0 = RW { gen = g, size = 0, targetSize = t, out = mempty }
  (_,rw)  = runM m rw0

getNeed :: Gen Int
getNeed =
  do rw <- get
     pure $! max 0 (targetSize rw - size rw)

getSize :: Gen Int
getSize = size <$> get

doNested :: Int -> Gen () -> Gen (Int,LBS.ByteString)
doNested lim g =
  do rw <- get
     let (res,g1) = runGen (gen rw) lim g
     set rw { gen = g1 }
     pure res

gIntR :: (Int,Int) -> Gen Int
gIntR bounds =
  sets \rw -> case randomR bounds (gen rw) of
                (n,g1) -> (n, rw { gen = g1 })

emit :: Int -> Builder -> Gen ()
emit n f = sets_ \rw -> rw { out = out rw <> f, size = size rw + n }

encUtf8 :: [Int] -> ByteString
encUtf8 = Text.encodeUtf8 . Text.pack .map toEnum

gString :: Gen ()
gString =
  do len <- min 5 <$> getNeed
     bs  <- encUtf8 <$> replicateM len (gIntR (1,2000))
     emit (BS.length bs) $ byteString bs
     emit 1   $ word8 0

gBisonString :: Gen ()
gBisonString =
  do len <- min 24 <$> getNeed
     bs  <- encUtf8 <$> replicateM len (gIntR (1,255))
     let n = BS.length bs
     emit 4   $ word32LE (fromIntegral n + 1)
     emit n   $ byteString bs
     emit 1   $ word8 0

gDouble :: Gen ()
gDouble =
  emit 8 $ doubleLE 42

gElement :: Gen ()
gElement =
  do perc <- gIntR (1,1000)
     let (tag,gen) = case perc of
                       _ | perc < 10 -> (3,gDocument)
                         | perc < 60 -> (1,gDouble)
                         | otherwise -> (2,gBisonString)
     emit 1 $ word8 tag
     gString
     gen

gSizedDocument :: Int -> Gen ()
gSizedDocument docSize =
  do len     <- min docSize <$> getNeed
     (actual,bytes) <- doNested len loop
     emit 4       $ word32LE (fromIntegral actual + 5)
     emit actual  $ lazyByteString bytes
     emit 1       $ word8 0
  where
  loop =
    do need <- getNeed
       when (need > 0)
         do gElement
            loop

gDocument :: Gen ()
gDocument = gSizedDocument =<< gIntR (10,500)
