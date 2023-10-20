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
     let ((_,bytes),_) = runGen g size (gSizedNode size)
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

byte :: Char -> Gen ()
byte c = emit 1 (word8 (toEnum (fromEnum c)))

gLeaf :: Gen ()
gLeaf =
  do len <- max 1 . min 15 <$> getNeed
     bs  <- replicateM len (gIntR (fromEnum 'a',fromEnum 'z'))
     emit len (byteString (BS.pack (map toEnum bs)))

gNode :: Gen ()
gNode =
  do n <- getNeed
     if n < 10
       then gLeaf
       else do perc <- gIntR (1,100)
               if perc < 35
                  then gSizedNode =<< gIntR (10,70)
                  else gLeaf

gSizedNode :: Int -> Gen ()
gSizedNode docSize =
  do len <- min docSize <$> getNeed
     (actual,bytes) <- doNested len (loop True)
     byte '('
     emit actual  $ lazyByteString bytes
     byte ')'
  where
    loop start
      | start = gNode >> loop False
      | otherwise =
        do need <- getNeed
           when (need > 0)
             do perc <- gIntR (1,100)
                byte (if perc < 80 then ' ' else '\n')
                gNode
                loop False

