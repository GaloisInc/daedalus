{-# Language OverloadedStrings #-}
module Bin where

import Data.Int
import Data.Word
import Data.Binary.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


calcExp :: Int -> Calc
calcExp n
  | n <= 0 = Calc { ops  = [ Op "in  " 0 0, Op "out " 0 0 ]
                  , subs = []
                  }
  | otherwise =
    Calc { ops  = [ Op "in  " 0 0, Op "calc" 0 0, Op "calc" 0 0, Op "out " 0 0 ]
         , subs = [ calcExp (n-1) ]
         }

--------------------------------------------------------------------------------

writeExample :: FilePath -> Calc -> IO ()
writeExample file calc = LBS.writeFile file (makeExample calc)

makeExample :: Calc -> LBS.ByteString
makeExample c = example
  where
  example = toLazyByteString
          $ fromLazyByteString pref <>
            positioned (LBS.length pref) [ toLazyByteString (putMPET c) ]


  len    = 128 + 3 * 4 + LBS.length body

  body   = toLazyByteString (putMPET c)

  pref = toLazyByteString
       $ mconcat
           [ makeHeader len
           , w32 (1 :: Int)
           , fromByteString "A2M0"
           ]

makeHeader :: Int64 -> Builder
makeHeader len = fromLazyByteString pref <>
                fromByteString (BS.replicate pad 0)
  where
  pad = 128 - fromIntegral (LBS.length pref)

  pref =
    toLazyByteString $
    mconcat
      [ w32 len
      , w32 (0 :: Int)
      , singleton 5
      , singleton 0
      , w16 (0 :: Int)
      , fromByteString "scnr"
      , w32 (0 :: Int)
      , w32 (0 :: Int)

      , w32 (0 :: Int)
      , w32 (0 :: Int)
      , w32 (0 :: Int)
      , fromByteString "acsp"
      ]




putMPET :: Calc -> Builder
putMPET calc =
  fromLazyByteString pref
  <>
  positioned (LBS.length pref) [ toLazyByteString (putCalc calc) ]
  where
  pref =
    toLazyByteString $
    mconcat
      [ fromByteString "mpet"
      , w32 (0 :: Int)
      , w16 (1 :: Int)
      , w16 (1 :: Int)
      , w32 (1 :: Int)
      ]



data Calc = Calc
  { ops  :: [Op]
  , subs :: [Calc]
  }

data Op = Op BS.ByteString Word16 Word16

putOp :: Op -> Builder
putOp (Op tag s t) = mconcat [ fromByteString tag, w16 s, w16 t ]


putCalc :: Calc -> Builder
putCalc calc = fromLazyByteString pref <>
                positioned (LBS.length pref)
                    (mainBody : map (toLazyByteString . putCalc) (subs calc))
  where
  pref      = toLazyByteString
            $ mconcat [ fromByteString "calc"
                      , w32 (0 :: Int)
                      , w16 (1 :: Int) -- ins
                      , w16 (1 :: Int) -- ins
                      , w32 (length (subs calc))
                      ]

  mainBody  =
     toLazyByteString $
     mconcat $ fromByteString "func"
             : w32 (0 :: Int)
             : w32 (length (ops calc))
             : map putOp (ops calc)

w16 :: Integral a => a -> Builder
w16 x = putWord16be (fromIntegral x)

w32 :: Integral a => a -> Builder
w32 x = putWord32be (fromIntegral x)


positioned :: Int64 -> [LBS.ByteString] -> Builder
positioned off0 todo0 =
  mconcat [ w32 off <> w32 sz | ((off,sz),_) <- locs ]
  <>
  mconcat (map snd locs)
  where
  locs = go (off0 + 8 * fromIntegral (length todo0)) todo0

  go off todo =
    case todo of
      []     -> []
      x : xs -> ( (off, n), fromLazyByteString x ) : go (off + n) xs
        where n = LBS.length x



