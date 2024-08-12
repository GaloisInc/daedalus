module Daedalus.RTS.Input
  ( Input
  , inputName
  , inputOffset
  , inputBytes
  , inputLength
  , inputTopBytes
  , inputByte
  , inputHead
  , inputEmpty
  , limitLen
  , inputTake
  , advanceBy
  , inputDrop
  , arrayStream
  , newInput
  , newInputFromFile
  ) where

import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short(ShortByteString,toShort)
import Data.Word(Word8)
import Control.Monad(guard)
import Numeric(showHex)

import Daedalus.RTS.HasInputs
import Daedalus.RTS.JSON
import Daedalus.RTS.Numeric(UInt,sizeToInt)
import Daedalus.RTS.Vector(Vector)
import qualified Daedalus.RTS.Vector as Vector

-- | This is the representation of a stream.
data Input = Input
  { inputOffset   :: {-# UNPACK #-} !Int
    -- ^ Index of next character in 'inputAllBytes'.
    -- This is the only thing that changes for most parser operations.

  , inputAllBytes :: {-# UNPACK #-} !ByteString
    {- ^ The whole input for *this* stream.
         This is a prefix of 'iiBytes' in 'inputInfo' -}

  , inputInfo     :: !InputInfo
    -- ^ Information about the stream we are processing.
  }

{- | Information about the input, used for error reporting. -}
data InputInfo = InputInfo
  { iiName   :: !ShortByteString  -- ^ A name for the input
  , iiBytes  :: !ByteString
    {- ^ Content of the input as it was when we started.
         This does not change when move around and restrict the stream.
         It is nice to keep the whole string around even for restricted
         streams, so that we can show context on parse error. -}
  }

instance Eq Input where
  x == y = inputOffset x == inputOffset y &&
           BS.length (inputAllBytes x) == BS.length (inputAllBytes y) &&
           iiName (inputInfo x) == iiName (inputInfo y)
  {-# INLINE (==) #-}

instance Ord Input where
  compare x y = compare (view x) (view y)
    where view i = ( inputOffset i
                   , BS.length (inputAllBytes x)
                   , iiName (inputInfo i)
                   )
  {-# INLINE compare #-}

instance Show Input where
  show i =
    "Stream { off  = " ++ show (inputOffset i) ++
           ", len  = " ++ show (inputLength i) ++
           ", name = " ++ show (iiName (inputInfo i)) ++
           "}"

-- | The name of the input
inputName :: Input -> ShortByteString
inputName = iiName . inputInfo
{-# INLINE inputName #-}

-- | Original bytes from the input
inputTopBytes :: Input -> ByteString
inputTopBytes = iiBytes . inputInfo
{-# INLINE inputTopBytes #-}

-- | How many bytes remain in the input.
inputLength :: Input -> Int
inputLength Input { .. } = BS.length inputAllBytes - inputOffset
{-# INLINE inputLength #-}


inputBytes :: Input -> ByteString
inputBytes Input { .. } = BS.drop inputOffset inputAllBytes
{-# INLINE inputBytes #-}

-- | Extract one byte from the input.
inputByte :: Input -> Maybe (Word8,Input)
inputByte Input { .. } =
  do guard (inputOffset < BS.length inputAllBytes)
     pure ( BS.index inputAllBytes inputOffset
          , Input { inputOffset = inputOffset + 1, .. }
          )
{-# INLINE inputByte #-}

inputHead :: Input -> Word8
inputHead Input { .. } = BS.index inputAllBytes inputOffset
{-# INLINE inputHead #-}

-- | Is this input empty.
inputEmpty :: Input -> Bool
inputEmpty Input { .. } = inputOffset >= BS.length inputAllBytes
{-# INLINE inputEmpty #-}

-- | Limit the input to the given number of bytes.
-- Fails if there aren't enough bytes.
limitLen :: UInt 64 -> Input -> Maybe Input
limitLen n' i =
  do let n = sizeToInt n'
     let newLen = inputOffset i + n
         bs     = BS.take newLen (inputAllBytes i)
     guard (0 <= n && newLen == BS.length bs)
     pure i { inputAllBytes = bs }
{-# INLINE limitLen #-}

inputTake :: UInt 64 -> Input -> Input
inputTake n i = i { inputAllBytes = BS.take newLen (inputAllBytes i) }
  where newLen = inputOffset i + sizeToInt n
{-# INLINE inputTake #-}


-- | Advance the input by the given number of bytes.
-- Fails if we don't have enough bytes, although it is ok to
-- get to the very end of the input.
advanceBy :: UInt 64 -> Input -> Maybe Input
advanceBy n' i =
  do let n = sizeToInt n'
     guard (0 <= n && n <= inputLength i)
     pure i { inputOffset = inputOffset i + n }
{-# INLINE advanceBy #-}

inputDrop :: UInt 64 -> Input -> Input
inputDrop n i = i { inputOffset = inputOffset i + sizeToInt n }
{-# INLINE inputDrop #-}


-- | Make an input using the given name and bytes.
arrayStream :: Vector (UInt 8) -> Vector (UInt 8) -> Input
arrayStream name v = newInput (Vector.vecToRep name) (Vector.vecToRep v)
{-# INLINE arrayStream #-}


-- | Make an input using the given name and bytes.
newInput :: ByteString -> ByteString -> Input
newInput name bs =
  Input { inputOffset   = 0
        , inputAllBytes = bs
        , inputInfo     = InputInfo { iiName = toShort name, iiBytes = bs }
        }
{-# INLINE newInput #-}


-- | Either make an empty input, or get some bytes from a file
newInputFromFile :: Maybe FilePath -> IO Input
newInputFromFile mb =
  case mb of
    Nothing -> pure (newInput "(no input)" "")
    Just file ->
      do bs <- BS.readFile file
         pure (newInput (BS8.pack file) bs)

instance HasInputs Input where
  getInputs i = Map.singleton (inputName i) (inputTopBytes i)
  {-# INLINE getInputs #-}

instance ToJSON Input where
  toJSON inp =
    jsTagged "$$input" (jsText (SBS.fromShort (inputName inp) <> ":" <> rng))
      where
      rng = BS8.pack
          $ "0x" ++ showHex (inputOffset inp) (
          "--0x" ++ showHex (inputOffset inp + inputLength inp) "")


