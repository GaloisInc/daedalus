{-# Language DataKinds #-}
{-# Language TypeFamilies, UndecidableInstances #-}
{-# Language FlexibleContexts, FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language ConstraintKinds #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
module RTS.Vector
  ( RTS.Vector.Vector
  , RTS.Vector.VecElem

  , RTS.Vector.length

  , RTS.Vector.empty
  , RTS.Vector.fromList
  , RTS.Vector.unfoldrM
  , RTS.Vector.replicateM
  , RTS.Vector.concat

  , RTS.Vector.VecOf
  , RTS.Vector.vecToRep
  , RTS.Vector.vecFromRep
  , RTS.Vector.vecToString

  , RTS.Vector.imapM_
  , RTS.Vector.toList

  , (RTS.Vector.!?)
  , RTS.Vector.rangeUp
  , RTS.Vector.rangeDown

  , Builder
  , emptyBuilder
  , pushBack
  , pushBackVector
  , pushBackBuilder
  , finishBuilder
  ) where

import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Unsafe as BS (unsafeIndex)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Unboxed as U
import qualified VectorBuilder.Builder as VB
import qualified VectorBuilder.Vector  as VB
import Data.Word
import Data.Int
import Data.Coerce(coerce)
import Data.List(unfoldr)

import RTS.JSON
import RTS.Numeric
import RTS.Base


vecToRep :: Vector a -> VecOf a
vecToRep (Vec xs) = xs
{-# INLINE vecToRep #-}

vecFromRep :: VecOf a -> Vector a
vecFromRep = Vec
{-# INLINE vecFromRep #-}

vecToString :: Vector (UInt 8) -> String
vecToString = BS8.unpack . vecToRep
{-# INLINE vecToString #-}

--------------------------------------------------------------------------------

-- Representation of values when stored in a vector.
type family VecElT a where
  VecElT (UInt n) = UIntRep n
  VecElT (SInt n) = SIntRep n
  VecElT a        = a

-- Vector implementation to use for storing various representation of values.
type family VecT a where
  VecT Word8  = ByteString
  VecT Word16 = U.Vector Word16
  VecT Word32 = U.Vector Word32
  VecT Word64 = U.Vector Word64

  VecT Int8   = U.Vector Int8
  VecT Int16  = U.Vector Int16
  VecT Int32  = U.Vector Int32
  VecT Int64  = U.Vector Int64

  VecT Bool   = U.Vector Bool

  VecT a      = V.Vector a


type VecOf a = VecT (VecElT a)

newtype Vector a = Vec (VecOf a)

type ValidV a = ( VecImpl (VecOf a)
                , ElemOf (VecOf a) ~ VecElT a
                , Monoid (BuilderT (VecOf a))
                , Show (VecOf a)
                , Ord (VecOf a)
                )

class ValidV a => VecElem a where
  storing     :: a -> VecElT a
  storingList :: [a] -> [VecElT a]
  reading     :: VecElT a -> a
  readingList :: [VecElT a] -> [a]

instance {-# OVERLAPPING #-} ValidV (UInt n) => VecElem (UInt n) where
  storing = coerce
  storingList = coerce
  reading = coerce
  readingList = coerce
  {-# INLINE storing #-}
  {-# INLINE storingList #-}
  {-# INLINE reading #-}
  {-# INLINE readingList #-}

instance {-# OVERLAPPING #-} ValidV (SInt n) => VecElem (SInt n) where
  storing = coerce
  storingList = coerce
  reading = coerce
  readingList = coerce
  {-# INLINE storing #-}
  {-# INLINE storingList #-}
  {-# INLINE reading #-}
  {-# INLINE readingList #-}

instance (a ~ VecElT a, ValidV a) => VecElem a where
  storing = id
  storingList = id
  reading = id
  readingList = id
  {-# INLINE storing #-}
  {-# INLINE storingList #-}
  {-# INLINE reading #-}
  {-# INLINE readingList #-}


instance VecElem a => Show (Vector a) where
  showsPrec p (Vec xs) = showsPrec p xs

deriving instance VecElem a => Eq (Vector a)
deriving instance VecElem a => Ord (Vector a)

empty :: VecElem a => Vector a
empty = Vec vEmpty

length :: VecElem a => Vector a -> UInt 64
length (Vec x) = intToSize (vLen x)

fromList :: VecElem a => [a] -> Vector a
fromList xs = Vec (vFromList (storingList xs))

unfoldrM :: (VecElem a, Monad m) => (s -> m (Maybe (a,s))) -> s -> m (Vector a)
unfoldrM step s = Vec <$> vUnfoldrM step' s
  where step' s' = do mb <- step s'
                      pure (upd <$> mb)
        upd (a,s1) = (storing a, s1)


replicateM :: (VecElem a, Monad m) => UInt 64 -> m a -> m (Vector a)
replicateM n m = Vec <$> vReplicateM (sizeToInt n) (storing <$> m)

concat :: VecElem a => Vector (Vector a) -> Vector a
concat (Vec xs) = Vec (vConcat (coerce (vToList xs)))

imapM_ :: (VecElem a, Monad m) => (Int -> a -> m b) -> Vector a -> m ()
imapM_ f (Vec xs) = vImapM_ f' xs
  where f' i a = f i (reading a)

toList :: (VecElem a) => Vector a -> [a]
toList (Vec xs) = readingList (vToList xs)

(!?) :: VecElem a => Vector a -> UInt 64 -> Maybe a
Vec xs !? i = reading <$> vLookup xs (sizeToInt i)

rangeUp :: (VecElem a, Ord a, Numeric a) => a -> a -> a -> Vector a
rangeUp start stop step
  | step <= lit 0 = error "Non-positive step in `rangeUp`"
  | start >= stop = empty
  | otherwise = fromList (start : unfoldr next start)
    where next cur = if (stop `sub` cur) > step
                        then let x = cur `add` step
                             in x `seq` Just (x,x)
                        else Nothing

rangeDown :: (VecElem a, Ord a, Numeric a) => a -> a -> a -> Vector a
rangeDown start stop step
  | step <= lit 0 = error "Non-positive step in `rangeDown`"
  | start <= stop = empty
  | otherwise = fromList (start : unfoldr next start)
    where next cur = if (cur `sub` stop) > step
                        then let x = cur `sub` step
                             in x `seq` Just (x,x)
                        else Nothing



{-# INLINE empty #-}
{-# INLINE length #-}
{-# INLINE fromList #-}
{-# INLINE concat #-}
{-# INLINE unfoldrM #-}
{-# INLINE replicateM #-}
{-# INLINE imapM_ #-}
{-# INLINE toList #-}
{-# INLINE (!?) #-}
{-# INLINE rangeUp #-}
{-# INLINE rangeDown #-}



--------------------------------------------------------------------------------


class VecImpl t where
  type ElemOf t
  vEmpty      :: t
  vLen        :: t -> Int
  vFromList   :: [ElemOf t] -> t
  vUnfoldrM   :: Monad m => (s -> m (Maybe (ElemOf t,s))) -> s -> m t
  vReplicateM :: Monad m => Int -> m (ElemOf t) -> m t
  vConcat     :: [t] -> t
  vImapM_     :: Monad m => (Int -> ElemOf t -> m a) -> t -> m ()
  vFoldl'     :: (s -> ElemOf t -> s) -> s -> t -> s
  vIFoldl'    :: (s -> Int -> ElemOf t -> s) -> s -> t -> s
  vFoldM'     :: Monad m => (s -> ElemOf t -> m s) -> s -> t -> m s
  vIFoldM'    :: Monad m => (s -> Int -> ElemOf t -> m s) -> s -> t -> m s
  vToList     :: t -> [ElemOf t]
  vLookup     :: t -> Int -> Maybe (ElemOf t)

  vVecBuilder :: t -> BuilderT t
  vOneBuilder :: f t -> ElemOf t -> BuilderT t
  vBuild      :: BuilderT t -> t

instance (U.Unbox a) => VecImpl (U.Vector a) where
  type ElemOf (U.Vector a) = a
  vEmpty        = U.empty
  vLen          = U.length
  vFromList     = U.fromList
  vConcat       = U.concat
  vFoldl'       = U.foldl'
  vIFoldl'      = U.ifoldl'
  vIFoldM'      = U.ifoldM'
  vUnfoldrM     = U.unfoldrM
  vReplicateM   = U.replicateM
  vImapM_       = U.imapM_
  vFoldM'       = U.foldM'
  vToList       = U.toList
  vLookup       = (U.!?)

  vVecBuilder   = VB.vector
  vOneBuilder   = \_ -> VB.singleton
  vBuild        = VB.build

  {-# INLINE vEmpty #-}
  {-# INLINE vLen #-}
  {-# INLINE vFromList #-}
  {-# INLINE vConcat #-}
  {-# INLINE vFoldl' #-}
  {-# INLINE vUnfoldrM #-}
  {-# INLINE vReplicateM #-}
  {-# INLINE vImapM_ #-}
  {-# INLINE vFoldM' #-}
  {-# INLINE vIFoldM' #-}
  {-# INLINE vToList #-}
  {-# INLINE vLookup #-}
  {-# INLINE vVecBuilder #-}
  {-# INLINE vOneBuilder #-}
  {-# INLINE vBuild #-}



instance VecImpl (V.Vector a) where
  type ElemOf (V.Vector a) = a
  vEmpty        = V.empty
  vLen          = V.length
  vFromList     = V.fromList
  vConcat       = V.concat
  vFoldl'       = V.foldl'
  vIFoldl'      = V.ifoldl'
  vIFoldM'      = V.ifoldM'
  vUnfoldrM     = V.unfoldrM
  vReplicateM   = V.replicateM
  vImapM_       = V.imapM_
  vFoldM'       = V.foldM'
  vToList       = V.toList
  vLookup       = (V.!?)

  vVecBuilder   = VB.vector
  vOneBuilder   = \_ -> VB.singleton
  vBuild        = VB.build

  {-# INLINE vEmpty #-}
  {-# INLINE vLen #-}
  {-# INLINE vFromList #-}
  {-# INLINE vConcat #-}
  {-# INLINE vFoldl' #-}
  {-# INLINE vUnfoldrM #-}
  {-# INLINE vReplicateM #-}
  {-# INLINE vImapM_ #-}
  {-# INLINE vFoldM' #-}
  {-# INLINE vToList #-}
  {-# INLINE vIFoldM' #-}
  {-# INLINE vLookup #-}

  {-# INLINE vVecBuilder #-}
  {-# INLINE vOneBuilder #-}
  {-# INLINE vBuild #-}


instance VecImpl ByteString where
  type ElemOf ByteString = Word8
  vEmpty    = BS.empty
  vLen      = BS.length
  vFromList = BS.pack
  vConcat   = BS.concat
  vFoldl'   = BS.foldl'
  vToList   = BS.unpack

  vUnfoldrM   = bsUnfoldrM
  vReplicateM = bsReplicateM
  vImapM_     = bsImapM_
  vFoldM'     = bsFoldM'
  vIFoldl'    = bsIFoldl'
  vIFoldM'    = bsIFoldM'
  vLookup     = bsLookup

  vVecBuilder = BS.byteString
  vOneBuilder = \_ -> BS.word8
  vBuild      = LBS.toStrict . BS.toLazyByteString

  {-# INLINE vEmpty #-}
  {-# INLINE vLen #-}
  {-# INLINE vFromList #-}
  {-# INLINE vConcat #-}
  {-# INLINE vFoldl' #-}
  {-# INLINE vUnfoldrM #-}
  {-# INLINE vReplicateM #-}
  {-# INLINE vImapM_ #-}
  {-# INLINE vFoldM' #-}
  {-# INLINE vToList #-}
  {-# INLINE vIFoldl'  #-}
  {-# INLINE vIFoldM'  #-}
  {-# INLINE vLookup #-}

  {-# INLINE vVecBuilder #-}
  {-# INLINE vOneBuilder #-}
  {-# INLINE vBuild #-}

doBuild :: BS.Builder -> ByteString
doBuild = LBS.toStrict . BS.toLazyByteString
{-# INLINE doBuild #-}

bsUnfoldrM :: Monad m => (s -> m (Maybe (Word8, s))) -> s -> m ByteString
bsUnfoldrM step = go mempty
  where
  go done s = do mb <- step s
                 case mb of
                   Nothing -> pure (doBuild done)
                   Just (w,s1) -> go (done <> BS.word8 w) s1
{-# INLINE bsUnfoldrM #-}

bsReplicateM :: Monad m => Int -> m Word8 -> m ByteString
bsReplicateM n0 step = go mempty n0
  where
  go done n | n > 0 = do w <- step
                         go (done <> BS.word8 w) (n-1)
            | otherwise = pure (doBuild done)
{-# INLINE bsReplicateM #-}

bsImapM_ :: Monad m => (Int -> Word8 -> m b) -> ByteString -> m ()
bsImapM_ step bs = go 0
  where
  done = BS.length bs
  go i | i < done  = step i (BS.unsafeIndex bs i) >> go (i+1)
       | otherwise = pure ()
{-# INLINE bsImapM_ #-}

bsFoldM' :: Monad m => (s -> Word8 -> m s) -> s -> ByteString -> m s
bsFoldM' step s0 bs = go 0 s0
  where
  done = BS.length bs
  go i s | i < done  = go (i+1) =<< step s (BS.unsafeIndex bs i)
         | otherwise = pure s
{-# INLINE bsFoldM' #-}

bsIFoldl' :: (s -> Int -> Word8 -> s) -> s -> ByteString -> s
bsIFoldl' step s0 bs = go 0 s0
  where
  done = BS.length bs
  go i s | i < done  = go (i+1) $! step s i (BS.unsafeIndex bs i)
         | otherwise = s
{-# INLINE bsIFoldl' #-}

bsIFoldM' :: Monad m => (s -> Int -> Word8 -> m s) -> s -> ByteString -> m s
bsIFoldM' step s0 bs = go 0 s0
  where
  done = BS.length bs
  go i s | i < done  = go (i+1) =<< step s i (BS.unsafeIndex bs i)
         | otherwise = pure s
{-# INLINE bsIFoldM' #-}

bsLookup :: ByteString -> Int -> Maybe Word8
bsLookup bs i
  | 0 <= i && i < BS.length bs = Just $! BS.unsafeIndex bs i
  | otherwise = Nothing
{-# INLINE bsLookup #-}


type instance ElType (Vector a) = a
type instance KeyType (Vector a) = UInt 64

instance VecElem a => IsLoop (Vector a) where

  loopFold f s (Vec xs) = vFoldl' f' s xs
    where f' s' a = f s' (reading a)

  loopIFold f s (Vec xs) = vIFoldl' f' s xs
    where f' s' i a = f s' (intToSize i) (reading a)

  loopFoldM f s (Vec xs) = vFoldM' f' s xs
    where f' s' a = f s' (reading a)

  loopIFoldM f s (Vec xs) = vIFoldM' f' s xs
    where f' s' i a = f s' (intToSize i) (reading a)

  {-# INLINE loopFold   #-}
  {-# INLINE loopIFold  #-}
  {-# INLINE loopFoldM  #-}
  {-# INLINE loopIFoldM #-}


-- XXX: We can add special cases for when the representation does not change.
instance (VecElem a, VecElem b) => IsMapLoop (Vector a) (Vector b) where

  loopMap f (Vec xs) = Vec
                     $ vFromList
                     $ map (storing . f . reading)
                     $ vToList xs

  loopIMap f (Vec xs) = Vec
                      $ vFromList
                      $ map (\(i,a) -> storing (f i (reading a)))
                      $ zip (map UInt [ 0 .. ])
                      $ vToList xs

  loopMapM f (Vec xs) = fmap (Vec . vFromList)
                      $ traverse (fmap storing . f . reading)
                      $ vToList xs

  loopIMapM f (Vec xs) = fmap (Vec . vFromList)
                       $ traverse (\(i,a) -> fmap storing (f i (reading a)))
                       $ zip (map UInt [ 0 .. ])
                       $ vToList xs
  {-# INLINE loopMapM #-}
  {-# INLINE loopIMapM #-}
  {-# INLINE loopMap #-}
  {-# INLINE loopIMap #-}

instance (VecElem a, ToJSON a) => ToJSON (Vector a) where
  toJSON = jsArray . map toJSON . toList



--------------------------------------------------------------------------------

type family BuilderT container where
  BuilderT ByteString   = BS.Builder
  BuilderT (U.Vector a) = VB.Builder a
  BuilderT (V.Vector a) = VB.Builder a

newtype Builder a = Builder (BuilderT (VecOf a))

emptyBuilder :: VecElem a => Builder a
emptyBuilder = Builder mempty

finishBuilder :: VecElem a => Builder a -> Vector a
finishBuilder (Builder b) = Vec (vBuild b)

pushBackBuilder :: VecElem a => Builder a -> Builder a -> Builder a
pushBackBuilder (Builder x) (Builder y) = Builder (x <> y)

pushBack :: forall a. VecElem a => Builder a -> a -> Builder a
pushBack (Builder x) a = Builder (x <> vOneBuilder sig (storing a))
  where sig = Nothing :: Maybe (VecOf a)

pushBackVector :: VecElem a => Builder a -> Vector a -> Builder a
pushBackVector (Builder x) (Vec y) = Builder (x <> vVecBuilder y)

instance VecElem a => Eq (Builder a) where
  x == y = finishBuilder x == finishBuilder y

instance VecElem a => Ord (Builder a) where
  compare x y = compare (finishBuilder x) (finishBuilder y)

instance VecElem a => Show (Builder a) where
  showsPrec n b = showsPrec n (show (finishBuilder b))

instance (VecElem a, ToJSON a) => ToJSON (Builder a) where
  toJSON = toJSON . finishBuilder

