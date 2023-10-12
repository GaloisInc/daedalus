module Daedalus.RTS.Iterator where

import qualified Daedalus.RTS.Vector   as Vec
import qualified Daedalus.RTS.Map      as Map
import qualified Daedalus.RTS.Numeric  as Num

newtype Iterator t = Iterator [(ITKey t, ITVal t)]

iteratorDone :: Iterator t -> Bool
iteratorDone (Iterator x) = null x
{-# INLINE iteratorDone #-}

iteratorUncons :: Iterator t -> (ITKey t, ITVal t, Iterator t)
iteratorUncons (Iterator xs) =
  case xs of
    (k,v) : more -> (k,v,Iterator more)
    _            -> error "Iterator finished"
{-# INLINE iteratorUncons #-}

iteratorKey :: Iterator t -> ITKey t
iteratorKey it = case iteratorUncons it of
                   (k,_,_) -> k
{-# INLINE iteratorKey #-}

iteratorVal :: Iterator t -> ITVal t
iteratorVal it = case iteratorUncons it of
                   (_,v,_) -> v
{-# INLINE iteratorVal #-}

iteratorNext :: Iterator t -> Iterator t
iteratorNext it = case iteratorUncons it of
                    (_,_,next) -> next
{-# INLINE iteratorNext #-}


class HasIterators t where
  type ITKey t
  type ITVal t

  newIterator :: t -> Iterator t

instance Vec.VecElem a => HasIterators (Vec.Vector a) where
  type ITKey (Vec.Vector a) = Num.UInt 64
  type ITVal (Vec.Vector a) = a

  newIterator v = Iterator (keys `zip` Vec.toList v)
    where keys = [ Num.UInt i | i <- [ 0 .. ] ]
  {-# INLINE newIterator #-}

instance HasIterators (Map.Map k v) where
  type ITKey (Map.Map k v) = k
  type ITVal (Map.Map k v) = v

  newIterator mp = Iterator (Map.toList mp)
  {-# INLINE newIterator #-}



