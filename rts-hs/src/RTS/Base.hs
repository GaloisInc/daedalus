{-# Language DataKinds, KindSignatures, ConstraintKinds, FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeFamilies #-}
{-# Language ScopedTypeVariables #-}
module RTS.Base where

import GHC.TypeLits(Symbol)
import Control.Monad(foldM)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Coerce(coerce)
import Data.Functor.Identity(Identity(..))

import GHC.Records(HasField)

type HasStruct s (l::Symbol) a = HasField l s a
type HasUnion  s (l::Symbol) a = HasField l s (Maybe a)

class IsLoop t where
  loopFold    ::            (s ->              ElType t ->   s) -> s -> t ->   s
  loopFoldM   :: Monad m => (s ->              ElType t -> m s) -> s -> t -> m s
  loopIFold   ::            (s -> KeyType t -> ElType t ->   s) -> s -> t ->   s
  loopIFoldM  :: Monad m => (s -> KeyType t -> ElType t -> m s) -> s -> t -> m s

type family ElType t
type family KeyType t

type ColElType c e  = ElType c ~ e
type ColKeyType c e = KeyType c ~ e

type instance ElType Integer = Integer
type instance KeyType Integer = Integer

type instance ElType (Map k v) = v
type instance KeyType (Map k v) = k



instance IsLoop (Map k v) where
  loopFold        = Map.foldl'
  loopIFold       = Map.foldlWithKey'
  loopFoldM f s   = foldM f s . Map.elems
  loopIFoldM f s0 = foldM f' s0. Map.toList
    where f' s (k,v) = f s k v

  {-# INLINE loopFold   #-}
  {-# INLINE loopIFold  #-}
  {-# INLINE loopFoldM  #-}
  {-# INLINE loopIFoldM #-}



-- Basically "Traversable"
class IsMapLoop s t where
  loopMap   ::            (             ElType s -> ElType t) -> s -> t
  loopIMap  ::            (KeyType s -> ElType s -> ElType t) -> s -> t
  loopMapM  :: Monad m => (             ElType s -> m (ElType t)) -> s -> m t
  loopIMapM :: Monad m => (KeyType s -> ElType s -> m (ElType t)) -> s -> m t


instance (k ~ k1) => IsMapLoop (Map k a) (Map k1 b) where
  loopMap   = implMap
  loopIMap  = implIMap
  loopMapM  = traverse
  loopIMapM = Map.traverseWithKey
  {-# INLINE loopMapM #-}
  {-# INLINE loopIMapM #-}
  {-# INLINE loopMap #-}
  {-# INLINE loopIMap #-}


implMap :: forall k a b. (a -> b) -> Map k a -> Map k b
implMap = coerce (loopMapM :: (a -> Identity b) ->
                              Map k a -> Identity (Map k b))
{-# INLINE implMap #-}

implIMap :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
implIMap = coerce (loopIMapM :: (k -> a -> Identity b) ->
                                           Map k a -> Identity (Map k b))
{-# INLINE implIMap #-}


