{-# Language GeneralizedNewtypeDeriving #-}
module Daedalus.GUID (GUID
                     , invalidGUID
                     , firstValidGUID
                     -- , succGUID
                     , getNextGUID
                     , mkGUIDState
                     , mkGUIDState'
                     , HasGUID(..)
                     , FreshM, runFresh, runFreshIn
                     ) where

import MonadLib
import Daedalus.PP

newtype GUID = GUID { getGUID :: Int }
  deriving (Ord, Eq, Show)

instance PP GUID where
  pp (GUID n) = int n

-- Used before we have resolved scope
invalidGUID :: GUID
invalidGUID = GUID (-1)

firstValidGUID :: GUID
firstValidGUID = GUID 0

succGUID :: GUID -> GUID
succGUID guid = GUID (getGUID guid + 1)


mkGUIDState ::
  StateM m s => (s -> GUID) -> (GUID -> s -> s) -> (GUID -> (a, GUID)) -> m a
mkGUIDState proj inj f = sets (mkGUIDState' proj inj f)

mkGUIDState' ::
  (s -> GUID) -> (GUID -> s -> s) -> (GUID -> (a, GUID)) -> (s -> (a, s))
mkGUIDState' proj inj = \f s -> let (r, guid') = f (proj s)
                                in (r, inj guid' s)

getNextGUID :: HasGUID m => m GUID
getNextGUID = guidState (\guid -> (guid, succGUID guid))


newtype FreshM a = FreshM (StateT GUID Id a)
  deriving (Functor,Applicative,Monad)

instance HasGUID FreshM where
  guidState f = FreshM (sets f)

runFresh :: FreshM a -> GUID -> (a,GUID)
runFresh (FreshM m) n = runId (runStateT n m)

runFreshIn :: HasGUID m => FreshM a -> m a
runFreshIn = guidState . runFresh



class Monad m => HasGUID m where
  guidState :: (GUID -> (a, GUID)) -> m a

instance (HasGUID m) => HasGUID (IdT m) where
  guidState = lift . guidState
instance (HasGUID m) => HasGUID (ReaderT i m) where
  guidState = lift . guidState
instance (HasGUID m,Monoid i) => HasGUID (WriterT i m) where
  guidState = lift . guidState
instance (HasGUID m) => HasGUID (StateT s m) where
  guidState = lift . guidState
instance (HasGUID m) => HasGUID (ExceptionT i m) where
  guidState = lift . guidState
instance (HasGUID m) => HasGUID (ChoiceT m) where
  guidState = lift . guidState
instance (HasGUID m) => HasGUID (ContT i m) where
  guidState = lift . guidState

