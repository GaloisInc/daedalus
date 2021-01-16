
module Daedalus.GUID (GUID
                     , invalidGUID
                     , firstValidGUID
                     , succGUID
                     , mkGetNextGUID
                     , mkGetNextGUID'
                     , HasGUID(..)
                     ) where

import MonadLib
import Daedalus.PP

newtype GUID = GUID { getGUID :: Int }
  deriving (Ord, Eq, Show)

instance PP GUID where
  pp (GUID n) = text (show n)

-- Used before we have resolved scope
invalidGUID :: GUID
invalidGUID = GUID (-1)

firstValidGUID :: GUID
firstValidGUID = GUID 0

succGUID :: GUID -> GUID
succGUID guid = GUID (getGUID guid + 1)

mkGetNextGUID :: StateM m s => (s -> GUID) -> (GUID -> s -> s) -> m GUID
mkGetNextGUID proj inj = sets (mkGetNextGUID' proj inj)

mkGetNextGUID' :: (s -> GUID) -> (GUID -> s -> s) -> (s -> (GUID, s))
mkGetNextGUID' proj inj = \s -> let guid = proj s in (guid, inj (succGUID guid) s)

class Monad m => HasGUID m where
  getNextGUID :: m GUID

instance (HasGUID m) => HasGUID (IdT m) where
  getNextGUID = lift getNextGUID
instance (HasGUID m) => HasGUID (ReaderT i m) where
  getNextGUID = lift getNextGUID
instance (HasGUID m,Monoid i) => HasGUID (WriterT i m) where
  getNextGUID = lift getNextGUID
instance (HasGUID m) => HasGUID (StateT s m) where
  getNextGUID = lift getNextGUID  
instance (HasGUID m) => HasGUID (ExceptionT i m) where
  getNextGUID = lift getNextGUID
instance (HasGUID m) => HasGUID (ChoiceT m) where
  getNextGUID = lift getNextGUID
instance (HasGUID m) => HasGUID (ContT i m) where
  getNextGUID = lift getNextGUID


  
  

