{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Daedalus.Pass (PassM, runPassM)  where

import Control.Monad.IO.Class
import MonadLib

import Daedalus.GUID

data PassState =
  PassState { nextFreeGUID :: !GUID
            -- ^ Plumb through fresh names
            }

newtype PassM a = PassM { getPassM :: StateT PassState IO a }
  deriving (Functor, Applicative, Monad)

initState :: PassState
initState = PassState { nextFreeGUID = firstValidGUID }

runPassM :: PassM a -> IO a
runPassM m = fst <$> runM (getPassM m) initState 

instance RunM PassM a (IO a) where
  runM = runPassM

instance HasGUID PassM where
  getNextGUID =
    PassM $ mkGetNextGUID nextFreeGUID (\g t -> t { nextFreeGUID = g })

instance MonadIO PassM where
  liftIO = PassM . inBase

instance BaseM PassM PassM where
  inBase = id
