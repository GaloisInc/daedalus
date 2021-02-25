{-# Language BlockArguments, GeneralizedNewtypeDeriving #-}
module Daedalus.Core.Fresh where

import MonadLib
import Daedalus.GUID
import Daedalus.Core.Basics


newtype FreshM a = FreshM (StateT GUID Id a)
  deriving (Functor,Applicative,Monad)

instance HasGUID FreshM where
  guidState f = FreshM (sets f)

runFresh :: FreshM a -> GUID -> (a,GUID)
runFresh (FreshM m) n = runId (runStateT n m)

freshTName :: HasGUID m => TName -> m TName
freshTName n =
  do x <- getNextGUID
     pure n { tnameId = x }

freshFName :: HasGUID m => FName -> m FName
freshFName n =
  do x <- getNextGUID
     pure n { fnameId = x }

freshName :: HasGUID m => Name -> m Name
freshName n =
  do x <- getNextGUID
     pure n { nameId = x }

