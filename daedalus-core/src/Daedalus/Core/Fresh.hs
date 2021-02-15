{-# Language BlockArguments, GeneralizedNewtypeDeriving #-}
module Daedalus.Core.Fresh where

import MonadLib
import Daedalus.Core.Basics

-- XXX: Use Daedalus.GUID
newtype FreshM a = FreshM (StateT Int Id a)
  deriving (Functor,Applicative,Monad)

runFresh :: Int -> FreshM a -> (a,Int)
runFresh n (FreshM m) = runId (runStateT n m)

freshName :: Name -> FreshM Name
freshName x = FreshM
            $ sets \s -> let n = s + 1
                         in n `seq` (x { nameId = s }, n)



