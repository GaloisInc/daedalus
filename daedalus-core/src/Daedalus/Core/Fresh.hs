{-# Language BlockArguments #-}
module Daedalus.Core.Fresh where

import Daedalus.GUID
import Daedalus.Core.Basics


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

