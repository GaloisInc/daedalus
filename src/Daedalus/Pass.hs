{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Daedalus.Pass (PassM
                     , runPassM
                     -- Name functions
                     , deriveName
                     , deriveNameWith
                     , deriveTCName
                     , deriveTCNameWith
                     , freshName
                     , freshTCName
                     )  where

import Control.Monad.IO.Class
import MonadLib
import Daedalus.SourceRange

import Daedalus.GUID

import Daedalus.AST (Name(..), Context, ModuleName, Ident)
import Daedalus.Type.AST (TCName(..), ScopedIdent(..), Type)

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

-- FIXME: do these belong here?

deriveName :: HasGUID m => Name -> m Name
deriveName x = do
  nextg <- getNextGUID
  pure (x { nameID = nextg })

deriveNameWith :: HasGUID m => (Ident -> Ident) -> Name -> m Name
deriveNameWith f x = do
  nextg <- getNextGUID
  let si' = case nameScopedIdent x of
              Unknown t    -> Unknown (f t)
              Local t      -> Local (f t)
              ModScope m t -> ModScope m (f t)
  pure (x { nameScopedIdent = si', nameID = nextg })

deriveTCName :: HasGUID m =>  TCName k -> m (TCName k)
deriveTCName x = do
  n <- deriveName (tcName x)
  pure x { tcName = n  }

deriveTCNameWith :: HasGUID m => (Ident -> Ident) -> TCName k -> m (TCName k)
deriveTCNameWith f x = do
  n <- deriveNameWith f (tcName x)
  pure x { tcName = n }

freshName :: HasGUID m => ModuleName -> Ident -> Context c -> m Name
freshName m x c = Name (ModScope m x) c synthetic <$> getNextGUID

freshTCName :: HasGUID m => ModuleName -> Ident -> Type -> Context c -> m (TCName c)
freshTCName m x ty c  = do
  n <- freshName m x c
  pure (TCName n ty c)
  
--------------------------------------------------------------------------------
  
instance RunM PassM a (IO a) where
  runM = runPassM

instance HasGUID PassM where
  getNextGUID =
    PassM $ mkGetNextGUID nextFreeGUID (\g t -> t { nextFreeGUID = g })

instance MonadIO PassM where
  liftIO = PassM . inBase

instance BaseM PassM PassM where
  inBase = id


