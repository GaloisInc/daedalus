{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Daedalus.Pass
  ( PassM
  , runPassM
  , runPassM'
  , newPassState
  , PassState
  -- * Fresh Name
  , freshName
  , freshLocalName
  , deriveName
  , deriveNameWith

    -- * Fresh TCName
  , deriveTCName
  , deriveTCNameWith
  , freshTCName
  , freshLocalTCName
  ) where

import Control.Monad.IO.Class

import MonadLib
import Daedalus.SourceRange

import Daedalus.GUID

import Daedalus.AST (Name(..), Context, ModuleName, Ident)
import Daedalus.Type.AST (TCName(..), ScopedIdent(..), Type)
import Data.IORef (IORef, atomicModifyIORef')
import GHC.IORef (newIORef)

newtype PassState =
  PassState { guidVar :: IORef GUID
            -- ^ Plumb through fresh names
            }

newtype PassM a = PassM { getPassM :: ReaderT PassState IO a }
  deriving (Functor, Applicative, Monad)

runPassM :: PassM a -> IO a
runPassM m = do
  initState <- newPassState
  runM (getPassM m) initState 

newPassState :: IO PassState
newPassState = PassState <$> newIORef firstValidGUID

-- | Execute the pass, mutating the state inside PassState
runPassM' :: PassState -> PassM a -> IO a
runPassM' st m = runM (getPassM m) st

--------------------------------------------------------------------------------
-- Name

freshName :: HasGUID m => ModuleName -> Ident -> Context c -> m Name
freshName m x c = Name (ModScope m x) c synthetic True <$> getNextGUID

freshLocalName :: HasGUID m => Ident -> Context c -> m Name
freshLocalName x c = Name (Local x) c synthetic False <$> getNextGUID

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

--------------------------------------------------------------------------------
-- TCName

deriveTCName :: HasGUID m =>  TCName k -> m (TCName k)
deriveTCName x = do
  n <- deriveName (tcName x)
  pure x { tcName = n  }

deriveTCNameWith :: HasGUID m => (Ident -> Ident) -> TCName k -> m (TCName k)
deriveTCNameWith f x = do
  n <- deriveNameWith f (tcName x)
  pure x { tcName = n }

freshTCName ::
  HasGUID m => ModuleName -> Ident -> Type -> Context c -> m (TCName c)
freshTCName m x ty c  = do
  n <- freshName m x c
  pure (TCName n ty c)


freshLocalTCName :: HasGUID m => Ident -> Type -> Context c -> m (TCName c)
freshLocalTCName x ty c  = do
  n <- freshLocalName x c
  pure (TCName n ty c)

--------------------------------------------------------------------------------

instance RunM PassM a (IO a) where
  runM = runPassM

instance HasGUID PassM where
  guidState f = PassM $ do
    mv <- guidVar <$> ask
    let swp (a, b) = (b, a)
    lift $ atomicModifyIORef' mv (swp . f)
    
instance MonadIO PassM where
  liftIO = PassM . inBase

instance BaseM PassM PassM where
  inBase = id


