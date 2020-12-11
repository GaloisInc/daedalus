{-# LANGUAGE TupleSections, DataKinds, GADTs, TypeFamilies #-}
{-# Language BlockArguments, RecordWildCards, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# Language RankNTypes #-}

{- Makes all variable unique (again).  Useful for e.g. partial application -}

module Daedalus.Type.RefreshGUID (refreshDecl, refreshName) where

import MonadLib

import Daedalus.GUID

import Daedalus.Type.AST
import Daedalus.Type.Subst
import Daedalus.Type.Traverse

newtype RefreshGUIDM ann a =
  RefreshGUIDM { getRefreshGUIDM :: forall m. HasGUID m => ReaderT (Subst ann) m a }

deriving instance Functor (RefreshGUIDM ann)

instance Applicative (RefreshGUIDM ann) where
  RefreshGUIDM m <*> RefreshGUIDM m' = RefreshGUIDM (m <*> m')
  pure v = RefreshGUIDM $ pure v
  
instance Monad (RefreshGUIDM ann) where
  RefreshGUIDM m >>= f = RefreshGUIDM (m >>= getRefreshGUIDM . f)

instance ReaderM (RefreshGUIDM ann) (Subst ann) where
  ask = RefreshGUIDM ask

instance RunReaderM (RefreshGUIDM ann) (Subst ann) where
  local i m = RefreshGUIDM (local i (getRefreshGUIDM m))

instance HasGUID (RefreshGUIDM ann) where
  getNextGUID = RefreshGUIDM getNextGUID

runRefreshGUIDM :: HasGUID m => RefreshGUIDM ann a -> m a
runRefreshGUIDM m = runReaderT emptySubst (getRefreshGUIDM m)

refreshName :: HasGUID m =>  TCName k -> m (TCName k)
refreshName x = do
  nextg <- getNextGUID
  pure x { tcName = (tcName x) { nameID = nextg }}

-- We assume the decl's name has been refreshed at construction time, a bit hacky though :(  Order
-- of params shouldn't be that important as we shouldn't have overlap in param GUIDs
refreshDecl :: HasGUID m => TCDecl a -> m (TCDecl a)
refreshDecl TCDecl {..} = runRefreshGUIDM $ (foldr go mk tcDeclParams) []
  where
    mk params' = do def' <- refreshGUID tcDeclDef
                    pure TCDecl { tcDeclParams = params', tcDeclDef = def', .. } 

    go :: Param
      -> ([Param] -> RefreshGUIDM ann (TCDecl a))
      -> ([Param] -> RefreshGUIDM ann (TCDecl a))

    go param rest = \params'->
      case param of 
        ValParam n     -> withVar n \n' -> rest (ValParam n'     : params')
        ClassParam n   -> withVar n \n' -> rest (ClassParam n'   : params')
        GrammarParam n -> withVar n \n' -> rest (GrammarParam n' : params')

--------------------------------------------------------------------------------
-- type class and instances

class RefreshGUID t where
  type Annot t
  refreshGUID :: t -> RefreshGUIDM (Annot t) t

-- FIXME: replace by Traversable?
instance RefreshGUID a => RefreshGUID [a] where
  type Annot [a] = Annot a
  refreshGUID = mapM refreshGUID

instance RefreshGUID a => RefreshGUID (Maybe a) where
  type Annot (Maybe a) = Annot a
  refreshGUID = traverse refreshGUID

instance RefreshGUID a => RefreshGUID (ManyBounds a) where
  type Annot (ManyBounds a) = Annot a
  refreshGUID = traverse refreshGUID

withVar :: TCName k -> (TCName k -> RefreshGUIDM ann a) -> RefreshGUIDM ann a
withVar x f = do
  x' <- refreshName x
  mapReader (addSubst x (TCVar x')) (f x')

withVarMaybe :: Maybe (TCName k) -> (Maybe (TCName k) -> RefreshGUIDM ann a) -> RefreshGUIDM ann a
withVarMaybe Nothing  f = f Nothing
withVarMaybe (Just v) f = withVar v (f . Just)

-- instance RefreshGUID (LoopFlav a) where
--   refreshGUID lf =
--     case lf of
--       Fold _ s -> refreshGUID s
--       LoopMap  -> Set.empty

instance RefreshGUID (TCDeclDef a k) where
  type Annot (TCDeclDef a k) = a
  refreshGUID d@(ExternDecl {}) = pure d
  refreshGUID (Defined d) = Defined <$> refreshGUID d


instance RefreshGUID (TC a k) where
  type Annot (TC a k) = a
  refreshGUID = go
    where
      go :: TC a k' -> RefreshGUIDM a (TC a k')
      go (TC v) = TC <$> traverse go' v

      go' :: TCF a k' -> RefreshGUIDM a (TCF a k')
      go' texpr =
        case texpr of
          TCVar x -> do
            subst <- ask
            pure $ case lookupSubst x subst of
                     Nothing  -> texpr -- FIXME: this is an error?
                     Just tcf -> tcf
                     
          TCDo (Just x) e1 e2 -> do
            e1' <- go e1
            withVar x $ \x' -> TCDo (Just x') e1' <$> go e2

          -- TCCall f ts as -> TCCall f ts <$> (traverse (traverseArg go) as)

          -- HERE
          TCFor lp -> TCFor <$> do
            col' <- go (loopCol lp)
            mk \flav' ->
              withVarMaybe (loopKName lp) \k' ->
              withVar (loopElName lp) \el' -> do
              body' <- go (loopBody lp)
              pure (Loop { loopFlav   = flav'
                         , loopKName  = k'
                         , loopElName = el'
                         , loopCol    = col'
                         , loopBody   = body'
                         , loopType   = loopType lp
                         })
            
            where
              mk rest = case loopFlav lp of
                          Fold v e -> do e' <- go e
                                         withVar v \v' -> rest (Fold v' e')
                          LoopMap -> rest LoopMap

          e  -> traverseTCF go e
