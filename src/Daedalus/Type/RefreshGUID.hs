{-# LANGUAGE TupleSections, DataKinds, GADTs, TypeFamilies #-}
{-# Language BlockArguments, RecordWildCards, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# Language RankNTypes #-}

{- Makes all variable unique (again).  Useful for e.g. partial application -}

module Daedalus.Type.RefreshGUID (refreshDecl) where

import MonadLib

import Daedalus.Panic(panic)
import Daedalus.GUID
import Daedalus.Pass

import Daedalus.Type.AST
import Daedalus.Type.Subst
import Daedalus.Type.Traverse

newtype RefreshGUIDM ann a =
  RefreshGUIDM { getRefreshGUIDM :: ReaderT (Subst ann) PassM a }
  deriving (Functor, Applicative, Monad, HasGUID)

instance ReaderM (RefreshGUIDM ann) (Subst ann) where
  ask = RefreshGUIDM ask

instance RunReaderM (RefreshGUIDM ann) (Subst ann) where
  local i m = RefreshGUIDM (local i (getRefreshGUIDM m))

-- instance HasGUID (RefreshGUIDM ann) where
--   getNextGUID = RefreshGUIDM getNextGUID

runRefreshGUIDM :: RefreshGUIDM ann a -> PassM a
runRefreshGUIDM m = runReaderT emptySubst (getRefreshGUIDM m)

-- We assume the decl's name has been refreshed at construction time, a bit hacky though :(  Order
-- of params shouldn't be that important as we shouldn't have overlap in param GUIDs
refreshDecl :: TCDecl a -> PassM (TCDecl a)
refreshDecl TCDecl {..} = runRefreshGUIDM $ (foldl go mk tcDeclParams) []
  where
    mk params' = do def' <- refreshGUID tcDeclDef
                    pure TCDecl { tcDeclParams = params', tcDeclDef = def', .. } 

    go :: ([Param] -> RefreshGUIDM ann (TCDecl a))
      -> Param
      -> ([Param] -> RefreshGUIDM ann (TCDecl a))

    go rest param = \params'->
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
  x' <- deriveTCName x
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

          TCCall f ts as
            | isLocalName (tcName f) ->
              do subst <- ask
                 case lookupSubst f subst of
                   Just (TCVar f1) ->
                              TCCall f1 ts <$> traverse (traverseArg go) as
                   _ -> panic "refresh TCCall" ["Missing names"]

          TCDo (Just x) e1 e2 -> do
            e1' <- go e1
            withVar x $ \x' -> TCDo (Just x') e1' <$> go e2

          -- HERE
          TCFor lp -> TCFor <$> goLoop lp

          e  -> traverseTCF go e

      goLoop lp =
        case loopFlav lp of

          Fold v e col ->
            do e' <- go e
               c' <- goCol col
               withVar v \v' ->
                 withCol c' \col' ->
                   do body' <- go (loopBody lp)
                      pure Loop { loopFlav  = Fold v' e' col'
                                , loopBody  = body'
                                , loopType  = loopType lp
                                }

          LoopMap col ->
            do c' <- goCol col
               withCol c' \col' ->
                 do body' <- go (loopBody lp)
                    pure Loop { loopFlav = LoopMap col'
                              , loopBody = body'
                              , loopType = loopType lp
                              }

          LoopMany c v e ->
            do e' <- go e
               withVar v \v' ->
                 do body' <- go (loopBody lp)
                    pure Loop { loopFlav = LoopMany c v' e'
                              , loopBody = body'
                              , loopType = loopType lp
                              }

      goCol col =
        do c <- go (lcCol col)
           pure col { lcCol = c }

      withCol col cont =
        withVarMaybe (lcKName col)  \k'  ->
        withVar      (lcElName col) \el' ->
        cont col { lcKName = k', lcElName = el' }

