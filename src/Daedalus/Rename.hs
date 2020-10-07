{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, GADTs, DataKinds  #-}
{-# LANGUAGE RecordWildCards #-} -- for dealing with TCDecl and existential k
{-# LANGUAGE BlockArguments #-}

-- Makes all bound variables unique.
-- XXX: We should have done this during the scoping pass!
module Daedalus.Rename (rename) where

import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some

import Daedalus.Type.AST
import Daedalus.Type.Traverse
import Daedalus.Type.Free

-- ----------------------------------------------------------------------------------------
-- Monad

-- We want to make each bound variable unique, so we need to remember
-- all of them.  We also need to rename any variables we have seen
-- before.
newtype RenameM a =
  RenameM { getRenameM :: StateT (Set (Some TCName))
                                 (Reader (MapF TCName TCName)) a }
  deriving (Functor, Applicative, Monad)

runRenameM :: RenameM a -> a
runRenameM =  flip runReader MapF.empty . flip evalStateT Set.empty . getRenameM 

-- FIXME: Copied from Specialise.PartialApply
renameVarIn :: TCName k -> (TCName k -> RenameM a) -> RenameM a
renameVarIn tcn f = RenameM $ do
  tcn' <- gets (flip makeFreshFor tcn)
  modify (Set.insert (Some tcn'))
  local (MapF.insert tcn tcn') (getRenameM (f tcn'))

renameName :: TCName k -> RenameM (TCName k)
renameName tcn = RenameM (fromMaybe tcn <$> asks (MapF.lookup tcn))

-- ----------------------------------------------------------------------------------------
-- Top level

rename :: [TCDecl a] -> [TCDecl a]
rename = runRenameM . mapM renameDecl

renameDecl :: TCDecl a -> RenameM (TCDecl a)
renameDecl TCDecl { .. } = go [] tcDeclParams
  where
    go rparams [] = do
      tc' <- renameTCDeclDef tcDeclDef
      pure TCDecl { tcDeclParams = reverse rparams, tcDeclDef = tc', .. }
    go rparams (p : ps) =
      renameParamIn p $ \p' -> go (p' : rparams) ps

renameTCDeclDef :: TCDeclDef a k -> RenameM (TCDeclDef a k)
renameTCDeclDef def =
  case def of
    Defined d    -> Defined <$> renameTC d
    ExternDecl t -> pure (ExternDecl t)


renameTC :: TC a k -> RenameM (TC a k)
renameTC (TC k) = TC <$> (traverse renameTCF k)

renameTCF :: TCF a k -> RenameM (TCF a k)
renameTCF texpr = 
  case texpr of
    TCVar x -> TCVar <$> renameName x
    TCDo (Just x) e1 e2 -> do -- Nothing case falls through
      e1' <- renameTC e1
      renameVarIn x $ \x' -> TCDo (Just x') e1' <$> renameTC e2

    TCFor (Loop fl mbK x is e t) ->
      do renFl   <- loopFl
         is'     <- renameTC is
         renFl            \fl'  ->
           renameMb       \mbK' ->
           renameVarIn x  \x'   ->
             do e' <- renameTC e
                pure $ TCFor $ Loop fl' mbK' x' is' e' t

      where
      loopFl =
        case fl of
          LoopMap -> pure \k -> k LoopMap
          Fold sx se ->
            do se' <- renameTC se
               pure $ \k -> renameVarIn sx \sx' -> k (Fold sx' se')

      renameMb k =
        case mbK of
          Nothing -> k Nothing
          Just v  -> renameVarIn v $ \v' -> k (Just v')

    TCSelCase ctx e pats mdef t ->
      TCSelCase ctx
        <$> renameTC e
        <*> traverse renameMaplet pats
        <*> traverse renameTC mdef
        <*> pure t
      where
        renameMaplet (Nothing, e') = (,) Nothing <$> renameTC e'
        renameMaplet (Just v, e')  = renameVarIn v (\v' -> (,) (Just v') <$> renameTC e')

    e  -> traverseTCF renameTC e

renameParamIn :: Param -> (Param -> RenameM a) -> RenameM a
renameParamIn (ValParam p)     f = renameVarIn p (\p' -> f (ValParam p'))
renameParamIn (ClassParam p)   f = renameVarIn p (\p' -> f (ClassParam p'))
renameParamIn (GrammarParam p) f = renameVarIn p (\p' -> f (GrammarParam p'))
