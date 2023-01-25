module Daedalus.Core.Rename (Rename, rename) where

import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (maybeToList)
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           MonadLib

import           Daedalus.Core.Basics
import           Daedalus.Core.ByteSet
import           Daedalus.Core.Expr
import           Daedalus.Core.Grammar
import           Daedalus.GUID         (HasGUID)

type Renaming = Map Name Name

-- | Remame _free_ variables in a term, retuning the renaming.
rename :: (HasGUID m, Rename t) => t -> m (t, Renaming)
rename t = do
  runStateT mempty (runReaderT Set.empty (doRename t))

type RenameM m a = ReaderT (Set Name) (StateT Renaming m) a
  
boundsIn :: Monad m => [Name] -> RenameM m a -> RenameM m a
boundsIn xs = mapReader (Set.union (Set.fromList xs))

boundIn :: Monad m => Name -> RenameM m a -> RenameM m a
boundIn x = boundsIn [x]

-- | Compute value-level dependencies
class Rename t where
  doRename :: HasGUID m => t -> RenameM m t

instance Rename Name where
  doRename n = do
    bound <- asks (n `Set.member`)
    if bound
      then pure n
      else do
        m_rn <- Map.lookup n <$> get
        case m_rn of
          Nothing -> do
            n' <- freshName n
            sets_ (Map.insert n n')
            pure n'
          Just n' -> pure n'
          
instance Rename Expr where
  doRename expr =
    case expr of
      Var x           -> Var <$> doRename x
      PureLet x e1 e2 ->
        PureLet x <$> doRename e1 <*> boundIn x (doRename e2)
      ECase e         -> ECase <$> doRename e
      ELoop lm        -> ELoop <$> doRename lm
      _               -> dflt
    where
      dflt = childrenE doRename expr
      
instance Rename Grammar where
  doRename gram =
    case gram of
      Do  x g1 g2       ->
        Do x <$> doRename g1 <*> boundIn x (doRename g2)
      Let x e g         ->
        Let x <$> doRename e <*> boundIn x (doRename g)
      GCase c           -> GCase <$> doRename c
      Loop (RepeatLoop c n e b) ->
        Loop <$> (RepeatLoop c <$> doRename n
                               <*> doRename e
                               <*> doRename b)
      Loop (MorphismLoop lm) ->
        Loop . MorphismLoop <$> doRename lm
      _ -> dflt
    where
      dflt = gebChildrenG doRename doRename doRename gram
      
instance Rename ByteSet where
  doRename bs = 
    case bs of
      SetCase e -> SetCase <$> doRename e
      SetLet x e k ->
        SetLet x <$> doRename e <*> boundIn x (doRename k)
      _ -> ebChildrenB doRename doRename bs

instance Rename e => Rename (Case e) where
  doRename cs = do
    (Case v opts') <- traverse doRename cs
    Case <$> doRename v <*> pure opts'

instance Rename e => Rename (Maybe e) where
  doRename = traverse doRename

instance Rename e => Rename (LoopMorphism e) where
  doRename m = case m of
    FoldMorphism n e lc b -> do
      e' <- doRename e
      goLC (FoldMorphism n e') lc (boundIn n (doRename b))
    MapMorphism lc b -> goLC MapMorphism lc (doRename b)      
    where
      goLC f lc b = do
        let bnds = lcElName lc : maybeToList (lcKName lc)
            mklc e = lc { lcCol = e }         
        f <$> (mklc <$> doRename (lcCol lc))
          <*> boundsIn bnds b
  
