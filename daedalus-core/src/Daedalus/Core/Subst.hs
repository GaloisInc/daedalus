{-# Language BlockArguments, GeneralizedNewtypeDeriving #-}
module Daedalus.Core.Subst (Subst, substitute) where

import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import MonadLib

import Daedalus.GUID(HasGUID)

import Daedalus.Core.Free(freeVars)
import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar


-- | Substitute in a grammar or an expression.
-- Avoids capture by generating fresh names
substitute :: (Subst e, HasGUID m) => Map Name Expr -> e -> m e
substitute su e = runReaderT ro m
  where
  SubstM m = subst e
  vs       = freeVars (Map.elems su)
  ro       = RO { avoid    = vs
                , theSubst = su
                }


class Subst t where
  subst :: HasGUID m => t -> SubstM m t

instance Subst Expr where
  subst expr =
    case expr of
      Var x -> fromMaybe expr <$> shouldSubst x

      PureLet x e1 e2 -> letLike PureLet x e1 e2
      ECase c         -> substCase PureLet ECase c
      ELoop lm        -> ELoop <$> substLoopMorphism lm 
      _               -> childrenE subst expr

instance Subst Grammar where
  subst grammar =
    case grammar of
      Do  x g1 g2         -> letLike Do x g1 g2
      Let x e g           -> letLike Let x e g
      GCase c             -> substCase Let GCase c
      Loop lc             -> Loop <$> case lc of
        ManyLoop s b l m_u g -> do
          ManyLoop s b <$> subst l <*> traverse subst m_u <*> subst g
        RepeatLoop b n e g -> do
          e' <- subst e
          (n', g') <- bound n (subst g)
          pure (RepeatLoop b n' e' g')
        MorphismLoop lm -> MorphismLoop <$> substLoopMorphism lm
        
      _ -> gebChildrenG subst subst subst grammar
      
instance Subst Match where
  subst mat =
    case mat of
      MatchBytes e -> MatchBytes <$> subst e
      MatchByte  e -> MatchByte  <$> subst e
      MatchEnd     -> pure MatchEnd

instance Subst ByteSet where
  subst bs =
    case bs of
      SetAny                -> pure bs
      SetSingle e           -> SetSingle <$> subst e
      SetRange e1 e2        -> SetRange <$> subst e1 <*> subst e2
      SetComplement x       -> SetComplement <$> subst x
      SetUnion x y          -> SetUnion <$> subst x <*> subst y
      SetIntersection x y   -> SetIntersection <$> subst x <*> subst y
      SetCall f es          -> SetCall f <$> traverse subst es
      SetCase e             -> substCase SetLet SetCase e
      SetLet x e s          -> letLike SetLet x e s
      SetLoop lm            -> SetLoop <$> substLoopMorphism lm
      
substCase :: (Subst e, HasGUID m) => (Name -> Expr -> e -> e) -> (Case e -> e) ->
             Case e -> SubstM m e
substCase mklet mk cs = do
  r@(Case v ps') <- traverse subst cs
  subst_v <- shouldSubst v
  case subst_v of
    Nothing -> pure (mk r)
    Just e  -> do
      v' <- newName v
      pure (mklet v' e (mk (Case v' ps')))

letLike ::
  (Subst a, Subst b, HasGUID m) =>
  (Name -> a -> b -> c) -> Name -> a -> b -> SubstM m c
letLike f x a b =
  do a'     <- subst a
     (y,b') <- bound x (subst b)
     pure (f y a' b')

substLoopMorphism :: (Subst a, Monad m, HasGUID m) => LoopMorphism a -> SubstM m (LoopMorphism a)
substLoopMorphism lm =
  case lm of
    FoldMorphism s e lc b -> do
      e' <- subst e
      goLC (FoldMorphism s e') lc b
    MapMorphism lc b -> goLC MapMorphism lc b
  where
    goLC f lc b = do
      col' <- subst (lcCol lc)
      let rest = case lcKName lc of
            Nothing -> (,) Nothing <$> subst b
            Just k  -> do
              (k', b') <- bound k (subst b)
              pure (Just k', b')
      (el', (m_k', b')) <- bound (lcElName lc) rest
      pure (f (LoopCollection m_k' el' col') b')
  

--------------------------------------------------------------------------------
-- Monad to keep track of name capture etc

newtype SubstM m a = SubstM (ReaderT RO m a)
  deriving (Functor,Applicative,Monad)

data RO = RO
  { avoid    :: Set Name -- ^ Capturalble names in RHS of theSubst
  , theSubst :: Map Name Expr
  }

-- | These are guaranteed to not clash with the names in the RHS of the subst
-- however they may clash with existing binders, in which case we rename
-- the binders.
newName :: HasGUID m => Name -> SubstM m Name
newName x = SubstM $ lift $ freshName x

captures :: Monad m => Name -> SubstM m Bool
captures x = SubstM $ (x `Set.member`) . avoid <$> ask

shouldSubst :: Monad m => Name -> SubstM m (Maybe Expr)
shouldSubst x = SubstM $ Map.lookup x . theSubst <$> ask

rename :: Monad m => Name -> Name -> SubstM m a -> SubstM m a
rename x y (SubstM m) = SubstM (mapReader extend m)
  where extend ro = RO { avoid    = Set.insert y (avoid ro)
                       , theSubst = Map.insert x (Var y) (theSubst ro)
                       }

bound :: HasGUID m => Name -> SubstM m t -> SubstM m (Name,t)
bound x scope =
  do yes <- captures x
     if yes
       then do y <- newName x
               a <- rename x y scope
               pure (y,a)
       else (,) x <$> scope



