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

      Struct ut fs -> Struct ut <$> mapM substField fs
        where substField (a,b) = (,) a <$> subst b

      ECase c         -> ECase <$> subst c

      Ap0 {}          -> pure expr
      Ap1 op e        -> Ap1 op <$> subst e
      Ap2 op e1 e2    -> Ap2 op <$> subst e1 <*> subst e2
      Ap3 op e1 e2 e3 -> Ap3 op <$> subst e1 <*> subst e2 <*> subst e3
      ApN op es       -> ApN op <$> mapM subst es

instance Subst Grammar where
  subst grammar =
    case grammar of
      Pure e              -> Pure <$> subst e
      GetStream           -> pure grammar
      SetStream e         -> SetStream <$> subst e
      Match s m           -> Match s <$> subst m
      Fail er t mbE       -> Fail er t <$> traverse subst mbE
      Do_ g1 g2           -> Do_ <$> subst g1 <*> subst g2
      Do  x g1 g2         -> letLike Do x g1 g2
      Let x e g           -> letLike Let x e g
      OrBiased g1 g2      -> OrBiased <$> subst g1 <*> subst g2
      OrUnbiased g1 g2    -> OrUnbiased <$> subst g1 <*> subst g2
      Call f es           -> Call f <$> traverse subst es
      Annot a g           -> Annot a <$> subst g
      GCase c             -> GCase <$> subst c

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
      SetCase e             -> SetCase <$> subst e
      SetLet x e s          -> letLike SetLet x e s



instance Subst e => Subst (Case e) where
  subst (Case e ps) = Case <$> subst e <*> mapM substBranch ps
    where substBranch (p,rhs) = (,) p <$> subst rhs


letLike ::
  (Subst a, Subst b, HasGUID m) =>
  (Name -> a -> b -> c) -> Name -> a -> b -> SubstM m c
letLike f x a b =
  do a'     <- subst a
     (y,b') <- bound x (subst b)
     pure (f y a' b')



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
captures x = SubstM $ ((x `Set.member`) . avoid) <$> ask

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



