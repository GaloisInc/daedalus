module Daedalus.Core.Free where

import           Data.Maybe                      (maybeToList)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set

import           Daedalus.Core.Basics
import           Daedalus.Core.ByteSet
import           Daedalus.Core.Decl
import           Daedalus.Core.Expr
import           Daedalus.Core.Grammar
import           Daedalus.Core.TraverseUserTypes


-- | Compute value-level dependencies
class FreeVars t where
  freeVars :: t -> Set Name
  freeFVars :: t -> Set FName

-- | Compute user-defined type dependencies
freeTCons :: TraverseUserTypes t => t -> Set TName
freeTCons = foldMapUserTypes (\ut -> Set.insert (utName ut) (freeTCons (utTyArgs ut)))

instance FreeVars a => FreeVars [a] where
  freeVars = Set.unions . map freeVars
  freeFVars = Set.unions . map freeFVars

instance FreeVars Name where
  freeVars = Set.singleton
  freeFVars _ = mempty
  
instance FreeVars Expr where
  freeVars expr =
    case expr of
      Var x           -> Set.singleton x
      PureLet x e1 e2 -> freeVars e1 `Set.union` Set.delete x (freeVars e2)
      ECase e         -> freeVars e
      ELoop lm        -> freeVars lm
      _               -> dflt
    where
      dflt = foldMapChildrenE freeVars expr
      
  freeFVars expr =
    case expr of
      ApN (CallF f) es -> Set.insert f (freeFVars es)
      _ -> foldMapChildrenE freeFVars expr
      
instance FreeVars Grammar where
  freeVars gram =
    case gram of
      Do  x g1 g2       -> freeVars g1 `Set.union` Set.delete x (freeVars g2)
      Let x e g         -> freeVars e  `Set.union` Set.delete x (freeVars g)
      GCase c           -> freeVars c
      Loop lc           -> case lc of
        ManyLoop _ _ l m_u b -> freeVars l <> freeVars m_u <> freeVars b
        RepeatLoop _ n e b   -> freeVars e <> Set.delete n (freeVars b)
        MorphismLoop lm      -> freeVars lm
      _ -> dflt
    where
      dflt = foldMapChildrenG freeVars freeVars freeVars gram
        
  freeFVars gram =
    case gram of
      Call f es         -> Set.insert f (freeFVars es)
      _ -> foldMapChildrenG freeFVars freeFVars freeFVars gram
      
instance FreeVars ByteSet where
  freeVars bs = 
    case bs of
      SetCase e -> freeVars e
      SetLet x e k -> freeVars e `Set.union` Set.delete x (freeVars k)
      _ -> ebFoldMapChildrenB freeVars freeVars bs
      
  freeFVars bs =
    case bs of
      SetCall f es -> Set.insert f (freeFVars es)
      _ -> ebFoldMapChildrenB freeFVars freeFVars bs

instance FreeVars e => FreeVars (Case e) where
  freeVars  (Case v opts) = Set.insert v (freeVars (map snd opts))
  freeFVars (Case _ opts) = freeFVars (map snd opts)

instance FreeVars e => FreeVars (FunDef e) where
  freeVars def =
    case def of
      Def e -> freeVars e
      External -> Set.empty

  freeFVars def =
    case def of
      Def e -> freeFVars e
      External -> Set.empty


instance FreeVars e => FreeVars (Fun e) where
  freeVars f = freeVars (fDef f) `Set.difference` Set.fromList (fParams f)
  freeFVars f = freeFVars (fDef f)

instance FreeVars e => FreeVars (Maybe e) where
  freeVars  = maybe mempty freeVars
  freeFVars = maybe mempty freeFVars

instance (FreeVars e, FreeVars b) => FreeVars (LoopMorphism' e b) where
  freeVars m = case m of
    FoldMorphism n e lc b -> 
      freeVars e <> freeVars (lcCol lc) <>
      (freeVars b `Set.difference` Set.fromList (n : lcBinds lc))
    MapMorphism lc b -> freeVars (lcCol lc) <> (freeVars b `Set.difference` Set.fromList (lcBinds lc))
    where
      lcBinds lc = lcElName lc : maybeToList (lcKName lc)
    
  freeFVars (FoldMorphism _ e lc b) = freeFVars e <> freeFVars (lcCol lc) <> freeFVars b
  freeFVars (MapMorphism lc b)      = freeFVars (lcCol lc) <> freeFVars b
  
