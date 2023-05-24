{-# LANGUAGE RankNTypes, GADTs, BlockArguments, NamedFieldPuns #-}

-- -----------------------------------------------------------------------------
-- Bound, free, and free in a function position variables
--

module Daedalus.Type.Free where

import Control.Monad.State
import Control.Monad (unless)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph.SCC(stronglyConnComp)
import qualified Data.List.NonEmpty as NE

import Data.Parameterized.Some

import Daedalus.Rec(sccToRec)

import Daedalus.Type.AST
import Daedalus.Type.Traverse

-- -- | Oorder a bunch of declarations topologically
topoOrder :: [TCDecl a] -> [Rec (TCDecl a)]
topoOrder = map sccToRec . stronglyConnComp . map node
  where
  node d  = (d, tcDeclName d, getFree d)

  getFree TCDecl { tcDeclDef } =
    [ n | n@Name { nameScopedIdent = ModScope {} } <- Set.toList (tcCalls tcDeclDef) ]



 -- TCName because we need the context
class TCFree t where
  tcFree :: t -> Set (Some TCName)

class TCBinds t where
  tcBinds :: t -> Set (Some TCName)

instance TCBinds t => TCBinds (Maybe t) where
  tcBinds = maybe Set.empty tcBinds

instance (TCBinds a, TCBinds b) => TCBinds (a,b) where
  tcBinds (a,b) = Set.union (tcBinds a) (tcBinds b)

instance TCBinds (TCName k) where
  tcBinds x = Set.singleton (Some x)

instance TCBinds (LoopFlav a k) where
  tcBinds lf =
    case lf of
      Fold x _ col -> tcBinds (x,col)
      LoopMap col  -> tcBinds col
      LoopMany _ x _ -> tcBinds x

instance TCBinds (LoopCollection a) where
  tcBinds col = tcBinds (lcKName col, lcElName col)

forgetFree :: forall k. TCName k -> Set (Some TCName) -> Set (Some TCName)
forgetFree v = Set.delete (Some v)

instance TCFree a => TCFree [a] where
  tcFree = Set.unions . map tcFree

instance TCFree a => TCFree (Maybe a) where
  tcFree = maybe Set.empty tcFree

instance (TCFree a, TCFree b) => TCFree (a,b) where
  tcFree (a,b) = Set.union (tcFree a) (tcFree b)

instance TCFree a => TCFree (ManyBounds a) where
  tcFree b =
    case b of
      Exactly e -> tcFree e
      Between x y -> tcFree (x,y)

instance TCFree (LoopCollection a) where
  tcFree col = tcFree (lcCol col)

instance TCFree (LoopFlav a k) where
  tcFree lf =
    case lf of
      Fold _ s col -> tcFree (s,col)
      LoopMap col  -> tcFree col
      LoopMany _ _ s -> tcFree s

instance TCFree (Loop a k) where
  tcFree lp =
    Set.unions [ tcFree (loopFlav lp)
               , tcFree (loopBody lp) `Set.difference` tcBinds (loopFlav lp)
               ]

instance TCFree (TCF a k) where
  tcFree texpr = 
    case texpr of
      TCVar x             -> Set.singleton (Some x)
      TCDo (Just x) e1 e2 ->
        tcFree e1 `Set.union` (forgetFree x (tcFree e2))

      TCLet x e1 e2 -> tcFree e1 `Set.union` forgetFree x (tcFree e2)

      TCCall f _ts as | isLocalName (tcName f) ->
        Set.singleton (Some f) `Set.union` tcFree as

      TCFor lp -> tcFree lp
      
      TCCase e pats mdef ->
        Set.unions (tcFree e : tcFree mdef : map doAlt (NE.toList pats))
        where
          doAlt (TCAlt ps rhs) = foldr forgetFree (tcFree rhs) (patBinds (head ps))

      e  -> foldMapTCF tcFree e

-- XXX: Why are we doing this complicated traverals thing here??
instance TCFree (TC a k) where
  tcFree = tcFree . texprValue

instance TCFree (Arg a) where
  tcFree (GrammarArg a) = tcFree a
  tcFree (ValArg     a) = tcFree a
  tcFree (ClassArg   a) = tcFree a

-- Get the *top level* calls
tcCalls :: TCDeclDef a k -> Set Name
tcCalls def =
  case def of
    ExternDecl _ -> Set.empty
    Defined d    -> flip execState Set.empty (go d)
  where
    go :: forall a k'. TC a k' -> State (Set Name) (TC a k')
    go (TC m) = TC <$> traverse go' m

    go' :: forall a k'. TCF a k' -> State (Set Name) (TCF a k')
    go' texpr =
      case texpr of
        TCCall f ts as ->
          do unless (isLocalName (tcName f)) (modify (Set.insert (tcName f)))
             TCCall f ts <$> traverse (traverseArg go) as

        x -> traverseTCF go x

