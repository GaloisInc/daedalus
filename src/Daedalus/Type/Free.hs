{-# LANGUAGE RankNTypes, GADTs, BlockArguments, NamedFieldPuns #-}

-- -----------------------------------------------------------------------------
-- Bound, free, and free in a function position variables
--

module Daedalus.Type.Free where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph.SCC(stronglyConnComp)
import Data.Foldable(traverse_)
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



-- Generates new names for locals
-- makeFreshFor :: Set (Some TCName) -> TCName k -> TCName k
-- makeFreshFor bounds tnm = mkN nm'
--   where
--     mkN nm =  tnm { tcName = nm }
    
--     nm' = head  . dropWhile (flip Set.member bounds . Some . mkN)
--           . freshCandidates $ tcName tnm
    
--     freshCandidates :: Name -> [Name]
--     freshCandidates x =
--       x : [ x { nameScope = Local $ nameScopeAsLocal x <> T.pack (show n) }
--           | n <- [(1::Int)..] ]

-- Collect all variables bound.  Mainly used to avoid capture.
-- XXX: This is an odd function... why do we need it?
tcBounds :: TCDeclDef a k -> Set (Some TCName)
tcBounds it = case it of
                Defined d     -> flip execState Set.empty (go d)
                ExternDecl _  -> Set.empty
  where
    go :: forall a k'. TC a k' -> State (Set (Some TCName)) (TC a k')
    go (TC v) = TC <$> traverse go' v

    doAlt :: forall a k'. TCAlt a k' -> State (Set (Some TCName)) (TCAlt a k')
    doAlt a@(TCAlt ps e) =
      do let vs = Set.fromList $ map Some $ altBinds a
         modify (Set.union vs)
         TCAlt ps <$> go e

    go' :: forall a k'. TCF a k' -> State (Set (Some TCName)) (TCF a k')
    go' texpr =
      case texpr of
        TCDo x e1 e2 ->
          do traverse_ (modify . Set.insert . Some) x
             TCDo x <$> go e1 <*> go e2

        TCLet x e1 e2 ->
          do modify (Set.insert (Some x))
             TCLet x <$> go e1 <*> go e2

        TCFor lp ->
          do modify ( addK
                    . Set.insert (Some (loopElName lp))
                    )
             mk <$> addLF <*> go (loopCol lp) <*> go (loopBody lp)
          where
          mk s i e = TCFor lp { loopFlav = s, loopCol = i, loopBody = e }

          addLF = case loopFlav lp of
                    Fold x s -> do modify (Set.insert (Some x))
                                   Fold x <$> go s
                    LoopMap  -> pure LoopMap

          addK = case loopKName lp of
                   Nothing -> id
                   Just k  -> Set.insert (Some k)

        TCCase e pats mdef ->
          TCCase <$> go e <*> traverse doAlt pats <*> traverse go mdef
  
        x -> traverseTCF go x

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

instance TCBinds (LoopFlav a) where
  tcBinds lf =
    case lf of
      Fold x _ -> tcBinds x
      LoopMap  -> Set.empty

forgetFree :: forall k. TCName k -> Set (Some TCName) -> Set (Some TCName)
forgetFree v = Set.delete (Some v)

instance TCFree a => TCFree [a] where
  tcFree = Set.unions . map tcFree

instance TCFree a => TCFree (Maybe a) where
  tcFree = maybe Set.empty tcFree

instance TCFree a => TCFree (ManyBounds a) where
  tcFree b =
    case b of
      Exactly e -> tcFree e
      Between x y -> Set.union (tcFree x) (tcFree y)

instance TCFree (LoopFlav a) where
  tcFree lf =
    case lf of
      Fold _ s -> tcFree s
      LoopMap  -> Set.empty

instance TCFree (Loop a k) where
  tcFree lp =
    Set.unions [ tcFree (loopCol lp)
               , flavS
               , delM flavB
                 (delM (loopKName lp) 
                  (forgetFree (loopElName lp) (tcFree (loopBody lp))))
               ]
    where
      (flavB, flavS) = case loopFlav lp of
        Fold v i -> (Just v, tcFree i)
        LoopMap  -> (Nothing, Set.empty)
      
      delM :: Maybe (TCName Value) ->
              Set (Some TCName) ->
              Set (Some TCName)
      delM Nothing  = id
      delM (Just x) = forgetFree x
      
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

