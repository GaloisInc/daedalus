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
tcBounds :: TCDeclDef a k -> Set (Some TCName)
tcBounds it = case it of
                Defined d     -> flip execState Set.empty (go d)
                ExternDecl _  -> Set.empty
  where
    go :: forall a k'. TC a k' -> State (Set (Some TCName)) (TC a k')
    go (TC v) = TC <$> traverse go' v

    go' :: forall a k'. TCF a k' -> State (Set (Some TCName)) (TCF a k')
    go' texpr =
      case texpr of
        TCDo x e1 e2 ->
          do traverse_ (modify . Set.insert . Some) x
             TCDo x <$> go e1 <*> go e2

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




instance TCFree a => TCFree [a] where
  tcFree = Set.unions . map tcFree

instance TCFree a => TCFree (Maybe a) where
  tcFree = maybe Set.empty tcFree

instance TCFree a => TCFree (ManyBounds a) where
  tcFree b =
    case b of
      Exactly e -> tcFree e
      Between x y -> Set.union (tcFree x) (tcFree y)

type SNames     = Set (Some TCName)
newtype NameM a = NameM (SNames -> SNames)

instance Functor NameM where
  fmap _ (NameM x) = NameM x

instance Applicative NameM where
  pure _                = NameM \_ -> Set.empty
  NameM xs <*> NameM ys = NameM \b -> Set.union (xs b) (ys b)

runNameM :: NameM a -> SNames
runNameM (NameM m) = m Set.empty

withSomeVar :: Some TCName -> NameM a -> NameM a
withSomeVar x (NameM m) = NameM \bound -> m (Set.insert x bound)

withVar :: TCName k -> NameM a -> NameM a
withVar x = withSomeVar (Some x)

addVar :: TCName k -> NameM a
addVar x = NameM \bound -> if x' `Set.member` bound then Set.empty
                                                    else Set.singleton x'
  where x' = Some x

instance TCFree (LoopFlav a) where
  tcFree lf =
    case lf of
      Fold _ s -> tcFree s
      LoopMap  -> Set.empty

instance TCFree (TC a k) where
  tcFree = runNameM . go
    where
      go :: TC a k' -> NameM (TC a k')
      go (TC v) = TC <$> traverse go' v

      go' :: TCF a k' -> NameM (TCF a k')
      go' texpr =
        case texpr of
          TCVar x             -> addVar x
          TCDo (Just x) e1 e2 -> TCDo (Just x) <$> go e1 <*> withVar x (go e2)

          TCCall f ts as -> TCCall f ts <$>
                            (mbAdd *> traverse (traverseArg go) as)
            where mbAdd = when (isLocalName (tcName f)) (addVar f)

          TCFor lp -> mk <$> loopF
                         <*> go (loopCol lp)
                         <*> flip (foldr withSomeVar) vs     (
                                    withK                   (
                                    withVar (loopElName lp) (
                                      go (loopBody lp)
                                    )))

            where
            mk s i e = TCFor lp { loopFlav = s, loopCol = i, loopBody = e }

            vs       = Set.toList (tcBinds (loopFlav lp))
            loopF = case loopFlav lp of
                      Fold x s -> Fold x <$> go s
                      LoopMap  -> pure LoopMap

            withK = case loopKName lp of
                      Nothing -> id
                      Just k  -> withVar k

          e  -> traverseTCF go e



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

