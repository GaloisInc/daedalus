{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Support for functions, including defining/determining monomorphised variants

module Talos.SymExec.Funs where

-- import Data.Map (Map)
import           Control.Monad.Reader
import           Data.Foldable                   (fold)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Set                        (Set)
import qualified Data.Set                        as Set

import           SimpleSMT                       (SExpr)
import qualified SimpleSMT                       as S

import           Daedalus.Core                   hiding (freshName)
import           Daedalus.Core.Free
import           Daedalus.Core.TraverseUserTypes
import           Daedalus.Core.Type
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic
import           Daedalus.Rec

-- import Talos.Strategy.Monad
import           Talos.Analysis.Slice
import           Talos.SymExec.SolverT
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type
import           Talos.SymExec.Expr (symExecExpr, symExecByteSet)

--------------------------------------------------------------------------------
-- Functions

-- transitive closure from the roots in the grammar functions.
-- FIXME: (perf) we recalculate freeFVars in sliceToFunDefs

calcPureDeps :: Module -> Set FName -> Set FName
calcPureDeps md roots = go roots roots
  where
    go seen new
      | Set.null new = seen
      | otherwise =
        let new' = once new `Set.difference` seen
        in go (seen `Set.union` new') new'

    once new = fold (Map.restrictKeys depM new)

    depM = Map.fromList (map mkOne (mGFuns md) ++
                         map mkOne (mFFuns md) ++
                         map mkOne (mBFuns md))
      
    mkOne :: FreeVars e => Fun e -> (FName, Set FName)
    mkOne f = (fName f, freeFVars f)

-- We don't share code with sliceToFun as they are substantially different
funToFunDef :: (Monad m, FreeVars e, TraverseUserTypes e) => (e -> SolverT m SExpr) -> [(String, SExpr)] -> Fun e ->
               SolverT m SMTFunDef
funToFunDef _ _ Fun { fDef = External } =
  panic "Saw an external function" []

funToFunDef sexec extraArgs f@(Fun { fDef = Def body }) = do
  -- FIXME: this should be local to the body, not the context, and we
  -- can't really push as it would forget the defn. when popped
  mapM_ (\n -> modifyCurrentFrame (bindName n (nameToSMTName n))) (fParams f)
  b <- sexec body
  pure SMTFunDef { sfdName = fName f
                 , sfdArgs = args
                 , sfdRet  = symExecTy (fnameType (fName f))
                 , sfdBody = b
                 , sfdPureDeps = freeFVars f
                 , sfdTyDeps   = freeTCons f
                 }
  where
    args = map (\n -> (nameToSMTName n, symExecTy (nameType n))) (fParams f) ++ extraArgs -- For bytesets
    
-- FIXME: maybe calculate some of this once in StrategyM.
-- FIXME: filter by knownFNames here instead of in SolverT 
defineSliceFunDefs :: (MonadIO m, HasGUID m) => Module -> Slice -> SolverT m ()
defineSliceFunDefs md sl = do
  fdefs <- sequence $ foldMap (mkOneF [] symExecExpr) (mFFuns md)
  bdefs <- sequence $ foldMap (mkOneF byteArg (symExecByteSet byteV)) (mBFuns md)  
  let allDefs     = fdefs ++ bdefs
      rFDefs      = topoOrder (\sfd -> (sfdName sfd, sfdPureDeps sfd)) allDefs

  forM_ allDefs $ mapM_ defineSMTTypeDefs . tranclTypeDefs md . sfdTyDeps
    
  mapM_ defineSMTFunDefs rFDefs
  where
    roots = freeFVars sl -- includes grammar calls as well
    allFs = calcPureDeps md roots

    byteN        = "_$b"
    byteV        = S.const byteN
    byteArg      = [(byteN, tByte)]

    mkOneF :: (Monad m, FreeVars e, TraverseUserTypes e) =>
              [(String, SExpr)] -> (e -> ReaderT (Map Name SExpr) (SolverT m) SExpr) ->
              Fun e -> [SolverT m SMTFunDef]
    mkOneF extraArgs sexec f
      | fName f `Set.member` allFs =
          [funToFunDef (\x -> runReaderT (sexec x) mempty) extraArgs f]
      | otherwise                  = []


    -- ppS :: PP a => Set a -> Doc
    -- ppS  = braces . commaSep . map pp . Set.toList

    -- ppM :: (a -> Doc) -> (b -> Doc) ->  Map a b -> Doc
    -- ppM kf vf m = braces (commaSep [ kf x <> " -> " <> vf y | (x, y) <- Map.toList m])
    
    -- depM = Map.fromList (map mkOne (mFFuns md) ++ map mkOne (mBFuns md))
      
    -- mkOne :: FreeVars e => Fun e -> (FName, Set FName)
    -- mkOne f = (fName f, freeFVars f)

-- -----------------------------------------------------------------------------
-- Polymorphic functions

exprToPolyFuns :: Expr -> Set PolyFun
exprToPolyFuns = go
  where
    go expr =
      let children = foldMapChildrenE go expr
      in case expr of
        Ap2 MapMember me _ke ->
          let (kt, vt) = mapTys me
          in Set.insert (PMapMember (symExecTy kt) (symExecTy vt)) children
        Ap2 MapLookup me _ke ->
          let (kt, vt) = mapTys me
          in Set.insert (PMapLookup (symExecTy kt) (symExecTy vt)) children
        Ap3 MapInsert me _ke _ve ->
          let (kt, vt) = mapTys me
          in Set.insert (PMapInsert (symExecTy kt) (symExecTy vt)) children
        _              -> children

    mapTys e = case typeOf e of
      TMap kt vt -> (kt, vt)
      _          -> panic "Expecting a map type" [showPP (typeOf e)]

byteSetToPolyFuns :: ByteSet -> Set PolyFun
byteSetToPolyFuns = ebFoldMapChildrenB exprToPolyFuns byteSetToPolyFuns

defineSlicePolyFuns :: (MonadIO m, HasGUID m) => Slice -> SolverT m ()
defineSlicePolyFuns sl = mapM_ defineSMTPolyFun polys
  where
    polys = go sl
    go sl' = case sl' of
      SDontCare _n sl''  -> go sl''
      SDo _m_x l r       -> go l <> go r
      SUnconstrained     -> mempty
      SLeaf s            -> goL s

    goL l = case l of
      SPure _fset v -> exprToPolyFuns v
      SMatch m      -> byteSetToPolyFuns m
      SAssertion (GuardAssertion e) -> exprToPolyFuns e
      SChoice cs   -> foldMap go cs
      SCall cn     -> foldMap exprToPolyFuns (callNodeActualArgs cn)
      SCase _ c@(Case e _) -> exprToPolyFuns e <> foldMap go c
      SInverse _n fe pe -> exprToPolyFuns fe <> exprToPolyFuns pe
        
      
