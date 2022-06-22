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

import           Daedalus.Core                   hiding (freshName, tByte)
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
import Talos.Analysis.Exported (ExpSlice)

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

funToFunDef :: (Monad m, FreeVars e, TraverseUserTypes e, HasGUID m) =>
               (e -> ReaderT (Map Name SExpr) (SolverT m) SExpr) ->
               [(String, SExpr)] -> Fun e ->
               SolverT m SMTFunDef
funToFunDef _ _ Fun { fDef = External } =
  panic "Saw an external function" []

funToFunDef sexec extraArgs f@(Fun { fDef = Def body }) = do
  args <- mapM freshName (fParams f)
  let args' = zip args (map (symExecTy . nameType) (fParams f)) ++ extraArgs
      e     = Map.fromList (zip (fParams f) (map S.const args))
  b <- runReaderT (sexec body) e
  pure SMTFunDef { sfdName = fName f
                 , sfdArgs = args'
                 , sfdRet  = symExecTy (fnameType (fName f))
                 , sfdBody = b
                 , sfdPureDeps = freeFVars f
                 , sfdTyDeps   = freeTCons f
                 }
    
-- FIXME: maybe calculate some of this once in StrategyM.
-- FIXME: filter by knownFNames here instead of in SolverT 
defineSliceFunDefs :: (MonadIO m, HasGUID m, HasGUID m) =>
                      Module -> ExpSlice -> SolverT m ()
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

    mkOneF :: (Monad m, FreeVars e, TraverseUserTypes e, HasGUID m) =>
              [(String, SExpr)] ->
              (e -> ReaderT (Map Name SExpr) (SolverT m) SExpr) ->
              Fun e -> [SolverT m SMTFunDef]
    mkOneF extraArgs sexec f
      | fName f `Set.member` allFs = [funToFunDef sexec extraArgs f]
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

defineSlicePolyFuns :: (MonadIO m, HasGUID m) => ExpSlice -> SolverT m ()
defineSlicePolyFuns sl = mapM_ defineSMTPolyFun polys
  where
    polys = go sl
    go sl' = case sl' of
      SHole             -> mempty
      -- We turns an SLExpr into an Expr, replacing Holes with Units.
      -- This is a bit gross, but should be OK for this purpose.
      SPure e           -> exprToPolyFuns e
      SDo _m_x l r      -> go l <> go r
      SMatch m          -> byteSetToPolyFuns m
--      SAssertion  e     -> exprToPolyFuns e
      SChoice cs        -> foldMap go cs
      SCall {}          -> mempty
      SCase _ c         -> foldMap go c
      SInverse _n fe pe -> exprToPolyFuns fe <> exprToPolyFuns pe
        
      
