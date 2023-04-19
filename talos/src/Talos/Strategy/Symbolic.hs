{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language OverloadedStrings #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.Symbolic (symbolicStrat) where

import           Control.Lens                 (_3, over)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor               (second)
import qualified Data.ByteString              as BS
import           Data.Foldable                (asum, fold)
import           Data.Functor                 (($>))
import           Data.Functor.Identity        (Identity (Identity))
import           Data.Generics.Product        (field)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Word                    (Word8)
import           GHC.Generics                 (Generic)
import qualified SimpleSMT                    as S

import           Daedalus.Core                hiding (streamOffset, tByte)
import           Daedalus.Core.Free           (FreeVars, freeVars)
import qualified Daedalus.Core.Semantics.Env  as I
import qualified Daedalus.Core.Semantics.Expr as I
import           Daedalus.Core.Type
import           Daedalus.Panic
import           Daedalus.Rec                 (topoOrder)
import qualified Daedalus.Value               as I

import           Talos.Analysis.Exported      (ExpCallNode (..), ExpSlice,
                                               SliceId, sliceToCallees)
import           Talos.Analysis.Slice
import           Talos.Strategy.MemoSearch    (memoSearch, randAccelDFSPolicy,
                                               randDFSPolicy, randRestartPolicy)
import           Talos.Strategy.Monad
import           Talos.Strategy.OptParser     (Opt, parseOpts)
import qualified Talos.Strategy.OptParser     as P
import           Talos.Strategy.SymbolicM
import           Talos.SymExec.Expr           (symExecCaseAlts)
import           Talos.SymExec.Funs           (defineSliceFunDefs,
                                               defineSlicePolyFuns)
import           Talos.SymExec.ModelParser    (evalModelP, pByte, pValue)
import           Talos.SymExec.Path
import           Talos.SymExec.SemiExpr
import           Talos.SymExec.SemiValue      as SE
import           Talos.SymExec.SolverT        (SolverT, declareName,
                                               declareSymbol, liftSolver, reset,
                                               scoped)
import qualified Talos.SymExec.SolverT        as Solv
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type           (defineSliceTypeDefs, symExecTy)

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

symbolicStrat:: Strategy
symbolicStrat = Strategy
  { stratName = name
  , stratDescr = descr
  , stratParse = mkinst
  }
  where
    name  = "randsymb"
    descr = "Random symbolic execution"
    
    --, ("bfs", bfs), ("rand-dfs", randDFS), ("rand-restart", randRestart)    

    -- strats = [ ("dfs", dfs)
    --          , ("memo-rand-restart", memoSearch randRestartPolicy)
    --          , ("memo-rand-dfs",     memoSearch randDFSPolicy)
    --          , ("memo-rand-accel-dfs", memoSearch randAccelDFSPolicy)
    --          ]
    
    -- mkOne :: (Doc, SearchStrat) -> Strategy
    -- mkOne (name, sstrat) =
    --   Strategy { stratName  = "symbolic-" ++ show name
    --            , stratDescr =  (" <> name <> ")
    --            , stratFun   = SolverStrat (symbolicFun sstrat)
    --        }
      
    mkinst = do
      c <- parseOpts configOpts defaultConfig
      
      pure StrategyInstance
        { siName  = name
        , siDescr = descr -- <> parens (text s)
        , siFun   = symbolicFun c
        }

-- ----------------------------------------------------------------------------------------
-- Configuration

data Config = Config { cSearchStrat :: SearchStrat }
  deriving (Generic)

defaultConfig :: Config
defaultConfig = Config { cSearchStrat = dfs }

configOpts :: [Opt Config]
configOpts = [P.option "strat" (field @"cSearchStrat") parseStrat ]
  where
    parseStrat =
      asum [ P.tokenP "dfs"                 $> dfs 
           , P.tokenP "memo-rand-restart"   $> memoSearch randRestartPolicy
           , P.tokenP "memo-rand-dfs"       $> memoSearch randDFSPolicy
           , P.tokenP "memo-rand-accel-dfs" $> memoSearch randAccelDFSPolicy
           ]
      
-- ----------------------------------------------------------------------------------------
-- Main functions

-- FIXME: define all types etc. eagerly
symbolicFun :: Config ->
               ProvenanceTag ->
               ExpSlice ->
               StratGen
symbolicFun (Config sstrat) ptag sl = StratGen $ do
  -- defined referenced types/functions
  reset -- FIXME

  md <- getModule
  deps <- sliceToDeps sl
  let slAndDeps = map snd deps ++ [sl]

  forM_ slAndDeps $ \sl' -> do
    defineSliceTypeDefs md sl'
    defineSlicePolyFuns sl'
    defineSliceFunDefs md sl'

  -- FIXME: this should be calculated once, along with how it is used by e.g. memoSearch
  scoped $ do
    let topoDeps = topoOrder (second sliceToCallees) deps
    m_res <- runSymbolicM sstrat (sl, topoDeps) (stratSlice ptag sl <* check)
    case m_res of
      Nothing         -> pure ([], Nothing)
      Just (_, _, r)  -> (\m -> ([m], Nothing)) <$> buildPath r

-- FIXME: we could get all the sexps from the solver in a single query.
buildPath :: PathBuilder -> SolverT StrategyM SelectedPath
buildPath = traverse resolveResult
  where
    resolveResult :: SolverResult -> SolverT StrategyM BS.ByteString
    resolveResult (ByteResult b) = BS.singleton <$> byteModel b
    resolveResult (InverseResult env ifn) = do
      venv <- traverse valueModel env
      ienv <- liftStrategy getIEnv -- for fun defns
      let ienv' = ienv { I.vEnv = venv }
      pure (I.valueToByteString (I.eval ifn ienv'))

-- We need to get types etc for called slices (including root slice)
sliceToDeps :: (Monad m, LiftStrategyM m) => ExpSlice -> m [(SliceId, ExpSlice)]
sliceToDeps sl = go Set.empty (sliceToCallees sl) []
  where
    go seen new acc
      | Just (n, rest) <- Set.minView new = do
          sl' <- getSlice n
          let new' = sliceToCallees sl' `Set.difference` seen
          go (Set.insert n seen) (new' `Set.union` rest) ((n, sl') : acc)

    go _ _ acc = pure acc

-- We return the symbolic value (which may contain bound variables, so
-- those have to be all at the top level) and a computation for
-- constructing the path --- we do it in this way to avoid doing a lot
-- of work that gets thrown away on backtracking, so this gets run
-- once in the final (satisfying) state.  Note that the path
-- construction code shouldn't backtrack, so it could be in (SolverT IO)

choiceIdsFor :: FreeVars v => v -> SymbolicM (Set ChoiceId)
choiceIdsFor v = do
  e_cids <- fold <$> mapM getNameDeps (Set.toList (freeVars v))
  path_cids <- getPathDeps
  pure (e_cids <> path_cids)

stratSlice :: ProvenanceTag -> ExpSlice -> SymbolicM Result
stratSlice ptag = go
  where
    go sl =  do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SHole -> pure (vUnit, mempty, SelectedHole)

        SPure e -> do
          -- We could do this a lot more efficiently if we calculated
          -- as we go, for example.
          cids <- choiceIdsFor e          
          let mk v = (v, cids, SelectedHole)
          mk <$> synthesiseExpr e

        SDo x lsl rsl -> do
          bindNameIn x (go lsl)
            (\lpath -> over _3 (SelectedDo lpath) <$> go rsl)

        -- FIXME: we could pick concrete values here if we are willing
        -- to branch and have a concrete bset.
        SMatch bset -> do
          bname <- vSExpr (TUInt (TSize 8)) . S.const <$> liftSolver (declareSymbol "b" tByte)
          bassn <- synthesiseByteSet bset bname
          -- This just constrains the byte, we expect it to be satisfiable
          -- (byte sets are typically not empty)
          assert bassn
          -- inSolver check -- required?

          -- FIXME: Probably not needed?  Only if we case over a byte ...
          cids <- choiceIdsFor bset
          pure (bname, cids, SelectedBytes ptag (ByteResult bname))

        -- SAssertion e -> do
        --   se <- synthesiseExpr e
        --   assert se
        --   check
        --   pure (uncPath vUnit)

        SChoice sls -> do
          (cid, (i, sl')) <- choose (enumerate sls) -- select a choice, backtracking
          -- liftIO (putStrLn ("Chose " ++ show i ++ " " ++ showPP sl'))
          -- e <- ask
          -- liftIO (print [ (pp n, v) | (n, v) <- Map.toList e] )
          enterPathNode (Set.singleton cid) $
            over _3 (SelectedChoice . PathIndex i) <$> go sl'

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> stratCase ptag c

        SInverse n ifn p -> do
          n' <- vSExpr (typeOf n) . S.const <$> liftSolver (declareName n (symExecTy (typeOf n)))
          primBindName n n' mempty $ do
            pe <- synthesiseExpr p
            assert pe
            check -- FIXME: necessary?

            -- Once we have a model, we need to convert all the free
            -- variables in the inverse function expression into values,
            -- and then call the inverse function in the _interpreter_.
            -- Note that n is free in ifn.
            -- 
            -- We could also have the solver execute the function for
            -- us.
            --
            -- We need to resolve any locally bound variables before we
            -- return the path constructor.

            let fvM = Map.fromSet id $ freeVars ifn
            -- Resolve all Names (to semisexprs and solver names)
            venv <- traverse getName fvM
            cids <- choiceIdsFor p
            pure (n', cids, SelectedBytes ptag (InverseResult venv ifn))

-- FIXME: maybe name e?
stratCase :: ProvenanceTag -> Case ExpSlice -> SymbolicM Result
stratCase ptag cs = do
  m_alt <- liftSemiSolverM (semiExecCase cs)
  var_cids  <- getNameDeps (caseVar cs)
  path_cids <- getPathDeps
  let cids = var_cids <> path_cids
  case m_alt of
    DidMatch _i sl -> over _3 (SelectedCase . Identity) <$> enterPathNode cids (stratSlice ptag sl)
    NoMatch  -> do
      -- x <- fmap (text . flip S.ppSExpr "" . typedThing) <$> getName (caseVar cs)
      -- liftIO $ print ("No match for " <> pp (caseVar cs) <> " = " <> pp x $$ pp cs)
      
      backtrack (ConcreteFailure cids) -- just backtrack, no cases matched
      
    TooSymbolic -> do
      ps <- liftSemiSolverM (symExecToSemiExec (symExecCaseAlts cs))
      (cid, (p, sl)) <- choose ps
      enterPathNode (Set.singleton cid) $ do
        assert (vSExpr TBool p)
        check
        over _3 (SelectedCase . Identity) <$> stratSlice ptag sl

-- FIXME: this is copied from BTRand
stratCallNode :: ProvenanceTag -> ExpCallNode ->
                 SymbolicM Result
stratCallNode ptag cn = do
  -- liftIO $ print ("Entering: " <> pp cn)
  sl <- getSlice (ecnSliceId cn)
  over _3 (SelectedCall (ecnIdx cn)) <$> enterFunction (ecnParamMap cn) (stratSlice ptag sl)

-- ----------------------------------------------------------------------------------------
-- Solver helpers

synthesiseExpr :: Expr -> SymbolicM SemiSExpr
synthesiseExpr e = do
  liftSemiSolverM . semiExecExpr $ e

synthesiseByteSet :: ByteSet -> SemiSExpr -> SymbolicM SemiSExpr
synthesiseByteSet bs = liftSemiSolverM . semiExecByteSet bs

check :: SymbolicM ()
check = do
  r <- liftSolver Solv.check
  case r of
    S.Sat     -> pure ()
    S.Unsat   -> backtrack OtherFailure
    S.Unknown -> do
      -- liftIO $ putStrLn "**  Solver returned UNKNOWN **"
      backtrack OtherFailure

assert :: SemiSExpr -> SymbolicM ()
assert sv =
  case sv of
    VOther p -> liftSolver (Solv.assert (typedThing p))
    VValue (I.VBool True) -> pure ()
    -- FIXME: Set.empty is not quite right here, but this will usually
    -- not get here anyway.
    VValue (I.VBool False) -> backtrack (ConcreteFailure Set.empty)
    _ -> panic "Malformed boolean" [show sv]

-- ----------------------------------------------------------------------------------------
-- Utils

enumerate :: Traversable t => t a -> t (Int, a)
enumerate t = evalState (traverse go t) 0
  where
    go a = state (\i -> ((i, a), i + 1))

-- THis all happens after we have finished, so we need to be a bit
-- careful about what is in scope (only thins in the current solver
-- frame, i.e., not do-bound variables.)

byteModel :: SemiSExpr -> SolverT StrategyM Word8
byteModel (VOther symB) = do
  sexp <- Solv.getValue (typedThing symB)
  case evalModelP pByte sexp of
    [] -> panic "No parse" []
    b : _ -> pure b
-- probably shouldn't happen    
byteModel sv = panic "Unimplemented" [show sv]

valueModel :: SemiSExpr -> SolverT StrategyM I.Value
valueModel (VValue v)    = pure v
valueModel sv =
  SE.toValue <$> traverse go sv
  where
    go tse = do
      sexp <- Solv.getValue (typedThing tse)
      case evalModelP (pValue (typedType tse)) sexp of
        [] -> panic "No parse" []
        v : _ -> pure v












