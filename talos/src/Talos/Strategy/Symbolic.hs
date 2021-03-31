{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.Symbolic (symbolicStrat) where

import Control.Applicative
import Control.Monad.Reader

import Control.Monad.State
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Map as Map

import Data.Word (Word8)

import Daedalus.Panic
import Daedalus.PP hiding (empty)

import Daedalus.Core hiding (streamOffset)
import Daedalus.Core.Type
  
import SimpleSMT (SExpr)
import qualified SimpleSMT as S

import Talos.Analysis.EntangledVars 
import Talos.Analysis.Slice
import Talos.SymExec.Path
import Talos.Strategy.Monad

import Talos.SymExec.SolverT (SolverT, push, pop, scoped, defineName, declareSymbol, assert)
import qualified Talos.SymExec.SolverT as Solv
import Talos.SymExec.StdLib
import Talos.SymExec.Core
import Talos.SymExec.ModelParser (evalModelP, pByte)

import Talos.Strategy.DFST

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

symbolicStrat :: Strategy
symbolicStrat = 
  Strategy { stratName  = "symbolic"
           , stratDescr = "Symbolic execution"
           , stratFun   = SolverStrat symbolicFun
           }
  
-- ----------------------------------------------------------------------------------------
-- Main functions

-- FIXME: define all types etc. eagerly
symbolicFun :: ProvenanceTag -> Slice -> SolverT StrategyM (Maybe SelectedPath)
symbolicFun ptag sl = do
  -- defined referenced types/functions
  md <- getModule
  defineSliceTypeDefs md sl
  defineSliceFunDefs md sl
  scoped $ runSymbolicM $ do
    (_, pathM) <- stratSlice ptag sl
    check -- FIXME: only required if there was no recent 'check' command
    pathM


-- We return the symbolic value (which may contain bound variables, so
-- those have to be all at the top level) and a computation for
-- constructing the path --- we do it in this way to avoid doing a lot
-- of work that gets thrown away on backtracking, so this gets run
-- once in the final (satisfying) state.  Note that the path
-- construction code shouldn't backtrack, so it could be in (SolverT IO)

stratSlice :: ProvenanceTag -> Slice -> SymbolicM (SExpr, SymbolicM SelectedPath)
stratSlice ptag = go
  where
    go sl =  do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SDontCare n sl' -> onSlice (fmap (dontCare n)) <$> go sl'
        SDo m_x lsl rsl -> do
          (v, lpath)  <- go lsl
          -- bind name
          maybe (pure ()) (\x -> void $ solverOp (defineName x (symExecTy (typeOf x)) v)) m_x
          
          onSlice (\rhs -> pathNode <$> (SelectedDo <$> lpath) <*> rhs) <$> go rsl
          
        SUnconstrained  -> pure (sUnit, pure Unconstrained)
        SLeaf sl'       -> onSlice (fmap (flip pathNode Unconstrained)) <$> goLeaf sl'
        
    goLeaf sl = do
      -- liftIO (putStrLn "Leaf" >> print (pp sl))
      case sl of
        SPure e -> uncPath <$> synthesiseExpr e

        SMatch (MatchByte bset) -> do
          bname <- solverOp (declareSymbol "b" tByte)
          bassn <- solverOp (symExecByteSet bset bname)
          solverOp (assert bassn)
          -- solverOp check -- required?          
          pure (bname, SelectedMatch ptag . BS.singleton <$> byteModel bname)
  
        SMatch (MatchBytes _e) -> unimplemented -- should probably not happen?
        SMatch {} -> unimplemented
          
        SAssertion (GuardAssertion e) -> do
          se <- synthesiseExpr e
          solverOp (assert se)
          check          
          pure (uncPath sUnit)

        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          onSlice (fmap (SelectedChoice i)) <$> go sl'

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> stratCase ptag c

    uncPath v = (v, pure (SelectedDo Unconstrained))

    unimplemented = panic "Unimplemented" []

onSlice :: (SymbolicM a -> SymbolicM b)
        -> (c, SymbolicM a) -> (c, SymbolicM b)
onSlice f = \(a, sl') -> (a, f sl')

-- FIXME: maybe name e?
stratCase :: ProvenanceTag -> Case Slice -> SymbolicM (SExpr, SymbolicM SelectedNode)
stratCase ptag (Case e ps) = do
  se <- solverOp (symExec e)
  (i, (p, sl)) <- choose (enumerate ps)
  let assn = case p of
        PBool b  -> if b then se else S.not se
        PNothing -> S.fun "is-Nothing" [se]
        PJust    -> S.fun "is-Just" [se]
        PNum n   -> S.eq se (mkLit n)
        PCon l   -> S.fun ("is-" ++ labelToField tyName l) [se]
        PAny     -> S.bool True

  solverOp (assert assn)
  check
  onSlice (fmap (SelectedCase i)) <$> stratSlice ptag sl
  where
    ty = typeOf e
    mkLit n = -- FIXME: cut from Core.hs
      symExecOp0 (IntL n ty)

    tyName = case ty of
      TUser ut -> utName ut
      _        -> panic "Not a user type" [showPP ty]

-- Synthesise for each call 
stratCallNode :: ProvenanceTag -> CallNode -> SymbolicM (SExpr, SymbolicM SelectedNode)
stratCallNode ptag CallNode { callName = fn, callClass = cl, callAllArgs = allArgs, callPaths = paths } = do
  -- define all arguments.  We need to evaluate the args in the
  -- current env, as in recursive calls we will have shadowing (in the SolverT env.)
  allUsedArgsV <- traverse (solverOp . symExec) allUsedArgs
  Map.foldMapWithKey defineName' allUsedArgsV
  (_, nonRes) <- unzip <$> mapM (uncurry doOne) (Map.toList lpaths ++ Map.toList rpaths)
  (v, res)    <- maybe (pure (sUnit, pure Unconstrained)) (doOne ResultVar) m_rsl
  pure (v, SelectedCall cl <$> (foldl' merge <$> res <*> sequence nonRes))
  where
    (lpaths, m_rsl, rpaths) = Map.splitLookup ResultVar paths

    defineName' x e = void $ solverOp (defineName x (symExecTy (typeOf x)) e)
    
    doOne ev _ = do
      sl <- getParamSlice fn cl ev
      stratSlice ptag sl


    allUsedParams = foldMap (programVars . callParams) paths
    allUsedArgs   = Map.restrictKeys allArgs allUsedParams

-- ----------------------------------------------------------------------------------------
-- Strategy helpers

-- Backtracking choice + random permutation
choose :: (MonadPlus m, LiftStrategyM m) => [a] -> m a
choose bs = do
  bs' <- randPermute bs
  foldr mplus doFail (map pure bs')
  where
    doFail = do
      -- liftStrategy (liftIO (putStrLn "failing"))
      mzero

-- ----------------------------------------------------------------------------------------
-- Solver helpers

synthesiseExpr :: SymExec a => a -> SymbolicM SExpr
synthesiseExpr e = solverOp (symExec e)

check :: SymbolicM ()
check = do
  r <- solverOp Solv.check
  case r of
    S.Sat     -> pure ()
    S.Unsat   -> mzero
    S.Unknown -> mzero
    

-- ----------------------------------------------------------------------------------------
-- Utils

enumerate :: Traversable t => t a -> t (Int, a)
enumerate t = evalState (traverse go t) 0
  where
    go a = state (\i -> ((i, a), i + 1))

solverOp :: SolverT StrategyM a -> SymbolicM a
solverOp = SymbolicM . lift

getValue :: SExpr -> SymbolicM SExpr
getValue v = solverOp (Solv.getValue v)

byteModel :: SExpr -> SymbolicM Word8
byteModel symB = do
  sexp <- getValue symB
  case evalModelP pByte sexp of
    [] -> panic "No parse" []
    b : _ -> pure b

-- =============================================================================
-- Symbolic monad
--
-- We need:

--  * An environment mapping DDL variables to SMT variables --- note
--    we unfold loops etc. so we can have multiple occurences of the
--    same (DDL) variable.
--  * A continuation for the failure and success cases.  We will need
--    to be careful to pop contexts appropriately.
--  * A StrategyM

newtype SymbolicM a = SymbolicM { _getSymbolicM :: DFST (Maybe SelectedPath) (SolverT StrategyM) a }
  deriving (Applicative, Functor, Monad, MonadIO, LiftStrategyM)

runSymbolicM :: SymbolicM SelectedPath -> SolverT StrategyM (Maybe SelectedPath)
runSymbolicM (SymbolicM m) = runDFST m (pure . Just) (pure Nothing) 

instance Alternative SymbolicM where
  (SymbolicM m1) <|> (SymbolicM m2) = SymbolicM $ bracketS m1 <|> bracketS m2
    where
      bracketS m = do
        lift push
        m `onBacktrack` popFail
      popFail = lift pop >> mzero
    
  empty = SymbolicM empty
  
instance MonadPlus SymbolicM where -- default body (Alternative)
    
instance Semigroup a => Semigroup (SymbolicM a) where 
  m1 <> m2 = (<>) <$> m1 <*> m2

instance Monoid a => Monoid (SymbolicM a) where 
  mempty = pure mempty




        
        
        

          


