
-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.BTRand where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Map as Map


import Daedalus.Panic

import Daedalus.Core hiding (streamOffset)
import qualified Daedalus.Core.Semantics.Grammar as I
import qualified Daedalus.Core.Semantics.Expr as I
import qualified Daedalus.Core.Semantics.Value as I
import qualified Daedalus.Core.Semantics.Env as I

import Talos.Analysis.EntangledVars 
import Talos.Analysis.Slice
import Talos.SymExec.Path
import Talos.Strategy.Monad

-- A family of backtracking strategies indexed by a MonadPlus, so MaybeT StrategyM should give DFS
mkStrategy :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> Slice -> m SelectedPath
mkStrategy ptag = fmap snd . flip runReaderT I.emptyEnv . stratSlice ptag

stratSlice :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> Slice
           -> ReaderT I.Env m (I.Value, SelectedPath)
stratSlice ptag = go
  where
    go sl = 
      case sl of
        SDontCare n sl' -> onSlice (dontCare n) <$> go sl'
        SDo m_x lsl rsl -> do
          (v, lpath)  <- go lsl
          onSlice (pathNode (SelectedDo lpath)) <$> bindInMaybe m_x v (go rsl)
          
        SUnconstrained  -> pure (I.VUnit, Unconstrained)
        SLeaf sl'        -> onSlice (flip pathNode Unconstrained) <$> goLeaf sl'
        
    goLeaf sl =
      case sl of
        SPure e -> do
          v <- synthesiseExpr e
          pure (uncPath v)

        SMatch (MatchByte bset) -> do
          env <- ask
          -- Run the predicate over all bytes.
          -- FIXME: Too brute force? We could probably be smarter
          let bs = filter (I.evalByteSet bset env) [0 .. 255]
          guard (bs /= [])
          b <- choose bs -- select a byte from the set, backtracking
          pure (I.vUInt 8 (fromIntegral b), SelectedMatch ptag (BS.singleton b))

        SMatch (MatchBytes e) -> do
          v <- synthesiseExpr e
          let bs = I.fromVByteArray v
          pure (v, SelectedMatch ptag bs)

        SMatch {} -> unimplemented
          
        SAssertion (GuardAssertion e) -> do
          b <- I.fromVBool <$> synthesiseExpr e
          guard b
          pure (uncPath I.VUnit)

        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          onSlice (SelectedChoice i) <$> go sl'

        SCall cn -> ask >>= stratCallNode ptag cn

        SCase _ c -> do
          env <- ask
          I.evalCase (\(i, sl') _env -> onSlice (SelectedCase i) <$> go sl' ) mzero (enumerate c) env

    uncPath v = (v, SelectedDo Unconstrained)
          
    onSlice f = \(a, sl') -> (a, f sl')

    unimplemented = panic "Unimplemented" []

-- Synthesise for each call 
stratCallNode :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> CallNode -> I.Env -> 
                 ReaderT I.Env m (I.Value, SelectedNode)
stratCallNode ptag CallNode { callClass = cl, callAllArgs = allArgs, callPaths = paths } env = do
  (_, nonRes) <- unzip <$> mapM doOne (Map.elems lpaths ++ Map.elems rpaths)
  (v, res)    <- maybe (pure (I.VUnit, Unconstrained)) doOne m_rsl
  pure (v, SelectedCall cl (foldl' merge res nonRes))
  where
    (lpaths, m_rsl, rpaths) = Map.splitLookup ResultVar paths
    
    doOne CallInstance { callParams = evs, callSlice = sl } =
      local (const (evsToEnv evs)) (stratSlice ptag sl)

    -- we rely on laziness to avoid errors in computing values with free variables
    allArgsV     = flip I.eval env <$> allArgs
    evsToEnv evs = env { I.vEnv = Map.restrictKeys allArgsV (programVars evs) }

-- ----------------------------------------------------------------------------------------
-- Strategy helpers

-- Backtracking choice + random permutation
choose :: (MonadPlus m, LiftStrategyM m) => [a] -> m a
choose bs = do
  bs' <- randPermute bs
  foldl (<|>) mzero (map pure bs')

-- ----------------------------------------------------------------------------------------
-- Environment helpers

bindInMaybe :: Monad m => Maybe Name -> I.Value -> ReaderT I.Env m a -> ReaderT I.Env m a
bindInMaybe Nothing  _ m = m
bindInMaybe (Just x) v m = local upd m
  where
    upd e = e { I.vEnv = Map.insert x v (I.vEnv e) }

synthesiseExpr :: Monad m => Expr -> ReaderT I.Env m I.Value
synthesiseExpr e = I.eval e <$> ask

-- ----------------------------------------------------------------------------------------
-- Utils

enumerate :: Traversable t => t a -> t (Int, a)
enumerate t = evalState (traverse go t) 0
  where
    go a = state (\i -> ((i, a), i + 1))







        
        
        

          
