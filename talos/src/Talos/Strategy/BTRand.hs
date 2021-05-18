{-# Language OverloadedStrings #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.BTRand (randDFS, randRestart, randMaybeT, mkStrategyFun) where

import Control.Monad.Reader

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.List (foldl', foldl1', partition)
import qualified Data.Map as Map

import Daedalus.Panic
import qualified Daedalus.Value as I

import Daedalus.Core hiding (streamOffset)
import qualified Daedalus.Core.Semantics.Grammar as I
import qualified Daedalus.Core.Semantics.Expr as I
import qualified Daedalus.Core.Semantics.Env as I

import Talos.Analysis.EntangledVars 
import Talos.Analysis.Slice
import Talos.SymExec.Path
import Talos.Strategy.Monad
import Talos.Strategy.DFST

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

randDFS :: Strategy
randDFS = 
  Strategy { stratName  = "rand-dfs"
           , stratDescr = "Simple depth-first random generation"
           , stratFun   = SimpleStrat $ \ptag sl -> runDFST (go ptag sl) (return . Just) (return Nothing)
           }
  where
    go :: ProvenanceTag -> Slice -> DFST (Maybe SelectedPath) StrategyM SelectedPath
    go ptag sl = mkStrategyFun ptag sl

-- ----------------------------------------------------------------------------------------
-- Restarting strat (restart-on-failure)

randRestart :: Strategy
randRestart = 
  Strategy { stratName  = "rand-restart"
           , stratDescr = "Restart on failure with random selection"
           , stratFun   = SimpleStrat randRestartStrat
           }

restartBound :: Int
restartBound = 1000

randRestartStrat :: ProvenanceTag -> Slice -> StrategyM (Maybe SelectedPath)
randRestartStrat ptag sl = go restartBound
  where
    go 0 = pure Nothing
    go n = do
      m_p <- once
      case m_p of
        Just {} -> pure m_p
        Nothing -> go (n - 1)
    
    once :: StrategyM (Maybe SelectedPath)
    once = runRestartT (mkStrategyFun ptag sl) (return . Just) (return Nothing)

-- ----------------------------------------------------------------------------------------
-- Local backtracking, restart

randMaybeT :: Strategy
randMaybeT = 
  Strategy { stratName  = "rand-restart-local-bt"
           , stratDescr = "Backtrack locally on failure, restart on (global) failure with random selection"
           , stratFun   = SimpleStrat randMaybeStrat
           }

randMaybeStrat :: ProvenanceTag -> Slice -> StrategyM (Maybe SelectedPath)
randMaybeStrat ptag sl = go restartBound
  where
    go 0 = pure Nothing
    go n = do
      m_p <- once
      case m_p of
        Just {} -> pure m_p
        Nothing -> go (n - 1)
    
    once :: StrategyM (Maybe SelectedPath)
    once = runMaybeT (mkStrategyFun ptag sl)
  
-- ----------------------------------------------------------------------------------------
-- Main functions
             
-- A family of backtracking strategies indexed by a MonadPlus, so MaybeT StrategyM should give DFS
mkStrategyFun :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> Slice -> m SelectedPath
mkStrategyFun ptag sl = do
  env0 <- getIEnv -- for pure function implementations
  snd <$> runReaderT (stratSlice ptag sl) env0 

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
          
        SUnconstrained  -> pure (I.vUnit, Unconstrained)
        SLeaf sl'        -> onSlice (flip pathNode Unconstrained) <$> goLeaf sl'
        
    goLeaf sl =
      case sl of
        SPure e -> do
          v <- synthesiseExpr e
          pure (uncPath v)

        SMatch bset -> do
          env <- ask
          -- Run the predicate over all bytes.
          -- FIXME: Too brute force? We could probably be smarter
          let bs = filter (I.evalByteSet bset env) [0 .. 255]
          guard (bs /= [])
          b <- choose bs -- select a byte from the set, backtracking
          -- liftStrategy (liftIO $ putStrLn ("Chose byte " ++ show b))
          pure (I.vUInt 8 (fromIntegral b), SelectedMatch ptag (BS.singleton b))

        -- SMatch (MatchBytes e) -> do
        --   v <- synthesiseExpr e
        --   let bs = I.valueToByteString v
        --   pure (v, SelectedMatch ptag bs)

        -- SMatch {} -> unimplemented
          
        SAssertion (GuardAssertion e) -> do
          b <- I.valueToBool <$> synthesiseExpr e
          guard b
          pure (uncPath I.vUnit)

        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          -- liftStrategy (liftIO $ putStrLn ("Chose choice " ++ show i))
          onSlice (SelectedChoice i) <$> go sl'

        SCall cn -> ask >>= stratCallNode ptag cn

        SCase _ c -> do
          env <- ask
          I.evalCase (\(i, sl') _env -> onSlice (SelectedCase i) <$> go sl' ) mzero (enumerate c) env

    uncPath v = (v, SelectedDo Unconstrained)
          
    onSlice f = \(a, sl') -> (a, f sl')

    -- unimplemented = panic "Unimplemented" []

-- Synthesise for each call

-- Adding field sensitivity means that there can be multiple result
-- slices, with non-overlapping projections.  As a result, we need to
-- merge the resulting slices.
--
-- Merging all slices could introduce spurious internal backtracking,
-- although it is not clear whether that is an issue or not.

stratCallNode :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> CallNode -> I.Env -> 
                 ReaderT I.Env m (I.Value, SelectedNode)
stratCallNode ptag CallNode { callName = fn, callClass = cl, callAllArgs = allArgs, callPaths = paths } env = do
  (_, nonRes) <- unzip <$> mapM (uncurry doOne) asserts
  (v, res)    <- case results of
    [] -> pure (I.vUnit, Unconstrained)
    _  -> do sl <- foldl1' merge <$> mapM (getParamSlice fn cl . fst) results -- merge slices
             let evs = foldMap (callParams . snd) results
             local (const (evsToEnv evs)) (stratSlice ptag sl)

  pure (v, SelectedCall cl (foldl' merge res nonRes))
  where
    -- This works because 'ResultVar' is < than all over basevars
    (results, asserts) = partition (\(ev, _) -> baseVar ev == ResultVar) (Map.toList paths)
        
    doOne ev CallInstance { callParams = evs {- , callSlice = sl -} } = do
      sl <- getParamSlice fn cl ev
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
  foldr mplus mzero (map pure bs')

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
    
-- =============================================================================
-- Restart monad transformer
--
-- This is similar to the list monad, but it wraps another monad and
-- hence has to be a bit careful about what to do when --- if we use
-- ListT, we get effects from all the alternatives, which could be
-- expensive.  This is similar to ContT, but we also keep around a
-- failure continuation.
--

-- The dfsCont takes the return value, and also an updated failure
-- continuation, as we may want to backtrack into a completed
-- computation.
data RestartTContext r m a =
  RestartTContext { randCont   :: a -> m r
                  , randEscape :: m r
                  }

newtype RestartT r m a = RestartT { getRestartT :: RestartTContext r m a -> m r }

runRestartT :: RestartT r m a -> (a -> m r) -> m r -> m r
runRestartT m cont fl = getRestartT m (RestartTContext (\v -> cont v) fl)

instance Functor (RestartT r m) where
  fmap f (RestartT m) = RestartT $ \ctxt -> m (ctxt { randCont = randCont ctxt . f })

instance Applicative (RestartT r m) where
  pure v              = RestartT $ \ctxt -> randCont ctxt v
  (<*>)               = ap

instance Monad (RestartT r m) where
  RestartT m >>= f = RestartT $ \ctxt ->
    let cont v = getRestartT (f v) ctxt
    in m (ctxt { randCont = cont })

-- | We want
--
-- (a `mplus` b) >>= f == (a >>= f) `mplus` (b >>= f)
--
-- i.e., we give f the result of a, but if that fails, we run f on b's
-- result.

instance Alternative (RestartT r m) where
  m1 <|> _m2 = m1 
  empty = RestartT randEscape

instance MonadPlus (RestartT r m) where -- default body (Alternative)
                     
instance MonadTrans (RestartT r) where
  lift m = RestartT $ \ctxt -> m >>= \v -> randCont ctxt v
  
instance LiftStrategyM m => LiftStrategyM (RestartT r m) where
  liftStrategy m = lift (liftStrategy m)
    





        
        
        

          

