{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
{-# Language GeneralisedNewtypeDeriving #-}
{-# Language RankNTypes #-}
{-# Language PatternSynonyms #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language KindSignatures #-}
{-# Language ScopedTypeVariables #-}
{-# Language ViewPatterns #-}
{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.What4Sym (randDFS, randRestart, randMaybeT, mkStrategyFun) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString                 as BS
import           Data.Functor.Identity           (Identity (Identity))
import qualified Data.Map                        as Map
import qualified Data.IORef as IO

import qualified What4.Interface                 as W4
import           Data.Parameterized.NatRepr

import           Daedalus.Core                   hiding (streamOffset)
import qualified Daedalus.Core.Semantics.Env     as I
import qualified Daedalus.Core.Semantics.Expr    as I
import qualified Daedalus.Core.Semantics.Grammar as I
import qualified Daedalus.Core.Type              as I
import qualified Daedalus.Core.Basics            as I
import qualified Daedalus.Value                  as I
import           Daedalus.Panic
import           Daedalus.PP

import           Talos.Analysis.Exported         (ExpCallNode (..), ExpSlice)
import           Talos.Analysis.Slice
import           Talos.Strategy.DFST
import           Talos.Strategy.Monad
import           Talos.SymExec.Path
import           Talos.Strategy.What4.Exprs
import           Talos.Strategy.What4.Types
import           Talos.Strategy.What4.SymM


-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

-- FIXME: maybe unify these into a single parameterised strat 'rand backtrack=dfs ...'
randDFS :: Strategy
randDFS = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = \ptag sl -> trivialStratGen . lift $
                                   runDFST (go ptag sl) (return . Just) (return Nothing)
           }
    name  = "rand-dfs"
    descr = "Simple depth-first random generation"
    
    go :: ProvenanceTag -> ExpSlice -> DFST (Maybe SelectedPath) StrategyM SelectedPath
    go ptag sl = mkStrategyFun ptag sl

-- ----------------------------------------------------------------------------------------
-- Restarting strat (restart-on-failure)

randRestart :: Strategy
randRestart = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = randRestartStrat
           }
    name  = "rand-restart"
    descr = "Restart on failure with random selection"

restartBound :: Int
restartBound = 1000

randRestartStrat :: ProvenanceTag -> ExpSlice -> StratGen
randRestartStrat ptag sl = trivialStratGen . lift $ go restartBound
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
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = randMaybeStrat
           }
    name  = "rand-restart-local-bt"
    descr = "Backtrack locally on failure, restart on (global) failure with random selection"

randMaybeStrat :: ProvenanceTag -> ExpSlice -> StratGen
randMaybeStrat ptag sl = trivialStratGen . lift $ go restartBound
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


data NameEnv sym = NameEnv (Map.Map Name (Some (W4.SymExpr sym)))

type FnEnv sym = Map.Map FName (SomeSymFn sym, Fun Expr)

data W4StratEnv sym = W4.IsSymExprBuilder sym => W4StratEnv { sym_ :: sym, varEnv :: I.Env, nameEnv :: NameEnv sym, fnCache :: IO.IORef (FnEnv sym) }

newtype W4StratT_ sym m a = W4StratT { unW4StratM :: ReaderT (W4StratEnv sym) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (W4StratEnv sym), MonadTrans, LiftStrategyM)

instance Monad m => MonadFail (W4StratT_ sym m) where
  fail msg = panic "W4StratT Failure" [msg]

instance (W4.IsSymExprBuilder sym, LiftStrategyM m, MonadIO m, Monad m) => SymM sym (W4StratT_ sym m) where
  withSym f = do
    W4StratEnv{} <- ask
    sym <- asks sym_
    f sym
  -- FIXME: TODO
  bindVarIn _ _ _ = error "FIXME: TODO"

  getVar nm = do
    NameEnv env <- asks nameEnv
    case Map.lookup nm env of
      Just se -> return se
      Nothing -> panic "Unbound variable" [showPP nm]

  liftMaybe (Just a) = return a
  liftMaybe Nothing = panic "liftMaybe" []

  withFNameCache fname f = do
    ref <- asks fnCache
    cache <- liftIO $ IO.readIORef ref
    case Map.lookup fname cache of
      Just a -> return a
      Nothing -> do
        a <- f
        liftIO $ IO.modifyIORef' ref (Map.insert fname a)
        return a

type W4StratT sym m a = (W4.IsSymExprBuilder sym, LiftStrategyM m, MonadIO m, Monad m) => W4StratT_ sym m a

liftReaderT :: ReaderT I.Env m a -> W4StratT sym m a
liftReaderT f = do
  venv <- asks varEnv
  lift $ runReaderT f venv

-- TODO: use W4StratT

-- A family of backtracking strategies indexed by a MonadPlus, so MaybeT StrategyM should give DFS
mkStrategyFun :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> ExpSlice -> m SelectedPath
mkStrategyFun ptag sl = do
  env0 <- getIEnv -- for pure function implementations
  snd <$> runReaderT (stratSlice ptag sl) env0 

stratSlice :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> ExpSlice
           -> ReaderT I.Env m (I.Value, SelectedPath)
stratSlice ptag = go
  where
    go sl = 
      case sl of
        SHole -> pure (uncPath I.vUnit)
        SPure e -> uncPath <$> synthesiseExpr e

        SDo x lsl rsl -> do
          (v, lpath)  <- go lsl
          onSlice (SelectedDo lpath) <$> bindIn x v (go rsl)

        SMatch bset -> do
          env <- ask
          -- Run the predicate over all bytes.
          -- FIXME: Too brute force? We could probably be smarter
          let bs = filter (I.evalByteSet bset env) [0 .. 255]
          guard (bs /= [])
          b <- choose bs -- select a byte from the set, backtracking
          -- liftStrategy (liftIO $ putStrLn ("Chose byte " ++ show b))
          pure (I.vUInt 8 (fromIntegral b)
               , SelectedBytes ptag (BS.singleton b))
          
        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          -- liftStrategy (liftIO $ putStrLn ("Chose choice " ++ show i))
          onSlice (SelectedChoice . PathIndex i) <$> go sl'

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> do
          env <- ask
          I.evalCase (\(_i, sl') _env -> onSlice (SelectedCase . Identity) <$> go sl' ) mzero (enumerate c) env

        -- FIXME: For now we just keep picking until we get something which satisfies the predicate; this can obviously be improved upon ...
        SInverse n ifn p -> do
          let tryOne = do
                v <- synthesiseExpr =<< typeToRandomInhabitant (I.typeOf n)
                bindIn n v $ do
                  b <- I.valueToBool <$> synthesiseExpr p
                  guard b
                  bs <- synthesiseExpr ifn
                  pure (v, SelectedBytes ptag (I.valueToByteString bs))
              tryMany = tryOne <|> tryMany -- FIXME: this might run forever.
          tryMany 

    uncPath :: I.Value -> (I.Value, SelectedPath)
    uncPath v = (v, SelectedHole)
    onSlice f = \(a, sl') -> (a, f sl')

    -- unimplemented = panic "Unimplemented" []

-- Synthesise for each call

-- Adding field sensitivity means that there can be multiple result
-- slices, with non-overlapping projections.  As a result, we need to
-- merge the resulting slices.
--
-- Merging all slices could introduce spurious internal backtracking,
-- although it is not clear whether that is an issue or not.

stratCallNode :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> ExpCallNode -> 
                 ReaderT I.Env m (I.Value, SelectedPath)
stratCallNode ptag cn = do
  env <- ask
  let env' = env { I.vEnv = Map.compose (I.vEnv env) (ecnParamMap cn) }
  sl <- getSlice (ecnSliceId cn)
  (v, res) <- local (const env') (stratSlice ptag sl)
  pure (v, SelectedCall (ecnIdx cn) res)

-- ----------------------------------------------------------------------------------------
-- Strategy helpers

-- Backtracking choice + random permutation
choose :: (MonadPlus m, LiftStrategyM m) => [a] -> m a
choose bs = do
  bs' <- randPermute bs
  foldr mplus mzero (map pure bs')

-- ----------------------------------------------------------------------------------------
-- Environment helpers

bindIn :: Monad m => Name -> I.Value -> ReaderT I.Env m a -> ReaderT I.Env m a
bindIn x v m = local upd m
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
    





        
        
        

          

