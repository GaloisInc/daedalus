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
{-# Language LambdaCase #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.What4Sym (what4DFS, what4Restart, what4MaybeT, mkStrategyFun) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString                 as BS
import           Data.Functor.Identity           (Identity (Identity))
import qualified Data.Map                        as Map
import qualified Data.IORef as IO

import qualified What4.Interface                 as W4
import qualified What4.Expr                      as WE
import           Data.Parameterized.NatRepr
import qualified Data.Parameterized.Nonce as N

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
import           Talos.Strategy.What4.Solver

import Data.Parameterized.Some
import qualified System.IO.Unsafe as IO
import qualified What4.SatResult as W4R
import Data.Maybe
import Control.Monad.Catch
import Data.Parameterized.Vector


-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

-- FIXME: maybe unify these into a single parameterised strat 'rand backtrack=dfs ...'
what4DFS :: Strategy
what4DFS = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = \ptag sl -> trivialStratGen . lift $ do
                                     withSomeSolverEnv $ \solverEnv0 -> 
                                       runDFST (go solverEnv0 ptag sl) (return . Just) (return Nothing)
           }
    name  = "whatFour-dfs"
    descr = "Simple depth-first random generation"
    
    go :: SomeW4SolverEnv -> ProvenanceTag -> ExpSlice -> DFST (Maybe SelectedPath) StrategyM SelectedPath
    go solverEnv0 ptag sl = mkStrategyFun solverEnv0 ptag sl

-- ----------------------------------------------------------------------------------------
-- Restarting strat (restart-on-failure)

what4Restart :: Strategy
what4Restart = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = what4RestartStrat
           }
    name  = "whatFour-restart"
    descr = "Restart on failure with random selection"

restartBound :: Int
restartBound = 1000

what4RestartStrat :: ProvenanceTag -> ExpSlice -> StratGen
what4RestartStrat ptag sl = trivialStratGen . lift $ do
  withSomeSolverEnv $ \solverEnv0 -> 
    go solverEnv0 restartBound
  where
    go _ 0 = pure Nothing
    go solverEnv0 n = do
      m_p <- once solverEnv0
      case m_p of
        Just {} -> pure m_p
        Nothing -> go solverEnv0 (n - 1)
    
    once :: SomeW4SolverEnv -> StrategyM (Maybe SelectedPath)
    once solverEnv0 = runRestartT (mkStrategyFun solverEnv0 ptag sl) (return . Just) (return Nothing)

instance MonadIO m => MonadIO (RestartT r m) where
  liftIO f = RestartT $ \ctxt -> do
    a <- liftIO f
    randCont ctxt a

-- ----------------------------------------------------------------------------------------
-- Local backtracking, restart

what4MaybeT :: Strategy
what4MaybeT = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = what4MaybeStrat
           }
    name  = "what4-restart-local-bt"
    descr = "Backtrack locally on failure, restart on (global) failure with random selection"

what4MaybeStrat :: ProvenanceTag -> ExpSlice -> StratGen
what4MaybeStrat ptag sl = trivialStratGen . lift $ do
  withSomeSolverEnv $ \solverEnv0 -> go solverEnv0 restartBound
  where
    go _ 0 = pure Nothing
    go solverEnv0 n = do
      m_p <- once solverEnv0
      case m_p of
        Just {} -> pure m_p
        Nothing -> go solverEnv0 (n - 1)
    
    once :: SomeW4SolverEnv -> StrategyM (Maybe SelectedPath)
    once solverEnv0 = runMaybeT (mkStrategyFun solverEnv0 ptag sl)
  
-- ----------------------------------------------------------------------------------------

-- TODO: use W4StratT

-- A family of backtracking strategies indexed by a MonadPlus, so MaybeT StrategyM should give DFS
mkStrategyFun :: (MonadPlus m, LiftStrategyM m, MonadIO m, MonadMask m, MonadCatch m, MonadThrow m) => SomeW4SolverEnv -> ProvenanceTag -> ExpSlice -> m SelectedPath
mkStrategyFun (SomeW4SolverEnv solverEnv0) ptag sl = do
  env0 <- getIEnv -- for pure function implementations
  snd <$> runW4StratT solverEnv0 env0 (stratSlice ptag sl)

newtype W4StratT_ sym m a = W4StratT_ (ReaderT I.Env (W4SolverT_ sym m) a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader I.Env, LiftStrategyM, Alternative, MonadPlus)

instance MonadTrans (W4StratT_ sym) where
  lift f = W4StratT_ $ lift $ lift f

instance MonadIO m => MonadFail (W4StratT_ sym m) where
  fail msg = liftIO $ fail msg

instance (MonadIO m, MonadCatch m, MonadThrow m, MonadMask m) => SolverM sym (W4StratT_ sym m) where
  getSolverSym = lift $ getSolverSym

type W4StratT sym m a = 
  (W4.IsSymExprBuilder sym, LiftStrategyM m, MonadIO m, Monad m, MonadMask m, MonadThrow m, MonadCatch m, MonadPlus m) => W4StratT_ sym m a


runW4StratT :: 
  MonadIO m =>
  W4SolverEnv sym ->
  I.Env ->
  W4StratT_ sym m a -> 
  m a
runW4StratT solverEnv env (W4StratT_ f) = runW4Solver solverEnv (runReaderT f env)

liftSolver ::
  Monad m =>
  W4SolverT_ sym m a -> W4StratT_ sym m a
liftSolver f = W4StratT_ $ lift f




type SymbolicPath sym = SelectedPathF (SymbolicPathChoice sym) (SymbolicPathCase sym) (SymbolicResult sym)

-- FIXME: is the int actually redundant here?
data SymbolicPathChoice sym a =
    forall n. SymbolicPath (SymbolicChoice sym n (Int, a))
  | ConcreteChoice Int a

data SymbolicPathCase sym a =
  forall n. SymbolicCase (SymbolicVector sym n (Pattern, a))
  | ConcreteCase a

newtype SymbolicResult sym = SymbolicResult (W4.SymExpr sym (W4.BaseBVType 8))

-- type SelectedPath = SelectedPathF PathIndex Identity ByteString

-- data PathIndex a  = PathIndex { pathIndex :: Int, pathIndexPath :: a }
--  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

stratSlice :: 
  forall sym m.
  ProvenanceTag -> ExpSlice ->
  W4StratT sym m (Some (W4.SymExpr sym), SymbolicPath sym)
stratSlice ptag = go
  where
    go :: ExpSlice -> W4StratT sym m (Some (W4.SymExpr sym), SymbolicPath sym)
    go sl = 
      case sl of
        SHole -> go (SPure (Ap0 Unit))
        SPure e -> do
          Some e_sym <- synthesiseExpr e
          let (e', path) = uncPath e_sym
          return (Some e', path)

        SDo x lsl rsl -> do
          liftStrategy (liftIO $ putStrLn ("Do: lsl " ++ show (pp lsl)))
          liftStrategy (liftIO $ putStrLn ("Do: rsl " ++ show (pp rsl)))
          (Some v, lpath)  <- go lsl
          liftStrategy (liftIO $ putStrLn ("Do: lsl v " ++ show (pp v)))
          --liftStrategy (liftIO $ putStrLn ("Do: lsl path " ++ show (pp lpath)))
          onSlice (SelectedDo lpath) <$> bindIn x v (go rsl)

        SMatch bset -> liftSolver $ withSym $ \sym -> do
          b <- liftIO $ W4.freshConstant sym W4.emptySymbol (W4.BaseBVRepr (knownNat @8))
          p <- evalByteSet bset b
          

          -- Run the predicate over all bytes.
          -- FIXME: Too brute force? We could probably be smarter
          liftStrategy (liftIO $ putStrLn ("From byteset " ++ show (pp bset)))
          let bs = filter (I.evalByteSet bset env) [0 .. 255]
          guard (bs /= [])
          b <- choose bs -- select a byte from the set, backtracking
          liftStrategy (liftIO $ putStrLn ("Chose byte " ++ show b))
          pure (I.vUInt 8 (fromIntegral b)
               , SelectedBytes ptag (BS.singleton b))
          
        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          liftStrategy (liftIO $ putStrLn ("Chose choice " ++ show i))
          liftIO $ putStrLn ("Checking case" ++ show (pp sl'))
          onSlice (SelectedChoice . PathIndex i) <$> go sl'

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> do
          env <- ask
          liftIO $ putStrLn ("Checking case" ++ show (pp c))
          I.evalCase (\(_i, sl') _env -> onSlice (SelectedCase . Identity) <$> go sl' ) mzero (enumerate c) env

        -- FIXME: For now we just keep picking until we get something which satisfies the predicate; this can obviously be improved upon ...
        SInverse n ifn p -> do
          let
             tryOne = do
                v <- liftSolver $ withSym $ \sym -> do
                  Some n_T_repr <- typeToRepr (I.typeOf n)
                  n_sym <- liftIO $ W4.freshConstant sym W4.emptySymbol n_T_repr
                  bindVarIn n n_sym $ do
                    p_sym <- toWhat4Expr I.TBool W4.BaseBoolRepr p
                    liftIO $ putStrLn ("Synthesized pred.. " ++ show (W4.printSymExpr p_sym))
                    checkSat p_sym $ \case
                      W4R.Sat fn -> do
                        n_ground <- execGroundFn fn n_sym
                        groundToValue n_T_repr n_ground
                      W4R.Unsat{} -> mzero
                      W4R.Unknown{} -> mzero
                liftStrategy (liftIO $ putStrLn ("Trying value.. " ++ show (pp v)))
                bindIn n v $ do
                  b <- I.valueToBool <$> synthesiseExpr p
                  guard b
                  liftStrategy (liftIO $ putStrLn ("Value satisfies guard"))
                  bs <- synthesiseExpr ifn
                  liftStrategy (liftIO $ putStrLn ("Input:" ++ show (pp bs)))
                  pure (v, SelectedBytes ptag (I.valueToByteString bs))
          tryOne 

    uncPath :: forall tp. W4.SymExpr sym tp -> (W4.SymExpr sym tp, SymbolicPath sym)
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

stratCallNode :: (MonadPlus m, LiftStrategyM m, MonadIO m) => ProvenanceTag -> ExpCallNode -> 
                 W4StratT sym m (I.Value, SymbolicPath sym)
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

bindIn :: Name -> W4.SymExpr sym tp -> W4StratT_ sym m a -> W4StratT_ sym m a
bindIn x v (W4StratT_ (ReaderT m)) = W4StratT_ (ReaderT (\env -> bindVarIn x v (m env)))

synthesiseExpr :: Expr -> W4StratT sym m (Some (W4.SymExpr sym))
synthesiseExpr e = liftSolver $ do
  let t = I.typeOf e
  Some t_repr <- typeToRepr t
  e_sym <- toWhat4Expr t t_repr e
  return $ Some e_sym

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


instance MonadThrow m => MonadThrow (RestartT r m) where
    throwM e = lift $ throwM e

instance MonadCatch m => MonadCatch (RestartT r m) where
  catch (RestartT f) hndl = RestartT $ \ctxt ->
    catch (f ctxt) (\e -> getRestartT (hndl e) ctxt )

liftRestartT ::
  (forall a. m a -> m a) ->
  RestartT r m b -> 
  RestartT r m b
liftRestartT f g = RestartT $ \ctxt -> f (getRestartT g ctxt)

instance MonadMask m => MonadMask (RestartT r m) where
  mask f = RestartT $ \ctxt -> mask (\g -> getRestartT (f (liftRestartT g)) ctxt)
  uninterruptibleMask f = RestartT $ \ctxt -> uninterruptibleMask (\g -> getRestartT (f (liftRestartT g)) ctxt)
  -- FIXME: this isn't great because it doesn't defer to the underlying mask instance, but it's
  -- very unclear how to do that here
  generalBracket acquire release act = do
    a <- acquire
    result <- catch (act a >>= \c -> return $ ExitCaseSuccess c) (\e -> return $ ExitCaseException e)
    release_result <- release a result
    case result of
      ExitCaseSuccess c -> return (c, release_result)
      ExitCaseException e -> throwM e
      ExitCaseAbort -> RestartT randEscape        

        

          

