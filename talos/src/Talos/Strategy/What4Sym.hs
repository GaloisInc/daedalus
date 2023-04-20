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
import qualified Data.BitVector.Sized            as BVS

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
import qualified Data.Parameterized.Vector as V
import qualified System.IO.Unsafe as IO
import qualified What4.SatResult as W4R
import Data.Maybe
import Control.Monad.Catch
import Control.Monad.Writer.Class


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
  fst <$> runW4StratT solverEnv0 env0 (stratSliceConcrete ptag sl)

type W4StratT_ sym m a = W4SolverT_ sym (ReaderT I.Env m) a

type W4StratT sym m a = 
  (W4.IsSymExprBuilder sym, LiftStrategyM m, MonadIO m, Monad m, MonadMask m, MonadThrow m, MonadCatch m, MonadPlus m) => W4StratT_ sym m a


runW4StratT :: 
  MonadIO m =>
  W4SolverEnv sym ->
  I.Env ->
  W4StratT_ sym m a -> 
  m (a, W4.Pred sym)
runW4StratT solverEnv env f = runReaderT (runW4Solver solverEnv f) env


type SymbolicPath sym = SelectedPathF (SymbolicPathChoice sym) (SymbolicPathCase sym) (SymbolicResult sym)

getConcretePath ::
  SymGroundEvalFn sym ->
  SymbolicPath sym ->
  W4StratT sym m (SelectedPath)
getConcretePath fn p = case p of
  SelectedHole -> return SelectedHole
  SelectedBytes ptag result_sym -> do
    result_conc <- groundSymbolicResult fn result_sym
    return $ SelectedBytes ptag result_conc
  SelectedDo f1 f2 -> SelectedDo <$> getConcretePath fn f1 <*> getConcretePath fn f2
  SelectedChoice (ConcreteChoice i p') ->
    SelectedChoice <$> (PathIndex i <$> getConcretePath fn p')
  SelectedChoice (SymbolicPath symChoice) -> withSym $ \sym -> do
    (i, p') <- groundSymbolicChoice sym fn symChoice
    conc_path <- getConcretePath fn p'
    return $ SelectedChoice (PathIndex i conc_path)
  SelectedCall finst p' -> 
    SelectedCall <$> pure finst <*> getConcretePath fn p'
  SelectedCase (ConcreteCase p') -> do
    conc_p' <- getConcretePath fn p'
    return $ SelectedCase (return conc_p')
  SelectedCase (SymbolicCase symCase) -> withSym $ \sym -> do
    paths <- (mapMaybe id . V.toList) <$> groundSymbolicVector sym fn symCase
    path <- choose paths
    conc_p' <- getConcretePath fn path
    return $ SelectedCase (return conc_p')

data SymbolicPathChoice sym a =
    forall n. SymbolicPath (SymbolicChoice sym n (Int, a))
  | ConcreteChoice Int a

data SymbolicPathCase sym a =
  forall n. SymbolicCase (SymbolicVector sym n a)
  | ConcreteCase a

newtype SymbolicResult sym = SymbolicResult (ArrayLen sym (W4.BaseBVType 8))

groundSymbolicResult ::
  SymGroundEvalFn sym ->
  SymbolicResult sym ->
  W4StratT sym m BS.ByteString
groundSymbolicResult fn (SymbolicResult arr) = withSym $ \sym -> do
  sz <- liftIO $ arrayLenSize sym arr
  sz_conc <- BVS.asUnsigned <$> execGroundFn fn sz
  liftIO $ putStrLn $ ("groundSymbolicResult: sz_conc " ++ show sz_conc)
  bytes_sym <- liftIO $ arrayLenPrefix sym sz_conc arr
  bytes_conc <- mapM (execGroundFn fn) bytes_sym
  let ws = concat $ map (fromJust . BVS.asBytesBE (knownNat @8)) bytes_conc
  return $ BS.pack ws

-- type SelectedPath = SelectedPathF PathIndex Identity ByteString

-- data PathIndex a  = PathIndex { pathIndex :: Int, pathIndexPath :: a }
--  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)



stratSliceConcrete ::
  forall sym m.
  ProvenanceTag -> ExpSlice ->
  W4StratT sym m SelectedPath
stratSliceConcrete ptag sl = do
  senv <- ask
  env <- lift ask
  ((_, path), asm) <- lift $ runW4StratT senv env (stratSlice ptag sl)
  liftIO $ putStrLn ("checkSat:\n" ++ (show (W4.printSymExpr asm)))
  checkSat asm $ \case
    W4R.Sat fn -> getConcretePath fn path
    _ -> panic "asms unsat" [show (W4.printSymExpr asm)]



stratSlice :: 
  forall sym m.
  ProvenanceTag -> ExpSlice ->
  W4StratT sym m (Some (W4.SymExpr sym), SymbolicPath sym)
stratSlice ptag = go
  where
    go :: ExpSlice -> W4StratT sym m (Some (W4.SymExpr sym), SymbolicPath sym)
    go sl = checkAsms [showPP sl] (go' sl) 

    go' :: ExpSlice -> W4StratT sym m (Some (W4.SymExpr sym), SymbolicPath sym)
    go' sl = 
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
          --liftStrategy (liftIO $ putStrLn ("Do: lsl v " ++ show (pp v)))
          --liftStrategy (liftIO $ putStrLn ("Do: lsl path " ++ show (pp lpath)))
          onSlice (SelectedDo lpath) <$> bindVarIn x v (go rsl)

        SMatch bset -> withSym $ \sym -> do
          b <- liftIO $ W4.freshConstant sym W4.emptySymbol (W4.BaseBVRepr (knownNat @8))
          p <- evalByteSet bset b
          addAssumption p
          byte <- liftIO $ singletonArrayLen sym b
          return (Some b, SelectedBytes ptag (SymbolicResult byte))

        SChoice sls -> withSym $ \sym -> do
          case someNat (length sls) of
            Just (Some k) | Just LeqProof <- W4.isPosNat k -> do
              b <- liftIO $ W4.freshConstant sym W4.emptySymbol (W4.BaseBVRepr k)
              choice <- mkSymbolicChoice sym b $ \n -> do
                let (i :: Int) = fromIntegral (intValue n)
                let sl' = sls !! i
                (e,p') <- go sl'
                return (i, (e,p'))
              val <- getSymbolicChoice sym muxExprs (fmap (fst . snd) choice)
              return $ (val, SelectedChoice (SymbolicPath (fmap (\(i,(_,p')) -> (i, p')) choice)))
            _ -> panic "Empty choice" []

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> withSym $ \sym -> do
          let var = I.caseVar c
          Some e' <- getVar var
          case someNat (length (casePats c)) of
            Just (Some k) | Just LeqProof <- W4.isPosNat k -> do
              v <- mkSymbolicVector sym k $ \n -> do
                let (i :: Int) = fromIntegral (intValue n)
                let (pat,case_) = casePats c !! i
                p <- matchesPat (I.nameType var) pat e'
                (e_body,path) <- go case_
                return $ (p, (e_body,path))
              -- TODO: add extra case for Any pattern that negates other cases
              ((fpred, (e_body, _)),vs) <- vectorToList <$> getSymbolicVector sym v
              somePred <- foldM (\p' (p,_) -> liftIO $ W4.orPred sym p p') fpred vs
              addAssumption somePred
              val <- foldM (\e1 (p,(e2,_)) -> muxExprs p e1 e2) e_body vs
              return $ (val, SelectedCase (SymbolicCase (fmap snd v)))
            _ -> panic "Unexpected empty case:" [showPP c]

        SInverse n ifn p -> withSym $ \sym -> do
          Some n_T_repr <- typeToRepr (I.typeOf n)
          n_sym <- liftIO $ W4.freshConstant sym W4.emptySymbol n_T_repr
          bindVarIn n n_sym $ do
            p_sym <- toWhat4Expr I.TBool W4.BaseBoolRepr p
            checkAsms [show (W4.printSymExpr p_sym)] (addAssumption p_sym)
            ifn_sym <- toWhat4Expr (TArray (I.TUInt (I.TSize 8))) (ArrayLenRepr (W4.BaseBVRepr (knownNat @8))) ifn
            return (Some n_sym, SelectedBytes ptag (SymbolicResult ifn_sym))

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
                 W4StratT sym m (Some (W4.SymExpr sym), SymbolicPath sym)
stratCallNode ptag cn = do
  env <- lift $ ask
  let env' = env { I.vEnv = Map.compose (I.vEnv env) (ecnParamMap cn) }
  sl <- getSlice (ecnSliceId cn)
  return $ error ""
  -- (v, res) <- local (const env') (stratSlice ptag sl)
  -- pure (v, SelectedCall (ecnIdx cn) res)

-- ----------------------------------------------------------------------------------------
-- Strategy helpers

-- Backtracking choice + random permutation
choose :: (MonadPlus m, LiftStrategyM m) => [a] -> m a
choose bs = do
  bs' <- randPermute bs
  foldr mplus mzero (map pure bs')

-- ----------------------------------------------------------------------------------------
-- Environment helpers

synthesiseExpr :: Expr -> W4StratT sym m (Some (W4.SymExpr sym))
synthesiseExpr e = do
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

        

          

