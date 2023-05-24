{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- API for strategies, which say how to produce a path from a slice.

module Talos.Strategy.Monad ( Strategy(..)
                            , StrategyInstance(..)
                            , parseStrategies
                            , StratFun, StratGen(..), trivialStratGen
                            , StrategyM, StrategyMState
                            , runStrategyM -- just type, not ctors
                            , makeStrategyMState
                            , LiftStrategyM (..)
                            , summaries, getSlice, sccsFor, backEdgesFor, isRecVar
                            , rand, randR, randL, randPermute, typeToRandomInhabitant
                            -- , timeStrategy
                            -- , logMessage, logMessage'
                            ) where

import           Control.Monad             (forM, replicateM)
import           Control.Monad.Except      (throwError)
import           Control.Monad.Reader
import           Control.Monad.RWS         (RWST)
import qualified Control.Monad.RWS.CPS as RWSCPS
import qualified Control.Monad.RWS.Strict as RWSStrict
import           Control.Monad.State
import           Control.Monad.Trans.Free  (FreeT)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer      (WriterT)
import qualified Data.ByteString           as BS
import           Data.Foldable             (find, foldl')
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (maybeToList)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as V
import           System.Random

import           Daedalus.Core             (Expr (Struct), Name,
                                            SizeType (TSize), TDecl (tDef),
                                            TDef (TBitdata, TStruct, TUnion),
                                            TName, Type (..), UserType (utName),
                                            arrayL, boolL, byteArrayL, emit,
                                            inUnion, intL, just, mapEmpty,
                                            mapInsert, newBuilder, nothing,
                                            unit)
import           Daedalus.Panic
import           Daedalus.PP

import           Daedalus.GUID             (HasGUID)
import           Talos.Analysis.Exported
import           Talos.Analysis.Monad      (Summaries)
import           Talos.Monad               (LiftTalosM, TalosM, getTypeDefs)
import qualified Talos.Strategy.OptParser  as P
import           Talos.Strategy.OptParser  (Parser, runParser)
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT     (SolverT)

-- ----------------------------------------------------------------------------------------
-- Core datatypes

-- Gives some flexibility in how they are run.
-- data StratFun =
--   SimpleStrat   (ProvenanceTag -> ExpSlice -> StrategyM (Maybe SelectedPath))
--   | SolverStrat (ProvenanceTag -> ExpSlice -> SolverT StrategyM (Maybe SelectedPath))


-- Returns any new models, plus the option of getting more
newtype StratGen = StratGen { getStratGen :: SolverT StrategyM ([SelectedPath], Maybe StratGen) }

type StratFun = ProvenanceTag -> ExpSlice -> StratGen

data StrategyInstance = StrategyInstance
  { siName  :: String
  , siDescr :: Doc
  , siFun   :: StratFun
  }

data Strategy =
  Strategy { stratName  :: String
           , stratDescr :: Doc
           , stratParse  :: Parser StrategyInstance
           }

-- FIXME: Temporary hack to get things working
trivialStratGen :: SolverT StrategyM (Maybe SelectedPath) -> StratGen
trivialStratGen g = StratGen $ (\m_r -> (maybeToList m_r, Nothing)) <$> g
  
-- -----------------------------------------------------------------------------
-- Parsing strategy descriptions

parseStrategy :: [Strategy] -> Parser StrategyInstance
parseStrategy strats = do
  P.skipSpaces  
  n <- P.nameP
  P.skipSpaces
  case find ((==) n . stratName) strats of
    Nothing -> throwError ("Unknown strategy: '" ++ n ++ "'")
    Just s  -> stratParse s

parseStrategies :: [String] -> [Strategy] -> Either String [StrategyInstance]
parseStrategies opts strats =
  mapM (runParser (parseStrategy strats)) opts

-- -----------------------------------------------------------------------------
-- Monad

data StrategyMState  =
  StrategyMState { stsStdGen    :: StdGen
                   -- Read only
                 , stsSummaries :: ExpSummaries
                 }

newtype StrategyM a =
  StrategyM { getStrategyM :: StateT StrategyMState TalosM a }
  deriving (Functor, Applicative, Monad, MonadIO, LiftTalosM, HasGUID)

makeStrategyMState :: StdGen -> Summaries ae -> TalosM StrategyMState
makeStrategyMState gen ss = StrategyMState gen <$> exportSummaries ss

runStrategyM :: StrategyMState -> StrategyM a -> TalosM (a, StrategyMState)
runStrategyM st m = runStateT (getStrategyM m) st

-- -----------------------------------------------------------------------------
-- State access

summaries :: LiftStrategyM m => m ExpSummaries
summaries = liftStrategy (StrategyM (gets stsSummaries))

getSlice :: LiftStrategyM m => SliceId -> m ExpSlice
getSlice sid = do
  ss <- summaries
  case Map.lookup sid (esFunctionSlices ss) of
    Nothing -> panic "Missing SliceId" [showPP sid]
    Just sl -> pure sl

isRecVar :: LiftStrategyM m => Name -> m Bool
isRecVar n = do
  liftStrategy (StrategyM (gets (Set.member n . esRecVars . stsSummaries)))

sccsFor :: LiftStrategyM m => SliceId -> m (Maybe (Set SliceId))
sccsFor sid = do
  liftStrategy (StrategyM (gets (Map.lookup sid . esRecs . stsSummaries)))

backEdgesFor :: LiftStrategyM m => SliceId -> m (Map SliceId (Set SliceId))
backEdgesFor entrySid = do
  liftStrategy (StrategyM (gets (Map.findWithDefault mempty entrySid . esBackEdges . stsSummaries)))

-- callnodetoslices :: LiftStrategyM m => CallNode FInstId -> m [ ((Bool, Slice), Map Name Name) ]
-- callNodeToSlices cn = do
--   -- FIXME: This is maybe less efficient dep on how GHC optimises
--   sequence [ mk args <$> callIdToSlice (callName cn, callClass cn, i)
--            | (i, args) <- Map.toList (callSlices cn)
--            ]
--     where
--       mk args (r, ps) = (r, Map.fromList $ catMaybes (zipWith (\a m_b -> (,) a <$> m_b) ps args))

-- callIdToSlice :: LiftStrategyM m => (FName, FInstId, Int) -> m ((Bool, Slice), [Name])
-- callIdToSlice (fn, fid, i) = do
--   ss <- summaries
--   let m_s = do
--         summM <- Map.lookup fn ss
--         summ  <- Map.lookup fid summM
--         -- FIXME: _ !! _ here throws an exception :/
--         pure  (esSlices summ !! i, esParams summ)

--   case m_s of
--     Just sl -> pure sl
--     Nothing -> panic "Missing summary" [showPP fn, showPP fid]


-- -----------------------------------------------------------------------------
-- Random values

rand :: (LiftStrategyM m, Random a) => m a
rand = liftStrategy (StrategyM $ state go)
  where
    go s = let (b, g') = random (stsStdGen s) in (b, s { stsStdGen = g' })

randR :: (LiftStrategyM m, Random a) => (a, a) -> m a
randR r = liftStrategy (StrategyM $ state go)
  where
    go s = let (b, g') = randomR r (stsStdGen s) in (b, s { stsStdGen = g' })

randL :: LiftStrategyM m => [a] -> m a
randL [] = panic "randL: empty list" []
randL vs = (!!) vs <$> randR (0, length vs - 1)



-- | Randomly shuffle a list
--   /O(N)/
--
--  c.f. https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> StrategyM [a]
shuffle xs = do
  ar <- liftStrategy . liftIO $ V.thaw (V.fromList xs)
  forM [0..n-1] $ \i -> do
    j <- randR (i,n-1)
    vi <- liftIO $ V.read ar i
    vj <- liftIO $ V.read ar j
    liftIO $ V.write ar j vi
    return vj
  where
    n = length xs
  
randPermute :: LiftStrategyM m => [a] -> m [a]
randPermute = liftStrategy . shuffle

typeToRandomInhabitant :: LiftStrategyM m => Type -> m Expr
typeToRandomInhabitant ty = do
  tdecls <- liftStrategy getTypeDefs
  typeToRandomInhabitant' tdecls ty

typeToRandomInhabitant' :: (LiftStrategyM m) => Map TName TDecl -> Type -> m Expr
typeToRandomInhabitant' tdecls targetTy = go targetTy
  where
    go ty = case ty of
      TStream    -> unimplemented

      TUInt (TSize n) -> flip intL ty <$> randR (0, 2 ^ n - 1)
      TUInt _    -> unimplemented

      TSInt (TSize n) -> flip intL ty <$> randR (- 2 ^ (n - 1), 2 ^ (n - 1) - 1)
      TSInt _    -> unimplemented

      -- FIXME
      TFloat     -> unimplemented
      TDouble    -> unimplemented

      TInteger   -> flip intL ty <$> rand
      TBool      -> boolL <$> rand
      TUnit      -> pure unit
      TArray (TUInt (TSize 8)) -> do
        let maxLength = 100 -- FIXME
        len <- randR (0, maxLength)
        bs  <- replicateM len rand
        pure (byteArrayL (BS.pack bs))

      TArray t   ->  do
        let maxLength = 100 -- FIXME
        len <- randR (0, maxLength)
        vs  <- replicateM len (go t)
        pure (arrayL t vs)

      TMaybe t -> do
        b <- rand
        if b then pure (nothing t) else just <$> go t
      TMap tk tv -> do
        let maxLength = 100
        len <- randR (0, maxLength)
        ks <- replicateM len (go tk)
        vs <- replicateM len (go tv)
        pure $ foldl' (\m (k, v) -> mapInsert m k v) (mapEmpty tk tv) (zip ks vs)

      TBuilder t -> do
        let maxLength = 100
        len <- randR (0, maxLength)
        vs <- replicateM len (go t)
        pure $ foldl' emit (newBuilder t) vs

      TIterator _t -> unimplemented
      TUser ut     -> goUT ut
      TParam _     -> panic "Saw a type param" []

    goUT ut
      | Just decl <- Map.lookup (utName ut) tdecls =
          case tDef decl of
            TStruct fs -> Struct ut <$> sequence [ (,) l <$> go ty | (l, ty) <- fs ]
            TUnion  fs -> do
              (l, ty) <- randL fs
              inUnion ut l <$> go ty

            TBitdata {} -> unimplemented  -- FIXME

      | otherwise = panic "Unknown user type " [showPP ut]

    unimplemented = panic "Unimplemented" [showPP targetTy]

-- -----------------------------------------------------------------------------
-- Printing verbosely

-- logMessage' :: (Monad m, MonadIO m) => Int -> Int -> String -> m ()
-- logMessage' v lvl s
--   | lvl <= v = liftIO (putStrLn s >> hFlush stdout)
--   | otherwise = pure ()

-- logMessage :: LiftStrategyM m => Int -> String -> m ()
-- logMessage lvl s = liftStrategy $ do
--   v <- StrategyM (gets stsVerbosity)
--   logMessage' v lvl s

-- -----------------------------------------------------------------------------
-- Class

class Monad m => LiftStrategyM m where
  liftStrategy :: StrategyM a -> m a  
  default liftStrategy :: (m ~ t m', MonadTrans t, LiftStrategyM m') => StrategyM a -> m a
  liftStrategy = lift . liftStrategy

instance LiftStrategyM StrategyM where
  liftStrategy = id

instance LiftStrategyM m => LiftStrategyM (StateT s m) where
instance LiftStrategyM m => LiftStrategyM (ReaderT s m) where
instance (Monoid w, LiftStrategyM m) => LiftStrategyM (WriterT w m) where
instance LiftStrategyM m => LiftStrategyM (MaybeT m) where
instance LiftStrategyM m => LiftStrategyM (SolverT m) where
instance (Functor f, LiftStrategyM m) => LiftStrategyM (FreeT f m) where
instance (Monoid w, LiftStrategyM m) => LiftStrategyM (RWST r w s m) where
instance (Monoid w, LiftStrategyM m) => LiftStrategyM (RWSCPS.RWST r w s m) where
instance (Monoid w, LiftStrategyM m) => LiftStrategyM (RWSStrict.RWST r w s m) where

-- -----------------------------------------------------------------------------
-- Instances

