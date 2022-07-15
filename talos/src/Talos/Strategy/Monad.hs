{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

-- API for strategies, which say how to produce a path from a slice.

module Talos.Strategy.Monad ( Strategy(..)
                            , StratFun(..)
                            , StrategyM, StrategyMState, emptyStrategyMState
                            , runStrategyM -- just type, not ctors
                            , LiftStrategyM (..)
                            , summaries, getModule, getGFun, getSlice, sccsFor, backEdgesFor -- , getParamSlice
                            , getFunDefs, getBFunDefs, getTypeDefs, isRecVar
                            , getIEnv--, callNodeToSlices, sliceToCallees, callIdToSlice
                            , rand, randR, randL, randPermute, typeToRandomInhabitant
                            -- , timeStrategy
                            ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Free     (FreeT)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer         (WriterT)
import qualified Data.ByteString              as BS
import           Data.Foldable                (find, foldl')
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           System.Random

import           Daedalus.Core
import qualified Daedalus.Core.Semantics.Decl as I
import qualified Daedalus.Core.Semantics.Env  as I
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic
import           Daedalus.Rec                 (forgetRecs)

import           Talos.Analysis.Exported
import           Talos.Analysis.Monad         (Summaries)
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT        (SolverT)




-- ----------------------------------------------------------------------------------------
-- Core datatypes

-- FIXME: add: config (e.g. depth/max backtracks/etc.)

-- Gives some flexibility in how they are run.
data StratFun =
  SimpleStrat   (ProvenanceTag -> ExpSlice -> StrategyM (Maybe SelectedPath))
  | SolverStrat (ProvenanceTag -> ExpSlice -> SolverT StrategyM (Maybe SelectedPath))

data Strategy =
  Strategy { stratName  :: String
           , stratDescr :: Doc
           , stratFun   :: StratFun
           }

-- -----------------------------------------------------------------------------
-- Monad

data StrategyMState  =
  StrategyMState { stsStdGen    :: StdGen
                   -- Read only
                 , stsSummaries :: ExpSummaries
                 , stsModule    :: Module
                 -- Derived from the module
                 , stsFunDefs   :: Map FName (Fun Expr)
                 , stsBFunDefs  :: Map FName (Fun ByteSet)
                 , stsIEnv      :: I.Env
                 , stsNextGUID  :: GUID
                 }

emptyStrategyMState :: StdGen -> Summaries ae -> Module -> GUID -> StrategyMState
emptyStrategyMState gen ss md nguid  = StrategyMState gen expss md funDefs bfunDefs env0 nguid'
  where
    (expss, nguid') = exportSummaries tyDefs (ss, nguid)
    env0 = I.defTypes tyDefs (I.evalModule md I.emptyEnv)
    tyDefs  = Map.fromList [ (tName td, td) | td <- forgetRecs (mTypes md) ]
    funDefs = Map.fromList [ (fName f, f) | f <- mFFuns md ]
    bfunDefs = Map.fromList [ (fName f, f) | f <- mBFuns md ]

newtype StrategyM a =
  StrategyM { getStrategyM :: StateT StrategyMState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runStrategyM :: StrategyM a -> StrategyMState -> IO (a, StrategyMState)
runStrategyM m st = runStateT (getStrategyM m) st

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


getGFun :: LiftStrategyM m => FName -> m (Fun Grammar)
getGFun f = getFun <$> liftStrategy (StrategyM (gets stsModule))
  where
    getFun md = case find ((==) f . fName) (mGFuns md) of -- FIXME: us a map or something
      Nothing -> panic "Missing function" [showPP f]
      Just v  -> v

getModule :: LiftStrategyM m => m Module
getModule = liftStrategy (StrategyM (gets stsModule))

getTypeDefs :: LiftStrategyM m => m (Map TName TDecl)
getTypeDefs = liftStrategy (StrategyM (gets (I.tEnv . stsIEnv)))

getFunDefs :: LiftStrategyM m => m (Map FName (Fun Expr))
getFunDefs = liftStrategy (StrategyM (gets stsFunDefs))

getBFunDefs :: LiftStrategyM m => m (Map FName (Fun ByteSet))
getBFunDefs = liftStrategy (StrategyM (gets stsBFunDefs))

getIEnv :: LiftStrategyM m => m I.Env
getIEnv = liftStrategy (StrategyM (gets stsIEnv))

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

randPermute :: LiftStrategyM m => [a] -> m [a]
randPermute = go
  where
    go [] = pure []
    go xs = do idx <- randR (0, length xs - 1)
               let (pfx, x : sfx) = splitAt idx xs
               (:) x <$> go (pfx ++ sfx)

typeToRandomInhabitant :: (LiftStrategyM m) => Type -> m Expr
typeToRandomInhabitant ty = do
  tdecls <- getTypeDefs
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
-- Class

class Monad m => LiftStrategyM m where
  liftStrategy :: StrategyM a -> m a


instance LiftStrategyM StrategyM where
  liftStrategy = id

instance LiftStrategyM m => LiftStrategyM (StateT s m) where
  liftStrategy = lift . liftStrategy
instance LiftStrategyM m => LiftStrategyM (ReaderT s m) where
  liftStrategy = lift . liftStrategy
instance (Monoid w, LiftStrategyM m) => LiftStrategyM (WriterT w m) where
  liftStrategy = lift . liftStrategy  
instance LiftStrategyM m => LiftStrategyM (MaybeT m) where
  liftStrategy = lift . liftStrategy
instance LiftStrategyM m => LiftStrategyM (SolverT m) where
  liftStrategy = lift . liftStrategy
instance (Functor f, LiftStrategyM m) => LiftStrategyM (FreeT f m) where
  liftStrategy = lift . liftStrategy

-- -----------------------------------------------------------------------------
-- Instances

instance HasGUID StrategyM where
  guidState f = StrategyM (state go)
    where
      go s = let (r, guid') = f (stsNextGUID s)
             in (r, s { stsNextGUID = guid' })

