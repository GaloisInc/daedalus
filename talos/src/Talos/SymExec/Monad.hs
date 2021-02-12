{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleInstances, FlexibleContexts #-}

-- | Defines the symbolic parser API.  This wraps the SimpleSMT API

module Talos.SymExec.Monad (
  -- * Solver interaction monad
  SolverM,
  runSolverM,
  SolverMState,
  currentDeclName,
  freshSym,
  freshGlobalSym,
  -- * Model types
  tModel,  
  -- * Convenience wrappers for SMT
  -- ** Syntactic helpers
  mklet,
  mklets,  
  -- ** Bytes
  tByte,
  sByte,
  tBytes,
  -- ** Unit
  tUnit,
  sUnit,
  -- ** Maybe
  tMaybe,
  sJust,
  sNothing,
  -- ** Pairs
  tTuple,
  sTuple,
  sFst,
  sSnd,
  -- ** Sums
  tSum,
  sInl,
  sInr,
  sIsLeft,
  sGetLeft,
  sIsRight,
  sGetRight,
  -- ** List
  tList,
  sCons,
  sNil,
  sFromList,
  -- ** Map
  tMap
  ) where

import Data.Word
import Control.Monad.State

import SimpleSMT (SExpr)
import qualified SimpleSMT as S

data SolverMState = SolverMState { globalPrefix :: String
                                 , nextFresh :: Int }

newtype SolverM a = SolverM { getSolverM  :: StateT SolverMState IO a }
  deriving (Functor, Applicative, Monad, MonadState SolverMState, MonadIO)

-- | Generate a fresh symbol by appending a unqiue numeric suffix to a
-- hint string. Assumes that the hint string does not have a numeric
-- suffix.
freshSym ::
  MonadState SolverMState m =>
  String {- ^ The hint string to base the fresh name upon -} ->
  m String
freshSym hint =
  do nf <- state (\s -> (nextFresh s, s { nextFresh = nextFresh s + 1 }))
     pure (S.quoteSymbol (hint ++ " " ++ show nf))

currentDeclName ::
  MonadState SolverMState m => m String
currentDeclName = gets globalPrefix

freshGlobalSym ::
  MonadState SolverMState m =>
  String {- ^ The hint string to base the fresh name upon -} ->
  m String
freshGlobalSym hint = do
  pfx <- currentDeclName
  freshSym (pfx ++ " " ++ hint)

runSolverM :: String -> SolverM a -> IO a
runSolverM pfx m = evalStateT (getSolverM m) (SolverMState pfx 0)

tUnit :: SExpr
tUnit = S.const "Unit"

sUnit :: SExpr
sUnit = S.const "unit"

-- -----------------------------------------------------------------------------
-- Helpers

-- | Emit an SMT let expression
mklet :: String -> SExpr -> SExpr -> SExpr
mklet v e b = S.fun "let" [S.List [S.List [S.const v, e]], b]

mklets :: [(String, SExpr)] -> SExpr -> SExpr
mklets [] b = b
mklets xs b =
  S.fun "let" [S.List (map (\(v, e) -> S.List [S.const v, e]) xs), b]

-- -----------------------------------------------------------------------------
-- Types, both builtin and ours

sByte :: Word8 -> SExpr
sByte = S.bvHex 8 . fromIntegral


tList :: SExpr -> SExpr
tList t = S.fun "List" [t]

sCons :: SExpr -> SExpr -> SExpr
sCons x xs = S.fun "insert" [x, xs]

sNil :: SExpr -> SExpr
sNil elT = S.as (S.const "nil") (tList elT)

-- | Quote the structure of a list, given a type of its elements.  In
-- other words, a Haskell list is converted to an SMT list by
-- replacing Haskell list constructors with code to construct the
-- corresponing SMT list.
sFromList :: SExpr -> [SExpr] -> SExpr
sFromList elT = foldr sCons (sNil elT)

tByte :: SExpr
tByte = S.tBits 8

tBytes :: SExpr
tBytes = tList tByte

tMaybe :: SExpr -> SExpr
tMaybe t = S.fun "Maybe" [t]

sNothing :: SExpr -> SExpr
sNothing t = S.as (S.const "Nothing") (tMaybe t)

sJust :: SExpr -> SExpr
sJust v = S.fun "Just" [v]

tTuple :: SExpr -> SExpr -> SExpr
tTuple t1 t2 = S.fun "Tuple" [t1, t2]

sTuple :: SExpr -> SExpr -> SExpr
sTuple v1 v2 = S.fun "mk-tuple" [v1, v2]

sFst :: SExpr -> SExpr
sFst t = S.fun "fst" [t]

sSnd :: SExpr -> SExpr
sSnd t = S.fun "snd" [t]

tMap :: SExpr -> SExpr -> SExpr
tMap kt vt = S.fun "Map" [kt, vt]

tSum :: SExpr -> SExpr -> SExpr
tSum lt rt = S.fun "Sum" [lt, rt]

sInl :: SExpr -> SExpr -> SExpr -> SExpr
sInl lt rt v = S.as (S.fun "Left" [v]) (tSum lt rt)

sInr :: SExpr -> SExpr -> SExpr -> SExpr
sInr lt rt v = S.as (S.fun "Right" [v]) (tSum lt rt)

sIsLeft :: SExpr -> SExpr
sIsLeft v = S.fun "is-Left" [v]

sIsRight :: SExpr -> SExpr
sIsRight v = S.fun "is-Right" [v]

sGetLeft :: SExpr -> SExpr
sGetLeft v = S.fun "get-Left" [v]

sGetRight :: SExpr -> SExpr
sGetRight v = S.fun "get-Right" [v]

tModel :: SExpr
tModel = S.const "Model"
