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
  -- bindIn,
  -- * Solver-based parser generator monad
  -- SParserM,
  -- runSParserM,
  -- getState,
  -- withConts,
  -- withInput,
  -- ** Emitting monadic operators
  -- tParserM,
  -- *** Monad building blocks
  -- ssuccess,
  -- *** Monadic code
  -- spure,
  -- sbind,
  -- sfail,
  -- sguard,
  -- scallP,
  -- sEnd,
  -- sChooseN,  
  -- sChoose,
  -- * Configuration etc.
  tConfig,
  sConfig,
  sConfigRel,
  sConfigRes,
  -- * Convenience wrappers for SMT
  -- ** Syntactic helpers
  mklet,
  mklets,  
  -- ** Bytes
  tByte,
  sByte,
  tBytes,
  -- ** State
  tState,
  tInput,
  tOracle,
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
  tListWithLength,
  sCons,
  sNil,
  sFromList,
  sLength,
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

-- -----------------------------------------------------------------------------
-- ParserM

-- data SParserCont  = SParserCont { sFailC :: SParserM' SExpr -- FIXME: or just SolverM?
--                                  -- ^ Failure meta-continuation
--                                  , sSuccessC :: SExpr -> SExpr -> SParserM' SExpr
--                                  -- ^ Success meta-continuation,
--                                  -- taking as argument the resulting
--                                  -- state and values.
--                                  }

-- -- FIXME: Probably need just a readert state here (not IO)
-- {- | The SExpr is the state variable -}
-- type SParserM' = ReaderT SExpr SolverM

-- -- | A parser-generating monad that emits SMT solver code for a
-- -- parser. It maintains two "meta-continuations", which are code to be
-- -- emitted for successful and failing parsers, as well as the name of
-- -- a variable that stands for the current state.
-- type SParserM = ReaderT SParserCont SParserM' SExpr

-- runSParserM :: SParserM -> SExpr -> SExpr -> SolverM SExpr
-- runSParserM m resT i = runReaderT (runReaderT m conts) i
--   where
--     conts         = SParserCont failC successC
--     failC         = pure (mkFail resT)
--     successC i' r = pure (mkSuccess i' r)

-- -- Primitives

-- -- | Locally override the failure and success continuations.
-- withConts ::
--   SParserM {- ^ The new failure continuation -} ->
--   (SExpr -> SExpr -> SParserM) {- ^ The new success continuation -} ->
--   SParserM -> SParserM
-- withConts fc sc = local (\c -> SParserCont (runReaderT fc c) (\i r -> runReaderT (sc i r) c))

-- -- | Locally override the failure continuation
-- withFailC :: SParserM -> SParserM -> SParserM
-- withFailC fc = local (\c -> c { sFailC = runReaderT fc c })

-- -- | Use a different expression to stand for the current state
-- withInput :: SExpr -> SParserM -> SParserM
-- withInput st m = do c <- ask
--                     lift (local (const st) (runReaderT m c))

-- getState :: SParserM
-- getState = lift ask

-- -- bindIn :: String -> SExpr -> (SExpr -> SParserM) ->  SParserM
-- -- bindIn _ v@(S.Atom _) f = f v
-- -- bindIn name e       f = do
-- --   v <- S.const <$> freshSym name
-- --   mklet v e <$> f v

-- withSuccessC :: (SExpr -> SExpr -> SParserM) -> SParserM -> SParserM
-- withSuccessC sc m = do
--   c <- ask
--   let sc' i r = runReaderT (sc i r) c
--   local (\c' -> c' { sSuccessC = sc' }) m

--   -- where
--   --   bindingSC i r = bindIn "i" i (\vi -> bindIn "r" r (sc vi))

-- mkFail :: SExpr -> SExpr
-- mkFail ty = S.as (S.const "fail") (tParserM ty)

-- mkSuccess :: SExpr -> SExpr -> SExpr
-- mkSuccess   i r= S.fun "success" [i, r]

-- sfail :: SParserM
-- sfail =  do fc <- asks sFailC
--             lift fc

-- -- | Generate code that applies the current success metacontinuation to state st and result r
-- ssuccess ::
--   SExpr {- ^ The state at the time of success -} ->
--   SExpr {- ^ The result value -} ->
--   SParserM
-- ssuccess st r = do sc <- asks sSuccessC
--                    lift (sc st r)

-- spure :: SExpr -> SParserM
-- spure r = do st <- getState
--              ssuccess st r

-- left biased
-- salt :: SParserM -> SParserM -> SParserM
-- salt m1 m2 = do i <- getInput
--                 withFailC (withInput i m2) m1

-- Fair choice via the oracle
-- sChooseN :: [SParserM] -> SParserM
-- sChooseN [] = sfail
-- sChooseN (alt : alts) = do
--   sbind (scallP "getProphecy" [])
--         (\b -> bindIn "idx" (S.bvURem b (S.bvHex 32 (fromIntegral $ length alts + 1)))
--                (\idx -> foldl (\s (idx', s') -> S.ite (S.eq idx (S.bvHex 32 idx')) <$> s' <*> s) alt
--                               (zip [0..] alts) ))

-- sChoose :: SParserM -> SParserM -> SParserM
-- sChoose l r = sChooseN [l, r]
--   -- sbind (scallP "getProphecy" [])
--   --       (\b -> S.ite b <$> l <*> r)

-- -- | Emit parser code that will succeed or fail depending on the argument.
-- sguard :: SExpr -> SParserM
-- sguard b = S.ite b <$> spure sUnit <*> sfail

-- sbind :: SParserM -> (SExpr -> SParserM) -> SParserM
-- sbind m f = withSuccessC (\st r -> withInput st (f r)) m

-- scallP :: String -> [SExpr] -> SParserM
-- scallP f args = do
--   i <- getState
--   bindIn "r and i" (S.fun f (args ++ [i]))
--     (\r_i -> S.ite (S.fun "is-fail" [r_i])
--              <$> sfail
--              <*> ssuccess (S.fun "state" [r_i]) (S.fun "result" [r_i]))

-- -- | Emit a call to the parser that succeeds at the end of the input
-- sEnd :: SParserM
-- sEnd = scallP "end" []

-- tParserM :: SExpr -> SExpr
-- tParserM a = S.fun "ParserM" [a]

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

-- FIXME: This needs a predicate if symbolic
tListWithLength :: SExpr -> SExpr
tListWithLength t = S.fun "ListWithLength" [t]

-- FIXME: duplicates xs
sCons :: SExpr -> SExpr -> SExpr
sCons x xs =
  S.fun "mk-ListWithLength" [ S.fun "insert" [x, S.fun "get-list" [xs]]
                            , S.add (S.int 1) (S.fun "get-length" [xs])
                            ]

sNil :: SExpr -> SExpr
sNil elT =
  S.fun "mk-ListWithLength" [ S.as (S.const "nil") (S.fun "List" [elT]), S.int 0 ]

sLength :: SExpr -> SExpr
sLength xs = S.fun "get-length" [xs]

-- tList :: SExpr -> SExpr
-- tList t = S.fun "List" [t]

-- sCons :: SExpr -> SExpr -> SExpr
-- sCons x xs = S.fun "insert" [x, xs]

-- sNil :: SExpr -> SExpr
-- sNil elT = S.as (S.const "nil") (tList elT)

-- | Quote the structure of a list, given a type of its elements.  In
-- other words, a Haskell list is converted to an SMT list by
-- replacing Haskell list constructors with code to construct the
-- corresponing SMT list.
sFromList :: SExpr -> [SExpr] -> SExpr
sFromList elT = foldr sCons (sNil elT)

tInput :: SExpr
tInput = S.fun "Input" []

tOracle :: SExpr
tOracle = S.fun "Oracle" [] 

-- the state consists of the (remaining) input, along with an oracle
-- (list of bits) which tells how to handle alternation
tState :: SExpr
tState = S.fun "State" [] 

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

tConfig :: SExpr -> SExpr -> SExpr
tConfig rel res = S.fun "Config" [rel, res]

sConfig :: SExpr -> SExpr -> SExpr
sConfig rel res = S.fun "mk-Config" [rel, res]

sConfigRel :: SExpr -> SExpr
sConfigRel v = S.fun "config-rel" [v]

sConfigRes :: SExpr -> SExpr
sConfigRes v = S.fun "config-res" [v]
