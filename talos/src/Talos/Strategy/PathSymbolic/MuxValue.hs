{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- -----------------------------------------------------------------------------
-- Semi symbolic/concrete evaluation
--
-- This is comied and modified from SemiExpr

module Talos.Strategy.PathSymbolic.MuxValue (
  MuxValue
  , MuxValueF(..)
  , semiExecExpr
  , semiExecPatterns
  , VSequenceMeta(..)
  , SequenceTag
  -- * Constructors
  , vSymbolicBool
  , vSymbolicInteger
  , vInteger
  , vBool
  -- * Combinators
  , mux
  , op2
  -- * Destructors
  , toSExpr
  , toAssertion
  , asIntegers
  , ppMuxValue
  -- * Monad
  , SemiSolverM
  , runSemiSolverM
  , fromModel
  ) where

import           Control.Applicative                   (liftA2)
import           Control.Lens                          (Iso, Prism', _1, _2,
                                                        _Left, _Right, below,
                                                        each, from, imap, iso,
                                                        locally, over, preview,
                                                        re, traverseOf,
                                                        traversed, view, (.~),
                                                        (^.), (^?!), (^?))
import           Control.Monad                         (join, unless, zipWithM)
import           Control.Monad.Except                  (ExceptT, runExceptT,
                                                        throwError)
import           Control.Monad.Reader                  (MonadIO, ReaderT, asks,
                                                        runReaderT)
import           Control.Monad.State                   (StateT, modify,
                                                        runStateT)
import           Control.Monad.Writer.CPS              (runWriter, tell)
import qualified Data.ByteString                       as BS
import           Data.Foldable                         (foldlM, toList)
import           Data.Functor                          (($>))
import           Data.Generics.Labels                  ()
import           Data.List.NonEmpty                    (NonEmpty)
import qualified Data.List.NonEmpty                    as NE
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (fromMaybe, isJust)
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Vector                           as Vector
import           GHC.Generics                          (Generic)
import           GHC.Stack                             (HasCallStack)

import qualified SimpleSMT                             as S
import           SimpleSMT                             (SExpr)

import           Daedalus.Core                         hiding (freshName)
import           Daedalus.Core.Semantics.Expr          (evalOp0, evalOp1,
                                                        evalOp2, partial)
import           Daedalus.Core.Type                    (sizeType, typeOf)
import           Daedalus.GUID                         (GUID, HasGUID)
import           Daedalus.Panic                        (panic)
import           Daedalus.PP
import qualified Daedalus.Value.Type                   as V

import           Talos.Lib                             (andMany)
import           Talos.Monad                           (LiftTalosM, LogKey)
import           Talos.Solver.SolverT                  (MonadSolver (liftSolver),
                                                        SMTVar, SolverT,
                                                        defineSymbol)
import           Talos.Strategy.Monad                  (LiftStrategyM)
import qualified Talos.Strategy.PathSymbolic.Assertion as A
import           Talos.Strategy.PathSymbolic.Assertion (Assertion)
import qualified Talos.Strategy.PathSymbolic.Branching as B
import           Talos.Strategy.PathSymbolic.Branching (Branching)
import qualified Talos.Strategy.PathSymbolic.PathSet   as PS
import           Talos.Strategy.PathSymbolic.PathSet   (LoopCountVar (..),
                                                        PathSetModelMonad,
                                                        PathVar, psmmLoopVar,
                                                        psmmSMTVar)
import qualified Talos.Strategy.PathSymbolic.SymExec   as SE
import           Talos.Strategy.PathSymbolic.SymExec   (symExecTy)

--------------------------------------------------------------------------------
-- Logging and stats

_muxKey :: LogKey
_muxKey = "muxvalue"

--------------------------------------------------------------------------------
-- Types

-- FIXME: we should normalise this so there is only ever 1 symbolic
-- term.

-- | A collection of base values (integer or bool) with their
-- associated path conditions.
type BaseValues v s = Branching (Either v s)

singletonBaseValues :: v -> BaseValues v s
singletonBaseValues = B.singleton . Left

singletonSymBaseValues :: s -> BaseValues v s
singletonSymBaseValues = B.singleton . Right

-- | Unifies Maybe and Unions
type SumTypeMuxValueF l s = Branching (l, MuxValueF s)

singletonSumTypeMuxValueF :: l -> MuxValueF s -> SumTypeMuxValueF l s
singletonSumTypeMuxValueF k v = B.singleton (k, v)

data MuxValueF s =
  VUnit
  | VIntegers !(Typed (BaseValues Integer s))
  | VBools    !(BaseValues Bool s)

  -- FIXME: we could also have Map V.Label [(PathCondition, MuxValueF
  -- b)], i.e. merge lazily.  Maybe Haskell helps here anyway?
  | VUnion                !(SumTypeMuxValueF V.Label s)
  -- ^ Maps each possible label onto the paths that build that label,
  -- and the merged contents value.  Must have at least 1 key.

  | VStruct                !(Map V.Label (MuxValueF s))

  -- There are 2 main way to represent sequences:
  -- 1. [ PathSet, (VSequenceMeta, [MuxValueF s]) ]
  -- 2. VSequenceMeta' [MuxValueF s]
  --
  -- where the second pushes the paths into the elements, and needs a
  -- different VSM type.  Mostly this is just a question of when the
  -- merge happens: before or after we perform operations,
  -- respectively (i.e., for (1) above we would merge after the op.,
  -- (2) is merged once prior to any other list operation.)
  --
  -- In practice I don't expect we will merge lists that frequently,
  -- so whatever makes the implementation simpler seems to be best.
  -- This seems to be (1), although it breaks a little the idea of
  -- having paths only at the leaves (we also have them at sum types,
  -- so this is not the only place).
  --
  -- Operations on sequences:
  --  - Generators
  --  - Morphisms
  --  - Eq
  --  - Emit* (only on Builders, should be simple in general?)
  --  - ArrayLen
  --  - Concat
  --  - FinishBuilder
  --  - ArrayIndex
  --  - ArrayStream
  --  - RangeUp/RangeDown
  --
  -- Also, having this rep. allows concrete lengths (the alternative
  -- is to use a muxvalue/basevalue for the length, which would introduce all
  -- sorts of complexities).
  | VSequence             (Branching (VSequenceMeta, [MuxValueF s]))
  | VMaybe                !(SumTypeMuxValueF (Maybe ()) s)

  -- We support symbolic keys, so we can't use Map here

  -- FIXME: This is pretty naieve, we might duplicate paths, for
  -- example.  We might also keep track of merge points (useful
  -- e.g. if conditionally updating a map)
  | VMap                   ![(MuxValueF s, MuxValueF s)]
  deriving (Show, Eq, Ord, Foldable, Traversable, Functor, Generic)

-- Used by the rest of the simulator
type MuxValue = MuxValueF SMTVar

-- Used internally to avoid naming after every single operation.
type MuxValueSExpr = MuxValueF SExpr

-- ----------------------------------------------------------------------------------------
-- ValueLens
--
-- This is useful for being a bit generic when dealing with base values.

-- Simpler way of abstracting over bool/ints (and maybe later floats)
data ValueLens v = ValueLens
  { vlMVPrism :: forall s. Prism' (MuxValueF s) (Typed (BaseValues v s))
  , vlFromInterpValue :: V.Value -> v
  , vlToInterpValue :: Type -> v -> V.Value
  , vlToSExpr       :: Type -> v -> S.SExpr
  }

vlFromMuxValue :: ValueLens v -> MuxValueF s -> Maybe (Typed (BaseValues v s))
vlFromMuxValue vl mv = mv ^? vlMVPrism vl

vlToMuxValue :: ValueLens v -> Typed (BaseValues v s) -> MuxValueF s
vlToMuxValue vl v = v ^. re (vlMVPrism vl)

-- FIXME: a bit gross?
symExecInt :: Type -> Integer -> SExpr
symExecInt ty i = SE.symExecOp0 (IntL i ty)

typedIso :: Type -> Iso (Typed a) (Typed b) a b
typedIso ty = iso typedThing (Typed ty)

integerVL :: ValueLens Integer
integerVL = ValueLens
  { vlMVPrism = #_VIntegers
  , vlFromInterpValue =
      \case
        V.VUInt _n i -> i
        V.VSInt _n i -> i
        V.VInteger i -> i
        _          -> panic "Unexpected value shape" []
  , vlToInterpValue = \ty i -> partial (evalOp0 (IntL i ty))
  , vlToSExpr =
      \case
        TUInt (TSize n) ->
          if n `mod` 4 == 0
          then S.bvHex (fromIntegral n)
          else S.bvBin (fromIntegral n)
        TSInt (TSize n) -> -- FIXME: correct?
          if n `mod` 4 == 0
          then S.bvHex (fromIntegral n)
          else S.bvBin (fromIntegral n)
        TInteger -> S.int
        ty -> panic "Unexpected type for base value" [showPP ty]
  }

boolVL :: ValueLens Bool
boolVL = ValueLens
  { vlMVPrism = #_VBools . from (typedIso TBool)
  , vlFromInterpValue =
      \case
        V.VBool b -> b
        _          -> panic "Unexpected value shape" []
  , vlToInterpValue = \_ -> V.VBool
  , vlToSExpr       = \_ -> S.bool
  }

-- -- Conversion to/from interp. values
-- toValue :: BVClass v -> Type -> v -> V.Value
-- toValue IntegerC ty i = partial (evalOp0 (IntL i ty))
-- toValue BoolC    _  b = V.VBool b

-- -- Defined only for types which can converted to BaseValues
-- fromValue :: BVClass v -> V.Value -> v
-- fromValue BoolC (V.VBool b) = b
-- fromValue IntegerC v =
--   case v of
--     V.VUInt _n i -> i
--     V.VSInt _n i -> i
--     V.VInteger i -> i
--     _          -> panic "Unexpected value shape" [showPP v]

-- baseValueToSExpr :: BVClass v -> Type -> v -> SExpr
-- baseValueToSExpr BoolC _ = S.bool
-- baseValueToSExpr IntegerC ty =
--   case ty of
--     TUInt (TSize n) ->
--       if n `mod` 4 == 0
--       then S.bvHex (fromIntegral n)
--       else S.bvBin (fromIntegral n)
--     TSInt (TSize n) -> -- FIXME: correct?
--       if n `mod` 4 == 0
--       then S.bvHex (fromIntegral n) 
--       else S.bvBin (fromIntegral n)
--     TInteger -> S.int
--     _ -> panic "Unexpected type for base value" [showPP ty]

-- Sequences

-- | This type is used to track where a sequence was generated, so
-- that we can link uses and defs for doing pool-generation of
-- sequenece elements.
type SequenceTag = GUID

data VSequenceMeta = VSequenceMeta
  { vsmGeneratorTag :: Maybe SequenceTag
  , vsmLoopCountVar :: Maybe LoopCountVar
  , vsmMinLength    :: Int
  -- ^ This is the minimum length of the list, it allows us to not
  -- generate cases which are impossible.
  , vsmIsBuilder    :: Bool
  } deriving (Show, Eq, Ord, Generic)

emptyVSequenceMeta :: VSequenceMeta
emptyVSequenceMeta = VSequenceMeta
  { vsmGeneratorTag = Nothing
  , vsmLoopCountVar = Nothing
  , vsmMinLength    = 0
  , vsmIsBuilder    = False
  }

-- -----------------------------------------------------------------------------
-- Values

-- | An empty base value is malformed and should not occur.
nullBaseValues :: BaseValues v s -> Bool
nullBaseValues = B.null

vSymbolicInteger :: Type -> s -> MuxValueF s
vSymbolicInteger ty s = VIntegers (Typed ty $ singletonSymBaseValues s)

vSymbolicBool :: s -> MuxValueF s
vSymbolicBool s = VBools (singletonSymBaseValues s)

vBool :: Bool -> MuxValueF b
vBool = VBools . singletonBaseValues

vInteger :: Type -> Integer -> MuxValueF b
vInteger ty = VIntegers . Typed ty . singletonBaseValues

-- | Construct a fixed-length sequence
vFixedLenSequence :: [MuxValueF b] -> MuxValueF b
vFixedLenSequence els =
  VSequence (B.singleton (emptyVSequenceMeta { vsmMinLength = length els }, els ))

-- Sum types

vNothing :: MuxValueF b
vNothing = VMaybe (singletonSumTypeMuxValueF Nothing VUnit)

vJust :: MuxValueF b -> MuxValueF b
vJust = VMaybe . singletonSumTypeMuxValueF (Just ())

asIntegers :: MuxValueF s -> Maybe (NonEmpty Integer)
asIntegers (VIntegers (Typed _ bvs))
  | Just is <- bvs ^? below _Left = NE.nonEmpty (toList is)
asIntegers _ = Nothing

-- ----------------------------------------------------------------------------------------
-- Multiplexing values

-- FIXME: should we check for emptiness?
muxBaseValues :: Ord v => Branching (BaseValues v SExpr) -> BaseValues v SExpr
muxBaseValues = join

-- These should all be well-formed
muxMuxValues :: Branching MuxValueSExpr -> MuxValueSExpr
muxMuxValues bmv =
  -- FIXME: this is a bit gross
  case B.select bmv of
    Just VUnit -> VUnit 
    Just (VIntegers (Typed ty _bvs)) -> mkBase integerVL ty
    Just VBools {} -> mkBase boolVL TBool
    Just VUnion {} -> VUnion $ stmvMerge #_VUnion
    Just VMaybe {} -> VMaybe $ stmvMerge #_VMaybe
    Just VStruct {} -> VStruct $ muxMuxValues <$> B.muxMaps (bmv ^?! below #_VStruct)
    -- join here is over Branching.
    Just VSequence {} -> VSequence (join (bmv ^?! below #_VSequence))
    Just VMap {} -> unsupported
    Nothing -> panic "Empty branching" []
  where
    unsupported = panic "Unsupported (VMap)" []

    stmvMerge :: Ord l => Prism' MuxValueSExpr (SumTypeMuxValueF l SExpr) ->
                 SumTypeMuxValueF l SExpr
    stmvMerge p = join (bmv ^?! below p)

    mkBase :: Ord v => ValueLens v -> Type -> MuxValueSExpr
    mkBase vl ty =
      let bvs' = muxBaseValues (bmv ^?! below (vlMVPrism vl . typedIso ty))
      in  vlToMuxValue vl (Typed ty bvs')
           
mux :: SemiCtxt m => Branching MuxValue -> SemiSolverM m MuxValue
mux bvs = nameSExprs v
  where
    v = muxMuxValues (fmap S.const <$> bvs)

op2 :: SemiCtxt m => Op2 -> Type -> Type -> Type ->
                     MuxValue -> MuxValue ->
                     SemiSolverM m MuxValue
op2 op rty ty1 ty2 v1 v2 =
  nameSExprs =<< semiExecOp2 op rty ty1 ty2 (S.const <$> v1) (S.const <$> v2)

conjBaseValues :: SemiCtxt m => [BaseValues Bool SExpr] ->
                  SemiSolverM m (BaseValues Bool SExpr)
conjBaseValues [] = panic "Empty conjBaseValues" []
conjBaseValues (v:vs) = foldlM go v vs
  where
    go bvs1 bvs2 =
      muxBinOp S.and (&&) TBool TBool bvs1 bvs2 boolVL

-- | Converts many symbolic values into a single value

-- ValueLens is for toSExpr
boolsToSExpr :: BaseValues Bool SExpr -> SExpr
boolsToSExpr bvs = B.toSExpr (go <$> bvs)
  where
    go (Left b)  = S.bool b
    go (Right s) = s

toSExpr :: MuxValue -> SExpr
toSExpr (VBools bvs)    = boolsToSExpr (over _Right S.const <$> bvs)
toSExpr v = panic "Non-base value" [show v]

toAssertion :: MuxValue -> Assertion
toAssertion (VBools bvs) = A.BAssert (go <$> bvs)
  where
    go (Left b) = A.BoolAssert b
    go (Right s) = A.SExprAssert (S.const s)
toAssertion _ = panic "Value has wrong shape: expected VBools" []

semiExecName :: (SemiCtxt m, HasCallStack) =>
                Name -> SemiSolverM m MuxValueSExpr
semiExecName n = asks (fromMaybe missing . Map.lookup n . localBoundNames)
  where
    missing = do
      -- ns <- asks localBoundNames
      -- let ppM kf vf m = braces (commaSep [ kf x <> " -> " <> vf y | (x, y) <- Map.toList m])
      panic "Missing name" [showPP n] -- , show (ppM pp (pp . fmap (text . typedThing)) ns)]


bindNameIn :: Monad m => Name -> MuxValueSExpr ->
              SemiSolverM m a -> SemiSolverM m a
bindNameIn n v = locally #localBoundNames (Map.insert n v)

-- -- -- Stolen from Synthesis
-- -- -- projectEnvFor :: FreeVars t => t -> I.Env -> SynthEnv -> Maybe I.Env
-- -- -- projectEnvFor tm env0 se = doMerge <$> Map.traverseMaybeWithKey go (synthValueEnv se)
-- -- --   where
-- -- --     frees = freeVars tm

-- -- --     doMerge m = env0 { I.vEnv = Map.union m (I.vEnv env0) }

-- -- --     go k v | k `Set.member` frees = Just <$> projectInterpValue v
-- -- --     go _ _                        = Just Nothing

-- -- -- projectEnvForM :: FreeVars t => t -> SynthesisM I.Env
-- -- -- projectEnvForM tm = do
-- -- --   env0 <- getIEnv
-- -- --   m_e <- SynthesisM $ asks (projectEnvFor tm env0)
-- -- --   case m_e of
-- -- --     Just e  -> pure e
-- -- --     Nothing -> panic "Captured stream value" []

-- -- Stolen from Daedalus.Core.Semantics.Expr.  Returns Nothing if the
-- -- pattern will naver match, otherwise Just (sideConds)
-- matches' :: GuardedSemiSExpr -> Pattern -> Bool
-- matches' v pat =
--   case (pat, v) of
--     (PAny, _) -> found
--     (_, VValue v') -> matches pat v'
--     (_, VOther {}) -> panic "Saw VOther" []
--     (PNothing, VNothing) -> found
--     (PJust, VJust {})    -> found
--     (PCon l, VUnionElem l' _) -> l == l'
--     _ -> False
--   where
--     found = True

-- -- Case is a bit tricky
-- --
-- -- In general, if we have values (g1, v1) ... (gn, vn) and patterns
-- -- p1, ..., pM, then a symbolic vk may need to be run on all alts,
-- -- with the constraint that the pattern matches the value.  We also
-- -- require both that some value is reachable, and that
-- --
-- -- For each pattern we generate a refined value (may not be needed,
-- -- but might be more efficient and help in debugging) and a path
-- -- condition extension.
-- --
-- -- For example, consider the following case
-- --
-- --   case x of
-- --     1 -> r1
-- --     2 -> r2
-- --
-- -- Given x is the singleton [ (g1, VOther n) ] we generate the path condition
-- -- (n = 1) for r1, (n = 2) for r2, and
-- --
-- --      (g1 --> (n = 1 \/ n = 2))
-- --
-- -- which says if the value is reachable, then one branch must be true.
-- -- We also assert g1 to say that some value must be reachable (is this required?)
-- -- Finally, we update x to be (g1 /\ n = 1, VValue 1) in r1 and
-- -- (g1 /\ n = 2, VValue 2) in r2.
-- --
-- --
-- -- For the case of multiple values, say
-- --
-- --  [ (g1, VOther n), (g2, VValue 1), (g3, VValue 42) ]
-- --
-- -- we have for
-- --  r1: x = [ (g1 /\ n = 1, VOther 1), (g2, VValue 2) ]
-- --  r2: x = [ (g1 /\ n = 2, VOther 2) ]
-- --
-- -- (g1 --> (n = 1 \/ n = 2)) /\ (g2 --> True) /\ (g3 --> False)

-- -- ====================
-- -- x = First
-- --   inl = {}
-- --   inr = {}
-- -- case x of
-- --   inl -> {}
-- --
-- -- x = [ (c = 0, inl), (c = 1, inr) ]

-- -- ====================
-- -- x = ^1 | ^2 -- x |-> [ (c = 0, 1), (c = 1, 2) ]
-- -- y = ^ x -- x |-> [ (c = 0, 1), (c = 1, 2) ], y |-> [ (c = 0, 1), (c = 1, 2) ]
-- --
-- -- case x of
-- --   1 -> { case (y = 1) of true -> {} }

-- -- ====================
-- -- x = UInt8 -- x |-> [ (True, VOther n) ]
-- -- y = ^ x   -- x |-> [ (True, VOther n) ]; y |-> [ (True, VOther n) ]
-- -- case x of
-- --   1 -> -- PC = { n = 1 }
-- --        { let p = (y = 1) -- p |-> [ ( True, VOther (n = 1) ) ]
-- --        ; case p of true -> {}
-- --        -- n = 1 = (True /\ n = 1)
-- --        }
-- -- (True --> n = 1)

-- -- ==================== (data dep)
-- -- x = First
-- --   A = {}
-- --   B = {}
-- --   C = {}
-- -- y = case x of
-- --   A -> 1
-- --   B -> 2
-- --   C -> 1
-- -- z = (y = 1) -- z |-> [ (c = 0, true), (c = 1, false), (c = 2, true) ]
-- -- case z of true -> {}
-- -- either: (c = 0 /\ true) \/ )c = 1 /\ false) \/ (c = 2 /\ true)
-- -- or: (c = 0 --> true) /\ (c = 1 --> false) /\ (c = 2 --> true)

-- -- x = [ (c = 0, inl), (c = 1, inr) ]


-- -- ASSUME: values in env are non-empty
-- --

-- -- | This function calculates which RHSs of the case are enabled by
-- -- the values for the target variable.  For each RHS which is enabled,
-- -- this function returns the valueguards for the values which match
-- -- the pattern.  These valueguards are those for the values, extended
-- -- with predicates claiming that the value matches the pattern; this
-- -- only matters for symbolic values, where we assert that the symbolic
-- -- value matches the pattern, or, for the case of the default pattern,
-- -- that the value _doesn't_ match the other patterns.
-- --
-- -- The second result is a list of (valueguard, predicates) which are
-- -- used to ensure that some value matches a pattern, and that values
-- -- which do not match any pattern are discarded.

-- -- FIXME: we could also refine the targeted value, but it may not help
-- -- much (we could e.g. throw away the non-matching values and then
-- -- merge the environment at each join point, which might make the code
-- -- more efficient?)

branchingPatterns :: Ord v => PathVar -> [v] -> Bool -> Branching v -> 
                     (Branching Assertion, Set Int)
branchingPatterns pv vs hasAny b =
  ( ba, if hasAny then mempty else missing)
  where
    missing = Set.fromList allIxs `Set.difference` seen
    allIxs | hasAny    = [0..length vs]
           | otherwise = [0..length vs - 1]

    vmap = Map.fromList (zip vs [0..])
    (ba, seen) = runWriter (traverse go b)
    go v
      | Just i <- Map.lookup v vmap = tell (Set.singleton i) $> A.PSAssert (pvIs i)
      | otherwise = pure (A.BoolAssert hasAny)
           
    pvIs = PS.choiceConstraint pv

baseValuesPatterns :: Ord v => PathVar -> ValueLens v -> [v] -> Bool ->
                      Type -> BaseValues v SMTVar -> 
                      (Assertion, Set Int)
baseValuesPatterns pv vl vs hasAny ty bvs =
  ( A.BAssert $ B.disjointUnion ca sa
  , if B.null sa then missing else mempty
  )
  where
    -- conc and symb are disjoint so we could e.g. re-merge
    (cb, sb) = B.partitionEithers bvs    
    (ca, missing) = branchingPatterns pv vs hasAny cb

    sa    = sgo . S.const <$> sb

    svs   = map (vlToSExpr vl ty) vs
    sgo s = A.BAssert $ B.branching (dflt s ++ imap (\i v -> (pvIs i, A.SExprAssert (S.eq s v))) svs)
    dflt s | hasAny    = [ (pvIs (length vs), A.SExprAssert (S.distinct (s : svs))) ]
           | otherwise = []
           
    pvIs = PS.choiceConstraint pv

stvmPatterns :: Ord l => PS.PathVar -> [l] -> Bool -> SumTypeMuxValueF l SMTVar ->
                (Assertion, Set Int)
stvmPatterns pv vs hasAny =
  over _1 A.BAssert . branchingPatterns pv vs hasAny . fmap fst

-- Returns an Assertion constraining the allowed values of pv, along
-- with the set of patterns which are not matched --- this is
-- important as it allows recursion etc. to terminate when a check
-- fails (concretely).
semiExecPatterns :: HasCallStack => MuxValue -> PS.PathVar -> [Pattern] ->
                    (Assertion, Set Int)
semiExecPatterns mv pv pats = 
  case pats'  of
    []            -> panic "Empty patterns" []
    PBool {}  : _ -> bvPat boolVL #_PBool
    PNum {}   : _ -> bvPat integerVL #_PNum          
    PNothing  : _ | VMaybe m <- mv -> stmvPat mbPat m
    PJust     : _ | VMaybe m <- mv -> stmvPat mbPat m
    PCon {}   : _ | VUnion m <- mv -> stmvPat conPat m
    PBytes {} : _ -> unexpected -- should be erased by one of the passes
    _             -> unexpected
  where
    mbPat PNothing = Nothing
    mbPat PJust    = Just ()
    mbPat _        = unexpected

    conPat (PCon l) = l
    conPat _        = unexpected

    -- Primitive.
    bvPat :: Ord v => ValueLens v -> Prism' Pattern v -> (Assertion, Set Int)
    bvPat vl p
      | Just (Typed ty bvs) <- vlFromMuxValue vl mv =
          let vs = fromMaybe unexpected (traverse (preview p) pats')
          in baseValuesPatterns pv vl vs hasAny ty bvs
      | otherwise = unexpected

    stmvPat :: Ord l => (Pattern -> l) -> SumTypeMuxValueF l SMTVar ->
               (Assertion, Set Int)
    stmvPat getPat = stvmPatterns pv (map getPat pats') hasAny

    -- ASSUME that a PAny is the last element
    pats'  = filter ((/=) PAny) pats
    hasAny = PAny `elem` pats

    unexpected = panic "Unexpected pattern" []

-- semiExecCase :: (Monad m, HasGUID m, HasCallStack) =>
--                 Case a ->
--                 SemiSolverM m ( [ ( NonEmpty PathCondition, (Pattern, a)) ]
--                         1      , [ (PathCondition, ValueMatchResult) ] )
-- semiExecCase (Case y pats) = do
--   els <- guardedValues <$> semiExecName y
--   let (vgs, preds) = unzip (map goV els)
--       vgs_for_pats = map (nonEmpty . concat) (transpose vgs)
--       allRes       = zipWith (\a -> fmap (, a)) pats vgs_for_pats
--   pure (catMaybes allRes, concat preds)
--   where
--     -- This function returns, for a given value, a path-condition for
--     -- each pattern stating when that PC is enabled, and a predicate
--     -- for when some pattern matched.
--     goV :: (PathCondition, GuardedSemiSExpr) ->
--            ( [ [PathCondition] ], [ (PathCondition, ValueMatchResult) ] )
--     -- Symbolic case
--     goV (g, VOther x) =
--       let basePreds = map (\p -> SE.patternToPredicate ty p (S.const (typedThing x))) pats'
--           (m_any, assn)
--             -- if we have a default case, the constraint is that none
--             -- of the above matched
--             | hasAny    = ([ VPCNegative (Set.fromList pats') ], YesMatch)
--             | otherwise = ([], SymbolicMatch (orMany basePreds))
--           vgciFor c = Typed ty c
--           mkG c = [ g' | let g' = PC.insertValue (typedThing x) (vgciFor c) g
--                        , PC.isFeasibleMaybe g' ]
--       in ( map mkG (map VPCPositive pats' ++ m_any)
--          , [ (g, assn) ])

--     -- This only matches the bytestring pattern.
--     goV (g, VSequence vsm vs) =
--       let ms = map (bytesPatternMatch g vsm vs) pats'
--           m_any = [ [g] | hasAny ]
--       in ( ms ++ m_any, [ (g', SymbolicMatch (S.bool True)) | gs' <- ms, g' <- gs'] ) -- FIXME

--     -- v is a concrete value.
--     goV (g, v) =
--       let ms = map (matches' v) pats'
--           noMatch = not (or ms)
--           m_any = [ noMatch | hasAny ]
--           assn | hasAny || not noMatch = YesMatch
--                | otherwise             = NoMatch
--       in ( [ [g | b] | b <- ms ++ m_any]
--          , [ (g, assn) ] )

--     -- ASSUME that a PAny is the last element
--     pats'  = [ p | p <- map fst pats, p /= PAny ]
--     hasAny = PAny `elem` map fst pats

--     ty = typeOf y

-- -----------------------------------------------------------------------------
-- Monad

data SemiSolverEnv = SemiSolverEnv
  { localBoundNames :: Map Name MuxValueSExpr
  , currentName     :: Text
  } deriving (Generic)

-- typeDefs :: SemiSolverEnv -> Map TName TDecl
-- typeDefs = I.tEnv . interpEnv

type SemiCtxt m = (Monad m, MonadIO m, HasGUID m, LiftStrategyM m, LiftTalosM m)

-- We may try to insert multiple times (presumably at the same type).
type SemiState = Set (Typed SMTVar)

type SemiSolverM m = ExceptT () (StateT SemiState (ReaderT SemiSolverEnv (SolverT m)))

runSemiSolverM :: SemiCtxt m =>
                  Map Name MuxValue ->
                  Text ->
                  SemiSolverM m a -> SolverT m (Either () a, Set (Typed SMTVar))
runSemiSolverM lenv pfx m =
  runReaderT (runStateT (runExceptT m) mempty) (SemiSolverEnv lenvSExpr pfx)
  where
    lenvSExpr = fmap S.const <$> lenv

nameSExprs :: SemiCtxt m => MuxValueSExpr -> SemiSolverM m MuxValue
nameSExprs mv =
  case mv of
    VUnit -> pure VUnit
    VIntegers (Typed ty bvs) ->
      VIntegers . Typed ty <$> traverseOf (traversed . _Right) (sexprAsSMTVar ty) bvs
    VBools bvs -> VBools <$> traverseOf (traversed . _Right) (sexprAsSMTVar TBool) bvs
    VUnion m -> VUnion <$> goSTVM m
    VMaybe m -> VMaybe <$> goSTVM m 
    VStruct m -> VStruct <$> traverse nameSExprs m
    VSequence bvs -> VSequence <$> traverseOf (traverse . _2 . traverse) nameSExprs bvs
    VMap {} -> unsupported
  where
    unsupported = panic "Unsupported (VMap)" []
    
    goSTVM :: SemiCtxt m => SumTypeMuxValueF k SExpr ->
              SemiSolverM m (SumTypeMuxValueF k SMTVar)
    goSTVM = traverseOf (traversed . _2) nameSExprs

sexprAsSMTVar :: (SemiCtxt m) => Type -> SExpr -> SemiSolverM m SMTVar
sexprAsSMTVar _ty (S.Atom x) = pure x
sexprAsSMTVar ty e = do
  -- c.f. PathSymbolic.Monad.makeNicerName
  n <- asks currentName
  let n' = "N" <> "." <> n
  liftSolver (defineSymbol n' (symExecTy ty) e)

nameSExprForSideCond :: SemiCtxt m => Type -> SExpr -> SemiSolverM m SMTVar
nameSExprForSideCond ty sexpr = do
  s <- sexprAsSMTVar ty sexpr
  modify (Set.insert (Typed ty s))
  pure s

nameLoopCountVar :: SemiCtxt m => SExpr -> SemiSolverM m LoopCountVar
nameLoopCountVar sexpr = LoopCountVar <$> nameSExprForSideCond sizeType sexpr
  
unreachable :: SemiCtxt m => SemiSolverM m a
unreachable = throwError ()

-- getMaybe :: SemiCtxt m => SemiSolverM m a -> SemiSolverM m (Maybe a)
-- getMaybe = lift . runMaybeT

-- putMaybe :: SemiCtxt m => SemiSolverM m (Maybe a) -> SemiSolverM m a
-- putMaybe m = hoistMaybe =<< m

-- hoistMaybe :: SemiCtxt m => Maybe a -> SemiSolverM m a
-- hoistMaybe r =
--   case r of
--     Nothing -> fail "Ignored"
--     Just v  -> pure v

-- collectMaybes :: SemiCtxt m => [SemiSolverM m a] -> SemiSolverM m [a]
-- collectMaybes = fmap catMaybes . mapM getMaybe

-- -- | Strips out failing values and produces a single value.
-- gseCollect :: SemiCtxt m => [SemiSolverM m GuardedSemiSExprs] ->
--               SemiSolverM m GuardedSemiSExprs
-- gseCollect gvs = collectMaybes gvs >>= hoistMaybe . unions'
--   -- gvs' <- collectMaybes gvs
--   -- let m_v   = unions' gvs'
--   --     ngvs  = length gvs
--   --     ngvs' = length gvs'
--   --     nvs   = length (concatMap guardedValues gvs')
--   --     nvs'  = maybe 0 (length . guardedValues) m_v

--   -- T.statistic (muxKey <> "collect") (Text.pack $ printf "non-empty: %d/%d pruned: %d/%d" ngvs' ngvs (nvs - nvs') nvs)
--   -- hoistMaybe m_v

-- getEFun :: SemiCtxt m => FName -> SemiSolverM m (Fun Expr)
-- getEFun f = do
--   fdefs <- liftStrategy getFunDefs
--   case Map.lookup f fdefs of
--     Just fn -> pure fn
--     Nothing -> panic "Missing pure function" [showPP f]

--------------------------------------------------------------------------------
-- Exprs

semiExecExpr :: (SemiCtxt m, HasCallStack) => Expr -> SemiSolverM m MuxValue
semiExecExpr e = nameSExprs =<< semiExecExpr' e
  
semiExecExpr' :: (SemiCtxt m, HasCallStack) =>
                 Expr -> SemiSolverM m MuxValueSExpr
semiExecExpr' expr =
  case expr of
    Var n          -> semiExecName n
    PureLet n e e' -> do
      ve  <- semiExecExpr' e
      -- FIXME: duplicates; we might want to name here if n occurs in e' multiple times
      bindNameIn n ve (semiExecExpr' e')

    Struct _ut ctors -> do
      let (ls, es) = unzip ctors
      VStruct . Map.fromList . zip ls <$> mapM semiExecExpr' es

    ECase {}  -> impossible
    ELoop _lm -> impossible

    Ap0 op       -> pure (semiExecOp0 op)
    Ap1 op e     -> semiExecOp1 op rty =<< semiExecExpr' e
    Ap2 op e1 e2 ->
      join (semiExecOp2 op rty (typeOf e1) (typeOf e2)
            <$> semiExecExpr' e1
            <*> semiExecExpr' e2)
    Ap3 {} -> unimplemented -- MapInsert, RangeUp, RangeDown
      -- join (semiExecOp3 op rty (typeOf e1)
      --        <$> semiExecExpr' e1
      --        <*> semiExecExpr' e2
      --        <*> semiExecExpr' e3)
    ApN (ArrayL _ty) es -> vFixedLenSequence <$> mapM semiExecExpr' es
    ApN (CallF {}) _es  -> impossible
  where
    rty = typeOf expr
    unimplemented = panic "semiExecExpr': UNIMPLEMENTED" [showPP expr]
    impossible = panic "semiExecExpr': IMPOSSIBLE" [showPP expr]

semiExecOp0 :: Op0 -> MuxValueSExpr
semiExecOp0 op =
  case op of
    Unit          -> VUnit
    IntL i ty     -> vInteger ty i
    FloatL {}     -> unimplemented
    BoolL b       -> vBool b
    ByteArrayL bs -> vFixedLenSequence (map (vInteger tByte . fromIntegral) (BS.unpack bs))
    NewBuilder _ty -> VSequence (B.singleton (emptyVSequenceMeta {vsmIsBuilder = True}, []))
    MapEmpty _kty _vty -> VMap []
    ENothing _ty    -> vNothing
  where
    unimplemented = panic "semiExecOp0: UNIMPLEMENTED" [showPP op]

semiExecOp1 :: SemiCtxt m => Op1 -> Type -> 
               MuxValueSExpr ->
               SemiSolverM m MuxValueSExpr
semiExecOp1 op rty mv =
  case op of
    CoerceTo {}   -> viaInterp integerVL integerVL
    IsEmptyStream -> unsupported
    Head          -> unsupported
    StreamOffset  -> unsupported
    BytesOfStream -> unsupported
    OneOf {}      -> viaInterp integerVL boolVL
    Neg           -> viaInterp integerVL integerVL
    BitNot        -> viaInterp integerVL integerVL
    Not           -> viaInterp boolVL boolVL
    ArrayLen | VSequence bvs <- mv -> do
      let go (vsm, els)
            | Just lcv <- vsmLoopCountVar vsm = vSymbolicInteger sizeType (PS.loopCountVarToSExpr lcv)
            | otherwise = vInteger sizeType (toInteger (length els))
      pure $ muxMuxValues (go <$> bvs)

    Concat        -> unsupported
    FinishBuilder | VSequence bvs <- mv -> do
      let go = _1 . #vsmIsBuilder .~ True
      pure $ VSequence (go <$> bvs)

    NewIterator  -> unsupported
    IteratorDone -> unsupported
    IteratorKey  -> unsupported
    IteratorVal  -> unsupported
    IteratorNext -> unsupported

    EJust        -> pure $ vJust mv
    -- We can ignore pcs here as we should be guarded by a case.  We
    -- could (should?) refine the case var to discard the pc and
    -- non-matching ctors.
    FromJust
      | VMaybe m <- mv -> pure $ muxMuxValues $ do
          (l, v) <- m
          unless (isJust l) B.empty
          pure v

    SelStruct _ty l
      | VStruct m <- mv, Just mv' <- Map.lookup l m -> pure mv'

    InUnion _ut l ->
      pure $ VUnion (singletonSumTypeMuxValueF l mv)

    -- c.f. FromJust
    FromUnion _ty l
      | VUnion m <- mv -> pure $ muxMuxValues $ do
          (l', v) <- m
          unless (l == l') B.empty
          pure v

    WordToFloat  -> unsupported
    WordToDouble -> unsupported
    IsNaN        -> unsupported
    IsInfinite   -> unsupported
    IsDenormalized -> unsupported
    IsNegativeZero -> unsupported
    _ -> panic "Malformed Op1 expression" [showPP op]
    where
      unsupported = panic "Unsupported Op1" [showPP op]
      viaInterp :: (Ord w, SemiCtxt m) => ValueLens v -> ValueLens w ->
                   SemiSolverM m MuxValueSExpr
      viaInterp (ValueLens { vlMVPrism = fromP, vlToInterpValue = argToI })
                (ValueLens { vlFromInterpValue = fromI, vlMVPrism = toP })
        | Just (Typed ty' bvs) <- mv ^? fromP = do
            let go (Left v) = Left . fromI . evalOp1 mempty op ty' . argToI ty' $ v
                go (Right s) = Right $ SE.symExecOp1 op ty' s
            pure (Typed rty (go <$> bvs) ^. re toP)
        | otherwise = panic "Malformed Op1 value" [showPP op]

-- typeToElType :: Type -> Maybe Type
-- typeToElType ty =
--   case ty of
--     TBuilder elTy -> Just elTy
--     TArray   elTy -> Just elTy
--     _ -> Nothing

-- -- Short circuiting op
-- -- scBinOp :: MonadReader SemiSolverEnv m =>
-- --   (SExpr -> SExpr -> SExpr) ->
-- --   (SemiSExpr -> SemiSExpr) ->
-- --   (SemiSExpr -> SemiSExpr) ->
-- --   SemiSExpr -> SemiSExpr -> m SemiSExpr
-- -- scBinOp op tc fc x y =
-- --   case (x, y) of
-- --     (VBool True, _)  -> pure (tc y)
-- --     (VBool False, _) -> pure (fc y)
-- --     (_, VBool True)  -> pure (tc x)
-- --     (_, VBool False) -> pure (fc x)
-- --     _ -> do
-- --       tys <- asks typeDefs
-- --       pure (vSExpr TBool (op (semiSExprToSExpr tys TBool x)
-- --                              (semiSExprToSExpr tys TBool y)))

-- -- bAnd, bOr :: MonadReader SemiSolverEnv m => SemiSExpr -> SemiSExpr -> m SemiSExpr
-- -- bAnd = scBinOp S.and id (const (VBool False))
-- -- bOr  = scBinOp S.or (const (VBool True)) id

-- bOpMany :: SemiCtxt m => Bool -> PathCondition ->
--            [GuardedSemiSExprs] ->
--            SemiSolverM m GuardedSemiSExprs
-- bOpMany opUnit g svs = gseCollect (map mkOne combs)
--   where
--     elss = map guardedValues svs
--     -- This is a sneaky way to get the list of products of the
--     -- members.  This is potentially very expensive.
--     combs = sequence elss

--     mkOne (unzip -> (gs, els)) =
--       go els (mconcat (g : gs))

--     go els g'
--       | PC.isInfeasible g' = fail "Ignored"
--       | any (isBool absorb) els = mkB g' absorb
--       | otherwise = do
--           tys <- asks typeDefs
--           -- strip out units and convert to sexpr
--           let nonUnits = [ toSExpr1 tys TBool sv
--                          | sv <- els, not (isBool opUnit sv) ]
--           case nonUnits of
--             [] -> mkB g' opUnit
--             [el] -> vSExpr g' TBool el
--             _    -> vSExpr g' TBool (op nonUnits)

--     mkB g' = pure . vBool g'

--     isBool b (VValue (V.VBool b')) = b == b'
--     isBool _ _ = False
--     op = if opUnit then andMany else orMany
--     absorb = not opUnit

-- bAndMany, bOrMany :: SemiCtxt m => PathCondition ->
--                      [GuardedSemiSExprs] ->
--                      SemiSolverM m GuardedSemiSExprs
-- bAndMany = bOpMany True
-- bOrMany  = bOpMany False

-- reportingSemiExec2 :: SemiCtxt m =>
--   LogKey ->
--   (PathCondition -> GuardedSemiSExpr -> GuardedSemiSExpr ->
--    SemiSolverM m GuardedSemiSExprs) ->
--   GuardedSemiSExprs ->
--   GuardedSemiSExprs ->
--   SemiSolverM m GuardedSemiSExprs
-- reportingSemiExec2 key go gvs1 gvs2 = do
--   let rs = [ go g sv1 sv2
--            | (g1, sv1) <- guardedValues gvs1
--            , (g2, sv2) <- guardedValues gvs2
--            , let g = g1 <> g2, PC.isFeasibleMaybe g
--            ]
--   r <- gseCollect rs
--   let ncomb = length (guardedValues gvs1) * length (guardedValues gvs2)
--       nres  = length rs
--       lv1   = length (guardedValues gvs1)
--       lv2   = length (guardedValues gvs2)
--       nv1   = length [ () | (g, _) <- guardedValues gvs1, PC.isFeasibleMaybe g ]
--       nv2   = length [ () | (g, _) <- guardedValues gvs2, PC.isFeasibleMaybe g ]
--   T.statistic (muxKey <> "exec2" <> key)
--     (Text.pack $ printf "pruned: %d/%d v1: %d/%d v2: %d/%d"
--                         (ncomb - nres) ncomb
--                         (lv1 - nv1) lv1
--                         (lv2 - nv2) lv2
--     )
--   pure r

-- semiExecEq, semiExecNEq :: SemiCtxt m => Type ->
--                            GuardedSemiSExprs -> GuardedSemiSExprs ->
--                            SemiSolverM m GuardedSemiSExprs
-- semiExecEq  = semiExecEqNeq True
-- semiExecNEq = semiExecEqNeq False

-- semiExecEqNeq :: SemiCtxt m => Bool -> Type ->
--                  GuardedSemiSExprs ->
--                  GuardedSemiSExprs ->
--                  SemiSolverM m GuardedSemiSExprs
-- semiExecEqNeq iseq ty = reportingSemiExec2 "eqneq" go
--   where
--     go g sv1 sv2 = do
--       let mkB = pure . vBool g

--       tys <- asks typeDefs

--       case (sv1, sv2) of
--         (VValue v1, VValue v2) -> mkB (v1 `eqcmp` v2)
--         (VUnionElem l sv1', VUnionElem l' sv2')
--           | l == l', Just (_, ty') <- typeAtLabel tys ty l ->
--               semiExecEqNeq iseq ty' sv1' sv2'
--           | l == l'    -> panic "Missing label" [showPP l]
--           | otherwise -> mkB (not iseq)

--         (VStruct flds1, VStruct flds2) ->
--           opMany g =<< zipWithM (go' tys) flds1 flds2

--         (VJust sv1', VJust sv2')
--           | TMaybe ty' <- ty -> semiExecEqNeq iseq ty' sv1' sv2'
--         (VJust {}, VNothing) -> mkB (not iseq)
--         (VNothing, VJust {}) -> mkB (not iseq)

--         -- List/sequence equality.  We don't have the ability to just
--         -- assert lengths are equal, so we have to add as a path
--         -- condition.
--         _ | Just (vsm1, svs1) <- gseToList sv1
--           , Just (vsm2, svs2) <- gseToList sv2
--           , Just elTy <- typeToElType ty ->
--             let go1 g' svs1' svs2'
--                   | length svs1' /= length svs2' = pure (vBool g' (not iseq))
--                   | otherwise = opMany g' =<< zipWithM (semiExecEqNeq iseq elTy) svs1' svs2'
--             in gseCollect [ go1 g' svs1' svs2'
--                           | (g1, svs1') <- explodeSequence vsm1 svs1
--                           , (g2, svs2') <- explodeSequence vsm2 svs2
--                           , let g' = g <> g1 <> g2, PC.isFeasibleMaybe g'
--                           ]
--         _ -> do
--           se <- liftSolver (SE.symExecOp2 op TBool
--                             (toSExpr1 tys ty sv1)
--                             (toSExpr1 tys ty sv2))
--           vSExpr g TBool se

--     (op, eqcmp, opMany) =
--       if iseq
--       then (Eq   , (==), bAndMany)
--       else (NotEq, (/=), bOrMany)

--     go' tys (l, sv1') (l', sv2') =
--       if l == l'
--       then case typeAtLabel tys ty l of
--              Just (_, ty') -> semiExecEqNeq iseq ty' sv1' sv2'
--              _             -> panic "Missing label" [showPP l]
--       else panic "Label mismatch" [showPP l, showPP l']

sequenceEq :: SemiCtxt m =>
              (VSequenceMeta, [MuxValueSExpr]) ->
              (VSequenceMeta, [MuxValueSExpr]) ->
              SemiSolverM m (BaseValues Bool SExpr)              
sequenceEq (vsm1, els1) (vsm2, els2) =
  case  (vsmLoopCountVar vsm1, vsmLoopCountVar vsm2) of
    (Nothing, Nothing) ->
      if length els1 /= length els2
      then pure neverEq
      else do
        bvs <- conjBaseValues =<< zipWithM semiExecEq els1 els2
        -- FIXME: figure out how to keep null base values inside the basevalue functions
        if nullBaseValues bvs
          then unreachable
          else pure bvs
    (Nothing, Just szv)
      | length els2 < length els1 -> pure neverEq
      | length els1 < vsmMinLength vsm2 -> pure neverEq
      | otherwise -> do
          ses <- makeEqs
          let lenEqAssn = S.eq (PS.loopCountVarToSExpr szv) (PS.loopCountToSExpr (length els1))
              assn = andMany (lenEqAssn : ses)
          pure (singletonSymBaseValues assn)
    -- Symmetric, just use the above case.
    (Just {}, Nothing)  -> sequenceEq (vsm2, els2) (vsm1, els1)
    (Just szv1, Just szv2)
      | length els1 < minLen || length els2 < minLen -> pure neverEq
      | otherwise -> do
          ses <- makeEqs
          -- FIXME: duplicates; we may need to name the size terms if they are not vars.
          let lenEqAssn = S.eq (PS.loopCountVarToSExpr szv1) (PS.loopCountVarToSExpr szv2)
              (always, conds0) = splitAt minLen ses
              guardLen n = S.implies (S.bvULt (PS.loopCountToSExpr n) (PS.loopCountVarToSExpr szv1))
              conds = zipWith guardLen [minLen ..] conds0
              assn = andMany (lenEqAssn : always ++ conds)
          pure (singletonSymBaseValues assn)
  where
    -- FIXME: do we need a path here?  This is unconditionally false.
    neverEq = singletonBaseValues False
    minLen  = max (vsmMinLength vsm1) (vsmMinLength vsm2)

    makeEqs =
      map boolsToSExpr <$> zipWithM semiExecEq els1 els2

{-


def Main =
  v1 = Choose -- p1 
    Left = UInt8 -- b1
    Right = UInt8 -- b2
  v2 = Choose -- p2 
    Left = UInt8 -- c1
    Right = UInt8 -- c2
  case v1 == v2 of
    true -> ()

want:
  (p1 = 0 /\ p2 = 0 /\ b1 == c1) \/ (p1 = 1 /\ p2 = 1 /\ b2 = c2)

  False ==> (p1 = 0 /\ p2 = 1) \/ (p1 = 1 /\ p2 = 0) 
  symb: Just (b1 == c2)



def Main =
  v1 = Choose -- p1 
    Left = (UInt8 -- b1
           | 3) -- p3
    Right = UInt8 -- b2
  v2 = Choose -- p2 
    Left = (UInt8 -- c1
           | 3) -- p4
    Right = UInt8 -- c2
  case v1 == v2 of
    true -> ()


  p1 = 0, p2 = 0
   p3 = 0, p4 = 0
    b1 = c1
   p3 = 0, p4 = 1
    b1 = 3
   p3 = 1, p4 = 0
    3  = c1
   p3 = 1, p4 = 1
    True

value under Left is
   True => { p3 = 1 /\ p0 = 1 }
   symb: if p3 = 0 /\ p4 = 0
         then b1 = c1
         else if p3 = 0 /\ p4 = 1
         then b1 = 3
         else 3  = c1  -- p3 = 0, p4 = 1 is implicit (?)

value under Right is
   symb: b2 = c2

combining is
   False => { p1 = 0 /\ p2 = 1 \/ p1 = 1 /\ p2 = 0 }
   True => { p3 = 1 /\ p4 = 1 /\ p1 = 0 /\ p2 = 0 }
   symb: if p1 = 0 /\ p2 = 0 
           if p3 = 0 /\ p4 = 0
           then b1 = c1
           else if p3 = 0 /\ p4 = 1
           then b1 = 3
           else 3  = c1  -- p3 = 0, p4 = 1 is implicit (?)
         else b2 = c2 -- p1 = 1 /\ p2 = 1

-}

semiExecEq :: SemiCtxt m => MuxValueSExpr -> MuxValueSExpr -> SemiSolverM m (BaseValues Bool SExpr)
semiExecEq mv1 mv2 =  
  case (mv1, mv2) of
    (VUnit, VUnit) -> pure (singletonBaseValues True)
    (VIntegers (Typed ty1 bvs1), VIntegers (Typed ty2 bvs2)) ->
      muxBinOp S.eq (==) ty1 ty2 bvs1 bvs2 integerVL
    (VBools bvs1, VBools bvs2) ->
      muxBinOp S.eq (==) TBool TBool bvs1 bvs2 boolVL
    (VUnion m1, VUnion m2)   -> stmvEq m1 m2
    (VStruct m1, VStruct m2) -> do
      let vs = toList $ Map.intersectionWith (,) m1 m2 -- should always work
      bvs <- conjBaseValues =<< mapM (uncurry semiExecEq) vs
      if nullBaseValues bvs
        then unreachable
        else pure bvs
  
    (VSequence bvs1, VSequence bvs2) -> do
      bvs <- muxBaseValues <$> sequence (sequenceEq <$> bvs1 <*> bvs2)
      if nullBaseValues bvs
        then unreachable
        else pure bvs
    
    (VMaybe m1, VMaybe m2) -> stmvEq m1 m2
    (VMap _els1, VMap _els2) -> unsupported    
    _ -> panic "Incompatible value comparison" []
  where
    unsupported = panic "Unsupported Eq?NotEq comparison" []

    stmvEq :: (SemiCtxt m, Eq l) => SumTypeMuxValueF l SExpr ->
              SumTypeMuxValueF l SExpr ->
              SemiSolverM m (BaseValues Bool SExpr)             
    stmvEq m1 m2 = muxBaseValues <$> sequence r
      where
        r = do
          (l1, v1) <- m1
          (l2, v2) <- m2
          pure $ if l1 == l2
            then semiExecEq v1 v2
            else pure (singletonBaseValues False)

semiExecEmit :: SemiCtxt m => MuxValueSExpr -> (VSequenceMeta, [MuxValueSExpr]) -> 
                SemiSolverM m (VSequenceMeta, [MuxValueSExpr])
semiExecEmit new (vsm, els)
  | Just lcv <- vsmLoopCountVar vsm = do
      let minLen = vsmMinLength vsm
          (pfx, rest) = splitAt minLen els
          mkNew i old =
            muxMuxValues $ B.branching [ (PS.loopCountEqConstraint lcv i, new)
                                       , (PS.loopCountGtConstraint lcv i, old)
                                       ]
          newels  = zipWith mkNew [minLen ..] rest
      newsz <- nameLoopCountVar (S.bvAdd (PS.loopCountVarToSExpr lcv) (symExecInt sizeType 1))
      let vsm' = vsm { vsmLoopCountVar = Just newsz
                     , vsmMinLength = minLen + 1
                     } -- FIXME: we ignore the tag
      pure (vsm', pfx ++ newels)
      
  -- Normal case (?)
  | otherwise = pure (vsm, els ++ [new])

semiExecOp2 :: SemiCtxt m => Op2 -> Type -> Type -> Type ->
               MuxValueSExpr -> MuxValueSExpr -> SemiSolverM m MuxValueSExpr
semiExecOp2 op rty ty1 ty2 mv1 mv2 =
  case op of
    -- Stream operations 
    IsPrefix  -> unsupported
    Drop      -> unsupported
    DropMaybe -> unsupported
    Take      -> unsupported

    Eq        -> VBools <$> semiExecEq mv1 mv2
    NotEq     -> semiExecOp1 Not TBool =<< semiExecOp2 Eq TBool ty1 ty2 mv1 mv2
    Leq       -> viaInterp integerVL boolVL
    Lt        -> viaInterp integerVL boolVL

    Add       -> viaInterp integerVL integerVL
    Sub       -> viaInterp integerVL integerVL
    Mul       -> viaInterp integerVL integerVL
    Div       -> viaInterp integerVL integerVL
    Mod       -> viaInterp integerVL integerVL

    BitAnd    -> viaInterp integerVL integerVL
    BitOr     -> viaInterp integerVL integerVL
    BitXor    -> viaInterp integerVL integerVL
    Cat       -> viaInterp integerVL integerVL
    LCat      -> viaInterp integerVL integerVL
    LShift    -> viaInterp integerVL integerVL
    RShift    -> viaInterp integerVL integerVL

    ArrayIndex
      | VSequence bvs <- mv1
      , VIntegers (Typed _ ixvs) <- mv2 ->
          muxMuxValues <$> traverse (semiExecArrayIndex ixvs) bvs
      | otherwise -> panic "Incorrect value shapes" [show mv1, show mv2]
      
    Emit
      | VSequence bvs <- mv1 ->
          VSequence <$> traverse (semiExecEmit mv2) bvs
          
      | otherwise -> panic "Incorrect value shapes" []
           
    EmitArray -> unsupported
    EmitBuilder -> unsupported
    MapLookup -> unsupported
    MapMember -> unsupported

    ArrayStream -> unsupported
  where
    unsupported = panic "Unsupported Op2" [showPP op]
    viaInterp :: (SemiCtxt m, Ord w) => ValueLens v -> ValueLens w ->
                 SemiSolverM m MuxValueSExpr
    viaInterp = viaInterpOp2 op rty mv1 mv2

-- The idea here is that we collect all the concrete values and pass
-- them off pairwise to the interpreter; the symbolic value is
-- constructed (if required) by converting the concrete values on one
-- side to sexprs, and executing with the other side's symbolic value.
-- The final symbolic value is formed by muxing these values together.
viaInterpOp2 :: (SemiCtxt m, Ord w) => Op2 -> Type ->  MuxValueSExpr -> MuxValueSExpr ->
                ValueLens v -> ValueLens w -> SemiSolverM m MuxValueSExpr
viaInterpOp2 op rty mv1 mv2 argVL resVL
  | Just (Typed ty1 bvs1) <- mv1 ^? vlMVPrism argVL
  , Just (Typed ty2 bvs2) <- mv2 ^? vlMVPrism argVL =
      view (re (vlMVPrism resVL)) . Typed rty <$>
        muxBinOp (SE.symExecOp2 op ty1) (interp ty1 ty2) ty1 ty2 bvs1 bvs2 argVL
  | otherwise = panic "Malformed muxBinOp value" [showPP op
                                                 , show mv1
                                                 , show mv2]
  where
    interp ty1 ty2 x y =
      vlFromInterpValue resVL
       (evalOp2 op (vlToInterpValue argVL ty1 x)
                   (vlToInterpValue argVL ty2 y))

-- The idea here is that we collect all the concrete values and pass
-- them off pairwise to the interpreter; the symbolic value is
-- constructed (if required) by converting the concrete values on one
-- side to sexprs, and executing with the other side's symbolic value.
-- The final symbolic value is formed by muxing these values together.
muxBinOp :: (SemiCtxt m, Ord w) =>
            (SExpr -> SExpr -> SExpr) ->
            (v -> v -> w) ->
            Type -> Type -> 
            BaseValues v SExpr -> BaseValues v SExpr ->
            ValueLens v -> SemiSolverM m (BaseValues w SExpr) -- just so it can fail.
muxBinOp sfun cfun ty1 ty2 bvs1 bvs2 argVL
  | nullBaseValues bvs = unreachable
  | otherwise          = pure bvs
  where
    go (Left v) (Left w)   = Left $ cfun v w
    go (Left v) (Right t)  = Right $ sfun (vlToSExpr argVL ty1 v) t
    go (Right s) (Left w)  = Right $ sfun s (vlToSExpr argVL ty2 w)
    go (Right s) (Right t) = Right $ sfun s t

    bvs = liftA2 go  bvs1 bvs2
      

-- This is a bit weird in that we don't care if the list has symbolic
-- length.  This is because this operation is always (well, should be)
-- guarded by a check that the index < length arr.  In practice this
-- means the SMT context will contain an assertion like
--
-- if PS1 then v1 < len else if PS2 then v2 < len else if ... else sym < len
--
-- so we don't need to tag the result value with a constraint on the length.
semiExecArrayIndex :: SemiCtxt m =>
                      BaseValues Integer SExpr ->
                      (VSequenceMeta, [MuxValueSExpr]) ->
                      SemiSolverM m MuxValueSExpr
semiExecArrayIndex ixs (_vsm, els) = do
  sb <- join <$> traverse mksym sixs
  pure (muxMuxValues (B.disjointUnion cb sb))
  where
    (cixs, sixs) = B.partitionEithers ixs
    cb = (els !!) . fromIntegral <$> cixs
    
    mksym sexp = do
      -- FIXME: not exactly a loop count here (is a loop index, same type)
      sv <- nameSExprForSideCond sizeType sexp
      pure $ B.branching $ zipWith (\i el -> (PS.indexConstraint sv i, el)) [0..] els

-- semiExecOp3 :: SemiCtxt m => Op3 -> Type -> Type ->
--                GuardedSemiSExprs -> GuardedSemiSExprs -> GuardedSemiSExprs ->
--                SemiSolverM m GuardedSemiSExprs

-- -- We only need sv to be concrete here.
-- semiExecOp3 MapInsert rty@(TMap kt vt) _ty gvs k v =
--   -- FIXME: this shouldn't fail, we gseCollect may be overkill
--   gseCollect (map go (guardedValues gvs))
--   where
--     go (g, sv)
--       | VMap els <- sv = pure $ singleton g (VMap ((k, v) : els))
--       -- FIXME: this picks an ordering
--       | VValue (V.VMap m) <- sv  =
--           let ms = [ (vValue mempty k', vValue mempty v')
--                    | (k', v') <- Map.toList m ]
--           in pure $ singleton g (VMap ((k, v) : ms))
--       | VOther x <- sv = do
--           tys <- asks typeDefs
--           vSExpr g rty =<< liftSolver (SE.symExecOp3 MapInsert rty (S.const (typedThing x))
--                                        (toSExpr tys kt k)
--                                        (toSExpr tys vt v))
--       | otherwise = panic "BUG: unexpected value shape" []

-- -- RangeUp and RangeDown
-- semiExecOp3 op        _    _   _         _ _ = panic "Unimplemented" [showPP op]


-- -----------------------------------------------------------------------------
-- Getting a concrete value in a model
  
fromModel :: PathSetModelMonad m => MuxValue -> m V.Value
fromModel mv =
  case mv of
    VUnit -> pure V.vUnit 
    VIntegers (Typed ty bvs) -> mkBase integerVL ty bvs
    VBools bvs -> mkBase boolVL TBool bvs
    VUnion m -> stmvValue V.VUnionElem m
    VMaybe m -> stmvValue (\k v -> V.VMaybe (v <$ k)) m
    VStruct m -> V.VStruct <$> traverseOf (each . _2) fromModel (Map.toList m)
    VSequence bvs -> do
      m_r <- B.resolve bvs
      case m_r of
        Nothing -> panic "Empty model" []
        Just (vsm, els) -> do
          -- get the actual elements
          els' <- maybe (pure els) (fmap (flip take els) . psmmLoopVar) (vsmLoopCountVar vsm)
          vs   <- traverse fromModel els'
          pure $ if vsmIsBuilder vsm
                 then V.VBuilder vs
                 else V.VArray (Vector.fromList vs)

    VMap {} -> unsupported

  where
    unsupported = panic "Unsupported (VMap)" []

    stmvValue f m = do
      m_res <- B.resolve m
      case m_res of
        Just (k, mv') -> f k <$> fromModel mv'
        Nothing -> panic "Encountered unreachable value" []

    mkBase :: (PathSetModelMonad m, Ord v) => ValueLens v -> Type -> BaseValues v SMTVar -> m V.Value
    mkBase vl ty bvs = do
      m_res <- B.resolve bvs
      case m_res of
        Nothing -> panic "Encountered unreachable value" []
        Just (Left i)  -> pure (vlToInterpValue vl ty i)
        Just (Right s) -> psmmSMTVar (Typed ty s)
  
  -- case gse of
  --   VValue v -> pure (Just v)
  --   VOther x -> Just <$> getValueVar x
  --   VUnionElem l gses -> fmap (I.VUnionElem l) <$> gsesModel gses
  --   VStruct flds -> do
  --     let (ls, gsess) = unzip flds
  --     m_gsess <- sequence <$> mapM gsesModel gsess
  --     pure (I.VStruct . zip ls <$> m_gsess)

  --   VSequence vsm gsess
  --     | vsmIsBuilder vsm ->
  --       fmap (I.VBuilder . reverse) . sequence <$> mapM gsesModel gsess
  --     | otherwise -> 
  --       fmap (I.VArray . Vector.fromList) . sequence <$> mapM gsesModel gsess

  --   VJust gses -> fmap (I.VMaybe . Just) <$> gsesModel gses
  --   VMap els -> do
  --     let (ks, vs) = unzip els
  --     m_kvs <- sequence <$> mapM gsesModel ks
  --     m_vvs <- sequence <$> mapM gsesModel vs
  --     pure (I.VMap . Map.fromList <$> (zip <$> m_kvs <*> m_vvs))

  -- We support symbolic keys, so we can't use Map here
    -- VIterator els -> do
    --   let (ks, vs) = unzip els
    --   m_kvs <- sequence <$> mapM gsesModel ks
    --   m_vvs <- sequence <$> mapM gsesModel vs
    --   pure (I.VIterator <$> (zip <$> m_kvs <*> m_vvs))
  



-- -- -----------------------------------------------------------------------------
-- -- Instances

ppBaseValues :: (PP v, PP s) => BaseValues v s -> Doc
ppBaseValues = pp . fmap (either pp pp)
  
ppMuxValueF :: PP s => MuxValueF s -> Doc
ppMuxValueF mv =
  case mv of
    VUnit -> "()"
    VIntegers (Typed _ty bvs) -> ppBaseValues bvs
    VBools    bvs -> ppBaseValues bvs
    VUnion m -> stmv pp m
    VStruct m -> block "{" "," "}" (map ppFld (Map.toList m))
      where ppFld (x,t) = pp x <.> colon <+> go t
    VSequence b -> pp (ppSeq <$> b)
    VMaybe m -> stmv (maybe "nothing" (const "just")) m
    VMap {} -> unsupported
  where
    stmv :: PP s => (l -> Doc) -> SumTypeMuxValueF l s -> Doc
    stmv f = pp . fmap ppOne
      where ppOne (x, t) = f x <.> colon <+> ppMuxValueF t

    ppSeq (vsm, els) = 
      block (maybe "" (parens . pp) sid <> "[") "," "]" (map go els)
      where sid = vsmGeneratorTag vsm

    unsupported = panic "Unsupported (VMap)" []
    go = ppMuxValueF

instance PP s => PP (MuxValueF s) where
  pp = ppMuxValueF

ppMuxValue :: MuxValue -> Doc
ppMuxValue = pp . fmap text 

-- ppMuxValue :: (f a -> Doc) -> (a -> Doc) -> MuxValue f a -> Doc
-- ppMuxValue ppF ppA val =
--   case val of
--     VValue v -> pp v
--     VOther v -> ppA v
--     VUnionElem lbl v -> braces (pp lbl <.> colon <+> ppF v)
--     VStruct xs      -> block "{" "," "}" (map ppFld xs)
--       where ppFld (x,t) = pp x <.> colon <+> ppF t

--     VSequence vsm vs ->  block (maybe "" (parens . pp) sid <> "[") "," "]" (map ppF vs)
--       where sid = vsmGeneratorTag vsm

--     VJust v   -> "Just" <+> ppF v
--     VMap m -> block "{|" ", " "|}" [ ppF k <+> "->" <+> ppF v | (k,v) <- m ]

--     VIterator vs -> block "[iterator|" ",        " "|]"
--                     [ ppF x <+> "->" <+> ppF y | (x,y) <- vs ]

-- instance PP a => PP (GuardedValues a) where
--   pp gvs = block "∨{" ";" "}" [ pp g <+> "⊨" <+> pp v | (g, v) <- guardedValues gvs ]

-- instance PP a => PP (GuardedValue a) where
--   pp = ppMuxValue pp pp

-- instance PP ValueMatchResult where
--   pp vmr =
--     case vmr of
--       NoMatch -> "no"
--       YesMatch -> "yes"
--       SymbolicMatch s -> "symbolic:" <> text (S.ppSExpr s "")

