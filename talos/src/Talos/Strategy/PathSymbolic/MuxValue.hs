{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- -----------------------------------------------------------------------------
-- Semi symbolic/concrete evaluation
--
-- This is comied and modified from SemiExpr

module Talos.Strategy.PathSymbolic.MuxValue (
  MuxValue
  ) where

import           Control.Lens                              (locally, traverseOf, _2, each)
import           Control.Monad                             (join, zipWithM)
import           Control.Monad.Reader
import           Control.Monad.State                       (StateT, modify,
                                                            runStateT)
import           Control.Monad.Trans.Maybe                 (MaybeT, runMaybeT)
import qualified Data.ByteString                           as BS
import           Data.Generics.Product                     (field)
import           Data.List                                 (transpose)
import           Data.List.NonEmpty                        (NonEmpty (..),
                                                            nonEmpty)
import qualified Data.List.NonEmpty                        as NE
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe,
                                                            isNothing, mapMaybe)
import           Data.Semigroup                            (sconcat)
import           Data.Set                                  (Set)
import qualified Data.Set                                  as Set
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import qualified Data.Vector                               as Vector
import           GHC.Generics                              (Generic)
import           GHC.Stack                                 (HasCallStack)

import qualified SimpleSMT                                 as S
import           SimpleSMT                                 (SExpr)

import           Daedalus.Core                             hiding (freshName)
import qualified Daedalus.Core.Semantics.Env               as I
import           Daedalus.Core.Semantics.Expr              (evalOp0, evalOp1,
                                                            evalOp2, matches,
                                                            partial)
import           Daedalus.Core.Type
import           Daedalus.GUID
import           Daedalus.Panic
import           Daedalus.PP
import qualified Daedalus.Value.Type                       as V

import qualified Talos.Monad                               as T
import           Talos.Monad                               (LiftTalosM, LogKey,
                                                            getFunDefs)
import           Talos.Solver.SolverT
import           Talos.Strategy.Monad                      (LiftStrategyM,
                                                            liftStrategy)
import qualified Talos.Strategy.PathSymbolic.PathCondition as PC
import           Talos.Strategy.PathSymbolic.PathCondition (LoopCountVar,
                                                            PathCondition (..),
                                                            ValuePathConstraint (..),
                                                            loopCountVarToSMTVar)
import qualified Talos.Strategy.PathSymbolic.SymExec       as SE

import           Data.String                               (fromString)
import           Text.Printf                               (printf)
import Data.Bifunctor (second)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList, foldl')
import Control.Monad.Except (ExceptT, throwError)
--------------------------------------------------------------------------------
-- Logging and stats

muxKey :: LogKey
muxKey = "muxvalue"

--------------------------------------------------------------------------------
-- Types

data BaseValue = BVInteger Integer | BVBool Bool
  deriving (Show, Eq, Ord)

-- Empty doesn't really make sense.
data BaseValues s = BaseValues
  { bvConcrete :: !(Seq (PathCondition, BaseValue))
  , bvSymbolic :: !(Maybe s)
  }
  deriving (Show, Eq, Ord, Foldable, Traversable, Functor)

singletonBaseValues :: PathCondition -> BaseValue -> BaseValues s
singletonBaseValues pc v = BaseValues
  { bvConcrete = Seq.singleton (pc, v)
  , bvSymbolic = Nothing
  }

singletonSymBaseValues :: s -> BaseValues s
singletonSymBaseValues s = BaseValues
  { bvConcrete = mempty
  , bvSymbolic = Just s
  }

-- | Converts many symbolic values into a single value
collapseSymbolic :: [(PathCondition, SExpr)] -> Maybe SExpr -> Maybe SExpr
collapseSymbolic xs m_base =
  case (xs, m_base) of
    ([], Nothing)  -> Nothing
    (_, Just base) -> Just (mk base xs)
    ((_, base) : xs', Nothing) -> Just (mk base xs')
  where
    mk = foldl' (\acc (pc, se) -> S.ite (PC.toSExpr pc) se acc)

-- FIXME: we could also have Map V.Label [(PathCondition, MuxValueF
-- b)], i.e. merge lazily.  Maybe Haskell helps here anyway?

-- | Unifies Maybe and Unions
type SumTypeMuxValueF l s = Map l ([PathCondition], MuxValueF s)

data MuxValueF s =
    VBase                 !(Typed (BaseValues s))
    -- These are special, so we don't need to stick them in VBase
  | VUnit
  -- FIXME: we could also have Map V.Label [(PathCondition, MuxValueF
  -- b)], i.e. merge lazily.  Maybe Haskell helps here anyway?
  | VUnion                !(SumTypeMuxValueF V.Label s)
  -- ^ Maps each possible label onto the paths that build that label,
  -- and the merged contents value.  Must have at least 1 key.

  | VStruct                !(Map V.Label (MuxValueF s))

  | VSequence             !VSequenceMeta ![MuxValueF s]
  | VMaybe                !(SumTypeMuxValueF (Maybe ()) s)

  -- We support symbolic keys, so we can't use Map here

  -- FIXME: This is pretty naieve, we might duplicate paths, for
  -- example.  We might also keep track of merge points (useful
  -- e.g. if conditionally updating a map)
  | VMap                   ![(MuxValueF s, MuxValueF s)]
  deriving (Show, Eq, Ord, Foldable, Traversable, Functor)

-- Used by the rest of the simulator
type MuxValue = MuxValueF SMTVar

-- Used internally to avoid naming after every single operation.
type MuxValueSExpr = MuxValueF SExpr


-- Conversion to/from interp. values
toValue :: Type -> BaseValue -> V.Value
toValue ty (BVInteger i) = partial (evalOp0 (IntL i ty))
toValue _  (BVBool    b) = V.VBool b

-- Defined only for types which can converted to BaseValues
fromValue :: V.Value -> BaseValue
fromValue v =
  case v of
    V.VUInt _n i -> BVInteger i
    V.VSInt _n i -> BVInteger i
    V.VInteger i -> BVInteger i
    V.VBool b    -> BVBool b
    _          -> panic "Unexpected value shape" [showPP v]

baseValueToSExpr :: Type -> BaseValue -> SExpr
baseValueToSExpr ty v =
  case (ty, v) of
    (TUInt n, BVInteger i) ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    (TSInt n, BVInteger i) -> -- FIXME: correct?
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    (TInteger, BVInteger i) -> S.int i
    (TBool, BVBool b) -> S.bool b
    _ -> panic "Unexpected type for base value" [showPP ty]

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

-- Typed so we can turn into a value.

-- type GuardedSemiSExprs = GuardedValues (Typed SMTVar)
-- type GuardedSemiSExpr  = GuardedValue  (Typed SMTVar)

-- singleton :: PathCondition -> GuardedSemiSExpr -> GuardedSemiSExprs
-- singleton g gv = GuardedValues ((g, gv) :| [])

-- muxValueToList :: (V.Value -> f a) -> MuxValue f a -> Maybe (VSequenceMeta, [f a])
-- muxValueToList mk mv =
--   case mv of
--     VSequence m xs       -> Just (m, xs)
--     VValue (V.VArray v)    -> fromValue False (map mk (Vector.toList v))
--     -- Builders are stored in reverse order.
--     VValue (V.VBuilder vs) -> fromValue True (map mk (reverse vs))
--     _ -> Nothing

--   where
--     -- We don't weed out ArrayL and builder ops, so it is possible for
--     -- interp arrays to occur, so we need to construct a vsequence meta)
--     fromValue b vs =
--       Just ( emptyVSequenceMeta { vsmMinLength = length vs, vsmIsBuilder = b }
--            , vs)

-- gseToList :: GuardedSemiSExpr -> Maybe (VSequenceMeta, [GuardedSemiSExprs])
-- gseToList = muxValueToList (vValue mempty)

-- -- -----------------------------------------------------------------------------
-- -- Values

-- | An empty base value is malformed and should not occur.
nullBaseValues :: BaseValues s -> Bool
nullBaseValues bv = null (bvConcrete bv) && isNothing (bvSymbolic bv)

singletonBase :: Type -> PathCondition -> BaseValue -> MuxValueF b
singletonBase ty pc v = VBase (Typed ty $ singletonBaseValues pc v)

-- vOther :: PathCondition -> Typed SMTVar -> MuxValueF b
-- vOther g = singleton g . VOther

-- vSExpr :: (MonadIO m, HasGUID m) =>
--           PathCondition -> Type -> SExpr -> SemiSolverM m GuardedSemiSExprs
-- vSExpr g ty se = vOther g . Typed ty <$> sexprAsSMTVar ty se

-- vValue :: PathCondition -> V.Value -> GuardedSemiSExprs
-- vValue g = singleton g . VValue

vSymbolic :: Type -> s -> MuxValueF s
vSymbolic ty s = VBase (Typed ty $ singletonSymBaseValues s)

vBool :: PathCondition -> Bool -> MuxValueF b
vBool pc b = singletonBase TBool pc (BVBool b)

vInteger :: Type -> PathCondition -> Integer -> MuxValueF b
vInteger ty pc i = singletonBase ty pc (BVInteger i)

-- | Construct a fixed-length sequence
vFixedLenSequence :: [MuxValueF b] -> MuxValueF b
vFixedLenSequence = VSequence emptyVSequenceMeta

-- Sum types

vNothing :: MuxValueF b
vNothing = VMaybe (Map.singleton Nothing (mempty, VUnit))

vJust :: MuxValueF b -> MuxValueF b
vJust el = VMaybe (Map.singleton (Just ()) (mempty, el))

-- vUInt :: PathCondition -> Int -> Integer -> MuxValueF b
-- vUInt g n = vValue g . V.vUInt n

-- pattern VNothing :: MuxValue f a
-- pattern VNothing = VValue (V.VMaybe Nothing)

-- refine :: PathCondition -> GuardedSemiSExprs -> Maybe GuardedSemiSExprs
-- refine g gvs =
--   GuardedValues <$> nonEmpty [ (gr, v)
--                              | (g', v) <- guardedValues gvs
--                              , let gr = g <> g'
--                              , gr /= Infeasible
--                              ]

-- unions :: NonEmpty GuardedSemiSExprs -> GuardedSemiSExprs
-- unions ((GuardedValues ((_, VValue V.VUnit) :| _)) :| _) = vUnit
-- unions xs = GuardedValues . sconcat . fmap getGuardedValues $ xs

-- unions' :: [GuardedSemiSExprs] -> Maybe GuardedSemiSExprs
-- unions' = fmap unions . nonEmpty

-- -- -----------------------------------------------------------------------------
-- -- Sequences
-- --
-- -- These two operations could be horrendously expensive.

-- -- | Turns a sequence, which may be a super-position of multiple
-- -- sequence lengths, into multiple sequences where the length is
-- -- fixed.
-- explodeSequence :: VSequenceMeta -> [GuardedSemiSExprs]
--                 -> [ (PathCondition, [GuardedSemiSExprs]) ]
-- explodeSequence vsm els
--   | Just lcv <- vsmLoopCountVar vsm = map (go lcv) [vsmMinLength vsm .. length els]
--   | otherwise = [(mempty, els)]
--   where
--     -- FIXME: this is inefficient, maybe store loops in reversed order (to get sharing)
--     go lcv len = (PC.insertLoopCount lcv (PC.LCCEq len) mempty, take len els)

-- -- | Turns a sequence (of fixed length) into a sequence for each
-- -- possible combination of paths to the elements.  This could be
-- -- exponential in the length of the list, in the worst case.

-- unzipSequence :: [GuardedSemiSExprs] -> [ (PathCondition, [GuardedSemiSExpr]) ]
-- unzipSequence = go []
--   where
--     go acc [] = [ (g, reverse vs) | (g, vs) <- acc ]
--     go acc (gs : rest) =
--       go [ (merged_g, v : vs)
--          | (g, vs) <- acc
--          , (g', v) <- guardedValues gs
--          , let merged_g = g <> g'
--          , PC.isFeasibleMaybe merged_g
--          ] rest

-- -- -----------------------------------------------------------------------------
-- -- Byte sets
-- --
-- -- At the moment we just symbolically execute

-- -- Maybe we could do better here than just symbolically evaluating,
-- -- but given that the byte is usually symbolic, just sending it to the
-- -- solver seems to be OK.
-- -- semiExecByteSet :: (HasGUID m, Monad m, MonadIO m) => ByteSet -> SExpr ->
-- --                    SemiSolverM m GuardedSemiSExprs
-- -- semiExecByteSet byteSet b =
-- --   vSExpr mempty TBool <$> symExecToSemiExec (SE.symExecByteSet b byteSet) -- go byteSet
--   -- where
--   --   go bs = case bs of
--   --     SetAny         -> pure (vBool mempty True)
--   --     SetSingle e    -> semiExecEq byteT b =<<
--   --                         (vSExpr mempty TBool =<< semiExecExpr e)
--   --     SetRange el eh -> conj <$> (flip (op2 Leq) (vSExpr b) =<< semiExecExpr el)
--   --                                 <*> (op2 Leq (vSExpr b) =<< semiExecExpr eh)
--   --     SetComplement bs' -> op1 Neg TBool =<< go bs'
--   --     SetLet n e bs' -> do
--   --       ve  <- semiExecExpr e
--   --       bindNameIn n ve (go bs')
--   --     SetCall fn args -> do
--   --       fdefs <- asks byteFunDefs
--   --       let (ps, bs') = case Map.lookup fn fdefs of
--   --             Just fdef | Def d <- fDef fdef -> (fParams fdef, d)
--   --             _   -> panic "Missing function " [showPP fn]
--   --       vs <- mapM semiExecExpr args
--   --       foldr (uncurry bindNameIn) (go bs') (zip ps vs)
--   --     -- This
--   --     SetCase _cs -> panic "Saw a case in ByteSet" []
--   --       -- m_e <- semiExecCase cs
--   --       -- case m_e of
--   --       --   -- can't determine match, just return a sexpr
--   --       --   TooSymbolic   -> symExec expr
--   --       --   DidMatch _ e' -> go e'
--   --       --   -- Shouldn't happen in pure code
--   --       --   NoMatch -> panic "No match" []

--   --     where
--   --       op1 op = semiExecOp1 op TBool
--   --       op2 op = semiExecOp2 op TBool (TUInt (TSize 8)) (TUInt (TSize 8))

-- -- -- -----------------------------------------------------------------------------
-- -- -- Fallin back to (fully) symbolic execution

-- symExecToSemiExec :: (HasGUID m, Monad m, MonadIO m) => SE.SymExecM m a -> SemiSolverM m a
-- symExecToSemiExec = lift . lift . withReaderT envf
--   where
--     envf env = envToSymEnv (typeDefs env) (localBoundNames env)

-- -- symExec :: (HasGUID m, Monad m, MonadIO m) => Expr -> SemiSolverM m SemiSExpr
-- -- symExec e = vSExpr (typeOf e) <$> symExecToSemiExec (SE.symExecExpr e)

-- symExecByteSet :: (HasGUID m, Monad m, MonadIO m) => ByteSet -> SExpr -> SemiSolverM m GuardedSemiSExprs
-- symExecByteSet bs b = do
--   se <- symExecToSemiExec (SE.symExecByteSet b bs)
--   vSExpr mempty TBool se

-- envToSymEnv :: Map TName TDecl -> Map Name GuardedSemiSExprs ->  Map Name SExpr
-- envToSymEnv tenv = Map.mapWithKey (toSExpr tenv . nameType)

-- -- ASSUME: one guard is true, so the negation of all but one entails
-- -- the remaining guard.
-- toSExpr :: Map TName TDecl -> Type ->
--            GuardedSemiSExprs -> SExpr
-- toSExpr tys ty gvs =
--   foldr (\(g', el') -> S.ite (PC.toSExpr g') (go el')) (go el) els
--   where
--     go = toSExpr1 tys ty
--     (_g, el) :| els = getGuardedValues gvs

-- toSExpr1 :: Map TName TDecl -> Type ->
--             GuardedSemiSExpr -> SExpr
-- toSExpr1 tys ty sv =
--   case sv of
--     VValue v -> valueToSExpr tys ty v
--     VOther s -> S.const (typedThing s)
--     -- FIXME: copied from valueToSExpr, unify.
--     VUnionElem l v' | Just (ut, ty') <- typeAtLabel tys ty l
--       -> S.fun (labelToField (utName ut) l) [go ty' v']

--     -- FIXME: copied from valueToSExpr, unify.
--     VStruct els
--       | TUser ut <- ty
--       , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
--       -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)

--     -- Should probably never happen?
--     VSequence _ _ -> panic "Saw toSExpr1 on VSequence" []
--       --  | Just elTy <- typeToElType ty ->
--       --      sArrayLit (symExecTy elTy) (typeDefault elTy) (map (go elTy) vs)

--     VJust v' | TMaybe ty' <- ty -> sJust (symExecTy ty') (go ty' v')

--     VMap ms | TMap kt vt <- ty ->
--       -- FIXME: breaks abstraction of maps
--       sFromList (tTuple (symExecTy kt) (symExecTy vt))
--                 [ sTuple (go kt k) (go vt v) | (k, v) <- ms ]

--     VIterator vs -> case ty of
--       -- FIXME: this breaks abstractions
--       TIterator (TArray elTy) ->
--         let emptyA = sEmptyL (symExecTy elTy) (typeDefault elTy)
--             els    = [ (go sizeType k, go elTy v) | (k, v) <- vs ]
--             arr    = foldr (\(i, v) arr' -> S.store arr' i v) emptyA els
--         in case els of
--           []             -> sArrayIterNew emptyA
--           ((fstI, _) : _) -> S.fun "mk-ArrayIter" [arr, fstI]

--       TIterator (TMap   _kt' _vt') -> panic "Unimplemented" []
--       _ -> panic "Malformed iterator type" []

--     _ -> panic "Malformed value" [showPP ty]
--   where
--     go = toSExpr tys
--     goStruct (l, e) (l', ty') | l == l' = go ty' e
--     goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']

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
bindNameIn n v = locally (field @"localBoundNames") (Map.insert n v)

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

-- data ValueMatchResult = NoMatch | YesMatch | SymbolicMatch SExpr
--   deriving (Eq, Ord, Show)

-- semiExecCase :: (Monad m, HasGUID m, HasCallStack) =>
--                 Case a ->
--                 SemiSolverM m ( [ ( NonEmpty PathCondition, (Pattern, a)) ]
--                               , [ (PathCondition, ValueMatchResult) ] )
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

-- -- -----------------------------------------------------------------------------
-- -- Monad

data SemiSolverEnv = SemiSolverEnv
  { localBoundNames :: Map Name MuxValueSExpr
  , currentName     :: Text
  } deriving (Generic)

-- typeDefs :: SemiSolverEnv -> Map TName TDecl
-- typeDefs = I.tEnv . interpEnv

type SemiCtxt m = (Monad m, MonadIO m, HasGUID m, LiftStrategyM m, LiftTalosM m)

type SemiState = Set (Typed SMTVar)

type SemiSolverM m = ExceptT () (StateT SemiState (ReaderT SemiSolverEnv (SolverT m)))

-- runSemiSolverM :: SemiCtxt m =>
--                   Map Name GuardedSemiSExprs ->
--                   I.Env ->
--                   Text ->
--                   SemiSolverM m a -> SolverT m (Maybe a, Set (Typed SMTVar))
-- runSemiSolverM lenv env pfx m =
--   runReaderT (runStateT (runMaybeT m) mempty) (SemiSolverEnv lenv pfx env)

-- sexprAsSMTVar :: (MonadIO m, HasGUID m) => Type -> SExpr -> SemiSolverM m SMTVar
-- sexprAsSMTVar _ty (S.Atom x) = pure x
-- sexprAsSMTVar ty e = do
--   -- c.f. PathSymbolic.Monad.makeNicerName
--   n <- asks currentName
--   let n' = "N" <> "." <> n
--   s <- liftSolver (defineSymbol n' (symExecTy ty) e)
--   modify (Set.insert (Typed ty s))
--   pure s

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

-- --------------------------------------------------------------------------------
-- -- Exprs

semiExecExpr :: (SemiCtxt m, HasCallStack) =>
                Expr -> SemiSolverM m MuxValueSExpr
semiExecExpr expr =
  case expr of
    Var n          -> semiExecName n
    PureLet n e e' -> do
      ve  <- semiExecExpr e
      -- FIXME: we might want to name here if n occurs in e' multiple times
      bindNameIn n ve (semiExecExpr e')

    Struct _ut ctors -> do
      let (ls, es) = unzip ctors
      VStruct . Map.fromList . zip ls <$> mapM semiExecExpr es

    ECase cs  -> impossible
    ELoop _lm -> impossible

    Ap0 op       -> pure (semiExecOp0 op)
    Ap1 op e     -> semiExecOp1 op rty (typeOf e) =<< semiExecExpr e
    Ap2 op e1 e2 ->
      join (semiExecOp2 op rty (typeOf e1) (typeOf e2)
            <$> semiExecExpr e1
            <*> semiExecExpr e2)
    Ap3 op e1 e2 e3 ->
      join (semiExecOp3 op rty (typeOf e1)
             <$> semiExecExpr e1
             <*> semiExecExpr e2
             <*> semiExecExpr e3)
    ApN (ArrayL _ty) es -> vFixedLenSequence <$> mapM semiExecExpr es
    ApN (CallF {}) _es  -> impossible
  where
    rty = typeOf expr

    impossible = panic "semiExecExpr: IMPOSSIBLE" [showPP expr]

semiExecOp0 :: Op0 -> MuxValueSExpr
semiExecOp0 op =
  case op of
    Unit          -> VUnit
    IntL i ty     -> vInteger ty mempty i
    FloatL {}     -> unimplemented
    BoolL b       -> vBool mempty b
    ByteArrayL bs -> vFixedLenSequence (map (vInteger tByte mempty . fromIntegral) (BS.unpack bs))
    NewBuilder _ty -> VSequence (emptyVSequenceMeta {vsmIsBuilder = True}) []
    MapEmpty kty vty -> VMap []
    ENothing ty    -> vNothing
  where
    unimplemented = panic "semiExecOp0: UNIMPLEMENTED" [showPP op]

semiExecOp1 :: SemiCtxt m => Op1 -> Type -> Type ->
               MuxValueSExpr ->
               SemiSolverM m MuxValueSExpr
semiExecOp1 op rty ty mv =
  case op of
    CoerceTo {}   -> viaInterp
    IsEmptyStream -> unsupported
    Head          -> unsupported
    StreamOffset  -> unsupported
    BytesOfStream -> unsupported
    OneOf {}      -> viaInterp
    Neg           -> viaInterp
    BitNot        -> viaInterp
    Not           -> viaInterp
    ArrayLen
      | VSequence vsm _mvs <- mv
      , Just lcv <- vsmLoopCountVar vsm -> pure $ vSymbolic sizeType (S.const (loopCountVarToSMTVar lcv))
        -- Otherwise we just use the length.  This should be the non-many generated case.
      | VSequence _ mvs <- mv -> pure $ vInteger sizeType mempty (toInteger (length mvs))

    Concat        -> unsupported
    FinishBuilder
      | VSequence vsm els <- mv -> pure $ VSequence (vsm { vsmIsBuilder = False}) els

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
      | VMaybe m <- mv
      , Just (_pcs, mv') <- Map.lookup (Just ()) m -> pure mv'

    SelStruct _ty l
      | VStruct m <- mv, Just mv' <- Map.lookup l m -> pure mv'

    InUnion _ut l -> pure $ VUnion (Map.singleton l (mempty, mv))

    -- c.f. FromJust
    FromUnion _ty l
      | VUnion m <- mv
      , Just (_pcs, mv') <- Map.lookup l m -> pure mv'

    WordToFloat  -> unsupported
    WordToDouble -> unsupported
    IsNaN        -> unsupported
    IsInfinite   -> unsupported
    IsDenormalized -> unsupported
    IsNegativeZero -> unsupported
    _ -> panic "Malformed Op1 expression" [showPP op]
    where
      unsupported = panic "Unsupported Op1" [showPP op]
      viaInterp
        | VBase (Typed _ty' bvs) <- mv = do
            m_s' <- traverse (SE.symExecOp1 op ty) (bvSymbolic bvs)
            let cs   = fmap (second viaInterpBase) (bvConcrete bvs)
                bvs' = BaseValues { bvConcrete = cs, bvSymbolic = m_s' }
            pure (VBase (Typed rty bvs'))
        | otherwise = panic "Malformed Op1 value" [showPP op]

      viaInterpBase = fromValue . evalOp1 mempty op ty . toValue ty


-- -- | Concatentates a list of list-valued exprs
-- gseConcat :: VSequenceMeta -> [GuardedSemiSExprs] -> Maybe GuardedSemiSExprs
-- gseConcat _vsm _svs = panic "UNIMPLEMENTED: Concat" []

--   -- unions' (mapMaybe mkOne combs)
--   -- where
--   --   elss = map guardedValues svs
--   --   -- This is a sneaky way to get the list of products of the
--   --   -- members.  This is potentially very expensive.
--   --   combs = sequence elss

--   --   doConcat els g'
--   --     | PC.isInfeasible g' = Nothing
--   --     | Just els' <- mapM toL els = Just $ singleton g' (VSequence emptyVSequenceMeta (concat els'))
--   --     | otherwise  = panic "UNIMPLEMENTED: saw symbolic list" []

--   --   mkOne comb =
--   --     let (gs, els) = unzip comb
--   --     in doConcat els (mconcat gs)

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

semiExecOp2 :: SemiCtxt m => Op2 -> Type -> Type -> Type ->
               MuxValueSExpr -> MuxValueSExpr -> SemiSolverM m MuxValueSExpr
semiExecOp2 op rty ty1 ty2 mv1 mv2 =
  case op of
    -- Stream operations 
    IsPrefix  -> unsupported
    Drop      -> unsupported
    DropMaybe -> unsupported
    Take      -> unsupported

    Eq        -> undefined
    NotEq     -> undefined
    Leq       -> viaInterp
    Lt        -> viaInterp

    Add       -> viaInterp
    Sub       -> viaInterp
    Mul       -> viaInterp
    Div       -> viaInterp
    Mod       -> viaInterp

    BitAnd    -> viaInterp
    BitOr     -> viaInterp
    BitXor    -> viaInterp
    Cat       -> viaInterp
    LCat      -> viaInterp
    LShift    -> viaInterp
    RShift    -> viaInterp

    ArrayIndex -> undefined
    Emit -> undefined
    EmitArray -> undefined
    EmitBuilder -> undefined
    MapLookup -> undefined
    MapMember -> undefined

    ArrayStream -> unsupported
  where
    unsupported = panic "Unsupported Op2" [showPP op]

    -- The idea here is that we collect all the concrete values and
    -- pass them off pairwise to the interpreter; the symbolic value
    -- is constructed (if required) by converting the concrete values
    -- on one side to sexprs, and executing with the other side's
    -- symbolic value.  The final symbolic value is formed by muxing
    -- these values together. 
    viaInterp
      -- ty1 should be _ty1, same for ty2
      | VBase (Typed _ty1 bvs1) <- mv1
      , VBase (Typed _ty2 bvs2) <- mv2 = do
          let cvs = [ (g, viaInterpBase v1 v2)
                    | (g1, v1) <- toList (bvConcrete bvs1)
                    , (g2, v2) <- toList (bvConcrete bvs2)
                    -- strip out infeasible values
                    , let g = g1 <> g2, PC.isFeasibleMaybe g
                    ]

          m_svs1 <- traverse (viaInterpSymbolic False bvs1) (bvSymbolic bvs2)
          m_svs2 <- traverse (viaInterpSymbolic True  bvs2) (bvSymbolic bvs1)

          m_svBoth <- sequence (SE.symExecOp2 op ty1 <$> bvSymbolic bvs1 <*> bvSymbolic bvs2)
          let -- concat :: Maybe [a] -> [a]
              sv  = collapseSymbolic (concat m_svs1 <> concat m_svs2) m_svBoth
              bv  = BaseValues { bvConcrete = Seq.fromList cvs
                               , bvSymbolic = sv }

          if nullBaseValues bv
            then unreachable
            else pure (VBase (Typed rty bv))

      | otherwise = panic "Malformed Op1 value" [showPP op]

    viaInterpSymbolic flipped bvs sexp = do
      let f | flipped   = \x -> SE.symExecOp2 op ty1 sexp (baseValueToSExpr ty2 x)
            | otherwise = \x -> SE.symExecOp2 op ty1 (baseValueToSExpr ty1 x) sexp
      traverseOf (each . _2) f (toList $ bvConcrete bvs)

    viaInterpBase x y = fromValue (evalOp2 op (toValue ty1 x) (toValue ty2 y))

-- -- semiExecOp2 op _rty _ty _ty' (VValue v1) (VValue v2) = pure $ VValue (evalOp2 op v1 v2)
-- -- semiExecOp2 op rty   ty _ty' (VOther v1) (VOther v2) =
-- --   lift (vSExpr rty <$> SE.symExecOp2 op ty (typedThing v1) (typedThing v2))
-- semiExecOp2 Eq    _rty ty1 _ty2 sv1 sv2 = semiExecEq  ty1 sv1 sv2
-- semiExecOp2 NotEq _rty ty1 _ty2 sv1 sv2 = semiExecNEq ty1 sv1 sv2

-- -- If all the vals in sv1 are spine-concrete (which is the usual case) we can just append.
-- semiExecOp2 Emit _rty _ty1 _ty2 gvs1 gvs2
--   | Just vss1 <- traverse gseToList gvss1 =
--       -- FIXME: this breaks the abstraction a little
--       pure (GuardedValues (NE.zipWith mk gs vss1))
--   where
--     mk g (vsm, vs) = (g, VSequence (vsm {vsmIsBuilder = True}) (vs ++ [gvs2]))
--     (gs, gvss1) = NE.unzip (getGuardedValues gvs1)

-- semiExecOp2 op rty ty1 ty2 gvs1 gvs2 = reportingSemiExec2 ("op2" <> fromString (showPP op)) go gvs1 gvs2
--   where
--     -- point-wise
--     go g (VValue v1) (VValue v2) =
--       pure (singleton g (VValue (evalOp2 op v1 v2)))

--     -- Some of these are lazy so we don't eagerly convert to sexpr if
--     -- only one side is a sexpr
--     go g sv1 sv2 = do
--       case op of
--         IsPrefix  -> unimplemented
--         Drop      -> unimplemented
--         DropMaybe -> unimplemented
--         Take      -> unimplemented

--         -- Eq       -> semiExecEq ty1 sv1 sv2
--         -- NotEq    -> semiExecNEq ty1 sv1 sv2

--         -- sv1 is arr, sv2 is ix. Note that the translation inserts a
--         -- check on the length, so probably we don't need an explicit
--         -- check here.
--         ArrayIndex -> semiExecArrayIndex g sv1 sv2

--         EmitArray -> unimplemented
--           --  | Just (vsmbs,  bs) <- gseToList sv1
--           --  , Just (vsmarr, arrs) <- gseToList sv2 ->
--           --      unless (isNothing (vsmLoopCountVar vsmbs)
--           --              && isNothing (vsmLoopCountVar vsmarr)) $ panic "Invariant violated?" []
--           --      pure $ singleton g (VSequence vsmbs (bs ++ arrs))

--         EmitBuilder -> unimplemented

--         -- sv1 is map, sv2 is key
--         MapLookup -> mapOp g sv1 sv2
--                            VNothing        (sNothing . symExecTy)
--                            VJust           sJust

--         MapMember -> mapOp g sv1 sv2
--                            (VValue $ V.VBool False)        (const (S.bool False))
--                            (const (VValue $ V.VBool True)) (\_ _ -> S.bool True)

--         ArrayStream -> unimplemented

--         -- Leq
--         -- Lt
--         -- Add
--         -- Sub
--         -- Mul
--         -- Div
--         -- Mod
--         -- BitAnd
--         -- BitOr
--         -- BitXor
--         -- Cat
--         -- LCat
--         -- LShift
--         -- RShift

--         -- Otherwise generate a symbolic term.
--         _ -> def g sv1 sv2

--     -- sv1 is map, sv2 is key
--     -- mapOp :: PathCondition -> GuardedSemiSExpr -> GuardedSemiSExpr ->
--     --          GuardedSemiSExpr -> (Type -> SExpr) -> (GuardedSemiSExprs -> GuardedSemiSExprs) ->
--     --          (SExpr -> SExpr -> SExpr) -> SemiSolverM m GuardedSemiSExprs

--     -- FIXME: do something better here, i.e., return the values in the
--     -- map guarded by the path expression to get there.
--     mapOp g sv1 sv2 _missing _smissingf _found _sfound
--       -- -- Concrete case
--       --  | Just els <- toL sv1, VValue kv <- sv2
--       -- , Just res <- mapLookupV missing found kv els
--       --   = pure res
--       -- -- Expand into if-then-else.
--       --  | Just els <- toL sv1
--       -- , TMap kt vt <- ty1 = do
--       --     tys <- asks typeDefs
--       --     let symkv = toSExpr1 tys kt sv2
--       --         mk    = sfound (symExecTy vt) . toSExpr1 tys vt
--       --     vSExpr g rty (foldr (mapLookupS tys mk kt symkv sv2)
--       --                         (smissingf vt)
--       --                         els)
--       | otherwise = def g sv1 sv2

--     def g sv1 sv2 = do
--       tys <- asks typeDefs
--       vSExpr g rty =<< liftSolver (SE.symExecOp2 op ty1
--                                    (toSExpr1 tys ty1 sv1)
--                                    (toSExpr1 tys ty2 sv2))

--     unimplemented = panic "semiEvalOp2: Unimplemented" [showPP op]

--     -- mapLookupV z _f  _kv  [] = Just z
--     -- mapLookupV z f  kv ((VValue kv', el) : rest) =
--     --   if kv == kv' then Just (f el) else mapLookupV z f kv rest
--     -- mapLookupV _ _ _ _ = Nothing

--     -- mapLookupS tys f kTy symkv skv (skv', sel) rest =
--     --   case (skv, skv') of
--     --     (VValue kv, VValue kv')
--     --       -> if kv == kv' then f sel else rest
--     --     _ -> S.ite (S.eq symkv (toSExpr1 tys kTy skv')) (f sel) rest


-- semiExecArrayIndex :: SemiCtxt m => PathCondition ->
--                       GuardedSemiSExpr ->
--                       GuardedSemiSExpr ->
--                       SemiSolverM m GuardedSemiSExprs
-- -- concrete spine, concrete index
-- semiExecArrayIndex g arrv (VValue v)
--   | Just (vsm, svs) <- gseToList arrv
--   , Just ix <- V.valueToIntSize v =
--       let g' | Just lc <- vsmLoopCountVar vsm = PC.insertLoopCount lc (PC.LCCGt ix) g
--              | otherwise = g
--       in if length svs <= ix
--          then hoistMaybe Nothing
--          else hoistMaybe (refine g' (svs !! ix))

-- -- Symbolic index, we assert it at concrete indices and get n results.
-- semiExecArrayIndex g arrv (VOther (Typed ty sIx))
--   | Just (vsm, svs) <- gseToList arrv =
--       let mkIxG ix =
--             PC.insertValue sIx (Typed ty (VPCPositive $ PNum (fromIntegral ix))) g

--           mkG' | Just lc <- vsmLoopCountVar vsm =
--                    \ix -> PC.insertLoopCount lc (PC.LCCGt ix) (mkIxG ix)
--                | otherwise = mkIxG

--           mkV ix el = hoistMaybe (refine (mkG' ix) el)

--       in gseCollect (zipWith mkV [0..] svs)

-- semiExecArrayIndex _g _arrv _sv = panic "BUG: saw non-symbolic sequence" []

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

-- semiExecOpN :: SemiCtxt m => OpN -> Type -> [GuardedSemiSExprs] ->
--                SemiSolverM m GuardedSemiSExprs
-- semiExecOpN (ArrayL _ty) _rty vs =
--   pure (singleton mempty (VSequence emptyVSequenceMeta vs))

-- -- FIXME: this will recurse forever if the varying parameter(s) are
-- -- symbolic.  This is only used for inverses (well, predicates for
-- -- inverses), so it should be OK.
-- semiExecOpN (CallF fn) _rty vs = do
--   fdef <- getEFun fn
--   let e = case fDef fdef of
--         Def d -> d
--         _   -> panic "Missing function " [showPP fn]
--       ps = fParams fdef

--   foldr (uncurry bindNameIn) (semiExecExpr e) (zip ps vs)

-- -- -- -----------------------------------------------------------------------------
-- -- -- Value -> SExpr

-- -- FIXME: move
-- -- Probably an error if they don't match?
-- typeAtLabel :: Map TName TDecl -> Type -> Label -> Maybe (UserType, Type)
-- typeAtLabel tys (TUser ut) l
--   | Just TDecl { tDef = TUnion flds }  <- tdecl = (,) ut <$> lookup l flds
--   | Just TDecl { tDef = TStruct flds } <- tdecl = (,) ut <$> lookup l flds
--   where
--     tdecl = Map.lookup (utName ut) tys
-- typeAtLabel _ _ _ = Nothing

-- valueToSExpr :: Map TName TDecl -> Type -> V.Value -> SExpr
-- valueToSExpr tys ty v =
--   case v of
--     V.VUInt n i ->
--       if n `mod` 4 == 0
--       then S.bvHex (fromIntegral n) i
--       else S.bvBin (fromIntegral n) i
--     V.VSInt n i -> -- FIXME: correct?
--       if n `mod` 4 == 0
--       then S.bvHex (fromIntegral n) i
--       else S.bvBin (fromIntegral n) i
--     V.VInteger i -> S.int i
--     V.VBool b -> S.bool b
--     V.VUnionElem l v'
--       | Just (ut, ty') <- typeAtLabel tys ty l
--       -> S.fun (labelToField (utName ut) l) [go ty' v']

--     -- FIXME: assumes the order is the same
--     V.VStruct els
--       | TUser ut <- ty
--       , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
--       -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)
--       | ty == TUnit -> sUnit

--     V.VArray vs | TArray elty <- ty ->
--       let sVals     = map (go elty) (Vector.toList vs)
--           emptyArr = sArrayL (sEmptyL (symExecTy elty) (typeDefault elty)) -- FIXME, a bit gross?
--           arr      = foldr (\(i, b) arr' -> S.store arr' (sSize i) b) emptyArr (zip [0..] sVals)
--       in sArrayWithLength (symExecTy elty) arr (sSize (fromIntegral $ Vector.length vs))

--     V.VMaybe mv | TMaybe ty' <- ty ->
--       case mv of
--         Nothing -> sNothing (symExecTy ty')
--         Just v' -> sJust (symExecTy ty') (go ty' v')

--     V.VMap m | TMap kt vt <- ty ->
--       -- FIXME: breaks abstraction of maps
--       sFromList (tTuple (symExecTy kt) (symExecTy vt))
--                 [ sTuple (go kt k) (go vt v') | (k, v') <- Map.toList m ]


--     V.VStream {}                       -> unimplemented
--     V.VBuilder vs
--       | TBuilder elTy <- ty -> sFromList (symExecTy elTy) (reverse (map (go elTy) vs))

--     -- FIXME: copied from semiSExprToSExpr
--     V.VIterator vs -> case ty of
--       -- FIXME: this breaks abstractions
--       TIterator (TArray elTy) ->
--         let emptyA = sEmptyL (symExecTy elTy) (typeDefault elTy)
--             els    = [ (go sizeType k, go elTy v') | (k, v') <- vs ]
--             arr    = foldr (\(i, v') arr' -> S.store arr' i v') emptyA els
--         in case els of
--           []             -> sArrayIterNew emptyA
--           ((fstI, _) : _) -> S.fun "mk-ArrayIter" [arr, fstI]
--       TIterator (TMap   _kt' _vt') -> panic "Unimplemented" []
--       _ -> panic "Malformed iterator type" []

--     _ -> unexpectedTy
--   where
--     unexpectedTy = panic "Unexpected type" [showPP v, showPP ty]

--     go = valueToSExpr tys
--     goStruct (l, e) (l', ty') | l == l' = go ty' e
--     goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']

--     unimplemented = panic "Unimplemented" [showPP v]


-- -- ----------------------------------------------------------------------------------------
-- -- Helpers

-- orMany :: [SExpr] -> SExpr
-- orMany [] = S.bool False
-- orMany [x] = x
-- orMany xs  = S.orMany xs

-- andMany :: [SExpr] -> SExpr
-- andMany [] = S.bool True
-- andMany [x] = x
-- andMany xs  = S.andMany xs

-- -- | If the argument consists of only values, then those values are
-- -- retunred.
-- asValues :: GuardedSemiSExprs -> Maybe (NonEmpty V.Value)
-- asValues = traverse (go . snd) . getGuardedValues
--   where
--     go (VValue v) = Just v
--     go _ = Nothing

-- -- -----------------------------------------------------------------------------
-- -- Instances


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

