{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Demand analysis 
--
-- This is used for the semi-symbolic execution, it says when a
-- function can be unrolled (vs being sent to the prover).


module Talos.Analysis.Demands (calculateDemands, Demand(..)) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Daedalus.Core
import Control.Monad.State
import Data.Foldable

import Daedalus.Panic
import Daedalus.PP
import Data.Maybe (fromMaybe)

import Talos.Analysis.Fixpoint 
import Daedalus.Rec (topoOrder)
import Daedalus.Core.Free (freeFVars)

-- FIXME: could we use this instead of FieldSet?

-- -----------------------------------------------------------------------------
-- Top level

-- Ultimately we ony care about the 'DNone' demand, the rest are
-- intermediate results.  
calculateDemands :: Module -> Map FName (Map Demand DemandSummary)
calculateDemands md = summaries
  where
    (_, summaries) = calcFixpoint (==) doOne wl0 s0
    s0             = initState (mFFuns md) 

    doOne nm d
      | Just decl <- Map.lookup nm decls = summariseDecl d decl
      | otherwise = panic "Missing decl" [showPP nm]

    wl0   = Set.fromList funDecls
    
    decls = Map.fromList (map (\tc -> (fName tc, tc)) (mFFuns md))
    
    funDecls = [ (name, DNone)
               | Fun { fName = name, fDef = Def _ } <- mFFuns md ]

summariseDecl :: Demand -> Fun Expr -> DemandM DemandSummary
summariseDecl d Fun { fDef = Def def, fParams = ps } = do
  (_, dset) <- demandExpr d def
  pure (map (dsLookup dset) ps)
  
summariseDecl _ _ = panic "Missing definition" []

--------------------------------------------------------------------------------
-- Summary functions

-- | A demand.  This tells which parts of a value need to be concrete
-- for the computation to produce the result we want.  It is
-- dynamically typed, in that we rely on the context begin well-typed.
-- Note we can represent DNone/DAll multiple ways for structs, e.g. {
-- _ -> DAll } , or {} for DNone

data Demand = DAll  -- ^ Top, we need the whole thing
            | DNone -- ^ Bottom 
            | DUser (Map Label Demand)
            -- ^ Models both structs and unions.  Missing labels are assumbed to be DNone.
            | DSequence Bool Demand
            -- ^ Iterators and arrays, the boolean is whether it is
            -- tail strict, the demand is for the elements.
            | DMap Demand Demand
            -- ^ Maps, demands are for keys and values
            | DMaybe Demand -- ^ If we care about the Maybe at all, the Nothing is DAll
            deriving (Eq, Ord, Show)

-- Demand is a complete lattice
instance Semigroup Demand where
  d <> d' = case (d, d') of
    (DAll, _   )        -> DAll
    (_   , DAll)        -> DAll
    (DNone, _)          -> d'
    (_   , DNone)       -> d
    
    (DUser m, DUser m') -> DUser (Map.unionWith (<>) m m')
    (DUser _, _)        -> panic "Expected DUser" []
    (_      , DUser _)  -> panic "Expected DUser" []

    (DSequence b ds, DSequence b' ds') -> DSequence (b || b') (ds <> ds')
    (DSequence {}, _)   -> panic "Expected DSequence" []
    (_, DSequence {})   -> panic "Expected DSequence" []

    (DMap ds1 ds2, DMap ds1' ds2') -> DMap (ds1 <> ds1') (ds2 <> ds2')
    (DMap {}, _)   -> panic "Expected DMap" []
    (_      , DMap {})   -> panic "Expected DMap" []

    (DMaybe ds, DMaybe ds') -> DMaybe (ds <> ds')
    -- (DMaybe {}, _)   -> panic "Expected DMaybe" []
    -- (_, DMaybe {})   -> panic "Expected DMaybe" []

instance Monoid Demand where
  mempty = DNone

-- We order this by argument order so we don't have to keep the params around.
type DemandSummary = [Demand]
   
data DemandState =
  DemandState { allRecursivePeers :: Map FName (Set FName)
              -- ^ All functions which are mutually recursive, mapped
              -- to the set they are recursive with.
              }
  
initState :: [Fun Expr] -> DemandState
initState funs = DemandState
  { allRecursivePeers = Map.fromList recFuns
  }
  where
    orderedFuns = topoOrder (\f -> (fName f, freeFVars f)) funs
    recFuns = concatMap oneRecFun orderedFuns
    
    oneRecFun (NonRec {}) = []
    oneRecFun (MutRec fs) =
      let names = map fName fs
          nset  = Set.fromList names
      in [ (name, nset) | name <- names ]
    
type DemandM = FixpointM FName Demand DemandSummary DemandState 

isRecursivePeer :: FName -> DemandM Bool
isRecursivePeer n = do
  me <- currentDeclName
  fixpointState (\s -> (go me s, s))
  where
    go me s = maybe False (Set.member n) (Map.lookup me (allRecursivePeers s))
  
-- -----------------------------------------------------------------------------
-- Demand sets

newtype DemandSet = DemandSet { getDemandSet :: Map Name Demand }
  deriving (Eq)

instance Semigroup DemandSet where
  (DemandSet l) <> (DemandSet r) = DemandSet $ Map.unionWith (<>) l r

instance Monoid DemandSet where
  mempty = DemandSet Map.empty

dsSingleton :: Name -> Demand -> DemandSet
dsSingleton n d = DemandSet $ Map.singleton n d

dsSplitOn :: Name -> DemandSet -> (Demand, DemandSet)
dsSplitOn n (DemandSet ds) = 
  let (m_d, ds') = Map.updateLookupWithKey (\_ _ -> Nothing) n ds
  in (fromMaybe DNone m_d, DemandSet ds')

dsLookup :: DemandSet -> Name -> Demand
dsLookup (DemandSet m) n = Map.findWithDefault DNone n m

-- -----------------------------------------------------------------------------
-- Whether a path is required/interesting
--
-- The point of the analysis is to make sure we expand recursive calls
-- only when doing so doesn't result in an infinite term (modulo
-- termination of the underlying function).  The question then is:
-- 
--   what is the least we need to know s.t. the simulation makes progress
--
-- The claim is then that it is sufficient to have enough concrete
-- value to reach recursive calls (or know that they can't be
-- reached). For example, in
--
-- def mylen (xs : MyList) : int =
--   case xs of
--     nil _  -> 0
--     cons x -> 1 + mylen x.tail
--
-- we need to know whether xs is nil or not, as otherwise we will
-- continually expand mylen if xs is symbolic.
--
-- This is determined by demandExpr returning a flag saying whether we
-- need to know whether the target expression needs to be reached.  In
-- practice this means whether the expression contains a recursive
-- call.  If so, the algorithm determines how much we need to know
-- about the context in order to reach (or otherwise) that expression.
--
-- Consider
--
-- def mylen2 (xs : MyList) : int =
--   case xs of
--     nil _  -> 0
--     cons x -> let len = mylen2 x.tail in 1 + len
--
-- We don't care about the rhs of the let, but we do about the bound
-- expression (and hence about the whole let term).

data DemandStatus = Critical | NonCritical

instance Semigroup DemandStatus where
  NonCritical <> NonCritical = NonCritical
  _ <> _ = Critical

instance Monoid DemandStatus where
  mempty = NonCritical

-- -----------------------------------------------------------------------------
-- Core of the algorithm, transfers results demands into demands of
-- the free variables.

demandExpr :: Demand -> Expr -> DemandM (DemandStatus, DemandSet)
demandExpr dRes expr =
  case expr of
    Var n -> pure (NonCritical, dsSingleton n dRes)
    PureLet n el er -> do
      (dsr, mr) <- demandExpr dRes er
      -- delete and lookup
      let (dl, mr') = dsSplitOn n mr
      (dsl, ml) <- demandExpr dl el
      pure (dsl <> dsr, ml <> mr')
    Struct _ut flds -> do
      case dRes of
        DAll  -> fold <$> mapM (demandExpr dRes . snd) flds
        DNone -> fold <$> mapM (demandExpr dRes . snd) flds
        DUser m   ->
          let doOne (l, e) = demandExpr (Map.findWithDefault DNone l m) e
          in fold <$> mapM doOne flds
        _ -> panic "Expecting a DUser/DAll/DNone" [show dRes]

    -- dependencies on the ctor etc. happen via FromUnion
    ECase cs@(Case e pats) -> do
      (d, mr) <- fold <$> traverse (demandExpr dRes) cs
      let demands = foldMap (patternToDemand . fst) pats
          ed = case d of
            NonCritical -> DNone
            Critical    -> demands
      (d', me) <- demandExpr ed e
      pure (d <> d', mr <> me)
    Ap0 {}   -> pure mempty

    Ap1 op e ->
      let trivial = demandExpr dRes e
          go d    = demandExpr d e
      in case op of
        CoerceTo {}   -> trivial
        IsEmptyStream -> unimplemented
        Head          -> unimplemented
        StreamOffset  -> unimplemented
        StreamLen     -> unimplemented
        OneOf {}      -> trivial
        Neg           -> trivial
        BitNot        -> trivial
        Not           -> trivial
        ArrayLen
          | dRes == DNone -> trivial
          -- dRes == DAll
          | otherwise     -> go $ DSequence True DNone
        Concat
          | dRes == DNone -> trivial
          | DSequence b _ <- dRes -> go $ DSequence b dRes
          -- dRes == DAll
          | otherwise -> go $ DSequence True DAll
        
        FinishBuilder -> trivial
        NewIterator   -> trivial -- FIXME: maps?
        IteratorDone
          | dRes == DNone -> trivial
          -- dRes == DAll
          | otherwise     -> go $ DSequence True DNone
        IteratorKey
          | dRes == DNone -> trivial
          | otherwise     -> go $ DSequence False dRes
        IteratorVal
          | dRes == DNone -> trivial
          | otherwise     -> go $ DSequence False dRes
        IteratorNext      -> trivial -- FIXME: nothing to add here(?}
        EJust
          | DMaybe d <- dRes -> go d
          | otherwise        -> trivial
        FromJust
          | dRes == DNone -> trivial
          | otherwise     -> go $ DMaybe dRes
        SelStruct _ l
          | dRes == DNone -> trivial
          | otherwise     -> go $ DUser (Map.singleton l dRes)
        InUnion _ l      
          | DUser m <- dRes -> go (Map.findWithDefault DNone l m)
          | otherwise -> trivial
        FromUnion _ l    
          | dRes == DNone -> trivial
          | otherwise     -> go $ DUser (Map.singleton l dRes)
          
    Ap2 op e1 e2 ->
      let trivial = (<>) <$> demandExpr dRes e1 <*> demandExpr dRes e2
      in case op of
        IsPrefix -> unimplemented
        Drop     -> unimplemented
        Take     -> unimplemented

        -- e1 is the array, e2 is the index
        ArrayIndex
          | dRes == DNone -> trivial
          -- we push the demand into the sequence, and demand a precise index.
          | otherwise     -> (<>) <$> demandExpr (DSequence True dRes) e1 <*> demandExpr DAll e2
        ConsBuilder
          | DSequence _b d <- dRes -> (<>) <$> demandExpr d e1 <*> demandExpr dRes e2
          -- None, All
          | otherwise -> trivial
          
        MapLookup
          | dRes == DNone -> trivial
          -- e1 is map, e2 is key
          | otherwise     -> (<>) <$> demandExpr (DMap DAll dRes) e1 <*> demandExpr DAll e2
          
        MapMember
          | dRes == DNone -> trivial
          -- e1 is map, e2 is key
          | otherwise     -> (<>) <$> demandExpr (DMap DAll DNone) e1 <*> demandExpr DAll e2

        ArrayStream       -> unimplemented
        
        _ -> trivial
        
    Ap3 op e1 e2 e3 ->
      let trivial = fold <$> mapM (demandExpr dRes) [e1, e2, e3]
          rangeUpOrDown | DSequence _b d <- dRes = fold <$> mapM (demandExpr d) [e1, e2, e3]
                        | otherwise     = trivial
      in case op of
        RangeUp   -> rangeUpOrDown
        RangeDown -> rangeUpOrDown
        -- e1 is the map, e2 is k, e3 is v
        MapInsert
         | DMap dk dv <- dRes ->
           fold <$> sequence [demandExpr dRes e1, demandExpr dk e2, demandExpr dv e3]
         | otherwise -> trivial
    ApN (ArrayL _) es
      | DSequence _b d <- dRes -> fold <$> mapM (demandExpr d)    es
      | otherwise              -> fold <$> mapM (demandExpr dRes) es
    ApN (CallF fn) es -> do
      peer <- isRecursivePeer fn
      ds <- fromMaybe (repeat DNone) <$> requestSummary fn dRes
      r <- fold <$> zipWithM demandExpr ds es
      pure ((if peer then Critical else NonCritical, mempty) <> r)
  where
    unimplemented = panic "Unimplemented" [showPP expr]

-- Given that we need to be able to resolve the pattern match, what do
-- we need to know about the target expression.  If we had a 'whnf'
-- pattern we could also just use that.
patternToDemand :: Pattern -> Demand
patternToDemand p =
  case p of
    PBool _  -> DAll
    PNothing -> DMaybe DNone
    PJust    -> DMaybe DNone
    PNum _   -> DAll
    PCon l   -> DUser (Map.singleton l DNone)
    PAny     -> DNone
