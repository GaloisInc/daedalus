-- Self-contatined file for experimenting with constraint slicing

module Talos.Analysis.Transfer where

import Data.Map (Map)

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))

import Daedalus.Core
import qualified Daedalus.Value  as V
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map as Map
import Daedalus.Panic
import Control.Monad hiding (join)
import Data.List (partition)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as BS
import Daedalus.PP
import Daedalus.Core.Type (typeOf)

data ValueConstraint =
  VCAny
  | VCUnionElem Label ValueConstraint
  | VCNothing
  | VCJust      ValueConstraint
  | VCStruct    [(Label, ValueConstraint)]
  | VCFixedSeq  [ ValueConstraint ]
  -- Maps have to have concrete keys for now.  Also, maps may have
  -- more keys than in the constraint, so merging is union.
  | VCMap      (Map V.Value ValueConstraint)
  deriving (Eq, Ord, Show)

mergeConstraintMap :: Ord a =>
                     Map a ValueConstraint ->
                     Map a ValueConstraint ->
                     Maybe (Map a ValueConstraint)
mergeConstraintMap =
  Map.mergeA
    (Map.traverseMissing (const pure))    
    (Map.traverseMissing (const pure))
    (Map.zipWithAMatched (const mergeConstraint))

-- Must satisfy both
mergeConstraint :: ValueConstraint -> ValueConstraint -> Maybe ValueConstraint
mergeConstraint x y =
  case (x, y) of
    (VCAny, _) -> Just y
    (_, VCAny) -> Just x
    (VCUnionElem l vc, VCUnionElem l' vc')
      | l == l' -> VCUnionElem l <$> mergeConstraint vc vc'
    (VCNothing, VCNothing) -> Just VCNothing
    (VCJust x', VCJust y') -> VCJust <$> mergeConstraint x' y'
    (VCStruct flds, VCStruct flds') -> VCStruct <$> zipWithM goS flds flds'
    (VCFixedSeq vcs, VCFixedSeq vcs')
      | length vcs == length vcs' -> VCFixedSeq <$> zipWithM mergeConstraint vcs vcs'
    (VCMap mx, VCMap my) -> VCMap <$>  mergeConstraintMap mx my
    _ -> Nothing
  where
    goS (l, x') (l', y')
      | l == l' = (,) l <$> mergeConstraint x' y'
      | otherwise = panic "Mismatched labels" []


-- A model M of an expression e relative to a constraint C is an
-- assignment to the (some?) free variables of e s.t. any value
-- resulting from the evaluation of the expression under an
-- environment derived from M will satisfy C.
--
-- An environment from M is any where the values match the constraints
-- in M.

type Model = Map Name ValueConstraint

-- A Domain is a complete lattice, with top begin DModels [mempty]
-- (i.e., maps each variable to the universal set of values) 
data Domain = DModels (NonEmpty Model) | DBottom

singletonD :: Model -> Domain
singletonD = DModels . (:| [])

fromListD :: [Model] -> Domain
fromListD = maybe DBottom DModels . NE.nonEmpty

toListD :: Domain -> [Model]
toListD DBottom = []
toListD (DModels ms) = NE.toList ms

-- ---------------------------------------------------------
-- Domain operations

join :: Domain -> Domain -> Domain
join DBottom d = d
join d DBottom = d
join (DModels ms1) (DModels ms2) = DModels (ms1 <> ms2)

joins :: [Domain] -> Domain
joins = foldr join DBottom
  
-- meet top x = x, meet bottom x = bottom
meet :: Domain -> Domain -> Domain
meet (DModels ms1) (DModels ms2) =
  fromListD (concatMap go (NE.toList ms1))
  where
    go m = mapMaybe (mergeConstraintMap m) (NE.toList ms2)
meet _ _ = DBottom

meets :: [Domain] -> Domain
meets = foldr meet trivialDomain

nullDomain, trivialDomain :: Domain
nullDomain = DBottom

-- Equivalend to { _ -> VCAny }
trivialDomain = singletonD mempty

-- -- We could check for subsumption and remove subsumed models?
-- unionDomains :: Domain -> Domain -> Domain
-- unionDomains = (++)

-- -----------------------------------------------------------------------------
-- Propagating a constraint backwards along an expression

type TyEnv = Map TName TDef

-- We can't short-cut on VCAny as the expression may contain partial
-- elements (fromJust, etc.).  Probably these could be caught by the
-- case that usually surrounds these elements, but this seems less
-- fragile.
transfer :: TyEnv -> ValueConstraint -> Expr -> Domain
transfer tys vc expr = 
  case expr of
    Var n | vc /= VCAny -> singletonD (Map.singleton n vc)
          | otherwise   -> trivialDomain
          
    PureLet n e e' ->
      let vcs = go vc e'
          (haves, havesNot) = partition (Map.member n) (toListD vcs)
          goHave h =
            -- n is known to exist from 'has'
            let (Just vc', h') = Map.updateLookupWithKey (\_ _ -> Nothing) n h
            in meet (singletonD h') (go vc' e)
          -- If n isn't in the domain (i.e., implicitly Any), we still
          -- need to process e to get any partial elements.
          justPartials = go VCAny e
      in joins (fromListD (map (meet justPartials) havesNot) ++ map goHave haves))

    -- This is tricky as we need to find a solution which works across
    -- all fields, so we can't just or together the models.  Given we have
    --
    -- {| foo = e1, bar = e2 |}
    --
    -- and d_foo = { mf1, ..., mfn },  d_bar = {mb1, ..., mbn }
    --
    -- we pick all compatible pairings of mfk, mbj as the resulting
    -- domain.  This could be very expensive!
    --
    -- Note that if any domain is empty, all will be (we take the
    -- cross product, basically).
    Struct _ut ctors
      -- should always hold
      -- FIXME: order shoud be the same
      | VCStruct vcs <- vc ->
        -- FIXME: assert labels are the same
        meets (zipWith go (map snd vcs) (map snd ctors))
      | otherwise -> panic "Constraint mismatch" []

    -- Case is where we get the alternatives.
    ECase c -> transferCase (go vc <$> c)

    Ap0 op       -> transferOp0 tys vc op
    Ap1 op e     -> transferOp1 tys vc op e
  --   Ap2 op e1 e2 -> join (semiExecOp2 op rty (typeOf e1) (typeOf e2) <$> go e1 <*> go e2)
  --   Ap3 op e1 e2 e3 -> join (semiExecOp3 op rty (typeOf e1) <$> go e1 <*> go e2 <*> go e3)
  --   ApN opN vs     -> semiExecOpN opN rty =<< mapM go vs
  -- where
  --   go = semiExecExpr
  --   rty = typeOf expr

  where
    go = transfer tys

-- VCAny is handled above, but we do so again to keep ghc happy
transferOp0 :: TyEnv -> ValueConstraint -> Op0 -> Domain
transferOp0 _tys VCAny _ = trivialDomain
transferOp0 tys vc op =
  case (op, vc) of
    (Unit,    _)   -> trivialDomain
    (IntL {}, _)   -> trivialDomain
    (FloatL {}, _) -> trivialDomain
    (BoolL {}, _)  -> trivialDomain
    (ByteArrayL bs, VCFixedSeq vcs) ->
      if BS.length bs == length vcs
      -- probably this will be 'trivialDomain' as we don't constrain
      -- literals, but it seems better to leave this here.
      then meets (zipWith (transfer tys) vcs (map byteL (BS.unpack bs)))
      else DBottom
    (NewBuilder {}, VCFixedSeq vcs) ->
      if null vcs then trivialDomain else DBottom
    (MapEmpty {}, VCMap m) ->
      if Map.null m then trivialDomain else DBottom
    (ENothing {}, VCNothing) -> trivialDomain
    _ -> panic "Unexpected case in transferOp0" [showPP op, show vc]


transferOp1 :: TyEnv -> ValueConstraint -> Op1 -> Expr -> Domain
transferOp1 tys vc op e =
  case op of
    CoerceTo {}   -> go vc e
    IsEmptyStream -> unimplemented
    Head          -> unimplemented
    StreamOffset  -> unimplemented
    StreamLen     -> unimplemented

    -- Most numeric operations aren't predicated
    OneOf {}      -> justPartials -- should be VCAny
    Neg           -> justPartials -- should be VCAny
    BitNot        -> justPartials -- should be VCAny
    Not           -> justPartials -- should be VCAny
    ArrayLen      -> justPartials -- should be VCAny

    -- This is tricky we would need to pick N arrays which combine to
    -- the result we want.
    Concat        -> unimplemented 
    FinishBuilder
      | VCFixedSeq vcs <- vc -> go (VCFixedSeq (reverse vcs)) e
        
    NewIterator   -> unimplemented
    IteratorDone  -> unimplemented
    IteratorKey   -> unimplemented
    IteratorVal   -> unimplemented
    IteratorNext  -> unimplemented
    EJust | VCJust vc' <- vc -> go vc' e
    FromJust      -> go (VCJust vc) e
    
    -- FromJust 
    SelStruct _t l
      | TUser ut <- e_ty
      , Just (TStruct flds) <- Map.lookup (utName ut) tys
      , (pfx, (l', _) : sfx) <- break ((==) l . fst) flds
      , l == l'
        -> let mkOne (l'', _) = (l'', VCAny)
               vc' = VCStruct (map mkOne pfx ++ [(l, vc)] ++ map mkOne sfx)
           in go vc' e
      | otherwise -> justPartials
      
    InUnion _ut l
      | VCUnionElem l' vc' <- vc ->
          if l == l' then go vc' e else DBottom
      | otherwise -> justPartials

    FromUnion _t l -> go (VCUnionElem l vc) e
    
    -- FP
    WordToFloat    -> unimplemented
    WordToDouble   -> unimplemented
    IsNaN          -> unimplemented
    IsInfinite     -> unimplemented
    IsDenormalized -> unimplemented
    IsNegativeZero -> unimplemented
    _ -> panic "Unexpected values" [showPP op, showPP e]
  where
    unimplemented = panic "Unimplemented" [showPP op]
    go = transfer tys
    e_ty = typeOf e
    justPartials = go VCAny e

transferOp2 :: TyEnv -> ValueConstraint -> Op2 -> Expr -> Expr -> Domain
transferOp2 tys vc op e1 e2 =
  case op of
    IsPrefix    -> unimplemented
    Drop        -> unimplemented
    Take        -> unimplemented

    -- FIXME: maybe we need a VCBool b to catch predicates
    Eq          -> justPartials
    NotEq       -> justPartials
    Leq         -> justPartials
    Lt          -> justPartials

    Add         -> justPartials
    Sub         -> justPartials
    Mul         -> justPartials
    Div         -> justPartials
    Mod         -> justPartials

    BitAnd      -> justPartials
    BitOr       -> justPartials
    BitXor      -> justPartials
    Cat         -> justPartials
    LCat        -> justPartials
    LShift      -> justPartials
    RShift      -> justPartials

    ArrayIndex  -> justPartials -- FIXME: maybe we could do more here
    ConsBuilder -> unimplemented
    MapLookup   -> unimplemented
    MapMember   -> unimplemented

    ArrayStream -> unimplemented
  where
    unimplemented = panic "Unimplemented" [showPP op]
    go = transfer tys
    justPartials = meet (go VCAny e1) (go VCAny e2)


transferCase :: Case Domain -> Domain
transferCase (Case y alts) =
  case typeOf y of
    TUser {}  -> joins (map (refineConPat seenLabels y) alts)
    TMaybe {} -> joins (map (refineMaybePat seenMaybes y) alts)
    _         -> joins (map snd alts)
  where
    seenLabels = [ l | (PCon l, _) <- alts ]
    seenMaybes = [ PNothing | (PNothing, _) <- alts ]
                 ++ [ PJust | (PJust, _) <- alts]
                 
-- Refines the result of the body of 'pat'
refineConPat :: [Label] -> Name -> (Pattern, Domain) -> Domain
refineConPat seenLabels x (pat, d) =
  case pat of
    PCon l   -> singletonD (Map.singleton n (VCUnionElem l VCAny)) `meet` d
    PAny     -> fromListD (filter (maybe True overlapCheck . Map.lookup x) (toListD d))
    _        -> panic "Unexpected pattern" [showPP pat]
  where
    overlapCheck (VCUnionElem l _) = l `notElem` seenLabels
    overlapCheck _ = False

refineMaybePat :: [Pattern] -> Name -> (Pattern, Domain) -> Domain
refineMaybePat seenMaybes x (pat, d) =
  case pat of
    PNothing -> singletonD (Map.singleton n VCNothing) `meet` d
    PJust    -> singletonD (Map.singleton n (VCJust VCAny)) `meet` d
    PAny     -> fromListD (filter (maybe True overlapCheck . Map.lookup x) (toListD d))
    _        -> panic "Unexpected pattern" [showPP pat]
  where
    overlapCheck VCNothing   = PNothing `notElem` seenMaybes
    overlapCheck (VCJust {}) = PJust    `notElem` seenMaybes
    overlapCheck _ = False
    
-- transferPat :: Name -> Pattern -> Domain -> Domain
-- transferPat n pat d = 
--   where
--     vc = case pat of
--            PBool {} -> VCAny
--            PNothing -> VCNothing
--            PJust    -> VCJust VCAny
--            PNum {}  -> VCAny
--            PCon l   -> VCUnionElem l VCAny
--            -- FIXME: this is too permissive, as it doesn't refine the
--            -- rhs of the case by the negation of the checked cases.
--            PAny     -> VCAny 

    
