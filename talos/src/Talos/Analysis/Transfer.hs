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

{-
forall x, x is number 

proc _F6409(_x6402 : Builder (), _x6410 : Iter [Value], y : Word 32) : Builder () =
  do let _x6412 = itNull _x6410
     case _x6412 of
       True ->
         pure _x6402
       False ->
         do let x = itVal _x6410
            let _x6407 = x
            _6317 <- case _x6407 of
                       number ->
                         do let _6316 = fromTag number _x6407
                            pure _6316
                       _ ->
                         fail_sys @(Word 32) "Pattern match failure"
            let _6318 = _6317 < y
            let _x6408 = _6318
            _x6403 <- case _x6408 of
                        True ->
                          pure ()
                        False ->
                          fail_sys @() "Pattern match failure"
            let _x6411 = cons _x6403 _x6402
            _F6409(_x6411, itNext _x6410, y)
 
proc P_bounded(xs : [Value], y : Word 32) : () =
  do _x6413 <- _F6409(nil @(), itNew xs, y)
     pure (listToArray _x6413)
     pure ()

====================

_F90  ::  • Assertions: • exported {y, _x91} => do _x93 <- pure (itNull _x91)
                                                   case _x93 of
                                                     True ->
                                                       [..]*;
                                                     False ->
                                                       do x <- pure (itVal _x91)
                                                          _x88 <- pure x
                                                          _44 <- case _x88 of
                                                                   number ->
                                                                     do _43 <- pure (fromTag number _x88)
                                                                        pure _43
                                                          _45 <- pure (_44 < y)
                                                          _x89 <- pure _45
                                                          assert _x89
                                                          [..]; call _F90 {{y, _x91}}

====================

  { _x90: nil  -> T }
, { _x90: cons ->
          ( hd -> ( key -> T , val -> number -> T  )
          , tl -> R@1
          )
  }

-}

{-

do _x92 <- pure (itNull _x90)
   case _x92 of
     True ->
       [..]*;
     False ->
       do x <- pure (itVal _x90)
          this <- case x of
                    number ->
                      do _46 <- pure (fromTag number x)
                         pure _46
          _x87 <- pure (last <= this)
          assert _x87
          _x91 <- pure this
          call _F89 {{last, _x90}}

===>

  { _x90: nil  -> T
  , _last : T }
, { _x90: cons ->
          ( hd -> ( key -> T , val -> number -> R@2  )
          , tl -> R@1
          )
  , _last : T }

-}

{-

force each element to be > 0

P (last, _x90) = 
 1 do _x92 <- pure (itNull _x90)
 2    case _x92 of
 3      True ->
 4        [..]*;
 5      False ->
 6        do x <- pure (itVal _x90)
 7           this <- case x of
 8                     number ->
 9                       do _46 <- pure (fromTag number x)
10                          pure _46
11           _x87 <- pure (last < this)
12           assert _x87
13           _x91 <- pure this
14           call _F89 {{last, _x90}}
15

--------
steps
--------

vc = VCAny
2:
 vc = VCAny

 3: -> { _x92: true }

 5:
  14:
   -> { { this: R(0), _x90: itNext -> R(1) } }
  12:
   -> { { this: R(0), _x90: itNext -> R(1), _x87: true } }
  11: vc = true
   -> { { this: R(0), _x90: itNext -> R(1), this: > 0, last: (< MAX) } }
  7: vc = R(0) && (> 0)
   9: vc = R(0) && (> 0)
    { x: number -> R(0) && (> 0) }
  -> { { x: number -> R(0) && (> 0), _x90: cons -> { hd: Top, tl: R(1) }, last: (< MAX) } }

  6: vc = number -> R(0) && (> 0)
  -> { { _x90: cons -> { hd: number -> R(0) && (> 0), tl: R(1) }, last: (< MAX) } }

  5: { { _x92: false: _x90: cons -> { hd: number -> R(0) && (> 0), tl: R(1) }, last: (< MAX) } }

  -> { { _x92: true }
     , { _x92: false: _x90: cons -> { hd: number -> R(0) && (> 0), tl: R(1) }, last: (< MAX) }
     }

  1: -> 
     { { _x90: nil -> T }
     , { _x90: cons -> { hd: number -> R(0) && (> 0), tl: R(1) }, last: (< MAX) }
     } 

--------

One iteration yields 

     { { _x90: nil -> T }
     , { _x90: cons -> { hd: number -> R(last) && (> 0), tl: R(_x90) }, last: (< MAX) }
     }

we want

Fix R. \x -> case x of nil => T | cons => { hd : number -> (> 0), tl : R }



-}

{-

There are a number of different classes of predicates we want to support:
  1) sum type predictes 'this is a particular constructor' like 'x is number',
     and 'each constructor C in x matches P' (i.e, Fix R. case x of C -> P ; _ -> R x, sort of)
  2) list predicates 'the list is at most/exactly N elements', 'each element matches P'
  3) map predicates  'each key/value matches P', 'the map has these keys, whos value matches P(key)'

-}
  






--------------------------------------------------------------------------------

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


-- Although we have partioal functions, like fromJust, they are
-- guarded by case statements, so we safely (?) ignore them for VCAny.
transfer :: TyEnv -> ValueConstraint -> Expr -> Domain
transfer _tys VCAny _ = trivialDomain
transfer tys vc expr = 
  case expr of
    Var n -> singletonD (Map.singleton n vc)
          
    PureLet n e e' ->
      let vcs = go vc e'
          (haves, havesNot) = partition (Map.member n) (toListD vcs)
          goHave h =
            -- n is known to exist from 'has'
            let (Just vc', h') = Map.updateLookupWithKey (\_ _ -> Nothing) n h
            in meet (singletonD h') (go vc' e)
      in joins (fromListD havesNot : map goHave haves)

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

    -- Case is where we get the alternatives.
    ECase c -> transferCase (go vc <$> c)

    Ap0 op       -> transferOp0 tys vc op
    Ap1 op e     -> transferOp1 tys vc op e
    Ap2 op e1 e2 -> transferOp2 tys vc op e1 e2
    Ap3 op e1 e2 e3 -> transferOp3 tys vc op e1 e2 e3
    -- ApN opN vs     -> semiExecOpN opN rty =<< mapM go vs
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
-- Partial operations
transferOp1 tys vc (FromUnion _t l) e = transfer tys (VCUnionElem l vc) e
transferOp1 tys vc FromJust         e = transfer tys (VCJust vc) e
transferOp1 _tys VCAny _            _ = trivialDomain
transferOp1 tys vc op e =
  case op of
    CoerceTo {}   -> go vc e
    IsEmptyStream -> unimplemented
    Head          -> unimplemented
    StreamOffset  -> unimplemented
    StreamLen     -> unimplemented

    -- Most numeric operations aren't predicated
    OneOf {}      -> unimplemented -- should be VCAny
    Neg           -> unimplemented -- should be VCAny
    BitNot        -> unimplemented -- should be VCAny
    Not           -> unimplemented -- should be VCAny
    ArrayLen      -> unimplemented -- should be VCAny

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
      
    InUnion _ut l
      | VCUnionElem l' vc' <- vc ->
          if l == l' then go vc' e else DBottom
    
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

transferOp2 :: TyEnv -> ValueConstraint -> Op2 -> Expr -> Expr -> Domain
transferOp2 _tys VCAny _ _ _ = trivialDomain
transferOp2 tys vc op e1 e2 =
  case op of
    -- Stream stuff
    IsPrefix    -> unimplemented
    Drop        -> unimplemented
    Take        -> unimplemented

    -- FIXME: maybe we need a VCBool b to catch predicates
    Eq          -> unimplemented -- should be VCAny
    NotEq       -> unimplemented -- should be VCAny
    Leq         -> unimplemented -- should be VCAny
    Lt          -> unimplemented -- should be VCAny

    Add         -> unimplemented -- should be VCAny
    Sub         -> unimplemented -- should be VCAny
    Mul         -> unimplemented -- should be VCAny
    Div         -> unimplemented -- should be VCAny
    Mod         -> unimplemented -- should be VCAny

    BitAnd      -> unimplemented -- should be VCAny
    BitOr       -> unimplemented -- should be VCAny
    BitXor      -> unimplemented -- should be VCAny
    Cat         -> unimplemented -- should be VCAny
    LCat        -> unimplemented -- should be VCAny
    LShift      -> unimplemented -- should be VCAny
    RShift      -> unimplemented -- should be VCAny

    ArrayIndex  -> unimplemented 
    ConsBuilder -> unimplemented
    MapLookup   -> unimplemented
    MapMember   -> unimplemented

    ArrayStream -> unimplemented
  where
    unimplemented = panic "Unimplemented" [showPP op]
    go = transfer tys

transferOp3 :: TyEnv -> ValueConstraint -> Op3 ->
               Expr -> Expr -> Expr -> Domain
transferOp3 _tys VCAny _ _ _ _ = trivialDomain
transferOp3 tys vc op e1 e2 e3 =
  case op of
    RangeUp   -> unimplemented
    RangeDown -> unimplemented
    MapInsert -> unimplemented
  where
    unimplemented = panic "Unimplemented" [showPP op]

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

    
