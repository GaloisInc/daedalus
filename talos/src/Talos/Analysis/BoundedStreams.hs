{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

module Talos.Analysis.BoundedStreams (boundedStreams, BoundedStreams, isStreamBoundedMaybe) where

import qualified Data.Map            as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Daedalus.Panic (panic)
import           Daedalus.Core
import           Daedalus.Core.Type

import Talos.Monad (TalosM)
import Daedalus.PP (showPP)
import Control.Monad.State (StateT, MonadState, gets)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import GHC.Generics.Lens ()
import Control.Lens (at, (.=), (&), each, (%~), _2, _1, has, gplate, traverseOf, over)
import Data.Generics.Labels ()
import Data.Foldable (foldl', toList)
import Daedalus.GUID (invalidGUID)
import Data.Data (Data)
import Control.Monad.Writer.CPS (tell, runWriter, Writer)
import Data.Functor (($>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad (join)

newtype BoundedStreams = BoundedStreams { getBoundedStreams :: Map Name Bool } 

boundedStreams :: TalosM BoundedStreams
boundedStreams = undefined

-- | Returns whether a stream may be bounded (or, the negation of
-- whether the stream is definitely unbounded)
isStreamBoundedMaybe :: BoundedStreams -> Expr -> Bool
isStreamBoundedMaybe = undefined

-- -----------------------------------------------------------------------------
-- Monad

-- This is not scoped (as it would be ordinarily) as we have unique
-- names, and we are calculating the abs value for each assignment.
data BSMState = BSMState
  { bsmVars :: Map Name AbsValue
  , bsmTInfo :: TyInfo
  } deriving Generic

newtype BSM a = BSM { getBSM :: StateT BSMState TalosM a }
  deriving (Functor, Applicative, Monad, MonadState BSMState )

recordName :: Name -> AbsValue -> BSM ()
recordName n v = #bsmVars . at n .= Just v

getTypeInfo :: TName -> BSM (TDef, Set TName)
getTypeInfo tn = do
  tinfo <- gets bsmTInfo
  let err = panic "Missing type def" [showPP tn]  
  pure $ Map.findWithDefault err tn tinfo

-- -----------------------------------------------------------------------------
-- Domain

{-
  Recursive types.
  ================

  The basic idea with domains for recursive types is that the type
appears unfolded once, so given the type for lists (where P is some
primitive type we care about):

data T = { a : P, b : L }
data L = Null | Cons T

we have the abstract value (for a L)

{ Null -> {}
, Cons -> { a -> D_P, b -> Rec "L" }
}

For the more general case of a mutually recursive type like

data F = NullF | One a F | Many ManyF
data ManyF = NullManyF | Cons a F ManyF

In DDL:

data T1 = { T1a : a, T1b : F }
data T2 = { T2a : a, T2b : F, T2c : ManyF }

data F = NullF | One T1 | Many ManyF

data ManyF = NullManyF | Cons T2

we have a F looks like

{ NullF -> {}
, One   -> { T1a : ..., T1b : Rec "F" }
, Many  -> { NullManyF -> {}, Cons -> { T2a : ..., T2b : Rec "F", T2c : Rec "ManyF" }
}

-} 

type TyInfo = Map TName (TDef, Set TName)

-- Finds all instances of the tn type in av and replaces them by 'AVRec tn'
foldRecursive :: TName -> Set TName -> AbsValue -> (AbsValue, [AbsValue])
foldRecursive tn recs av = over _2 toList $ runWriter (go av)
  where
    go :: AbsValue -> Writer (Seq AbsValue) AbsValue
    go av'@(AVUser tn' m)
      | tn == tn' = tell (Seq.singleton av') $> AVRec tn
      | tn' `Set.member` recs = AVUser tn' <$> traverse go m
      | otherwise = pure av'
    go av' = traverseOf gplate go av'

unfoldRecursive :: AbsValue -> TName -> Set TName -> AbsValue -> AbsValue
unfoldRecursive  topAv tn recs = go
  where
    go (AVRec tn') | tn == tn' = topAv
    go av'@(AVUser tn' m)
      | tn' `Set.member` recs = AVUser tn' (go <$> m)
      | otherwise             = av'
    go av' = av' & gplate %~ go

absInUnion :: UserType -> Label -> AbsValue -> BSM AbsValue
absInUnion ut l av = do
  (_tdef, recs) <- getTypeInfo tn
  pure $ if Set.null recs
     -- We are not recursive, so just construct, we could also check tnameRec.
    then AVUser tn (Map.singleton l av)
    -- we need to fold the recursive occurences and lub the
    -- folded values.    
    else let (av', avs) = foldRecursive tn recs av
             newav0 = AVUser tn (Map.singleton l av')
         in foldl' lub newav0 avs
  where
    tn = utName ut

-- Replace the recursive occurrences of a type with itself.
absFromUnion :: Type -> Label -> AbsValue -> BSM AbsValue
absFromUnion ty l av =
  case av of
    AVUser tn m
      | Just av' <- Map.lookup l m -> do
          (_tdef, recs) <- getTypeInfo tn
          pure (unfoldRecursive av tn recs av')
      | otherwise -> do
          tinfo <- gets bsmTInfo
          pure (bottomForTy tinfo ty)
    _ -> panic "Saw non-AVUser value in absFromUnion" []

-- | Produce the bottom value for the given type, which sets all the
-- streams to unbounded, and unions to the empty union (no labels set).
bottomForTy :: TyInfo -> Type -> AbsValue
bottomForTy tinfo = goTy mempty mempty
  where
    goNamed recCtxt recs tn
      | tn `Set.member` recCtxt = AVRec tn
      | Just (tdef, recs') <- Map.lookup tn tinfo =
          let recCtxt' -- types outside the SCC get an empty ctxt
                | tn `Set.member` recs = recCtxt -- recs == recs'
                | otherwise            = Set.empty
          in goDef tn (Set.insert tn recCtxt') recs' tdef 
      | otherwise = panic "Missing type definition" []
    
    goDef tn recCtxt recs tdef = AVUser tn $  
      case tdef of
        TUnion {}  -> Map.empty
        TStruct fs -> Map.fromList (fs & each . _2 %~ goTy recCtxt recs)
        TBitdata {} -> impossible

    goTy recCtxt recs ty =
      case ty of
        TStream    -> AVStream False
        TUInt {}   -> AVOther
        TSInt {}   -> AVOther
        TInteger   -> AVOther
        TBool      -> AVOther
        TFloat     -> AVOther
        TDouble    -> AVOther
        TUnit      -> AVOther
        TArray ty' -> AVSequence (goTy recCtxt recs ty')
        TMaybe ty' -> AVMaybe (goTy recCtxt recs ty')
        TMap kty vty -> AVMap (goTy recCtxt recs kty) (goTy recCtxt recs vty)
        TBuilder ty' -> AVSequence (goTy recCtxt recs ty')
        TIterator (TMap kty vty) -> AVIterator (goTy recCtxt recs kty) (goTy recCtxt recs vty)
        TIterator (TArray ty') -> AVIterator (goTy recCtxt recs TInteger) (goTy recCtxt recs ty')
        TIterator _ -> panic "Unexpected iterator type" []
        TUser ut -> goNamed recCtxt recs (utName ut)
        TParam {} -> panic "Unexpected type" [showPP ty]

    impossible = panic "Unexpected type" []

-- This allows us to derive traversable etc.
data AbsValue = 
    AVStream Bool
  | AVOther -- AUInt SizeType | ASInt SizeType | AInteger | ABool | AFloat | ADouble | AUnit
  | AVSequence AbsValue -- Arrays ans builders
  | AVMaybe AbsValue -- Just case
  | AVMap AbsValue AbsValue
  | AVIterator AbsValue AbsValue
  | AVUser TName (Map Label AbsValue) -- Both struct and union.  
  | AVRec TName
  deriving (Show, Generic)

-- instance Plated AbsValue where -- default

{-
>>> :set -XOverloadedStrings
>>> import Control.Lens
>>> t1 = TName { tnameId = invalidGUID, tnameText = "T1", tnameMod = MName "M1", tnameAnon = Nothing, tnameRec = False, tnameBD = False, tnameFlav = TFlavStruct [] }
>>> let { go AVOther = AVStream False; go x = x }
>>> AVMaybe AVOther & gplate %~ go
>>> AVUser t1 (Map.singleton "l1" AVOther) & gplate %~ go
AVMaybe (AVStream False)
AVUser (TName {tnameId = GUID {getGUID = -1}, tnameText = "T1", tnameMod = MName {mNameText = "M1"}, tnameAnon = Nothing, tnameRec = False, tnameBD = False, tnameFlav = TFlavStruct []}) (fromList [("l1",AVOther)])
-}


lub :: AbsValue -> AbsValue -> AbsValue
lub = go 
  where
    go v1 v2 =
      case (v1, v2) of
        (AVStream b1, AVStream b2) -> AVStream (b1 || b2)
        (AVOther,     AVOther)     -> AVOther
        (AVSequence v1', AVSequence v2') -> AVSequence (go v1' v2')
        (AVMaybe v1', AVMaybe v2') -> AVMaybe (go v1' v2')
        (AVMap kv1 vv1, AVMap kv2 vv2) -> AVMap (go kv1 kv2) (go vv1 vv2)
        (AVIterator kv1 vv1, AVIterator kv2 vv2) -> AVIterator (go kv1 kv2) (go vv1 vv2)
        -- This allows us to represent bottom as an empty map
        -- (s.t. lubbing with non-bottom will prefer the non-bottom
        -- value).
        (AVUser tn flds1, AVUser _ flds2) -> AVUser tn (Map.unionWith lub flds1 flds2)
        (AVUser {}, AVRec {}) -> v1
        (AVRec {}, AVUser {}) -> v2
        (AVRec tn1, AVRec _tn2) -> v1
        _ -> panic "Mismatched values" []

-- lt :: AbsValue -> AbsValue -> Bool

-- -----------------------------------------------------------------------------
-- Transfer functions

transferName :: Name -> BSM AbsValue
transferName n = gets (fromMaybe err . Map.lookup n . bsmVars)
  where
    err = panic "Missing key" []

transferG :: Bool -> Grammar -> BSM (Bool, AbsValue)
transferG bndd g =
  case g of
    Pure e    -> (,) bndd <$> transferE e
    GetStream -> (,) bndd <$> pure (AVStream bndd)
    SetStream e -> do
      av <- transferE e
      case av of
        AVStream bndd' -> pure (bndd', AVOther)
        _ -> unexpected
    -- MatchEnd shouldn't impact us here
    Match {} -> other 
    Fail {}  -> other
    Do_ lhs rhs -> do
      (bndd', _) <- transferG bndd lhs
      transferG bndd' rhs
    Do n lhs rhs -> do
      (bndd', av) <- transferG bndd lhs
      recordName n av
      transferG bndd' rhs
    Let n e rhs -> transferG bndd (Do n (Pure e) rhs)
    OrBiased lhs rhs  -> orG lhs rhs
    OrUnbiased lhs rhs -> orG lhs rhs
    Call fn args -> undefined
    Annot _ g' -> transferG bndd g'
    GCase (Case n pats) -> undefined
    Loop lc -> undefined
  where    
    orG lhs rhs = do
      (bnddl, avl) <- transferG bndd lhs
      (bnddr, avr) <- transferG bndd rhs
      pure (bnddl || bnddr, avl `lub` avr)
      
.    other = pure (bndd, AVOther)
    unexpected = panic "Unexpected grammar or value" []
    

transferE :: Expr -> BSM AbsValue
transferE e = 
  case e of
    Var n -> transferName n
    PureLet n e1 e2 -> do
      av1 <- transferE e1
      recordName n av1
      transferE e2
      
    Struct ut flds -> AVUser (utName ut) . Map.fromList <$> mapM goFld flds

    -- Removed by LiftExpr
    ECase {} -> unexpected    
    ELoop {} -> unexpected
    
    Ap0 op0 -> transferOp0 op0
    Ap1 op1 e -> transferOp1 op1 =<< transferE e
    Ap2 op2 e1 e2 -> join (transferOp2 op2 <$> transferE e1 <*> transferE e2)
    Ap3 op3 e1 e2 e3 ->
      join (transferOp3 op3 <$> transferE e1 <*> transferE e2 <*> transferE e3)
    ApN opN es -> transferOpN opN =<< traverse transferE es
  where
    goFld (l, e') = (,) l <$> transferE e'
    unexpected = panic "Unexpected expression" [showPP e]

transferOp0 :: Op0 -> BSM AbsValue
transferOp0 op0 =
  case op0 of
    Unit -> pure AVOther
    IntL {} -> pure AVOther
    FloatL {} -> pure AVOther
    BoolL {}  -> pure AVOther
    ByteArrayL {}   -> pure (AVSequence AVOther)
    NewBuilder {}   -> pure (AVSequence AVOther)
    MapEmpty ty ty' -> do
      tinfo <- gets bsmTInfo
      pure (bottomForTy tinfo (TMap ty ty'))
    ENothing ty -> do
      tinfo <- gets bsmTInfo
      pure (bottomForTy tinfo (TMaybe ty))

transferOp1 :: Op1 -> AbsValue -> BSM AbsValue
transferOp1 op1 av =
  case op1 of
    CoerceTo {} -> pure av -- shouldn't change the rep?
    IsEmptyStream -> other
    Head -> other
    StreamOffset -> other
    BytesOfStream -> other
    OneOf {} -> other
    Neg -> other
    BitNot -> other
    Not -> other
    ArrayLen -> other
    Concat ->
      case av of
        AVSequence av' -> pure av'
        _ -> unexpected
    FinishBuilder -> pure av
    NewIterator   ->
      case av of
        AVSequence av' -> pure (AVIterator AVOther av')
        AVMap kav vav  -> pure (AVIterator kav vav)
        _ -> unexpected
    IteratorDone -> other
    IteratorKey  ->
      case av of
        AVIterator kv _ -> pure kv
        _ -> unexpected
    IteratorVal ->
      case av of
        AVIterator _ vv -> pure vv
        _ -> unexpected
    IteratorNext -> pure av
    EJust -> pure (AVMaybe av)
    FromJust ->
      case av of
        AVMaybe av' -> pure av'
        _ -> unexpected
    SelStruct ty l
      | AVUser _ m <- av, Just av' <- Map.lookup l m -> pure av'
      | otherwise -> unexpected
      
    InUnion ut l -> absInUnion ut l av
    FromUnion ty l -> absFromUnion ty l av
    WordToFloat -> other
    WordToDouble -> other
    IsNaN -> other
    IsInfinite -> other
    IsDenormalized -> other
    IsNegativeZero -> other
  where
    other = pure AVOther
    unexpected = panic "Unexpected expression" []

-- Doesn't need to be monadic.
transferOp2 :: Op2 -> AbsValue -> AbsValue -> BSM AbsValue
transferOp2 op2 av1 av2 =
  case op2 of
    IsPrefix -> other
    Drop     -> pure av2 -- stream is bounded if input is
    DropMaybe -> pure av2
    Take -> pure (AVStream True) 
    Eq -> other
    NotEq -> other
    Leq -> other
    Lt  -> other
    Add -> other
    Sub -> other
    Mul -> other
    Div -> other
    Mod -> other

    BitAnd -> other
    BitOr  -> other
    BitXor -> other
    Cat    -> other
    LCat   -> other
    LShift -> other
    RShift -> other

    ArrayIndex | AVSequence av' <- av1 -> pure av'
               | otherwise -> unexpected
    Emit | AVSequence av' <- av1 -> pure (AVSequence (av' `lub` av2))
         | otherwise -> unexpected
    EmitArray | AVSequence av1' <- av1
              , AVSequence av2' <- av2 -> pure (AVSequence (av1' `lub` av2'))
              | otherwise -> unexpected
    EmitBuilder | AVSequence av1' <- av1
                , AVSequence av2' <- av2 -> pure (AVSequence (av1' `lub` av2'))
                | otherwise -> unexpected
    MapLookup | AVMap _kv vv <- av2 -> pure vv
              | otherwise -> unexpected
    
    MapMember   -> other
    ArrayStream -> pure (AVStream True) -- array is bounded
  where
    other = pure AVOther
    unexpected = panic "Unexpected expression" []

transferOp3 :: Op3 -> AbsValue -> AbsValue -> AbsValue -> BSM AbsValue
transferOp3 op3 av1 av2 av3 =
  case op3 of
    RangeUp   -> pure (AVSequence AVOther)
    RangeDown -> pure (AVSequence AVOther)
    MapInsert | AVMap kv vv <- av3 -> pure (AVMap (kv `lub` av1) (vv `lub` av2))
              | otherwise -> unexpected
  where
    unexpected = panic "Unexpected expression" []

transferOpN :: OpN -> [AbsValue] -> BSM AbsValue
transferOpN opN avs =
  case opN of
    ArrayL ty -> do
      tinfo <- gets bsmTInfo
      pure (foldl lub (bottomForTy tinfo ty) avs)
    CallF {} -> unexpected
  where
    unexpected = panic "Unexpected expression" []
