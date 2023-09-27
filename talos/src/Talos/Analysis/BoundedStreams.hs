{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Talos.Analysis.BoundedStreams
  ( boundedStreams
  , BoundedStreams
  , isStreamBoundedMaybe
  ) where

import           Control.Lens                   (_2, at, each, gplate, ix, over,
                                                 traverseOf, use, (%=), (%~),
                                                 (&), (.=), (.~), (<>=))
import           Control.Monad                  (join, when)
import           Control.Monad.Reader           (MonadReader, ReaderT, asks,
                                                 local, runReaderT)
import           Control.Monad.State            (MonadState, StateT, execStateT)
import           Control.Monad.Trans            (lift)
import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Control.Monad.Writer.CPS       (Writer, WriterT, censor,
                                                 listen, runWriter, tell)
import           Data.Foldable                  (foldl', toList, find)
import           Data.Functor                   (($>))
import           Data.Generics.Labels           ()
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Map.Merge.Lazy            as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           GHC.Generics                   (Generic)
import           GHC.Generics.Lens              ()

import           Daedalus.Core                  hiding (leq)
import           Daedalus.Core.CFG              (NodeID, pattern WithNodeID)
import           Daedalus.Core.Type             (typeOf)
import           Daedalus.Panic                 (panic)
import           Daedalus.PP                    (showPP, PP(..), vcat)

import           Talos.Monad                    (LiftTalosM, TalosM, getGFun,
                                                 getModule)


-- For now this analysis is context insensitive, as making it context
-- sensitive requires the slicing to be context sensitive (which it
-- currently is not).
newtype BoundedStreams = BoundedStreams { getBoundedStreams :: Map NodeID (Bool, Bool) }
  deriving (Semigroup, Monoid)
  
boundedStreams :: TalosM BoundedStreams
boundedStreams = do
  md <- getModule
  let entry =
        case find fIsEntry (mGFuns md) of
          Nothing -> error "Missing entry function" []
          Just f  -> f
  calcFixpoint entry

--  | Returns whether a stream may be bounded (or, the negation of
-- whether the stream is definitely unbounded)
isStreamBoundedMaybe :: NodeID -> BoundedStreams -> (Bool, Bool)
isStreamBoundedMaybe n = Map.findWithDefault (False, False) n . getBoundedStreams

-- -----------------------------------------------------------------------------
-- Monad

data BSMEnv = BSMEnv
  { bsmVars :: Map Name AbsValue
  , bsmTInfo :: TyInfo
  , bsmCurrentFun :: FName  
  } deriving Generic

data BSMSummary = BSMSummary
  { bsmsArgs   :: [AbsValue]
  , bsmsArgBounded :: Bool
  , bsmsResult :: (Bool, AbsValue)
  , bsmsNodes  :: BoundedStreams
  } deriving Generic

-- FIXME: combine with Fixpoint.hs?
data BSMState = BSMState
  { bsmWorklist  :: Set FName
  , bsmSummaries :: Map FName BSMSummary
  , bsmRevDeps   :: Map FName (Set FName)
  } deriving Generic

newtype BSM a = BSM { _getBSM :: ReaderT BSMEnv (StateT BSMState TalosM) a }
  deriving (Functor, Applicative, Monad, MonadReader BSMEnv, MonadState BSMState, LiftTalosM)

runBSM :: FName -> TyInfo -> BSMState -> BSM a -> TalosM BSMState
runBSM fn tinfo st (BSM m) = execStateT (runReaderT m env) st
  where
    env = BSMEnv { bsmVars = mempty
                 , bsmTInfo = tinfo
                 , bsmCurrentFun = fn
                 }

bindNameIn :: MonadReader BSMEnv m => Name -> AbsValue -> m a -> m a
bindNameIn n v = local (#bsmVars . at n .~ Just v)

getTypeInfo :: TName -> BSM (TDef, Set TName)
getTypeInfo tn = do
  tinfo <- asks bsmTInfo
  let err = panic "Missing type def" [showPP tn]  
  pure $ Map.findWithDefault err tn tinfo

bottomSummary :: TyInfo -> Bool -> FName -> [AbsValue] -> BSMSummary
bottomSummary tinfo bndd fn args = do
  BSMSummary { bsmsArgs   = args
             , bsmsArgBounded = bndd
             , bsmsResult = (False, bot)
             , bsmsNodes  = mempty
             }
  where
    bot = bottomForTy tinfo (fnameType fn)
    
-- ----------------------------------------------------------------------------------------
-- Monadic fixpoint construction

getResultFor :: Bool -> FName -> [AbsValue] -> BSM (Bool, AbsValue)
getResultFor bndd fn args = do
  cfun <- asks bsmCurrentFun
  #bsmRevDeps . at fn <>= Just (Set.singleton cfun) -- note rev dep, could pre-calc.
  m_summary <- use (#bsmSummaries . at fn)

  let needsWork bndd' args' = do
        tinfo <- asks bsmTInfo
        let emptySummary = bottomSummary tinfo bndd' fn args'
        #bsmWorklist %= Set.insert fn                           
        #bsmSummaries . at fn .= Just emptySummary
        pure (bsmsResult emptySummary)
        
  case m_summary of
    Just bsm
      | args `allLeq` bsmsArgs bsm -> pure (bsmsResult bsm)
      | otherwise -> needsWork (bndd || bsmsArgBounded bsm) (zipWith lub args (bsmsArgs bsm))
    Nothing -> needsWork bndd args    
  where
    allLeq m1 m2 = and (zipWith leq m1 m2)

calcFixpoint :: Fun Grammar -> TalosM BoundedStreams
calcFixpoint entryf = do
  tinfo <- makeTyInfo
  -- We add a summary for entry so we can assume one exists in
  -- transferDecl (note thet getResultFor adds a summary if one is
  -- missing).
  let args = map (bottomForTy tinfo . typeOf) (fParams entryf)
      bsm = bottomSummary tinfo False entry args
  
  let st0 = BSMState { bsmWorklist = Set.singleton entry
                     , bsmSummaries = Map.singleton entry bsm
                     , bsmRevDeps  = mempty
                     }
  st <- go tinfo st0
  pure (foldMap bsmsNodes (bsmSummaries st))
  where
    go tinfo s@BSMState { bsmWorklist = wl } | Just (fn, wl') <- Set.minView wl
      = let s' = s { bsmWorklist = wl' }
        in go tinfo =<< runBSM fn tinfo s' (transferDecl fn)
    go _ st = pure st

    entry = fName entryf

transferDecl :: FName -> BSM ()
transferDecl fn = do
  decl <- getGFun fn
  case fDef decl of
    External -> panic "External function" []
    Def g    -> do
      bsm <- fromMaybe err <$> use (#bsmSummaries . at fn) -- should always work
      let argMap = Map.fromList (zip (fParams decl) (bsmsArgs bsm))
      (r, info) <- local (#bsmVars .~ argMap) (runWriterT (transferG (bsmsArgBounded bsm) g))
      -- Make sure we use any updates to the bsm (i.e., don't use bsm above)
      #bsmSummaries . ix fn %= ((#bsmsResult .~ r) . (#bsmsNodes .~ info))
      when (needToPropagate bsm r) propagate      
  where
    needToPropagate old r = bsmsResult old /= r -- Can get away with (==) here
    propagate = do
      revDeps <- use (#bsmRevDeps . ix fn)
      #bsmWorklist <>= revDeps
    
    err = panic "Missing summary" [showPP fn]
    
makeTyInfo :: TalosM TyInfo
makeTyInfo = do
  md  <- getModule
  let tys = mTypes md
      mk1 (NonRec td)  = [ (tName td, (tDef td, mempty)) ]
      mk1 (MutRec tds) =
        let recs = Set.fromList (map tName tds)
        in [ (tName td, (tDef td, recs)) | td <- tds ]
  pure (Map.fromList (concatMap mk1 tys))
  
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
          tinfo <- asks bsmTInfo
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


bottomForTyM :: MonadReader BSMEnv m => Type -> m AbsValue
bottomForTyM ty = do
  tinfo <- asks bsmTInfo
  pure (bottomForTy tinfo ty)

isBottom :: AbsValue -> Bool
isBottom av =
  case av of
    AVStream b -> not b
    AVOther    -> True
    AVSequence av'  -> isBottom av'
    AVMaybe av'     -> isBottom av'
    AVMap av1 av2   -> isBottom av1 && isBottom av2
    AVIterator av1 av2 -> isBottom av1 && isBottom av2
    AVUser _tn m       -> all isBottom m
    AVRec _            -> True

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
  deriving (Show, Eq, Generic)

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
        (AVRec _tn1, AVRec _tn2) -> v1
        _ -> panic "Mismatched values" []

-- Partial order
leq :: AbsValue -> AbsValue -> Bool
leq = go
  where
    go v1 v2 =
      case (v1, v2) of
        (AVStream b1, AVStream b2) -> if b1 then b2 else True
        (AVOther,     AVOther)     -> True
        (AVSequence v1', AVSequence v2') -> v1' `leq` v2'
        (AVMaybe v1', AVMaybe v2') -> v1' `leq` v2'
        (AVMap kv1 vv1, AVMap kv2 vv2) -> kv1 `leq` kv2 && vv1 `leq` vv2
        (AVIterator kv1 vv1, AVIterator kv2 vv2) -> kv1 `leq` kv2 && vv1 `leq` vv2
        (AVUser _tn flds1, AVUser _ flds2) ->
          and (Map.merge
               -- should always yield false?  We should maybe not have bottoms in AVUsers              
               (Map.mapMissing (const isBottom))
               Map.dropMissing
               (Map.zipWithMatched (const go)) flds1 flds2)
        (AVUser {}, AVRec {}) -> False -- ???
        (AVRec {}, AVUser {}) -> False -- ??/
        (AVRec _tn1, AVRec _tn2) -> False -- ???
        _ -> panic "Mismatched values" []

-- -----------------------------------------------------------------------------
-- Transfer functions

transferName :: Name -> BSM AbsValue
transferName n = asks (fromMaybe err . Map.lookup n . bsmVars)
  where
    err = panic "Missing key" []

transferG :: Bool -> Grammar -> WriterT BoundedStreams BSM (Bool, AbsValue)
transferG bndd (WithNodeID nid _anns g) = do
  (postb, r) <- case g of
    Pure e    -> simple (lift (transferE e))
    GetStream -> simple (pure (AVStream bndd))
    SetStream e -> do
      av <- lift (transferE e)
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
      bindNameIn n av (transferG bndd' rhs)
    Let n e rhs -> transferG bndd (Do n (Pure e) rhs)
    Choice _biased gs  -> do
      (bnds, vs) <- unzip <$> traverse (transferG bndd) gs
      pure (or bnds, foldl1 lub vs)
    Call fn args -> lift (transferCall bndd fn args)
    Annot {} -> unexpected
    GCase cs -> do
      (bnds, vs) <- unzip <$> traverse (transferG bndd) (toList cs)
      pure (or bnds, foldl1 lub vs)
    Loop lc -> transferLoop bndd lc
    _ -> unexpected -- keep pat completeness happy
  tell (BoundedStreams (Map.singleton nid (bndd, postb)))
  pure (postb, r)
  where
    simple :: WriterT BoundedStreams BSM AbsValue -> WriterT BoundedStreams BSM (Bool, AbsValue)
    simple m = (,) bndd <$> m
      
    other = pure (bndd, AVOther)
    unexpected = panic "Unexpected grammar or value" []

transferLoop :: Bool -> LoopClass Grammar -> WriterT BoundedStreams BSM (Bool, AbsValue)
transferLoop bndd lcl =
  case lcl of
    ManyLoop _sem _bt _lb _m_ub g -> do
      bot <- bottomForTyM (typeOf g)
      over _2 AVSequence <$> go False bot (\bndd' _ -> transferG bndd' g)
    RepeatLoop _bt n e g -> do
      rE <- lift (transferE e)
      bot <- bottomForTyM (typeOf n)
      let f bndd' r = bindNameIn n (lub r rE) (transferG bndd' g)
      over _2 (lub rE) <$> go False bot f
    MorphismLoop (FoldMorphism n e lc g) -> do
      -- See Repeat, maybe we could factor out
      rE <- lift (transferE e)
      bot <- bottomForTyM (typeOf n)
      bnds <- lift (bindsLC lc)
      let f bndd' r =  bindNameIn n (lub r rE) (bnds (transferG bndd' g))
      over _2 (lub rE) <$> go False bot f
    MorphismLoop (MapMorphism lc g) -> do
      bot <- bottomForTyM (typeOf g)
      bnds <- lift (bindsLC lc)
      let f bndd' _ = bnds (transferG bndd' g)
      over _2 AVSequence <$> go False bot f
  where
    go bndd0 r0 f = do
      ((bndd', r'), nodes) <-
        censor (const mempty) (listen (f (bndd0 || bndd) r0))
      if bndd' == bndd0 && r' == r0
        then tell nodes $> (bndd' || bndd, r')
        else go bndd' r' f
    
    bindsLC lc = do
      colE <- transferE (lcCol lc)
      let (kv, elv) = case colE of
                        AVSequence v -> (AVOther, v)
                        AVMap kv' elv' -> (kv', elv')
                        _ -> panic "Unexpected value" []
          bndK | Just kn <- lcKName lc = bindNameIn kn kv
               | otherwise             = id
          bndE = bindNameIn (lcElName lc) elv
      pure (bndK . bndE)

transferCall :: Bool -> FName -> [Expr] -> BSM (Bool, AbsValue)
transferCall bndd fn args
  | Just vs <- mapM exprAsVar args = do
      avals <- mapM transferName vs
      getResultFor bndd fn avals
  | otherwise = panic "Malformed call" []
  where
    -- Copied from Analysis.hs
    exprAsVar :: Expr -> Maybe Name
    exprAsVar (Var n) = Just n
    exprAsVar _       = Nothing

transferE :: Expr -> BSM AbsValue
transferE e = 
  case e of
    Var n -> transferName n
    PureLet n e1 e2 -> do
      av1 <- transferE e1
      bindNameIn n av1 (transferE e2)
      
    Struct ut flds -> AVUser (utName ut) . Map.fromList <$> mapM goFld flds

    -- Removed by LiftExpr
    ECase {} -> unexpected    
    ELoop {} -> unexpected
    
    Ap0 op0 -> transferOp0 op0
    Ap1 op1 e' -> transferOp1 op1 =<< transferE e'
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
    MapEmpty ty ty' -> bottomForTyM (TMap ty ty')
    ENothing ty -> bottomForTyM (TMaybe ty)

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
    SelStruct _ty l
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
    unexpected = panic "Unexpected Op1" [showPP op1, show av]

-- Doesn't need to be monadic.
transferOp2 :: Op2 -> AbsValue -> AbsValue -> BSM AbsValue
transferOp2 op2 av1 av2 =
  case op2 of
    IsPrefix -> other
    Drop     -> pure av2 -- stream is bounded if input is
    DropMaybe -> pure (AVMaybe av2)
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
      bot <- bottomForTyM ty
      pure (foldl lub bot avs)
    CallF {} -> unexpected
  where
    unexpected = panic "Unexpected expression" []

-- -----------------------------------------------------------------------------
-- Pretty printing

instance PP BoundedStreams where
  pp (BoundedStreams m) =
    vcat [ "id:" <> pp nid <> " " <> pp inB <> " -> " <> pp outB
         | (nid, (inB, outB)) <- Map.toList m]
    
