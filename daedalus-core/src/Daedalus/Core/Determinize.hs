{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
module Daedalus.Core.Determinize (determinizeModule) where

-- import Debug.Trace(trace)

import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as Set
import MonadLib
import Data.Word(Word8)
import Data.ByteString(ByteString, uncons, null, pack)

--import Daedalus.Panic(panic)
--import Daedalus.PP(pp)
import Daedalus.GUID(GUID, firstValidGUID, succGUID)

import Daedalus.Core.Decl
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar
    ( Grammar(..), Match(MatchByte, MatchBytes), Sem(..)
    , ErrorSource (ErrorFromSystem), mapChildrenG  )
import Daedalus.Core.Basics
import Daedalus.Core.Type (typeOf)

determinizeModule :: Module -> Module
determinizeModule m  =
  let newGram = map (fmap (detGram m)) (mGFuns m) in
  m {mGFuns = newGram}

detGram :: Module -> Grammar -> Grammar
detGram modl gram = go gram
  where
  go g = case g of
           OrBiased {}   -> determinize modl g
           OrUnbiased {} -> determinize modl g
           _             -> mapChildrenG go g

getGrammarDef :: Module -> FName -> Maybe Grammar
getGrammarDef modl name =
  goGetGrammar (mGFuns modl)
  where
  goGetGrammar [] = error "cannot find grammar"
  goGetGrammar ( Fun {fName = n1, fDef = fdef} : rest ) =
    if fnameId n1 == fnameId name
    then case fdef of
           Def g2 -> Just g2
           External -> Nothing
    else goGetGrammar rest

getByteSetDef :: Module -> FName -> Maybe ByteSet
getByteSetDef modl name =
  goGetByteSet (mBFuns modl)
  where
  goGetByteSet [] = error "cannot find byteset"
  goGetByteSet ( Fun {fName = n1, fDef = fdef} : rest ) =
    if fnameId n1 == fnameId name
    then case fdef of
           Def g2 -> Just g2
           External -> Nothing
    else goGetByteSet rest


{-
  The progression for how we do determinization is:
  * zipgrammar: to derive a grammar until
  * Alternation/Or Tree: to derive a grammar traversing through alternation
  * Derivation Tree: to repeat the derivation until branches are unique or no more progress can be made
-}


{- Zipped Grammar -}
data ZMatch =
    ZMatchByte ByteSet
  | ZMatchBytes ([Word8], ByteString) ByteString -- the pair is the zipper of a ByteString
                                                 -- the last ByteString is the original one in the grammar
  | ZMatchEnd

data ZGrammar =
    ZMatch Sem
  | ZDo_ Grammar
  | ZDo_2 Grammar
  | ZDo Name Grammar
  | ZDo2 Name Grammar
  | ZLet Name Expr
  | ZAnnot Annot
  | ZOrBiased
  | ZOrUnbiased

type PathGrammar = [ ZGrammar ]

data ZipGrammar =
    ZipNode { focus :: Grammar, path :: PathGrammar }
  | ZipLeaf { zmatch :: ZMatch, path :: PathGrammar }

emptyPathGrammar :: PathGrammar
emptyPathGrammar = []

mkPathGrammar :: ZGrammar -> PathGrammar -> PathGrammar
mkPathGrammar n z = n : z

initZipGrammar :: Grammar -> ZipGrammar
initZipGrammar gram = ZipNode {focus = gram, path = emptyPathGrammar}

mkZipGrammar :: Grammar -> PathGrammar -> ZipGrammar
mkZipGrammar g p = ZipNode g p

moveLeft :: ZipGrammar -> ZipGrammar
moveLeft (ZipNode {focus = foc, path = pth}) =
  case foc of
    Do_ g1 g2     -> ZipNode { focus = g1, path = mkPathGrammar (ZDo_ g2) pth }
    Do name g1 g2 -> ZipNode { focus = g1, path = mkPathGrammar (ZDo name g2) pth }
    Annot ann g1  -> ZipNode { focus = g1, path = mkPathGrammar (ZAnnot ann) pth }
    Let name e g1 -> ZipNode { focus = g1, path = mkPathGrammar (ZLet name e) pth }
    _ -> error "should not happen"
moveLeft (ZipLeaf {}) = error "should not happen"

goUpRight :: ZipGrammar -> Maybe ZipGrammar
goUpRight (ZipNode {focus = foc, path = pth}) =
  case pth of
    [] -> Nothing
    (ZDo_ g2 : z) ->
      let newPath = ZDo_2 foc : z in
      Just (mkZipGrammar g2 newPath)
    (ZDo_2 g1 : z) ->
      goUpRight (mkZipGrammar (Do_ g1 foc) z)
    (ZDo name g2 : z) ->
      let newPath = ZDo2 name foc : z in
      Just (mkZipGrammar g2 newPath)
    (ZDo2 name g1 : z) ->
      goUpRight (mkZipGrammar (Do name g1 foc) z)
    (ZAnnot ann : z) ->
      goUpRight (mkZipGrammar (Annot ann foc) z)
    (ZLet name e : z) ->
      goUpRight (mkZipGrammar (Let name e foc) z)
    (ZOrBiased : _) -> Nothing
    (ZOrUnbiased : _) -> Nothing
    _  -> error "case not handled"
goUpRight (ZipLeaf{}) = error "cannot be a leaf"


goNextLeaf :: Integer -> ZipGrammar -> Maybe ZipGrammar
goNextLeaf c (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch sem : pth}) =
  case sem of
    SemYes ->
      let newBuilt = Pure (Ap0 (IntL c (TUInt (TSize 8)))) in
      goUpRight (mkZipGrammar newBuilt pth)
    SemNo ->
      let newBuilt = Pure (Ap0 Unit) in
      goUpRight (mkZipGrammar newBuilt pth)
goNextLeaf _c (ZipLeaf{ zmatch = zm@(ZMatchBytes (_, rest) orig), path = fpth@(ZMatch sem : pth)}) =
  if not (Data.ByteString.null rest)
  then Just $ ZipLeaf {zmatch = zm, path = fpth }
  else
    case sem of
      SemYes ->
        let pureMatch = Pure (Ap0 (ByteArrayL orig))
        in
        goUpRight (mkZipGrammar pureMatch pth)
      SemNo ->
        let newBuilt = Pure (Ap0 Unit) in
        goUpRight (mkZipGrammar newBuilt pth)
goNextLeaf _ _ = error "Should not happen"

buildUp :: ZipGrammar -> Grammar
buildUp (ZipNode {focus = built, path = pth}) =
  case pth of
    [] -> built
    (ZDo_ g2 : z) ->
      let newBuilt = Do_ built g2 in
      buildUp (mkZipGrammar newBuilt z)
    (ZDo_2 g1 : z) ->
      let newBuilt = Do_ g1 built in
      buildUp (mkZipGrammar newBuilt z)
    (ZDo name g2 : z) ->
      let newBuilt = Do name built g2 in
      buildUp (mkZipGrammar newBuilt z)
    (ZDo2 name g1 : z) ->
      let newBuilt = Do name g1 built in
      buildUp (mkZipGrammar newBuilt z)
    (ZLet name e : z) ->
      let newBuilt = Let name e built in
      buildUp (mkZipGrammar newBuilt z)
    (ZAnnot ann : z) ->
      buildUp (mkZipGrammar (Annot ann built) z)
    (ZOrBiased : z) ->
      buildUp (mkZipGrammar built z)
    (ZOrUnbiased : z) ->
      buildUp (mkZipGrammar built z)
    _  -> error "case not handled"
buildUp (ZipLeaf {}) = error "broken invariant"

{- END of Zipped Grammar -}

data CharSet =
    CByteSet (Set Integer)
  | CWord8 Word8

fromCharSetToSet :: CharSet -> Set Integer
fromCharSetToSet c =
  case c of
    CByteSet s -> s
    CWord8 w -> Set.singleton $ fromIntegral w

memberCharSet :: Integer -> CharSet -> Bool
memberCharSet n c = Set.member n (fromCharSetToSet c)

fromByteSetToSet :: Module -> ByteSet -> Maybe (Set Integer)
fromByteSetToSet modl bs =
  go bs
  where
  go b =
    let allBytes = foldr (\ i s -> Set.insert i s) Set.empty [0 .. 255] in
    case b of
      SetAny -> Just allBytes
      (SetSingle (Ap0 (IntL s  (TUInt (TSize 8))))) -> Just (Set.singleton s)
      (SetRange  (Ap0 (IntL s1 (TUInt (TSize 8)))) (Ap0 (IntL s2 (TUInt (TSize 8))))) ->
        if s1 <= s2 && s1 >=0 && s2 <= 255
        then Just (foldr (\ i s -> Set.insert i s) Set.empty [s1 .. s2])
        else Nothing
      (SetComplement bs1) -> do
        s1 <- go bs1
        return $ Set.difference allBytes s1
      (SetUnion bs1 bs2) -> do
        s1 <- go bs1
        s2 <- go bs2
        Just (Set.union s1 s2)
      (SetCall name []) -> do
        s1 <- getByteSetDef modl name
        go s1
      _      -> Nothing

{- END of CharSet -}


data AltTree a = -- AltTree [ a ]
    AltLeaf     a
  | AltBiased   (AltTree a) (AltTree a)
  | AltUnbiased (AltTree a) (AltTree a)

instance Functor AltTree where
  fmap f (AltLeaf a) = AltLeaf (f a)
  fmap f (AltBiased a b) = AltBiased (fmap f a) (fmap f b)
  fmap f (AltUnbiased a b) = AltUnbiased (fmap f a) (fmap f b)

mapAlt :: (a -> Maybe (AltTree b)) -> AltTree a -> Maybe (AltTree b)
mapAlt f t =
  case t of
    AltLeaf a -> f a
    AltBiased t1 t2 ->
      let f1 = mapAlt f t1
          f2 = mapAlt f t2 in
      case (f1,f2) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a1, Just a2) -> Just (AltBiased a1 a2)
    AltUnbiased t1 t2 ->
      let f1 = mapAlt f t1
          f2 = mapAlt f t2 in
      case (f1,f2) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a1, Just a2) -> Just (AltUnbiased a1 a2)

foldAlt :: (a -> b -> b) -> b -> AltTree a -> b
foldAlt reduce b t =
  case t of
    AltLeaf a -> reduce a b
    AltBiased t1 t2 ->
      let f1 = foldAlt reduce b t1 in
      foldAlt reduce f1 t2
    AltUnbiased t1 t2 ->
      let f1 = foldAlt reduce b t1 in
      foldAlt reduce f1 t2

data GC a =
    GZero
  | GOne  a
  | GMany a

garbageCollect :: AltTree (Either z ()) -> GC (AltTree z)
garbageCollect t = case t of
  AltLeaf e -> case e of
    Left z -> GOne (AltLeaf z)
    Right () -> GZero
  AltBiased t1 t2 ->
    let x = garbageCollect t1
        y = garbageCollect t2 in
    case (x,y) of
      (GZero, _) -> y
      (_, GZero) -> x
      (GOne a, GOne b)   -> GMany (AltBiased a b)
      (GOne a, GMany b)  -> GMany (AltBiased a b)
      (GMany a, GOne b)  -> GMany (AltBiased a b)
      (GMany a, GMany b) -> GMany (AltBiased a b)
  AltUnbiased t1 t2 ->
    let x = garbageCollect t1
        y = garbageCollect t2 in
    case (x,y) of
      (GZero, _) -> y
      (_, GZero) -> x
      (GOne a, GOne b)   -> GMany (AltUnbiased a b)
      (GOne a, GMany b)  -> GMany (AltUnbiased a b)
      (GMany a, GOne b)  -> GMany (AltUnbiased a b)
      (GMany a, GMany b) -> GMany (AltUnbiased a b)

extractOne :: AltTree ZipGrammar -> Maybe ZipGrammar
extractOne t = case t of
  AltLeaf z -> Just z
  AltBiased t1 t2 ->
    let a1 = extractOne t1
        a2 = extractOne t2 in
    case (a1, a2) of
      (Nothing, Nothing) -> Nothing
      (Just z, Nothing) -> Just z
      (Nothing, Just z) -> Just z
      _ -> error "broken invariant"
  AltUnbiased t1 t2 ->
    let a1 = extractOne t1
        a2 = extractOne t2 in
    case (a1, a2) of
      (Nothing, Nothing) -> Nothing
      (Just z, Nothing) -> Just z
      (Nothing, Just z) -> Just z
      _ -> error "broken invariant"


deriveOneByte :: Module -> ZipGrammar -> Maybe (AltTree (CharSet, ZipGrammar))
deriveOneByte modl gram =
  deriveGo gram
  where
  deriveGo :: ZipGrammar -> Maybe (AltTree (CharSet, ZipGrammar))
  deriveGo gr@(ZipNode {focus = g, path = pth}) =
    case g of
      Pure _ -> deriveUp gr
      Do_ {}   -> deriveGo (moveLeft gr)
      Do  {}   -> deriveGo (moveLeft gr)
      Let {}   -> deriveGo (moveLeft gr)
      Match {} -> case deriveMatch gr of -- fmap (\ x -> [x]) $ deriveMatch gr
        Nothing -> Nothing
        Just c ->  Just (AltLeaf c)
      Annot {} -> deriveGo (moveLeft gr)
      Call name []  ->
        case getGrammarDef modl name of
          Nothing -> Nothing
          Just gram1 -> deriveGo (gr {focus = gram1 })
      OrBiased g1 g2 ->
        let g1' = deriveGo (ZipNode { focus = g1, path = mkPathGrammar ZOrBiased  pth })
            g2' = deriveGo (ZipNode { focus = g2, path = mkPathGrammar ZOrBiased  pth }) in
        case (g1', g2') of
          (Nothing, _) -> Nothing
          (_, Nothing) -> Nothing
          (Just a1, Just a2) -> Just (AltBiased a1 a2)
      OrUnbiased g1 g2 ->
        let g1' = deriveGo (ZipNode { focus = g1, path = mkPathGrammar ZOrUnbiased  pth })
            g2' = deriveGo (ZipNode { focus = g2, path = mkPathGrammar ZOrUnbiased pth }) in
        case (g1', g2') of
          (Nothing, _) -> Nothing
          (_, Nothing) -> Nothing
          (Just a1, Just a2) -> Just (AltUnbiased a1 a2)
      _        -> Nothing
  deriveGo (ZipLeaf {zmatch = zm, path = pth}) =
    case zm of
      ZMatchByte _          -> error "TODO should move up"
      ZMatchBytes (prev, next) orig ->
        case uncons next of
          Nothing -> error "TODO should move up"
          Just (w, rest) ->
            Just $ AltLeaf (CWord8 w, ZipLeaf { zmatch = ZMatchBytes (w : prev, rest) orig, path = pth})
      ZMatchEnd -> error "TODO END"

  deriveUp :: ZipGrammar -> Maybe (AltTree (CharSet, ZipGrammar))
  deriveUp (ZipNode {focus = g, path = pth}) = case pth of
    [] -> error "" -- TODO AltDone gr
    k : ks -> case k of
      ZMatch _   -> error "broken invariant"
      ZDo_ g2    -> deriveGo (ZipNode g2 (ZDo_2 g : ks))
      ZDo_2 g1   -> deriveUp (ZipNode (Do_ g1 g) ks)
      ZDo na g2  -> deriveGo (ZipNode g2 (ZDo2 na g : ks))
      ZDo2 na g1 -> deriveUp (ZipNode (Do na g1 g) ks)
      ZLet na ex -> deriveUp (ZipNode (Let na ex g) ks)
      ZAnnot an  -> deriveUp (ZipNode (Annot an g) ks)
      ZOrBiased   -> Nothing
      ZOrUnbiased -> Nothing
  deriveUp (ZipLeaf {}) = error "broken invariant"

  convArrayToByteString :: [Expr] -> ByteString
  convArrayToByteString lst =
    Data.ByteString.pack (map f lst)
    where
    f (Ap0 (IntL n (TUInt (TSize 8)))) =
      if 0 <= n && n <= 255
      then fromInteger n
      else error "error converting Integer to Word8"
    f _ = error "error converting Expr to Word8"

  deriveMatch :: ZipGrammar -> Maybe (CharSet, ZipGrammar)
  deriveMatch (ZipNode { focus = Match sem match, path = pth}) =
    let newPath = mkPathGrammar (ZMatch sem) pth in
    case match of
      MatchByte b ->
        do s <- fromByteSetToSet modl b
           let x = ZipLeaf{ zmatch = ZMatchByte b, path = newPath}
           return (CByteSet s, x)
      MatchBytes b ->
        case b of
          Ap0 (ByteArrayL bs) ->
            case uncons bs of
              Nothing -> error "TODO should move up"
              Just (w, rest) ->
                Just (CWord8 w, ZipLeaf { zmatch = ZMatchBytes ([w], rest) bs, path = newPath})
          Ap1 ArrayLen _ -> Nothing
          Ap1 _ _        -> Nothing
          Ap0 _          -> Nothing
          ApN (ArrayL (TUInt (TSize 8))) arr  ->
            let bs = convArrayToByteString arr in
            case uncons bs of
              Nothing -> error "TODO should move up"
              Just (w, rest) ->
                Just (CWord8 w, ZipLeaf { zmatch = ZMatchBytes ([w], rest) bs, path = newPath})
          _ -> Nothing
      _ -> Nothing -- TODO: replace this with an error, or look into it
  deriveMatch _ = error "function should be applied to Match"

{- END of AltTree -}


data Deriv =
    DerivStart      (AltTree ZipGrammar)
  | DerivUnResolved [(Integer, AltTree (Either ZipGrammar ()))] -- disjoing set of integers in list
  | DerivResolved   [(Integer, Resolution)]

data Resolution =
    YesResolvedZero
  | YesResolvedOne  ZipGrammar
  | YesResolvedMany (AltTree ZipGrammar)
  | NoResolved      Deriv


checkUnambiguousOrDone :: Deriv -> Bool
checkUnambiguousOrDone der =
  case der of
    DerivStart {} -> False
    DerivUnResolved {} -> False
    DerivResolved lst ->
      foldr
      (\ (_a, r) b -> b && case r of
                             YesResolvedZero   -> True
                             YesResolvedOne  _ -> True
                             YesResolvedMany _ -> True
                             NoResolved d -> checkUnambiguousOrDone d
      ) True lst

checkFullyDeterminized :: Deriv -> Bool
checkFullyDeterminized der =
  case der of
    DerivStart {} -> False
    DerivUnResolved {} -> False
    DerivResolved lst ->
      foldr
      (\ (_a, r) b -> b && case r of
                             YesResolvedZero   -> True
                             YesResolvedOne  _ -> True
                             YesResolvedMany _ -> False
                             NoResolved d -> checkFullyDeterminized d
      ) True lst

getDepth :: Deriv -> Int
getDepth der =
  go der
  where
  go d =
    case d of
      DerivStart {} -> error "getDepth on starting derivation"
      DerivUnResolved {} -> error "getDepth on not Resolved"
      DerivResolved lst ->
        foldr
        (\ (_a, r) b -> max b (case r of
                                YesResolvedZero   -> 0
                                YesResolvedOne  _ -> 1
                                YesResolvedMany _ -> 1
                                NoResolved d1 -> 1 + go d1)
        ) 0 lst

gLLkDEPTH :: Int
gLLkDEPTH = 10

determinize :: Module -> Grammar -> Grammar
determinize modl grammar =
  let ty = typeOf grammar in
  repeatStep ty (DerivStart $ AltLeaf (initZipGrammar grammar)) 0

  where
  repeatStep :: Type -> Deriv -> Int -> Grammar
  repeatStep ty der depth =
    if depth > gLLkDEPTH
    then grammar
    else let mder1 = mapStep der in
      case mder1 of
        Nothing -> grammar
        Just der1 ->
          if checkUnambiguousOrDone der1
          then translateToCaseDeriv ty der1
          else repeatStep ty der1 (depth + 1)


  applyCharToAltTree ::
    Integer -> AltTree (CharSet, ZipGrammar) -> AltTree (Either ZipGrammar ())
  applyCharToAltTree c t =
    fmap (\ (cset, z) -> if memberCharSet c cset then Left z else Right ()) t

  stepDerivFactor :: AltTree (ZipGrammar) -> Maybe Deriv
  stepDerivFactor t =
    do tStep <- deriveOneByteOnAltTree t
       let charSet = Set.toList $ foldAlt (\ (a,_) b -> Set.union (fromCharSetToSet a) b) Set.empty tStep
       let tStepApplied = map (\ c -> (c, applyCharToAltTree c tStep)) charSet
       return $ DerivUnResolved tStepApplied

  mapStep :: Deriv -> Maybe Deriv
  mapStep (DerivStart orLst) = stepDerivFactor orLst
  mapStep (DerivUnResolved opts) = do
    der <- forM opts
      (\ (c, orLst) ->
          let g = garbageCollect orLst in
          case g of
            GZero  -> return (c, YesResolvedZero)
            GOne t ->
              case extractOne t of
                Nothing -> error "broken Invariant"
                Just z -> return (c, YesResolvedOne z)
            GMany t ->
              let mepsAlt = mapAlt (\ z -> fmap (\ x -> AltLeaf x) (goNextLeaf c z)) t in -- epsilon transition
              case mepsAlt of
                Nothing -> return (c, YesResolvedMany t) -- stop deriving
                Just epsAlt ->
                  let mDerLst = stepDerivFactor epsAlt in
                  case mDerLst of
                    Nothing -> return (c, YesResolvedMany t) -- stop deriving
                    Just derLst -> return (c, NoResolved derLst)
      )
    return $ DerivResolved der

  mapStep (DerivResolved opts) = do
      der <- forM opts
        (\ x@(c, l) ->
          case l of
            YesResolvedZero  -> Just x
            YesResolvedOne _ -> Just x
            YesResolvedMany _ -> Just x
            NoResolved d ->
              do n <- mapStep d
                 return (c, NoResolved n)
        )
      return $ DerivResolved der

  deriveOneByteOnAltTree :: AltTree ZipGrammar -> Maybe (AltTree (CharSet, ZipGrammar))
  deriveOneByteOnAltTree t =
    mapAlt (deriveOneByte modl)  t

  {-
  factorize :: [(Set Integer, ZipGrammar)] -> [(Set Integer, [ZipGrammar])]
  factorize lst =
    factorizeGo lst []
    where
      factorizeGo :: [(Set Integer, ZipGrammar)] -> [(Set Integer, [ZipGrammar])] -> [(Set Integer, [ZipGrammar])]
      factorizeGo [] acc = acc
      factorizeGo ((s, g) : rs) acc =
        let newAcc = insertInResult (s,g) acc in
        factorizeGo rs newAcc

      insertInResult :: (Set Integer, ZipGrammar) -> [(Set Integer, [ZipGrammar])] -> [(Set Integer, [ZipGrammar])]
      insertInResult   (s, g) [] = [(s,[g])]
      insertInResult x@(s, g) ((y@(s1, gs)) : rest) =
        let inter = Set.intersection s s1 in
        if Set.null inter
        then y : insertInResult x rest
        else
          let diff1 = Set.difference s s1 in
          let diff2 = Set.difference s1 s in
          case (Set.null diff1, Set.null diff2) of
            (False, False) ->
              -- NNNNNNNNN
              --     OOOOOOOOOO
              let notNew = (diff2, gs) in
              let new = (inter, gs ++ [g]) in
              notNew : new : insertInResult (diff1, g) rest
            (False, True) ->
              -- NNNNNNNNN
              --    OOOOO
              let new = (inter, gs ++ [g]) in
              new : insertInResult (diff1, g) rest
            (True, False) ->
              --  NNNNN
              --  OOOOOOOOO
              let notNew = (diff2, gs) in
              let new = (inter, gs ++ [g]) in
              notNew : new : rest
            (True, True) ->
              --  NNNNN
              --  OOOOO
              let new = (inter, gs ++ [g]) in
              new : rest
  -}

  translateToCaseDeriv :: Type -> Deriv -> Grammar
  translateToCaseDeriv ty der =
    let guid1 = firstValidGUID in
    let depth = getDepth der in
    let isFullyDet = if checkFullyDeterminized der then " Fully" else "" in
    Annot (SrcAnnot $ Data.Text.pack ("DETERMINIZE " ++ show depth ++ isFullyDet)) $
      translateToCase ty guid1 der

  translateToCase :: Type -> GUID -> Deriv -> Grammar
  translateToCase _ _ (DerivStart {})         = error "impossible"
  translateToCase _ _ (DerivUnResolved {})    = error "impossible"
  translateToCase ty guid (DerivResolved lst) =
    translateList ty guid lst

  translateList :: Type -> GUID -> [(Integer, Resolution)] -> Grammar
  translateList ty guid lst =
    let name = Name { nameId = guid,
                      nameText = Nothing,
                      nameType = TUInt (TSize 8)
                    } in
    Do name (Match SemYes (MatchByte SetAny))
      (GCase (Case name
        ((map
            (\ (c, g1) ->
                case g1 of
                  YesResolvedZero -> (PNum c, Fail ErrorFromSystem ty Nothing)
                  YesResolvedOne zg -> buildCase (c, zg)
                  YesResolvedMany t ->
                    let newOr = buildOr c t
                    in (PNum c, newOr)
                  NoResolved der ->
                    let newG1 = translateToCase ty (succGUID guid) der
                    in (PNum c, newG1))
            lst
          ) ++ [ (PAny, Fail ErrorFromSystem ty Nothing) ]
        )))

    where
    buildCase :: (Integer, ZipGrammar) -> (Pattern, Grammar)
    buildCase (c, g1) = (PNum c, buildLeaf c g1)

    buildOr :: Integer -> AltTree ZipGrammar -> Grammar
    buildOr c t =
      let mtres = go t in
      case mtres of
        Just tres -> tres
        Nothing -> error "broken invariant"
      where
      go tr =
        let ann = SrcAnnot $ Data.Text.pack ("OR RECONSTRUCTION") in
        case tr of
          AltLeaf x -> Just $ buildLeaf c x
          AltBiased t1 t2 ->
            let a1 = go t1
                a2 = go t2 in
            case (a1, a2) of
              (Nothing, Nothing) -> Nothing
              (Just _, Nothing) -> a1
              (Nothing, Just _) -> a2
              (Just b1, Just b2) -> Just (Annot ann $ OrBiased b1 b2)
          AltUnbiased t1 t2 ->
            let a1 = go t1
                a2 = go t2 in
            case (a1, a2) of
              (Nothing, Nothing) -> Nothing
              (Just _, Nothing) -> a1
              (Nothing, Just _) -> a2
              (Just b1, Just b2) -> Just (Annot ann $ OrUnbiased b1 b2)



    -- TODO: is this not redundant with goNextLeaf ????
    buildLeaf :: Integer -> ZipGrammar -> Grammar
    buildLeaf c (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch sem : pth}) =
      case sem of
        SemYes ->
          let newBuilt = Pure (Ap0 (IntL c (TUInt (TSize 8)))) in
          buildUp (mkZipGrammar newBuilt pth)
        SemNo ->
          let newBuilt = Pure (Ap0 Unit) in
           buildUp (mkZipGrammar newBuilt pth)
    buildLeaf c (ZipLeaf{ zmatch = ZMatchBytes (zpast, rest) orig, path = ZMatch sem : pth}) =
      if not (c == (toInteger $ head zpast))
      then error "broken invariant"
      else
        let pureMatch = Pure (Ap0 (ByteArrayL orig)) in
        if Data.ByteString.null rest
        then
          case sem of
            SemYes ->
              let newBuilt = pureMatch in
              buildUp (mkZipGrammar newBuilt pth)
            SemNo ->
              let newBuilt = Pure (Ap0 Unit) in
              buildUp (mkZipGrammar newBuilt pth)
        else
          let matchRest = Match SemNo (MatchBytes (Ap0 (ByteArrayL rest))) in
          case sem of
            SemYes ->
              let newBuilt = Do_ matchRest pureMatch in
              buildUp (mkZipGrammar newBuilt pth)
            SemNo ->
              let newBuilt = matchRest in
              buildUp (mkZipGrammar newBuilt pth)
    buildLeaf _ _ = error "Should not happen"

