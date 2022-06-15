{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
{-# Language DeriveFunctor, DeriveFoldable #-}
module Daedalus.Core.Determinize (determinizeModule) where

-- import Debug.Trace(trace)
-- import Daedalus.PP(pp, showPP)

import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as Set
import MonadLib
import Data.Word(Word8)
import Data.ByteString(ByteString, uncons, null, pack)
import Data.Foldable(foldl')

import Daedalus.Panic (panic)
import Daedalus.GUID(GUID, firstValidGUID, succGUID)

import Daedalus.Core.Decl
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar
    ( Grammar(..), Match(MatchByte, MatchBytes), Sem(..)
    , ErrorSource (ErrorFromSystem)
    , mapChildrenG, gBinAnnotate )
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
           OrBiased g1 g2   ->
            case determinize modl g of
              Just g3 -> g3
              Nothing -> OrBiased (go g1) (go g2)
           OrUnbiased g1 g2 ->
            case determinize modl g of
              Just g3 -> g3
              Nothing -> OrUnbiased (go g1) (go g2)
           _             -> mapChildrenG go g

getGrammar :: Module -> FName -> Maybe (Grammar, [Name])
getGrammar modl name =
  goGetGrammar (mGFuns modl)
  where
  goGetGrammar [] = panic "getGrammar" ["Unable to get grammar"]
  goGetGrammar ( Fun {fName = n1, fDef = fdef, fParams = params} : rest ) =
    if fnameId n1 == fnameId name
    then case fdef of
           Def g2 -> Just (g2, params)
           External -> Nothing
    else goGetGrammar rest

getByteSet :: Module -> FName -> Maybe ByteSet
getByteSet modl name =
  goGetByteSet (mBFuns modl)
  where
  goGetByteSet [] = panic "getByteSet" ["cannot find byteset"]
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

  -- the pair is the zipper of a ByteString
  -- the last ByteString is the original one in the grammar
  | ZMatchBytes ([Word8], ByteString) ByteString
  | ZMatchEnd

data ZGrammar =
    ZMatch Sem
  | ZDo_ Grammar
  | ZDo_2 Grammar
  | ZDo Name Grammar
  | ZDo2 Name Grammar
  | ZLet Name Expr
  | ZCall FName [Name] [Expr]
  | ZAnnot Annot
  | ZOrBiased
  | ZOrUnbiased

type ZPath = [ ZGrammar ]

data ZipGrammar =
    ZipNode { focus :: Grammar, path :: ZPath }
  | ZipLeaf { zmatch :: ZMatch, path :: ZPath }

emptyZPath :: ZPath
emptyZPath = []

mkZPath :: ZGrammar -> ZPath -> ZPath
mkZPath n z = n : z

initZipGrammar :: Grammar -> ZipGrammar
initZipGrammar gram = ZipNode {focus = gram, path = emptyZPath}

mkZipGrammar :: Grammar -> ZPath -> ZipGrammar
mkZipGrammar g p = ZipNode g p

-- moves one step left
moveLeft :: ZipGrammar -> ZipGrammar
moveLeft (ZipNode {focus = foc, path = pth}) =
  case foc of
    Do_ g1 g2     -> ZipNode { focus = g1, path = mkZPath (ZDo_ g2) pth }
    Do name g1 g2 -> ZipNode { focus = g1, path = mkZPath (ZDo name g2) pth }
    Annot ann g1  -> ZipNode { focus = g1, path = mkZPath (ZAnnot ann) pth }
    Let name e g1 -> ZipNode { focus = g1, path = mkZPath (ZLet name e) pth }
    _ -> panic "moveLeft" [ "broken invariant, unexpected case" ]
moveLeft (ZipLeaf {}) = panic "moveLeft" [ "broken invariant, unexpected case" ]

convertCall2Let :: Grammar -> [Name] -> [Expr] -> Grammar
convertCall2Let g ps es =
  case (ps, es) of
    ([p], [e]) -> Annot (SrcAnnot $ Data.Text.pack ("DETCALL ")) $ Let p e g
    (p:pps, e:ees) ->
      let sube = convertCall2Let g pps ees in
      Let p e sube
    _ -> panic "convertCall2Let" ["Call has mismatched params/args"]

-- moves all the way up to the root of the Grammar
moveUp :: ZipGrammar -> Grammar
moveUp (ZipNode {focus = built, path = pth}) =
  case pth of
    [] -> built
    (ZDo_ g2 : z) ->
      let newBuilt = Do_ built g2 in
      moveUp (mkZipGrammar newBuilt z)
    (ZDo_2 g1 : z) ->
      let newBuilt = Do_ g1 built in
      moveUp (mkZipGrammar newBuilt z)
    (ZDo name g2 : z) ->
      let newBuilt = Do name built g2 in
      moveUp (mkZipGrammar newBuilt z)
    (ZDo2 name g1 : z) ->
      let newBuilt = Do name g1 built in
      moveUp (mkZipGrammar newBuilt z)
    (ZLet name e : z) ->
      let newBuilt = Let name e built in
      moveUp (mkZipGrammar newBuilt z)
    (ZCall _na params le : z) ->
      moveUp (mkZipGrammar (convertCall2Let built params le) z)
    (ZAnnot ann : z) ->
      moveUp (mkZipGrammar (Annot ann built) z)
    (ZOrBiased : z) ->
      moveUp (mkZipGrammar built z)
    (ZOrUnbiased : z) ->
      moveUp (mkZipGrammar built z)
    _  -> panic "moveUp" [ "broken invariant, should not happen on this case" ]
moveUp (ZipLeaf {}) = panic "moveUp" [ "broken invariant, should not happen on a leaf" ]

-- moves up until it can finds a spot to the left
findUpRight :: ZipGrammar -> Maybe ZipGrammar
findUpRight (ZipNode {focus = foc, path = pth}) =
  case pth of
    [] -> Nothing
    (ZDo_ g2 : z) ->
      let newPath = ZDo_2 foc : z in
      Just (mkZipGrammar g2 newPath)
    (ZDo_2 g1 : z) ->
      findUpRight (mkZipGrammar (Do_ g1 foc) z)
    (ZDo name g2 : z) ->
      let newPath = ZDo2 name foc : z in
      Just (mkZipGrammar g2 newPath)
    (ZDo2 name g1 : z) ->
      findUpRight (mkZipGrammar (Do name g1 foc) z)
    (ZAnnot ann : z) ->
      findUpRight (mkZipGrammar (Annot ann foc) z)
    (ZLet name e : z) ->
      findUpRight (mkZipGrammar (Let name e foc) z)
    (ZCall _na params le : z) ->
      findUpRight (mkZipGrammar (convertCall2Let foc params le) z)
    (ZOrBiased : _)   -> Nothing
    (ZOrUnbiased : _) -> Nothing
    _  -> panic "findUpRight" [ "broken invariant, unexpected case" ]
findUpRight (ZipLeaf{}) = panic "findUpRight" [ "broken invariant, unexpected case" ]

buildMatchByte :: Set Integer -> GUID -> Grammar
buildMatchByte c guid =
  if ((Set.size c) == 1)
  then let char = head (Set.toList c) in
       Pure (Ap0 (IntL char (TUInt (TSize 8))))
  else Pure (Var (mkNameFromGUID guid))


findNextLeaf :: Set Integer -> GUID -> ZipGrammar -> Maybe ZipGrammar
findNextLeaf c guid (ZipLeaf { zmatch = ZMatchByte _, path = ZMatch sem : pth}) =
  case sem of
    SemYes ->
      let newBuilt = buildMatchByte c guid in
      findUpRight (mkZipGrammar newBuilt pth)
    SemNo ->
      let newBuilt = Pure (Ap0 Unit) in
      findUpRight (mkZipGrammar newBuilt pth)
findNextLeaf _c _guid zg@(ZipLeaf{ zmatch = (ZMatchBytes (_, rest) orig), path = (ZMatch sem : pth)}) =
  if not (Data.ByteString.null rest)
  then Just $ zg
  else
    case sem of
      SemYes ->
        let pureMatch = Pure (Ap0 (ByteArrayL orig)) in
        findUpRight (mkZipGrammar pureMatch pth)
      SemNo ->
        let newBuilt = Pure (Ap0 Unit) in
        findUpRight (mkZipGrammar newBuilt pth)
findNextLeaf _ _ _ = panic "findNextLeaf" [ "broken invariant, on structure Zipped grammar" ]

buildLeaf :: (Set Integer, GUID) -> ZipGrammar -> Grammar
buildLeaf (c, guid) (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch sem : pth}) =
  case sem of
    SemYes ->
      let newBuilt = buildMatchByte c guid in
      moveUp (mkZipGrammar newBuilt pth)
    SemNo ->
      let newBuilt = Pure (Ap0 Unit) in
      moveUp (mkZipGrammar newBuilt pth)
buildLeaf _c (ZipLeaf{ zmatch = ZMatchBytes (_zpast, rest) orig, path = ZMatch sem : pth}) =
  let pureMatch = Pure (Ap0 (ByteArrayL orig)) in
  if Data.ByteString.null rest
  then
    case sem of
      SemYes ->
        let newBuilt = pureMatch in
        moveUp (mkZipGrammar newBuilt pth)
      SemNo ->
        let newBuilt = Pure (Ap0 Unit) in
        moveUp (mkZipGrammar newBuilt pth)
  else
    let matchRest = Match SemNo (MatchBytes (Ap0 (ByteArrayL rest))) in
    case sem of
      SemYes ->
        let newBuilt = Do_ matchRest pureMatch in
        moveUp (mkZipGrammar newBuilt pth)
      SemNo ->
        let newBuilt = matchRest in
        moveUp (mkZipGrammar newBuilt pth)
buildLeaf _ _ = panic "buildLeaf" [ "Broken invariant, unexpected case" ]


{- END of Zipped Grammar -}

data CharSet =
    CByteSet (Set Integer)
  | CWord8 Word8

fromCharSetToSet :: CharSet -> Set Integer
fromCharSetToSet c =
  case c of
    CByteSet s -> s
    CWord8 w -> Set.singleton $ fromIntegral w

fromByteSet2CharSet :: Module -> ByteSet -> Maybe CharSet
fromByteSet2CharSet modl bs = do
  cs <- go bs
  return $ CByteSet cs
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
        s1 <- getByteSet modl name
        go s1
      _      -> Nothing

{- END of CharSet -}


data AltTree a =
    AltLeaf     a
  | AltBiased   (AltTree a) (AltTree a)
  | AltUnbiased (AltTree a) (AltTree a)
    deriving (Functor,Foldable)

-- The three following types correspond to a progression in the determinization algorithm.
-- 1) It starts from `BasicAltTree`,
-- 2) after applying one byte of derivation we get to `DerivAltTree`
-- 3) and finally we factorize the subset of inputs that lead to the same AltTree
type BasicAltTree  = AltTree ZipGrammar
type DerivAltTree  = AltTree (CharSet, ZipGrammar)
type FactorAltTree = AltTree (Set Integer, ZipGrammar)

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
foldAlt reduce = foldl' (flip reduce)

data GC a =
    GZero
  | GOne  a
  | GMany a

-- This function eliminates the dead branches and wraps the result into a type that
-- indicates how many branches are alive.
garbageCollect :: FactorAltTree -> GC (FactorAltTree)
garbageCollect t = case t of
  AltLeaf (s, _) ->
    if Set.null s
    then GZero
    else GOne t
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

-- This function is supposed to be called after `garbageCollect`
extractOne :: FactorAltTree -> ZipGrammar
extractOne t = case t of
  AltLeaf (_, z) -> z
  _ -> panic "extractOne" [ "broken invariant, should be called with only one option left in tree" ]

-- This functions derives one byte on a zipped grammar and returns maybe a DerivAltTree
deriveOneByte :: Module -> ZipGrammar -> Maybe (DerivAltTree)
deriveOneByte modl gram =
  deriveGo gram
  where
  deriveGo :: ZipGrammar -> Maybe (DerivAltTree)
  deriveGo gr@(ZipNode {focus = g, path = pth}) =
    case g of
      Pure _   -> deriveUp gr
      Do_ {}   -> deriveGo (moveLeft gr)
      Do  {}   -> deriveGo (moveLeft gr)
      Let {}   -> deriveGo (moveLeft gr)
      Annot {} -> deriveGo (moveLeft gr)
      Match {} -> case deriveMatch gr of
        Nothing -> Nothing
        Just c ->  Just (AltLeaf c)
      Call name []  ->
        case getGrammar modl name of
          Nothing -> Nothing
          Just (gram1, _) -> deriveGo (gr {focus = gram1 })
      Call name args  ->
        case getGrammar modl name of
          Nothing -> Nothing
          Just (gram1, params) ->
            deriveGo (gr {focus = gram1, path = mkZPath (ZCall name params args) pth })
      OrBiased g1 g2 ->
        let g1' = deriveGo (ZipNode { focus = g1, path = mkZPath ZOrBiased pth })
            g2' = deriveGo (ZipNode { focus = g2, path = mkZPath ZOrBiased pth }) in
        case (g1', g2') of
          (Nothing, _) -> Nothing
          (_, Nothing) -> Nothing
          (Just a1, Just a2) -> Just (AltBiased a1 a2)
      OrUnbiased g1 g2 ->
        let g1' = deriveGo (ZipNode { focus = g1, path = mkZPath ZOrUnbiased pth })
            g2' = deriveGo (ZipNode { focus = g2, path = mkZPath ZOrUnbiased pth }) in
        case (g1', g2') of
          (Nothing, _) -> Nothing
          (_, Nothing) -> Nothing
          (Just a1, Just a2) -> Just (AltUnbiased a1 a2)
      _        -> Nothing
  deriveGo (ZipLeaf {zmatch = zm, path = pth}) =
    case zm of
      ZMatchByte _  -> panic "deriveGo" [ "Broken invariant, unexpected case" ]
      ZMatchBytes (prev, next) orig ->
        case uncons next of
          Nothing -> panic "deriveGo" [ "Broken invariant, unexpected case" ]
          Just (w, rest) ->
            Just $ AltLeaf (CWord8 w, ZipLeaf { zmatch = ZMatchBytes (w : prev, rest) orig, path = pth})
      ZMatchEnd -> panic "deriveGo" [ "Broken invariant, unhandled case END" ]

  deriveUp :: ZipGrammar -> Maybe (DerivAltTree)
  deriveUp (ZipNode {focus = g, path = pth}) = case pth of
    [] -> Nothing
    k : ks -> case k of
      ZMatch _   -> panic "deriveUp" [ "Broken invariant, unexpected match" ]
      ZDo_ g2    -> deriveGo (ZipNode g2 (ZDo_2 g : ks))
      ZDo_2 g1   -> deriveUp (ZipNode (Do_ g1 g) ks)
      ZDo na g2  -> deriveGo (ZipNode g2 (ZDo2 na g : ks))
      ZDo2 na g1 -> deriveUp (ZipNode (Do na g1 g) ks)
      ZLet na ex -> deriveUp (ZipNode (Let na ex g) ks)
      ZCall _na params le -> deriveUp (ZipNode (convertCall2Let g params le) ks)
      ZAnnot an  -> deriveUp (ZipNode (Annot an g) ks)
      ZOrBiased   -> Nothing
      ZOrUnbiased -> Nothing
  deriveUp (ZipLeaf {}) = panic "deriveUp" [ "broken invariant, unexpected case" ]


  convArrayToByteString :: [Expr] -> Maybe ByteString
  convArrayToByteString lst = do
      r <- forM lst (\ e -> f e)
      return $ Data.ByteString.pack r
    where
    f (Ap0 (IntL n (TUInt (TSize 8)))) =
      if 0 <= n && n <= 255
      then Just $ fromInteger n
      else panic "convArrayToByteString" [ "Integer not in 8 bits" ]
    f _ = Nothing

  deriveMatch :: ZipGrammar -> Maybe (CharSet, ZipGrammar)
  deriveMatch (ZipNode { focus = Match sem match, path = pth}) =
    let newPath = mkZPath (ZMatch sem) pth in
    case match of
      MatchByte b ->
        do s <- fromByteSet2CharSet modl b
           let x = ZipLeaf{ zmatch = ZMatchByte b, path = newPath}
           return (s, x)
      MatchBytes b ->
        case b of
          Ap0 (ByteArrayL bs) ->
            case uncons bs of
              Nothing -> Nothing
              Just (w, rest) ->
                Just (CWord8 w, ZipLeaf { zmatch = ZMatchBytes ([w], rest) bs, path = newPath})
          Ap1 ArrayLen _ -> Nothing
          Ap1 _ _        -> Nothing
          Ap0 _          -> Nothing
          ApN (ArrayL (TUInt (TSize 8))) arr  -> do
            bs <- convArrayToByteString arr
            return $
              case uncons bs of
                Nothing -> panic "deriveMatch" [ "Broken invariant, uncons empty bytestring" ]
                Just (w, rest) ->
                  (CWord8 w, ZipLeaf { zmatch = ZMatchBytes ([w], rest) bs, path = newPath})
          _ -> Nothing
      _ -> Nothing
  deriveMatch _ = panic "deriveMatch" [ "Broken invariant, unexpected case" ]

{- END of AltTree -}


data Deriv =
    DerivStart      (GUID, BasicAltTree)

  -- there is an invariant that the `Set Integer` is the same as
  -- the Sets in `FactorAltTree` if this set is not empty
  -- and the sets of integers are disjoint
  | DerivUnResolved (GUID, [(Set Integer, FactorAltTree)])
  | DerivResolved   (GUID, [(Set Integer, Resolution)])

data Resolution =
    YesResolvedZero
  | YesResolvedOne  ZipGrammar
  | YesResolvedMany FactorAltTree
  | NoResolved      Deriv


checkUnambiguousOrDone :: Deriv -> Bool
checkUnambiguousOrDone der =
  case der of
    DerivStart {} -> False
    DerivUnResolved {} -> False
    DerivResolved (_, lst) ->
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
    DerivResolved (_, lst) ->
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
      DerivStart {} -> panic "getDepth" [ "Broken invariant, unexpected case" ]
      DerivUnResolved {} -> panic "getDepth" [ "Broken invariant, unexpected case" ]
      DerivResolved (_, lst) ->
        foldr
        (\ (_a, r) b -> max b (case r of
                                YesResolvedZero   -> 0
                                YesResolvedOne  _ -> 1
                                YesResolvedMany _ -> 1
                                NoResolved d1 -> 1 + go d1)
        ) 0 lst

gLLkDEPTH :: Int
gLLkDEPTH = 10

mkNameFromGUID :: GUID -> Name
mkNameFromGUID guid =
  Name {
    nameId = guid,
    nameText = Nothing,
    nameType = TUInt (TSize 8)
  }


determinize :: Module -> Grammar -> Maybe Grammar
determinize modl grammar =
  let ty = typeOf grammar in
  repeatStep ty (DerivStart $ (firstValidGUID, AltLeaf (initZipGrammar grammar))) 0

  where
  repeatStep :: Type -> Deriv -> Int -> Maybe Grammar
  repeatStep ty der depth =
    if depth > gLLkDEPTH
    then Nothing
    else let mder1 = mapStep der in
      case mder1 of
        Nothing -> Nothing
        Just der1 ->
          if checkUnambiguousOrDone der1
          then Just $ translateToCaseDeriv ty der1
          else repeatStep ty der1 (depth + 1)

  deriveOneByteOnAltTree :: BasicAltTree -> Maybe (DerivAltTree)
  deriveOneByteOnAltTree t =
    mapAlt (deriveOneByte modl) t

  stepDerivFactor :: GUID -> BasicAltTree -> Maybe Deriv
  stepDerivFactor guid t =
    do tStep <- deriveOneByteOnAltTree t
       let aux = factorize tStep
       return $ DerivUnResolved (guid, aux)

  mapStep :: Deriv -> Maybe Deriv
  mapStep (DerivStart (guid, orLst)) = stepDerivFactor guid orLst
  mapStep (DerivUnResolved (guid, opts)) = do
    der <- forM opts
      (\ (c, orLst) ->
          let g = garbageCollect orLst in
          case g of
            GZero  -> return (c, YesResolvedZero)
            GOne t ->
              let z = extractOne t in
              return (c, YesResolvedOne z)
            GMany t ->
              let mepsAlt = mapAlt (\ (_, z) -> fmap (\ x -> AltLeaf x) (findNextLeaf c guid z)) t in -- epsilon transition
              case mepsAlt of
                Nothing -> return (c, YesResolvedMany t) -- stop deriving
                Just epsAlt ->
                  let mDerLst = stepDerivFactor (succGUID guid) epsAlt in
                  case mDerLst of
                    Nothing -> return (c, YesResolvedMany t) -- stop deriving
                    Just derLst -> return (c, NoResolved derLst)
      )
    return $ DerivResolved (guid, der)
  mapStep (DerivResolved (guid, opts)) = do
      der <- forM opts
        (\ x@(c, l) ->
          case l of
            YesResolvedZero   -> Just x
            YesResolvedOne _  -> Just x
            YesResolvedMany _ -> Just x
            NoResolved d ->
              do n <- mapStep d
                 return (c, NoResolved n)
        )
      return $ DerivResolved (guid, der)


  -- step1) find list of set of characters in derivation
  -- step2) convertListSet2DisjoinSet
  -- step3) applyDisjointSet2AltTree
  factorize :: DerivAltTree -> [(Set Integer, FactorAltTree)]
  factorize tree =
    let listSet = foldAlt (\ (a,_) b -> fromCharSetToSet a : b) [] tree in
    let disjointSets = convertListSet2DisjointSet listSet in
    applyDisjointSet2AltTree disjointSets tree

    where
      applyDisjointSet2AltTree ::
        [Set Integer] -> DerivAltTree -> [(Set Integer, FactorAltTree)]
      applyDisjointSet2AltTree ls t =
        map (\ s -> (s, applyInter s t)) ls

      applyInter :: Set Integer -> DerivAltTree -> FactorAltTree
      applyInter s t =
        case t of
          AltLeaf (c1, a) ->
            let s1 = fromCharSetToSet c1 in
            let inter = Set.intersection s s1 in
            if Set.isSubsetOf s s1 || Set.null inter
            then AltLeaf (inter, a)
            else panic "applyInter" ["Broken invariant. DisjointSets should be subset of original or absent"]
          AltBiased b1 b2 -> AltBiased (applyInter s b1) (applyInter s b2)
          AltUnbiased b1 b2 -> AltUnbiased (applyInter s b1) (applyInter s b2)

      convertListSet2DisjointSet :: [Set Integer] -> [Set Integer]
      convertListSet2DisjointSet lst =
        case lst of
          [] -> []
          s : ns -> insertInDisjointSet s (convertListSet2DisjointSet ns)

      insertInDisjointSet :: Set Integer -> [Set Integer] -> [Set Integer]
      insertInDisjointSet s [] = [s]
      insertInDisjointSet s (s1 : rest) =
        let inter = Set.intersection s s1 in
        if Set.null inter
        then s1 : insertInDisjointSet s rest
        else
          let diff1 = Set.difference s s1 in
          let diff2 = Set.difference s1 s in
          case (Set.null diff1, Set.null diff2) of
            (False, False) ->
              -- NNNNNNNNN
              --     OOOOOOOOOO
              let notNew = diff2 in
              let new = inter in
              notNew : new : insertInDisjointSet diff1 rest
            (False, True) ->
              -- NNNNNNNNN
              --    OOOOO
              let new = inter in
              new : insertInDisjointSet diff1 rest
            (True, False) ->
              --  NNNNN
              --  OOOOOOOOO
              let notNew = diff2 in
              let new = inter in
              notNew : new : rest
            (True, True) ->
              --  NNNNN
              --  OOOOO
              let new = inter in
              new : rest

  translateToCaseDeriv :: Type -> Deriv -> Grammar
  translateToCaseDeriv ty der =
    let depth = getDepth der in
    let isFullyDet = if checkFullyDeterminized der then " Fully" else "" in
    Annot (SrcAnnot $ Data.Text.pack ("DETERMINIZE " ++ show depth ++ isFullyDet)) $
      translateToCase ty der

  translateToCase :: Type -> Deriv -> Grammar
  translateToCase _ (DerivStart {})         =
    panic "translateToCase" [ "Broken invariant, unexpected case" ]
  translateToCase _ (DerivUnResolved {})    =
    panic "translateToCase" [ "Broken invariant, unexpected case" ]
  translateToCase ty (DerivResolved (guid, lst)) =
    translateList ty guid lst

  translateList :: Type -> GUID -> [(Set Integer, Resolution)] -> Grammar
  translateList ty guid lst =
    let name = mkNameFromGUID guid in
    if (length lst == 1 && (Set.size (fst (head lst)) == 256))
    then
      Do name (Match SemYes (MatchByte SetAny))
        (let (allchar, g1) = head lst in
         buildSubGramMultiple allchar g1)
    else
    if ((foldr (\ x accu -> x + accu) 0 (map (\ (s,_) -> Set.size s) lst)) == 256)
    then
      let maxpos = findMaxSet 0 0 0 lst in
      let (lst1, lst2) = splitAt maxpos lst in
      let (cmax, gmax) = head lst2 in
      let lst3 = tail lst2 in
      Do name (Match SemYes (MatchByte SetAny))
        (GCase (Case name
          ((map
              (\ (c, g1) -> (PNum c, buildSubGramSingle c g1))
              -- This explodes the sharing into all the characters in the set
              (concatMap (\ (s, r) -> map (\c -> (c, r)) (Set.toList s)) (lst1 ++ lst3))
            ) ++ [ (PAny, buildSubGramMultiple cmax gmax) ]
          )))
    else
      Do name (Match SemYes (MatchByte SetAny))
        (GCase (Case name
          ((map
              (\ (c, g1) ->(PNum c, buildSubGramSingle c g1))
              -- This explodes the sharing into all the characters in the set
              (concatMap (\ (s, r) -> map (\c -> (c, r)) (Set.toList s)) lst)
            ) ++ [ (PAny, Fail ErrorFromSystem ty Nothing) ]
          )))

    where

    buildSubGramSingle :: Integer -> Resolution -> Grammar
    buildSubGramSingle c g1 =
      case g1 of
        YesResolvedZero   -> Fail ErrorFromSystem ty Nothing
        YesResolvedOne zg -> buildLeaf (Set.singleton c, guid) zg
        YesResolvedMany t -> buildOr (Set.singleton c, guid) t
        NoResolved der    -> translateToCase ty der

    buildSubGramMultiple :: Set Integer -> Resolution -> Grammar
    buildSubGramMultiple allchar g1 =
      case g1 of
        YesResolvedZero   -> Fail ErrorFromSystem ty Nothing
        YesResolvedOne zg -> buildLeaf (allchar, guid) zg
        YesResolvedMany t -> buildOr (allchar, guid) t
        NoResolved der    -> translateToCase ty der

    findMaxSet :: Int -> Int -> Int -> [(Set Integer, Resolution)] -> Int
    findMaxSet _    pos _  [] = pos
    findMaxSet curr pos m ((s,_) : xs) =
      let (pos1, m1) = if Set.size s > m then (curr, Set.size s) else (pos, m) in
      findMaxSet (curr+1) pos1 m1 xs

    buildOr :: (Set Integer, GUID) -> AltTree (a, ZipGrammar) -> Grammar
    buildOr c t =
      let mtres = go t in
      case mtres of
        Just tres -> tres
        Nothing -> panic "buildOr" [ "Broken invariant, expected existing branch" ]
      where
      go tr =
        let ann = SrcAnnot $ Data.Text.pack ("OR RECONSTRUCTION") in
        case tr of
          AltLeaf (_, x) -> Just $ buildLeaf c x
          AltBiased t1 t2 ->
            let a1 = go t1
                a2 = go t2 in
            case (a1, a2) of
              (Nothing, Nothing) -> Nothing
              (Just _, Nothing) -> a1
              (Nothing, Just _) -> a2
              (Just b1, Just b2) ->
                Just (Annot ann (gBinAnnotate OrBiased b1 b2))
          AltUnbiased t1 t2 ->
            let a1 = go t1
                a2 = go t2 in
            case (a1, a2) of
              (Nothing, Nothing) -> Nothing
              (Just _, Nothing) -> a1
              (Nothing, Just _) -> a2
              (Just b1, Just b2) ->
                Just (Annot ann (gBinAnnotate OrUnbiased b1 b2))

